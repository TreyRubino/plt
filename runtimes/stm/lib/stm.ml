(** @file stm.ml
    @brief a transactional variable is a record containing the committed value,
    a monotonically increasing version counter, and a unique integer id
    handed out at allocation. every successful write to a tvar bumps its
    version. the version is what lets a transaction notice that something
    it read has been replaced underneath it.

    inside [atomically] we install an effect handler that intercepts
    [Read] and [Write] performs and routes them through a per-attempt
    hash table that holds both the read-set and the write-set, keyed by
    each tvar's id. the body never touches the tvars directly while it
    is running, so two transactions can race through their bodies
    without interfering. the only synchronized region is the commit
    itself, hidden inside the runtime. business logic in [demo.ml] never
    acquires a lock. 

    @author Trey Rubino
    @date 05/02/2026 *)

(** @brief the transaction value. [value] is the last committed value, 
    [version] is bumped on every commit that writes to this tvar, and [id] 
    is a stable unique integer used as the hash-table key inside transactions.
    [value] and [version] live behind the runtime's commit lock so
    readers during validation always see a consistent pair. *)
type 'a tvar = {
  id              : int;
  mutable value   : 'a;
  mutable version : int;
}

(** @brief these are the two operations a transaction can perform on a tvar.
    the handler in [atomically] is what gives them meaning. without a
    handler, performing one of these is an error, which is exactly the
    guarantee we want, transactional ops only make sense inside a
    transaction. *)
type _ Effect.t +=
  | Read  : 'a tvar -> 'a Effect.t
  | Write : 'a tvar * 'a -> unit Effect.t

(*a process-wide counter used to hand out new tvar ids. *)
let next_id = Atomic.make 0

(* make a new tvar *)
let make v =
  { 
    id = Atomic.fetch_and_add next_id 1; 
    value = v; 
    version = 0 
  }

(** @brief transactional read wrapper. just performs the [Read] effect.
    @param t the tvar to read.
    @return the value the handler decides to resume the body with. *)
let read t = Effect.perform (Read t)

(** @brief transactional write wrapper. just performs the [Write] effect
    @param t the tvar to update.
    @param v the value to publish on commit. *)
let write t v = Effect.perform (Write (t, v))

(** @brief one global mutex serialises the commit phase across all threads.
    bodies still run concurrently, this only protects validate+publish
    so a transaction cannot be invalidated halfway through its own
    commit. business logic never sees this lock, it lives entirely inside
    [atomically]. *)
let commit_lock = Mutex.create ()

(** @brief a log entry exists per tvar that the transaction has touched.
    [snapshot] is the value we sampled on first, which is what
    the body sees for subsequent reads if no write has happened yet.
    [witnessed] is the version we sampled at the same moment, which is
    what validation checks. [pending] holds a buffered write, if any.
    we hide the element type behind an existential so the table can mix
    tvars of different types under one uniform value type. *)
type 'a log_record = {
  tvar             : 'a tvar;
  witnessed        : int;
  snapshot         : 'a;
  mutable pending  : 'a option;
}

type entry = Entry : 'a log_record -> entry

(** @brief run a function as an atomic transaction.
    the runner installs an effect handler around [body], collects reads
    and writes into a hash-table log keyed by tvar id, validates the
    read-set under the commit lock, publishes the write-set on success,
    and re-runs [body] from scratch on conflict.
    @param body thunk holding the transactional code. https://en.wikipedia.org/wiki/Thunk
    @return the result of the [body] run that ultimately committed. *)
let atomically (type a) (body : unit -> a) : a =
  (* attempt counter is shared across retries so the log message can
     show which attempt actually succeeded. *)
  let attempts = ref 0 in
  let rec attempt () =
    incr attempts;
    (* this is the per-attempt log. keys are tvar ids, values are 
       entries that hide each tvar's element type *)
    let log : (int, entry) Hashtbl.t = Hashtbl.create 8 in

    (* look up the entry for this tvar, or create one by sampling its
       current value and version under the commit lock *)
    let touch (type b) (t : b tvar) : entry =
      match Hashtbl.find_opt log t.id with
      | Some e -> e
      | None ->
          Mutex.lock commit_lock;
          let snap = t.value in
          let ver  = t.version in
          Mutex.unlock commit_lock;
          let e = Entry {
            tvar      = t;
            witnessed = ver;
            snapshot  = snap;
            pending   = None;
          } in
          Hashtbl.add log t.id e;
          e
    in

    (* do the read inside the transaction, if a pending write exists, return that
       so the body observes its own writes. otherwise return the cached
       snapshot, which keeps the body's view of memory stable even if
       other threads are committing in parallel. *)
    let do_read (type b) (t : b tvar) : b =
      match touch t with
      | Entry r ->
          let v = match r.pending with
            | Some v -> v
            | None   -> r.snapshot
          in
          (* the existential hides the element type, but [r.tvar == t]
             by construction, so this cast is still safe.  *) 
          (Obj.magic v : b)
    in

    (* do the write inside a transaction. nothing global changes yet, the new
       value just lands in [pending] for later publication. *)
    let do_write (type b) (t : b tvar) (v : b) : unit =
      match touch t with
      | Entry r -> r.pending <- Some (Obj.magic v)
    in

    (* run the body under the effect handler. the handler reroutes
       reads and writes through the log and resumes the body with the
       result. if the body finishes, control returns here and we try
       to commit. *)
    let result =
      let open Effect.Deep in
      try_with body ()
        { effc = fun (type c) (eff : c Effect.t) ->
            match eff with
            | Read t ->
                Some (fun (k : (c, _) continuation) ->
                  continue k (do_read t))
            | Write (t, v) ->
                Some (fun (k : (c, _) continuation) ->
                  do_write t v;
                  continue k ())
            | _ -> None
        }
    in

    (* This is the commit phase. we take the lock, walk the log checking each
       witnessed version against the live version, and if ever check
       passes, publish all pending writes and bump their versions. if
       any check fails, drop the log and try the transaction body again. *)
    Mutex.lock commit_lock;
    let valid =
      Hashtbl.fold
        (fun _ (Entry r) acc -> acc && r.tvar.version = r.witnessed)
        log true
    in
    (* loop the log and match on the pending value, if something new update it, if none do nothing and loop *)
    if valid then begin
      Hashtbl.iter
        (fun _ (Entry r) ->
           match r.pending with
           | None   -> ()
           | Some v ->
               r.tvar.value   <- v;
               r.tvar.version <- r.tvar.version + 1)
        log; 
      Mutex.unlock commit_lock;
      if !attempts > 1 then
        Printf.printf "[stm] thread %d committed after %d attempts\n%!"
          (Thread.id (Thread.self ())) !attempts;
      result
    end else begin
      Mutex.unlock commit_lock; (* versions didn't match, drop the log and try again *)
      Printf.printf "[stm] thread %d retrying (attempt %d aborted)\n%!"
        (Thread.id (Thread.self ())) !attempts;
      attempt ()
    end
  in
  attempt ()