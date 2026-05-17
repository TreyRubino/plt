(** @file stm.mli
    @brief minimal software transactional memory built on ocaml 5 effects.
    transactions are written as plain sequential code inside [atomically],
    and the handler takes care of logging reads, buffering writes,
    validating against concurrent commits, and retrying on conflict. 

    @see stm.ml

    @author Trey Rubino
    @date 05/02/2026 *)

(** @brief abstract type of a transactional variable holding a value of type ['a].
    a tvar carries the current value, a version stamp the handler uses to
    detect conflicting commits, and a unique id used as a hash-table key. *)
type 'a tvar

(** @brief allocate a new transactional variable.
    @param v initial committed value of the new tvar.
    @return a new tvar whose contents start at [v] and version at zero. *)
val make : 'a -> 'a tvar

(** @brief read the current value of a tvar from inside a transaction.
    @param t the tvar to read.
    @return the value the transaction should observe, either the
            snapshot taken on first contact or a pending write made earlier
            in the same transaction.
    @raise  Effect.Unhandled if invoked outside of [atomically]. *)
val read : 'a tvar -> 'a

(** @brief schedule a write to a tvar from inside a transaction. the new value is
    only published if the surrounding transaction commits successfully.
    @param t the tvar to update.
    @param v the value to publish on commit.
    @raise  Effect.Unhandled if invoked outside of [atomically]. *)
val write : 'a tvar -> 'a -> unit

(** @brief run a function as a transaction. reads and writes performed inside
    are tracked in a private log. on commit the log is validated against
    the global state; if any tvar that was read has been modified in the
    meantime, the log is discarded and is invoked again from scratch.
    retries are reported on stdout so the demo can show them.
    @param f thunk containing the transactional body. https://en.wikipedia.org/wiki/Thunk
    @return  the result returned by the committed run. *)
val atomically : (unit -> 'a) -> 'a