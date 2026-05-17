(** @file demo.ml
    @brief several worker threads keep moving random amounts of money between
    two accounts. the invariant we want to preserve is that the total
    balance across both accounts never changes, regardless of how the
    threads interleave. nothing in [transfer] uses locks, the
    [atomically] handler is doing all the synchronization work. 

    @author Trey Rubino 
    @date 05/04/2026 *)

open Stm

(* the two accounts with arbitrary starting balances, the only thing
   this demo cares about is that the sum is invariant across the run. *)
let acct_a = make 1000
let acct_b = make 1000

(** @brief read both account balances inside a single transaction.
    @return  a pair [(a, b)], so [a + b] is always
             a valid total even if other threads are mid-transfer. *)
let snapshot () =
  atomically (fun () ->
    let a = read acct_a in
    let b = read acct_b in
    (a, b))

(* total we expect to see at the end. *)
let initial_total =
  let a, b = snapshot () in a + b

(** @brief move money between two accounts as a single atomic transaction.
    the body reads like ordinary sequential code, two reads followed by
    two writes, no try/finally and no acquire/release.
    @param src account to debit.
    @param dst account to credit.
    @param n   amount to transfer. *)
let transfer src dst n =
  atomically (fun () ->
    let s = read src in
    let d = read dst in
    write src (s - n);
    write dst (d + n))

(** @brief worker loop. picks a random direction and amount each round and
    issues a transfer between the two demo accounts.
    @param id     numeric id of this worker, used for log lines and the
                  rng seed so different workers explore different paths.
    @param rounds how many transfers this worker should issue. *)
let worker id rounds =
  let rng = Random.State.make [| id; rounds; Unix.getpid () |] in
  for _ = 1 to rounds do
    let amount = 1 + Random.State.int rng 50 in
    if Random.State.bool rng then transfer acct_a acct_b amount
    else transfer acct_b acct_a amount
  done;
  Printf.printf "[demo] worker %d done\n%!" id

(** entry point. spins up workers, waits for them to
    finish, and verifies the invariant. *)
let () =
  let n_workers = 4 in
  let rounds    = 2000 in
  let a0, b0 = snapshot () in
  Printf.printf "[demo] starting balances: A=%d B=%d total=%d\n%!"
    a0 b0 initial_total;
  let threads =
    List.init n_workers (fun i -> Thread.create (worker i) rounds)
  in
  List.iter Thread.join threads;
  let a1, b1 = snapshot () in
  let final_total = a1 + b1 in
  Printf.printf "[demo] final balances:    A=%d B=%d total=%d\n%!"
    a1 b1 final_total;
  assert (final_total = initial_total);
  print_endline "[demo] invariant preserved"