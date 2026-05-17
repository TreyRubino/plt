# Software Transactional Memory Demo
Trey Rubino -
CPSC 543 -
Multiprocessing and Concurrent Programming

## Project Structure
- `bin/demo.ml`: Entry point, allocates the two accounts, spawns worker threads, and verifies the invariant
- `lib/stm.ml`: Implementation of the STM runtime, including the effect handler, per-attempt log, and commit logic
- `lib/stm.mli`: Public interface exposing `tvar`, `make`, `read`, `write`, and `atomically`

## Overview
This project is a small software transactional memory library and an accompanying bank transfer demo, written to
accompany the presentation on STM in OCaml 5. The library lets a programmer wrap a block of reads and writes in
`atomically`, and the runtime takes responsibility for making that block appear atomic to every other thread. The
demo exercises the library by spinning up several worker threads that move random amounts of money between two
accounts, with the goal of showing that the invariant `bal(A) + bal(B)` is preserved across thousands of concurrent
transfers without any locks appearing in the business logic.

## Design
The runtime is built on top of OCaml 5 algebraic effects. Two effects, `Read` and `Write`, are declared as the only
operations a transaction can perform on a transactional variable. The `atomically` function installs an effect handler
that intercepts these operations and routes them through a private per-attempt log. Each transactional variable carries
a value, a version counter, and a stable id used as a hash table key. The log records the version a transaction witnessed
on first contact with each variable, along with any pending writes the transaction has made. At commit time the runtime
acquires a short global mutex, walks the log to confirm that every witnessed version still matches the live version, and
either publishes the pending writes or discards the log and retries the body from scratch. The mutex itself is an
implementation detail of the runtime and never appears in the business logic.

## Implementation
The transaction body runs without any synchronization. Reads return either a value buffered earlier in the same
transaction or a snapshot taken on first contact, which gives the body a stable view of memory even while other threads
are committing in parallel. Writes never touch shared state directly, they only update a pending field in the local log.
On commit the runtime takes the lock just long enough to validate and publish, which keeps the critical section bounded
and predictable. Every successful publish bumps the version of the affected variables, which is what allows future
transactions to detect that they were operating on stale information. The transfer function reads two balances, computes 
the new values, and writes them back, all inside an `atomically` block, with no locks or try/finally (or try/with) in sight.
Retries are reported on standard output so the audience can see when the runtime had to discard work and start over.

## How to Build and Run
The project requires OCaml 5 for the effect handler runtime, along with dune. From the project root, build with:

```
dune build
```

Then run the demo with:

```
dune exec ./bin/demo.exe
```

Clean up with:

```
dune clean
```

The output begins with the starting balances and total, followed by occasional retry messages from the STM runtime as
worker threads collide on commits, then the worker completion notices, and finally the closing balances and a
confirmation that the invariant was preserved. 