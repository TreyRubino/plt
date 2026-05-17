
let () =
  Eio_main.run @@ fun env ->
  Printf.printf "\n=== SAFE MULTICORE FIREHOSE (MUTEX + CONDITION) ===\n%!";
  
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  
  (* 1. The Shared Memory *)
  let shared_buf = Bytes.make 16 ' ' in
  
  (* 2. The Traditional OS Locking Tools *)
  let mutex = Eio.Mutex.create () in
  let cond = Eio.Condition.create () in
  
  (* 3. We must manually track the state so we don't overwrite! *)
  let has_data = ref false in 

  Eio.Switch.run @@ fun _sw ->
  Eio.Fiber.both
    (fun () ->
      (* ================= PRODUCER (Core 0) ================= *)
      for i = 1 to 5 do
        let msg = Printf.sprintf "Packet %d Data.." i in
        
        Eio.Mutex.lock mutex;
        
        (* If the consumer hasn't read the last packet, GO TO SLEEP.
           This is what prevents the Firehose overwrite! *)
        while !has_data do
          Eio.Condition.await cond mutex
        done;
        
        (* Critical Section: Safe to Write *)
        Bytes.blit_string msg 0 shared_buf 0 (String.length msg);
        
        (* Manually update state, wake up the Consumer, and unlock *)
        has_data := true;
        Eio.Condition.broadcast cond;
        Eio.Mutex.unlock mutex;
      done
    )
    (fun () ->
      (* ================= CONSUMER (Core 1) ================= *)
      Eio.Domain_manager.run domain_mgr (fun () ->
        for _ = 1 to 5 do
          Eio.Mutex.lock mutex;
          
          (* If the producer hasn't written yet, GO TO SLEEP *)
          while not !has_data do
            Eio.Condition.await cond mutex
          done;
          
          (* Critical Section: Safe to Read *)
          let data = Bytes.to_string shared_buf in
          Printf.printf "Consumer read: '%s'\n%!" data;
          
          (* Manually update state, wake up the Producer, and unlock *)
          has_data := false;
          Eio.Condition.broadcast cond;
          Eio.Mutex.unlock mutex;
        done
      )
    )