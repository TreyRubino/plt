
let () =
  Eio_main.run @@ fun env ->
  Printf.printf "\n=== TRUE RACE: MULTICORE FIREHOSE (RAW BYTES) ===\n%!";
  
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  let stream = Eio.Stream.create 5 in
  let shared_buf = Bytes.make 16 ' ' in

  Eio.Switch.run @@ fun _sw ->
  Eio.Fiber.both
    (fun () ->
      (* ================= PRODUCER (Core 0) ================= *)
      for i = 1 to 5 do
        let msg = Printf.sprintf "Packet %d Data.." i in
        
        (* Dynamically use the exact string length to avoid the crash *)
        Bytes.blit_string msg 0 shared_buf 0 (String.length msg);
        Eio.Stream.add stream shared_buf;
      done
    )
    (fun () ->
      (* ================= CONSUMER (Core 1) ================= *)
      
      (* This spawns a true parallel OS thread *)
      Eio.Domain_manager.run domain_mgr (fun () ->
        for _ = 1 to 5 do
          let received_buf = Eio.Stream.take stream in
          
          (* Core 1 tries to read, but Core 0 is already overwriting it! *)
          let data = Bytes.to_string received_buf in
          Printf.printf "Consumer read: '%s'\n%!" data;
        done
      )
    )