open Static_ownership

let () =
  Eio_main.run @@ fun env ->
  let stream = Eio.Stream.create 5 in
  let return_stream = Eio.Stream.create 5 in 

  Eio.Switch.run @@ fun _sw ->
  Eio.Fiber.both
    (fun () ->
      (* PRODUCER (Core 0) *)
      let buf = Linear_buffer.create 16 in
      
      let rec loop i (token : Linear_buffer.empty Linear_buffer.t) =
        if i > 5 then ()
        else begin
          let msg = Printf.sprintf "Packet %d Data.." i in
          
          (* State transitions *)
          let w_token = Linear_buffer.start_write token in
          let w_token = Linear_buffer.write w_token msg in
          let r_token = Linear_buffer.publish w_token in
          
          (* Hand off to consumer *)
          Eio.Stream.add stream r_token;
          
          (* BLOCK: Wait for Core 1 to recycle the buffer *)
          let recycled_token = Eio.Stream.take return_stream in
          loop (i + 1) recycled_token
        end
      in
      loop 1 buf
    )
    (fun () ->
      (* CONSUMER (Core 1) *)
      Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) (fun () ->
        for _ = 1 to 5 do
          let r_token = Eio.Stream.take stream in
          let (data, empty_token) = Linear_buffer.read r_token in
          Printf.printf "Consumer read: '%s'\n%!" data;
          Eio.Stream.add return_stream empty_token
        done
      )
    )