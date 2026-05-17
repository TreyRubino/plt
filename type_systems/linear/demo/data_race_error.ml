open Eio.Std
open Static_ownership

let () =
  Eio_main.run @@ fun _ ->
  let stream = Eio.Stream.create 5 in

  Eio.Switch.run @@ fun _sw ->
  Eio.Fiber.both
    (fun () ->
      let buf = Linear_buffer.create 16 in
      
      let rec loop i (token : Linear_buffer.empty Linear_buffer.t) =
        if i > 5 then ()
        else begin
          let w_token = Linear_buffer.start_write token in
          let r_token = Linear_buffer.publish (Linear_buffer.write w_token "Race") in
          
          Eio.Stream.add stream r_token;

          (* NATURAL TYPE ERROR:
             The loop expects 'empty t'. 
             You are giving it 'r_token' which is 'reading t'. *)
          loop (i + 1) r_token
        end
      in
      loop 1 buf
    )
    (fun () -> ())