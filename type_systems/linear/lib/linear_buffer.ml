type empty
type writing
type reading

type _ t =
  | Empty   : bytes -> empty t
  | Writing : bytes -> writing t
  | Reading : bytes -> reading t

let create n = Empty (Bytes.make n ' ')
let start_write (Empty b) = Writing b
let write (Writing b) str = 
  let len = min (String.length str) (Bytes.length b) in
  Bytes.blit_string str 0 b 0 len; Writing b
let publish (Writing b) = Reading b
let read (Reading b) = (Bytes.to_string b, Empty b)