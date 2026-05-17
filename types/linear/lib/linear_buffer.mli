type empty
type writing
type reading

type _ t

val create      : int -> empty t
val start_write : empty t -> writing t
val write       : writing t -> string -> writing t
val publish     : writing t -> reading t
val read        : reading t -> string * empty t