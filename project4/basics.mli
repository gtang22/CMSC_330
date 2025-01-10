val rev_tup : 'a * 'b * 'c -> 'c * 'b * 'a
val is_even : int -> bool
val volume : int * int * int -> int * int * int -> int
val fibonacci : int -> int
val log : int -> int -> int
val gcf : int -> int -> int
val maxFuncChain : 'a -> ('a -> 'a) list -> 'a
val reverse : 'a list -> 'a list
val zip : ('a * 'b) list -> ('c * 'd) list -> ('a * 'b * 'c * 'd) list
val is_palindrome : 'a list -> bool
val square_primes : int list -> (int * int) list
val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
val is_present: 'a list -> 'a -> int list
val count_occ : 'a list -> 'a -> int
val jumping_tuples : ('a * 'b) list -> ('c * 'a) list -> 'a list
val addgenerator : int -> (int -> int)
val uniq : 'a list -> 'a list
val ap : ('a -> 'b) list -> 'a list -> 'b list