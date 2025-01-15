class a =
  object
    method run = Random.bool ()
  end

class b =
  object
    val mutable state = 0

    method run up =
      if up then if state < 100 then state <- state + 1 else ()
      else if state > 0 then state <- state - 1
      else ();
      state
  end

class c =
  object
    method run x =
      print_int x;
      print_newline ()
  end

let () = ignore (new a, new b, new c)
let link_a : bool Queue.t = Queue.create ()
let link_b : int Queue.t = Queue.create ()
let node_a = new a
let node_b = new b
let node_c = new c

(*

step random () --> bool

step incr_decr up --> out
{
  out = 0 -> pre out + (if up then 1 else -1)
}

step print_int (int) --> ()
*)

module Make (P : sig
  val random : unit -> bool
  val print_int : int -> bool
end) =
struct
  class random =
    object
      method run () = P.random ()
    end

  class print_int =
    object
      method run = P.print_int
    end

  class incr_decr =
    object
      val mutable state = 0

      method run =
        function
        | true ->
            state <- state + 1;
            state
        | false ->
            state <- state - 1;
            state
    end
end
