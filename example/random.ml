module type Protos = sig
  val random_bool : unit -> bool
end

module type Intf = sig
  class random_bool : object
    method run : unit -> bool
    method reset : unit -> unit
  end

  class random : object
    method run : unit -> bool
    method reset : unit -> unit
  end
end

module Make =
functor
  (P : Protos)
  ->
  struct
    class random_bool =
      object
        method run = P.random_bool
        method reset () = ()
      end

    class random =
      object
        val x = new random_bool
        method run = x#run
        method reset () = x#reset
      end
  end
