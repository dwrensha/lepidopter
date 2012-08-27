structure Common =
struct

local
    val mersenne =
        MersenneTwister.initstring (Time.toString (Time.now ()))
in
   (* random real number from 0.0 to 1.0 *)
   fun random_real () = 
       (Real.fromInt (MersenneTwister.random_nat mersenne 10000)) / 10000.0

   val random_nat = MersenneTwister.random_nat mersenne
end

end
