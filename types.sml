structure DNA = 
struct

type moth_dna = real array

val DNA_SIZE = 7

val DNA_WINGSPAN = 0
val DNA_HEIGHT = 1
val DNA_DENSITY = 2
val DNA_FORCE = 3
val DNA_RED = 4
val DNA_GREEN = 5
val DNA_BLUE = 6

local
    val dna_mins = Array.fromList [0.1, 0.1, 0.1, 0.1, 0.3, 0.3, 0.3]
    val dna_maxs = Array.fromList [0.7, 0.6, 0.9, 2.0, 1.0, 1.0, 1.0]
in
  fun get_dna_val dna idx =
      let val mn = Array.sub (dna_mins, idx)
          val mx = Array.sub (dna_maxs, idx)
          val v = Array.sub (dna, idx)
      in
          v * (mx - mn) + mn
      end
   
end

end


structure Types =
struct

datatype body_data = Moth of {health : real ref,
                              goal : BDDMath.vec2 ref,
                              dna : DNA.moth_dna
                              }
                   | Block of unit

structure BDD = BDDWorld( 
                struct type fixture_data = unit
                       type body_data = body_data
                       type joint_data = unit
                end
                )

datatype spec = RGB of real * real * real;



end
