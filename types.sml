
structure Types =
struct

datatype spec = RGB of real * real * real;

datatype body_data = Moth of {health : real ref,
                              goal : BDDMath.vec2 ref,
                              dna : DNA.moth_dna
                              }
                   | Block of unit
                   | Ball of unit

datatype fixture_data = Fix of {health: real ref,
                                color : spec}

structure BDD = BDDWorld( 
                struct type fixture_data = fixture_data
                       type body_data = body_data
                       type joint_data = unit
                end
                )





end
