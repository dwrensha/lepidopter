structure Types =
struct

datatype body_data = Moth of {health : real ref,
                              goal : BDDMath.vec2 ref
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
