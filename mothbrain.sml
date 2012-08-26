structure MothBrain =
struct

  open Types

  open BDDOps
  infix 6 :+: :-: %-% %+% +++
  infix 7 *: *% +*: +*+ #*% @*:


 (* make a 4 x 4 grid of aabbs *)
 (* rows and columns like a matrix *)

 val rows = 4
 val cols = 4

 val aabbs = Array2.array (rows, cols, {lowerbound = BDDMath.vec2_zero,
                                        upperbound = BDDMath.vec2_zero})

 datatype cell_stats = CS of {lightbulbs : int,
                              moths : int,
                              balls : int,
                              blocks : int
                             }

 val stats = Array2.array (rows, cols,
                           CS {lightbulbs = 0,
                               moths = 0,
                               balls = 0,
                               blocks = 0})

 fun construct_aabbs (CONST {left, right, bottom, top, ...}) = 
     let val wwidth = right - left
         val wheight = top - bottom
         val col_width = wwidth / (Real.fromInt cols)
         val row_height = wheight / (Real.fromInt rows)
     in
         Util.for 0 (rows - 1)
           (fn ri =>
             Util.for 0 (cols - 1)
               (fn ci =>
                  let val (r, c) = (Real.fromInt ri, Real.fromInt ci)
                      val lx = left + c * col_width
                      val rx = left + (c + 1.0) * col_width
                      val by = top - r * row_height
                      val ty = top - (r + 1.0) * row_height
                      val lower = BDDMath.vec2 (lx, by)
                      val upper = BDDMath.vec2 (rx, ty)
                  in Array2.update (aabbs, ri, ci,
                                    { lowerbound = lower, upperbound = upper }
                                   )
                  end
             )
           )
     end

 fun update_stats world =
     Util.for 0 (rows - 1)
       (fn ri =>
         Util.for 0 (cols - 1)
           (fn ci =>
               let val aabb = Array2.sub (aabbs, ri, ci)
                   fun cb f = 
                       (case Box2d.fixture_to_body_data f of
                            Moth _ => true
                          | Lightbulb () => true
                          | Block () => true
                          | Ball () => true
                       )
                   val () = BDD.World.query_aabb (world, cb, aabb)
               in ()
               end 
           )
       )

 (* use BDD.World.query_aabb (world, callback, )*)

 val counter = ref 0





  fun domothbrain b = 
      case BDD.Body.get_data b of
          (Moth {health = ref health, goal = ref gl, dna}) =>
          let val pos = BDD.Body.get_position b
              val gdir = BDDMath.vec2normalized (gl :-: pos)
              val mforce = DNA.get dna DNA.FORCE
              val force = (health * mforce) *: gdir
              val planprob = DNA.get dna DNA.PLANPROB
              val () = BDD.Body.apply_force (b, force, pos)
          in () end
        | _ => ()



end
