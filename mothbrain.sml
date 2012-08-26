structure MothBrain =
struct

  open Common
  open Types

  open BDDOps
  infix 6 :+: :-: %-% %+% +++
  infix 7 *: *% +*: +*+ #*% @*:


 (* make a |rows| x |cols| grid of aabbs *)
 (* rows and columns like a matrix *)

 val rows = 7
 val cols = 7

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
                      val ty = top - r * row_height
                      val by = top - (r + 1.0) * row_height
                      val lower = BDDMath.vec2 (lx, by)
                      val upper = BDDMath.vec2 (rx, ty)
                  in Array2.update (aabbs, ri, ci,
                                    { lowerbound = lower, upperbound = upper }
                                   )
                  end
             )
           )
     end

 fun inc r = r := ((!r) + 1)

 fun update_stats world =
     Util.for 0 (rows - 1)
       (fn ri =>
         Util.for 0 (cols - 1)
           (fn ci =>
               let val aabb = Array2.sub (aabbs, ri, ci)
                   val nmoths = ref 0
                   val nlightbulbs = ref 0
                   val nblocks = ref 0
                   val nballs = ref 0
                   fun cb f = 
                       (case Box2d.fixture_to_body_data f of
                            Moth _ => (inc nmoths; true)
                          | Lightbulb () => (inc nlightbulbs; true)
                          | Block () => (inc nblocks; true)
                          | Ball () => (inc nballs; true)
                       )
                   val () = BDD.World.query_aabb (world, cb, aabb)
               in Array2.update (stats, ri, ci,
                                 CS {lightbulbs = !nlightbulbs,
                                     moths = !nmoths,
                                     balls = !nballs,
                                     blocks = !nblocks})
               end 
           )
       )

(* Find the most appealing cell and set the goal to be a random point
 inside of it.
*)

 fun plan pos dna =
     let  
         val mothweight = DNA.get dna DNA.MOTHWEIGHT
         val lightbulbweight = DNA.get dna DNA.LIGHTBULBWEIGHT
         val blockweight = DNA.get dna DNA.BLOCKWEIGHT
         val ballweight = DNA.get dna DNA.BALLWEIGHT
         val bestidx = ref (~1, ~1)
         val bestscore = ref ~1.0
         val () = Util.for 0 (rows - 1)
                   (fn ri =>
                     Util.for 0 (cols - 1)
                      (fn ci =>
                         let val CS {moths, lightbulbs, blocks, balls} = 
                                 Array2.sub (stats, ri, ci)
                             val score = (Real.fromInt moths * mothweight) +
                                         (Real.fromInt lightbulbs * lightbulbweight) +
                                         (Real.fromInt blocks * blockweight) +
                                         (Real.fromInt balls * ballweight)
                         in
                             if score > !bestscore
                             then (bestidx := (ri, ci);
                                   bestscore := score)
                             else ()
                         end
                      )
                   )
         val (r, c) = !bestidx
         val aabb = Array2.sub (aabbs, r, c)
         val goal = Box2d.random_vec_in_aabb aabb
     in goal end

 fun dosinglebrain world b = 
     case BDD.Body.get_data b of
         (Moth {health = ref health, goal, dna}) =>
         let
             val pos = BDD.Body.get_position b
             val planprob = DNA.get dna DNA.PLANPROB
             val () = if random_real() < planprob
                      then goal := (plan pos dna)
                      else ()
             val gdir = BDDMath.vec2normalized ((!goal) :-: pos)
             val mforce = DNA.get dna DNA.FORCE
             val force = (health * mforce) *: gdir
             val () = BDD.Body.apply_force (b, force, pos)
         in () end
       | _ => ()

 val counter = ref 0
 val update_period = 30

 fun dobrains world =
     let val () = if (!counter) mod update_period = 0
                  then update_stats world
                  else ()
         val () = inc counter
     in
         oapp BDD.Body.get_next
              (dosinglebrain world)
              (BDD.World.get_body_list world)
     end



end
