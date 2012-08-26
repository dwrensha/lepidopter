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

 fun construct_aabbs (CONST {left, right, bottom, top, ...}) = 
     let val wwidth = right - left
         val wheight = top - bottom
         val col_width = wwidth / (Real.fromInt cols)
         val row_height = wheight / (Real.fromInt rows)
     in
         Array2.tabulate
             Array2.RowMajor
             (rows, cols,
              (fn (ri, ci) =>
                  let val (r, c) = (Real.fromInt ri, Real.fromInt ci)
                      val lx = left + c * col_width
                      val rx = left + (c + 1.0) * col_width
                      val by = top - r * row_height
                      val ty = top - (r + 1.0) * row_height
                      val lower = BDDMath.vec2 (lx, by)
                      val upper = BDDMath.vec2 (rx, ty)
                  in { lowerbound = lower, upperbound = upper }
                  end
             ))
     end

 (* use BDD.World.query_aabb (world, callback, )*)

 val counter = ref 0

(*  fun initialize *)



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
