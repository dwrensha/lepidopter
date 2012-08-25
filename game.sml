structure Game :> GAME =
struct
  open Types

  val gravity = BDDMath.vec2 (0.0, ~1.0) 
  val world = BDD.World.world (gravity, true)

  open GL


  open BDDOps
  infix 6 :+: :-: %-% %+% +++
  infix 7 *: *% +*: +*+ #*% @*:

  val () = Box2d.create_moth world
                             (BDDMath.vec2 (10.0, 10.0))
                             (Moth {health = ref 1.0,
                                    goal = ref (BDDMath.vec2 (15.0, 15.0))})
  val () = Box2d.create_moth world
                             (BDDMath.vec2 (10.0, 17.0))
                             (Moth {health = ref 1.0,
                                    goal = ref (BDDMath.vec2 (16.0, 16.0))})

  val () = Box2d.create_block world (BDDMath.vec2 (15.0, 15.0)) (Block ())

  type state = unit
  type screen = SDL.surface

  (* Constant parameters *)
  val width = 500
  val height = 500
  val use_gl = true
  
  val initstate = ()

  fun initscreen screen = Opengl.init width height

  val ticks_per_second = 60.0

  fun domothbrain b = 
      case BDD.Body.get_data b of
          (Moth {health, goal = ref gl}) =>
          let val pos = BDD.Body.get_position b
              val gdir = BDDMath.vec2normalized (gl :-: pos)
              val force = 5.5 *: gdir
              val () = BDD.Body.apply_force (b, force, pos)
          in () end
        | _ => ()

  fun dophysics () = 
      let val timestep = 1.0 / ticks_per_second
          val () = BDD.World.step (world, timestep, 10, 10)
      in () end


  fun drawfixture screen pos theta f =
      case BDD.Fixture.shape f of
          BDDShape.Circle c => ()
        | BDDShape.Polygon p => 
          let val n = BDDPolygon.get_vertex_count p
              val prim = (case n of
                              3 => GL_TRIANGLES
                            | 4 => GL_QUADS
                            | _ => GL_TRIANGLE_FAN )
              val points = List.tabulate
                               (n, fn ii => BDDPolygon.get_vertex (p, ii))
              val tf = BDDMath.mat22angle theta
              val glpoints0 = List.map (fn pt => BDDMath.vec2xy (pos :+: (tf +*: pt))) points
              val glpoints = List.map (fn (x, y) => (x, y, 0.0)) glpoints0
          in Opengl.DrawPrim (prim,
                              [(RGB (0.0, 1.0, 1.0), glpoints)])
          end

  fun drawbody screen b = 
      let val pos = BDD.Body.get_position b
          val theta = BDD.Body.get_angle b
          val fl = BDD.Body.get_fixtures b
      in case BDD.Body.get_data b of
             Moth h => 
             BDDOps.oapp BDD.Fixture.get_next (drawfixture screen pos theta) fl
           | Block _ => 
             BDDOps.oapp BDD.Fixture.get_next (drawfixture screen pos theta) fl
      end

  fun render screen () =
  let in
      glClear(GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT);
      glLoadIdentity();
   
      BDDOps.oapp BDD.Body.get_next (drawbody screen) (BDD.World.get_body_list world);
      SDL.glflip();
      ()

  end

  fun keyDown (SDL.SDLK_ESCAPE) _ = NONE (* quit the game *)
    | keyDown key s = SOME s

  fun handle_event (SDL.E_KeyDown {sym = k}) s = keyDown k s
    | handle_event SDL.E_Quit s = NONE
    | handle_event _ s = SOME s




  fun tick s =
      let
      in
          oapp BDD.Body.get_next
               domothbrain
               (BDD.World.get_body_list world);
          dophysics (); 
          SOME s
      end
end

structure Main =
struct
  structure S = RunGame (Game)
end
