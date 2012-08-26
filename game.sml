structure Game :> GAME =
struct
  open Types
  open GL
  open BDDOps
  infix 6 :+: :-: %-% %+% +++
  infix 7 *: *% +*: +*+ #*% @*:

  type state = BDD.world
  type screen = SDL.surface

  (* Constant parameters *)
  val width = 500
  val height = 500

  (* world coordinates of window boundary *)
  val left = 0.0
  val right = 20.0
  val bottom = 0.0
  val top = 20.0


  fun window_to_world (x, y) = 
      ((Real.fromInt x  / Real.fromInt width) * (right - left) + left,
       (Real.fromInt (height - y)  / Real.fromInt height) * (top - bottom) + bottom)

  val use_gl = true
  
  val gravity = BDDMath.vec2 (0.0, ~1.0) 
  val initstate =
      let val world = BDD.World.world (gravity, true)
          val () = Box2d.create_body world
                                     (BDDMath.vec2 (10.0, 10.0))
                                     (Moth {health = ref 1.0,
                                            goal = ref (BDDMath.vec2 (15.0, 15.0)),
                                            dna = DNA.moth1 })
          val () = Box2d.create_body world
                                     (BDDMath.vec2 (10.0, 17.0))
                                     (Moth {health = ref 1.0,
                                            goal = ref (BDDMath.vec2 (15.0, 15.0)),
                                            dna = DNA.moth2 })

          val () = Box2d.create_body world
                                     (BDDMath.vec2 (1.0, 7.0))
                                     (Moth {health = ref 1.0,
                                            goal = ref (BDDMath.vec2 (15.0, 15.0)),
                                            dna = DNA.random () })

          val () = Box2d.create_body world
                                     (BDDMath.vec2 (7.0, 4.0))
                                     (Moth {health = ref 1.0,
                                            goal = ref (BDDMath.vec2 (15.4, 15.3)),
                                            dna = DNA.random () })

          val () = Box2d.create_body world
                                     (BDDMath.vec2 (11.0, 9.0))
                                     (Moth {health = ref 1.0,
                                            goal = ref (BDDMath.vec2 (15.2, 15.1)),
                                            dna = DNA.random () })

          val () = Box2d.create_body world (BDDMath.vec2 (15.0, 15.0)) (Block ())
          val () = BDD.World.set_begin_contact (world, Box2d.contact_listener)

          fun random_vec () = Box2d.random_vec left right bottom top
          val () = Util.for 1 50
                            (fn i =>
                                Box2d.create_body world
                                                  (random_vec ())
                                                  (Moth {health = ref 1.0,
                                                         goal = ref (random_vec()),
                                                         dna = DNA.random () })
                            )
      in world end


  fun initscreen screen = Opengl.init width height left right bottom top

  val ticks_per_second = 60.0

  fun domothbrain b = 
      case BDD.Body.get_data b of
          (Moth {health = ref health, goal = ref gl, dna}) =>
          let val pos = BDD.Body.get_position b
              val gdir = BDDMath.vec2normalized (gl :-: pos)
              val mforce = DNA.get dna DNA.FORCE
              val force = (health * mforce) *: gdir
              val () = BDD.Body.apply_force (b, force, pos)
          in () end
        | _ => ()

  fun dophysics world = 
      let val timestep = 1.0 / ticks_per_second
          val () = BDD.World.step (world, timestep, 10, 10)
      in () end

  fun scalecolor (RGB (r, g, b)) health = 
      let val s = 0.8 * health + 0.2
      in RGB (s * r, s * g, s * b) end

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
              val Fix {color, health = ref h} = BDD.Fixture.get_data f
          in Opengl.DrawPrim (prim,
                              [(scalecolor color h, glpoints)]);
             Opengl.DrawPrim (GL_LINE_LOOP,
                              [(RGB (0.5, 0.5, 0.5), glpoints)])
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

  fun render screen world =
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
    | handle_event (SDL.E_MouseDown {button, x, y}) w =
      let
          val () = Box2d.create_body w
                                     (BDDMath.vec2 (window_to_world (x,y)))
                                     (Block ())
      in SOME w end
    | handle_event _ s = SOME s


  fun tick world =
      let
      in
          oapp BDD.Body.get_next
               domothbrain
               (BDD.World.get_body_list world);
          dophysics world; 
          SOME world
      end
end

structure Main =
struct
  structure S = RunGame (Game)
end
