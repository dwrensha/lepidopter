structure Game :> GAME =
struct

type health = real

datatype body_data = Moth of health
                   | Block of unit

  structure BDD = BDDWorld( 
                    struct type fixture_data = unit
                           type body_data = body_data
                           type joint_data = unit
                    end
                    )

  val gravity = BDDMath.vec2 (0.0, ~1.0) 
  val world = BDD.World.world (gravity, true)

  open GL
  datatype spec = RGB of GLdouble * GLdouble * GLdouble;

  open BDDOps
  infix 6 :+: :-: %-% %+% +++
  infix 7 *: *% +*: +*+ #*% @*:

  val zero = BDDMath.vec2 (0.0, 0.0) 

  fun create_moth (p : BDDMath.vec2) world : unit = 
      let 
          val body = BDD.World.create_body
                         (world,
                          {typ = BDD.Body.Dynamic,
                           position = p,
                           angle = 0.0,
                           linear_velocity = zero,
                           angular_velocity = 0.0,
                           linear_damping = 0.0,
                           angular_damping = 0.0,
                           allow_sleep = true,
                           awake = true,
                           fixed_rotation = false,
                           bullet = false,
                           active = true,
                           data = Moth 1.0,
                           inertia_scale = 1.0
                         })

          val fixture = BDD.Body.create_fixture_default
                            (body,
                             BDDShape.Polygon
                                 (BDDPolygon.polygon
                                      [BDDMath.vec2 (0.0, 0.0),
                                       BDDMath.vec2 (0.4, ~0.2),
                                       BDDMath.vec2 (0.4, 0.2)]
                                 ),
                             (),
                             10.0)
          val () = BDD.Fixture.set_restitution (fixture, 0.2)
          val () = BDD.Fixture.set_friction (fixture, 0.0)
          val fixture = BDD.Body.create_fixture_default
                            (body,
                             BDDShape.Polygon
                                 (BDDPolygon.polygon
                                      [BDDMath.vec2 (0.0, 0.0),
                                       BDDMath.vec2 (~0.4, 0.2),
                                       BDDMath.vec2 (~0.4, ~0.2)]
                                 ),
                             (),
                             10.0)
          val () = BDD.Fixture.set_restitution (fixture, 1.0)
          val () = BDD.Fixture.set_friction (fixture, 0.1)
      in () end

  val () = create_moth (BDDMath.vec2 (0.3, 0.3)) world

  fun create_block (p : BDDMath.vec2) world : unit = 
      let 
          val body = BDD.World.create_body
                         (world,
                          {typ = BDD.Body.Static,
                           position = p,
                           angle = 0.0,
                           linear_velocity = zero,
                           angular_velocity = 0.0,
                           linear_damping = 0.0,
                           angular_damping = 0.0,
                           allow_sleep = true,
                           awake = true,
                           fixed_rotation = false,
                           bullet = false,
                           active = true,
                           data = Block (),
                           inertia_scale = 1.0
                         })

          val fixture = BDD.Body.create_fixture_default
                            (body,
                             BDDShape.Polygon
                                 (BDDPolygon.box (0.3, 0.3)),
                             (),
                             100.0)
          val () = BDD.Fixture.set_restitution (fixture, 1.0)
          val () = BDD.Fixture.set_friction (fixture, 0.4)
      in () end

  val () = create_block (BDDMath.vec2 (0.1, ~0.4)) world

fun DrawPrim (_,[]) = glFlush ()
  | DrawPrim (obj,l) =
    let
        fun draw_vertices [] = ()
          | draw_vertices ((x,y,z)::t) =
                    ((glVertex3d x y z); draw_vertices t)
          
        fun draw_all [] = ()
          | draw_all ((RGB(r,g,b), v)::t) =
            ((glColor3d r g b) ; draw_vertices(v);
             draw_all t)
    in
        (glBegin(obj);
         draw_all l;
         glEnd();
         glFlush())
    end

  type state = unit
  type screen = SDL.surface

  (* Constant parameters *)
  val width = 500
  val height = 500
  val use_gl = true
  
  val initstate = ()

  fun initscreen screen =
  (
   glClearColor 0.0 0.0 0.0 1.0;
   glClearDepth 1.0;
   glViewport 0 0 width height;
   glMatrixMode(GL_PROJECTION);
   glLoadIdentity();
   glOrtho ~5.0 5.0 ~5.0 5.0 5.0 ~5.0;
   glMatrixMode(GL_MODELVIEW);
   glEnable(GL_TEXTURE_2D);
   glLoadIdentity();
   SDL.glflip();
   ()
  )

  val ticks_per_second = 60.0

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
          in DrawPrim (prim,
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
        dophysics(); 
        SOME s
    end
end

structure Main =
struct
  structure S = RunGame (Game)
end
