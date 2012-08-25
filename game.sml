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

  val zero = BDDMath.vec2 (0.0, 0.0) 

  fun create_moth (p : BDDMath.vec2) world : unit = 
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
                           fixed_rotation = true,
                           bullet = false,
                           active = true,
                           data = Moth 1.0,
                           inertia_scale = 1.0
                         })

          val fixture = BDD.Body.create_fixture_default
                            (body,
                             BDDShape.Polygon
                                 (BDDPolygon.box (0.05,
                                                  1.0 / 2.0)),
                             (),
                             10000.0)
          val () = BDD.Fixture.set_restitution (fixture, 0.2)
          val () = BDD.Fixture.set_friction (fixture, 0.0)
      in () end

  val () = create_moth zero world

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


  fun drawbody screen b = 
      let val (x, y) = BDDMath.vec2xy (BDD.Body.get_position b)
          val theta = BDD.Body.get_angle b
      in case BDD.Body.get_data b of
             Moth h => DrawPrim (GL_TRIANGLES,
                                 [(RGB (1.0,1.0,1.0),
                                  [(x - 0.5, y - 0.5, 0.0),
                                   (x, y, 0.0),
                                   (x - 0.6, y, 0.0)])])
           | Block _ => ()
      end

  fun render screen () =
  let in
      glClear(GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT);
   glLoadIdentity();
   DrawPrim (GL_QUADS,
             [
              (RGB(0.9, 1.0, 0.0),
               [(~1.0, 1.0, 0.0), (~1.0, ~1.0, 0.0)]),
              (RGB(0.0,0.7,0.7),
               [(1.0, ~1.0, 0.0),( 1.0, 1.0, 0.0)])
              ]);
   
      
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
