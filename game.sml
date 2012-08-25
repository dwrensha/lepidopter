structure Game :> GAME =
struct

datatype pose = P of {x : real, y : real, theta : real}

type health = real

datatype body_data = Moth of pose * health
                   | Block of unit

  structure World = BDDWorld( 
                    struct type fixture_data = unit
                           type body_data = body_data
                           type joint_data = unit
                    end
                    )

  val gravity = BDDMath.vec2 (0.0, ~1.0) 
  val world = World.World.world (gravity, true)

  open GL
  datatype spec = RGB of GLreal * GLreal * GLreal;

fun DrawPrim (_,[]) = glFlush ()
  | DrawPrim (obj,l) =
    let
        fun draw_vertices [] = ()
          | draw_vertices ((x,y,z)::t) =
                    ((glVertex3f x y z); draw_vertices t)
          
        fun draw_all [] = ()
          | draw_all ((RGB(r,g,b), v)::t) =
            ((glColor3f r g b) ; draw_vertices(v);
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
          val () = World.World.step (world, timestep, 10, 10)
      in () end


  fun render screen () =
  let in
      glClear(GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT);
   glLoadIdentity();
    glBegin(GL_QUADS);
        glColor3f 1.0 0.0 0.0;
        glVertex3f 0.0 0.0 0.0;
        glColor3f 1.0 1.0 0.0; 
        glVertex3f 10.0 0.0 0.0;
        glColor3f 1.0 0.0 1.0;
       glVertex3f 10.0 20.0 0.0;
        glColor3f 1.0 1.0 1.0;
      glVertex3f 0.0 10.0 0.0;
    glEnd();

   DrawPrim (GL_QUADS,
             [
              (RGB(0.9, 1.0, 0.0),
               [(~1.0, 1.0, 1.0), (~1.0, ~1.0,1.0)]),
              (RGB(0.0,0.7,0.7),
               [(1.0,~1.0,1.0),( 1.0,1.0,1.0)])
              ]);
   


   SDL.glflip();
      ()

  end

  fun keyDown (SDL.SDLK_ESCAPE) _ = NONE (* quit the game *)
    | keyDown key s = SOME s

  fun handle_event (SDL.E_KeyDown {sym = k}) s = keyDown k s
    | handle_event SDL.E_Quit s = NONE
    | handle_event _ s = SOME s


  fun tick () =
    let
    in
        SOME ()
    end
end

structure Main =
struct
  structure S = RunGame (Game)
end
