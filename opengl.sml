structure Opengl =
struct

open GL
open Types

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


fun init width height = (
   glClearColor 0.0 0.0 0.0 1.0;
   glClearDepth 1.0;
   glViewport 0 0 width height;
   glMatrixMode(GL_PROJECTION);
   glLoadIdentity();
   glOrtho 0.0 20.0 0.0 20.0 5.0 ~5.0;
   glMatrixMode(GL_MODELVIEW);
   glEnable(GL_TEXTURE_2D);
   glLoadIdentity();
   SDL.glflip();
   ()
  )


end