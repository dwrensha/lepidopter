structure Game :> GAME =
struct
  open Types
  open GL
  open BDDOps
  infix 6 :+: :-: %-% %+% +++
  infix 7 *: *% +*: +*+ #*% @*:

  type state = game_state

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
  
  val gravity = BDDMath.vec2 (0.0, ~4.0) 
  val initstate =
      let val world = BDD.World.world (gravity, true)
          val () = Box2d.populate world
                                  (Box2d.get_level_data (left,right,bottom,top) 1)
          val () = BDD.World.set_begin_contact (world, Box2d.contact_listener)
      in GS {world = world, level = 1} end


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
          BDDShape.Circle {radius, p} =>
          let val n = 40
              val rad = radius *: (BDDMath.vec2 (1.0, 0.0))
              val points = List.tabulate
                           (n,
                         fn ii => 
                            let val ang = 2.0 * Math.pi * (Real.fromInt ii / Real.fromInt n)
                                val tf = BDDMath.mat22angle ang 
                            in BDDMath.vec2xy (pos :+: p :+: (tf +*: rad)) end
                           )
              val glpoints = List.map (fn (x, y) => (x, y, 0.0)) points
              val Fix {color, health} = BDD.Fixture.get_data f
          in Opengl.DrawPrim (GL_TRIANGLE_FAN,
                              [(color, glpoints)])
          end
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
           |  _ => 
             BDDOps.oapp BDD.Fixture.get_next (drawfixture screen pos theta) fl
      end

  fun render screen (GS {world, ...}) =
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
    | handle_event (SDL.E_MouseDown {button, x, y}) (s as GS {world, ...}) =
      let
          val () = Box2d.create_body world
                                     (BDDMath.vec2 (window_to_world (x,y)))
                                     (Ball ())
      in SOME s end
    | handle_event _ s = SOME s


  fun tick (s as GS {world, level}) =
      let
      in
          oapp BDD.Body.get_next
               domothbrain
               (BDD.World.get_body_list world);
          dophysics world; 
          SOME s
      end
end

structure Main =
struct
  structure S = RunGame (Game)
end
