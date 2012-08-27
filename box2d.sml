structure Box2d = 
struct
  open Common
  open Types
  open BDDOps
  infix 6 :+: :-: %-% %+% +++
  infix 7 *: *% +*: +*+ #*% @*:

  val zero = BDDMath.vec2_zero
  val purple = RGB (0.65, 0.2, 1.0)



  fun create_body world (p : BDDMath.vec2) theta (data as Moth {dna, health, ...}) : unit = 
      let 
          val body = BDD.World.create_body
                         (world,
                          {typ = BDD.Body.Dynamic,
                           position = p,
                           angle = theta,
                           linear_velocity = zero,
                           angular_velocity = 0.0,
                           linear_damping = 1.0,
                           angular_damping = 1.0,
                           allow_sleep = true,
                           awake = true,
                           fixed_rotation = false,
                           bullet = false,
                           active = true,
                           data = data,
                           inertia_scale = 1.0
                         })
          val red = DNA.get dna DNA.RED
          val green = DNA.get dna DNA.GREEN
          val blue = DNA.get dna DNA.BLUE
          val color = RGB (red, green, blue)

          val wing_width = (DNA.get dna DNA.WINGSPAN) / 2.0
          val half_height = (DNA.get dna DNA.HEIGHT) / 2.0

          val density = DNA.get dna DNA.DENSITY

          val fixture = BDD.Body.create_fixture_default
                            (body,
                             BDDShape.Polygon
                                 (BDDPolygon.polygon
                                      [BDDMath.vec2 (0.0, 0.0),
                                       BDDMath.vec2 (wing_width, ~half_height),
                                       BDDMath.vec2 (wing_width, half_height)]
                                 ),
                             Fix {color = color, health = health},
                             density)
          val () = BDD.Fixture.set_restitution (fixture, 0.2)
          val () = BDD.Fixture.set_friction (fixture, 0.0)
          val fixture = BDD.Body.create_fixture_default
                            (body,
                             BDDShape.Polygon
                                 (BDDPolygon.polygon
                                      [BDDMath.vec2 (0.0, 0.0),
                                       BDDMath.vec2 (~wing_width, half_height),
                                       BDDMath.vec2 (~wing_width, ~half_height)]
                                 ),
                             Fix {color = color, health = health},
                             density)
          val () = BDD.Fixture.set_restitution (fixture, 1.0)
          val () = BDD.Fixture.set_friction (fixture, 0.1)
      in () end

    | create_body world (p : BDDMath.vec2) theta (data as Block ()) : unit = 
      let 
          val body = BDD.World.create_body
                         (world,
                          {typ = BDD.Body.Static,
                           position = p,
                           angle = theta,
                           linear_velocity = zero,
                           angular_velocity = 0.0,
                           linear_damping = 0.0,
                           angular_damping = 0.0,
                           allow_sleep = true,
                           awake = true,
                           fixed_rotation = false,
                           bullet = false,
                           active = true,
                           data = data,
                           inertia_scale = 1.0
                         })

          val fixture = BDD.Body.create_fixture_default
                            (body,
                             BDDShape.Polygon
                                 (BDDPolygon.box (1.0, 0.5)),
                             Fix {color = RGB (0.6, 0.15, 0.15), health = ref 1.0},
                             100.0)
          val () = BDD.Fixture.set_restitution (fixture, 1.0)
          val () = BDD.Fixture.set_friction (fixture, 0.4)
      in () end
    | create_body world (p : BDDMath.vec2) theta (data as Ball ()) : unit = 
      let 
          val body = BDD.World.create_body
                         (world,
                          {typ = BDD.Body.Dynamic,
                           position = p,
                           angle = theta,
                           linear_velocity = BDDMath.vec2 (0.0, 0.5),
                           angular_velocity = 0.0,
                           linear_damping = 0.0,
                           angular_damping = 0.0,
                           allow_sleep = true,
                           awake = true,
                           fixed_rotation = false,
                           bullet = true,
                           active = true,
                           data = data,
                           inertia_scale = 1.0
                         })

          val fixture = BDD.Body.create_fixture_default
                            (body,
                             BDDShape.Circle {radius = 0.45, p = zero},
                             Fix {color = purple, health = ref 1.0},
                             10.0)
          val () = BDD.Fixture.set_restitution (fixture, 1.0)
          val () = BDD.Fixture.set_friction (fixture, 0.4)
      in () end
    | create_body world (p : BDDMath.vec2) theta (data as Lightbulb ()) : unit = 
      let 
          val body = BDD.World.create_body
                         (world,
                          {typ = BDD.Body.Static,
                           position = p,
                           angle = theta,
                           linear_velocity = BDDMath.vec2 (0.0, 0.5),
                           angular_velocity = 0.0,
                           linear_damping = 0.0,
                           angular_damping = 0.0,
                           allow_sleep = true,
                           awake = true,
                           fixed_rotation = false,
                           bullet = true,
                           active = true,
                           data = data,
                           inertia_scale = 1.0
                         })

          val color = RGB (1.0, 1.0, 0.6)
          val fixture = BDD.Body.create_fixture_default
                            (body,
                             BDDShape.Circle {radius = 0.4,
                                              p = BDDMath.vec2 (0.0, 0.4)},
                             Fix {color = color, health = ref 1.0},
                             10.0)
          val () = BDD.Fixture.set_restitution (fixture, 1.0)
          val () = BDD.Fixture.set_friction (fixture, 0.4)
          val fixture = BDD.Body.create_fixture_default
                            (body,
                             BDDShape.Polygon
                                 (BDDPolygon.box (0.2, 0.2)),
                             Fix {color = color, health = ref 1.0},
                             10.0)
          val () = BDD.Fixture.set_restitution (fixture, 1.0)
          val () = BDD.Fixture.set_friction (fixture, 0.4)
      in () end

  fun fixture_to_body_data f = 
      BDD.Body.get_data (BDD.Fixture.get_body f)


  fun random_vec_in left right bottom top =
      let 
          val x = random_real()
          val y = random_real()
      in BDDMath.vec2
             (x * (right - left) + left,
              y * (top - bottom) + bottom)
      end

  fun random_vec_in_aabb {lowerbound, upperbound} =
      let 
          val (left, bottom) = BDDMath.vec2xy lowerbound
          val (right, top) = BDDMath.vec2xy upperbound
      in random_vec_in left right bottom top
      end

  fun populate world [] = ()
    | populate world ((pos, theta, body_data)::bodies) =
      let val () = create_body world pos theta body_data
      in populate world bodies end

  fun get_level_data constants n dna_list = 
      let
          fun random_dna () = DNA.random (Array.fromList dna_list)
          val CONST {left, right, bottom, top, ...} = constants
          fun random_vec () = random_vec_in (left + 1.0) (right - 1.0)
                                            (bottom + 2.0) (top - 1.0)
          val floor = 
                     [(BDDMath.vec2 (1.0, 1.5), 0.0, Block ()),
                      (BDDMath.vec2 (3.0, 1.5), 0.0, Block ()),
                      (BDDMath.vec2 (5.0, 1.5), 0.0, Block ()),
                      (BDDMath.vec2 (7.0, 1.5), 0.0, Block ()),
                      (BDDMath.vec2 (9.0, 1.5), 0.0, Block ()),
                      (BDDMath.vec2 (11.0, 1.5), 0.0, Block ()),
                      (BDDMath.vec2 (13.0, 1.5), 0.0, Block ()),
                      (BDDMath.vec2 (15.0, 1.5), 0.0, Block ()),
                      (BDDMath.vec2 (17.0, 1.5), 0.0, Block ()),
                      (BDDMath.vec2 (19.0, 1.5), 0.0, Block ()),

                      (BDDMath.vec2 (0.0, 0.5), 0.0, Block ()),
                      (BDDMath.vec2 (2.0, 0.5), 0.0, Block ()),
                      (BDDMath.vec2 (4.0, 0.5), 0.0, Block ()),
                      (BDDMath.vec2 (6.0, 0.5), 0.0, Block ()),
                      (BDDMath.vec2 (8.0, 0.5), 0.0, Block ()),
                      (BDDMath.vec2 (10.0, 0.5), 0.0, Block ()),
                      (BDDMath.vec2 (12.0, 0.5), 0.0, Block ()),
                      (BDDMath.vec2 (14.0, 0.5), 0.0, Block ()),
                      (BDDMath.vec2 (16.0, 0.5), 0.0, Block ()),
                      (BDDMath.vec2 (18.0, 0.5), 0.0, Block ()),
                      (BDDMath.vec2 (20.0, 0.5), 0.0, Block ())]
          val ceiling = 
                     [(BDDMath.vec2 (1.0, 20.0), 0.0, Block ()),
                      (BDDMath.vec2 (3.0, 20.0), 0.0, Block ()),
                      (BDDMath.vec2 (5.0, 20.0), 0.0, Block ()),
                      (BDDMath.vec2 (7.0, 20.0), 0.0, Block ()),
                      (BDDMath.vec2 (9.0, 20.0), 0.0, Block ()),
                      (BDDMath.vec2 (11.0, 20.0), 0.0, Block ()),
                      (BDDMath.vec2 (13.0, 20.0), 0.0, Block ()),
                      (BDDMath.vec2 (15.0, 20.0), 0.0, Block ()),
                      (BDDMath.vec2 (17.0, 20.0), 0.0, Block ()),
                      (BDDMath.vec2 (19.0, 20.0), 0.0, Block ())]
          val leftwall = 
                     [(BDDMath.vec2 (0.0, 1.0), Math.pi / 2.0, Block ()),
                      (BDDMath.vec2 (0.0, 3.0),  Math.pi / 2.0, Block ()),
                      (BDDMath.vec2 (0.0, 5.0),  Math.pi / 2.0, Block ()),
                      (BDDMath.vec2 (0.0, 7.0),  Math.pi / 2.0, Block ()),
                      (BDDMath.vec2 (0.0, 9.0),  Math.pi / 2.0, Block ()),
                      (BDDMath.vec2 (0.0, 11.0),  Math.pi / 2.0, Block ()),
                      (BDDMath.vec2 (0.0, 13.0),  Math.pi / 2.0, Block ()),
                      (BDDMath.vec2 (0.0, 15.0),  Math.pi / 2.0, Block ()),
                      (BDDMath.vec2 (0.0, 17.0),  Math.pi / 2.0, Block ()),
                      (BDDMath.vec2 (0.0, 19.0),  Math.pi / 2.0, Block ())]
          val rightwall = 
                     [(BDDMath.vec2 (20.0, 1.0), Math.pi / 2.0, Block ()),
                      (BDDMath.vec2 (20.0, 3.0),  Math.pi / 2.0, Block ()),
                      (BDDMath.vec2 (20.0, 5.0),  Math.pi / 2.0, Block ()),
                      (BDDMath.vec2 (20.0, 7.0),  Math.pi / 2.0, Block ()),
                      (BDDMath.vec2 (20.0, 9.0),  Math.pi / 2.0, Block ()),
                      (BDDMath.vec2 (20.0, 11.0),  Math.pi / 2.0, Block ()),
                      (BDDMath.vec2 (20.0, 13.0),  Math.pi / 2.0, Block ()),
                      (BDDMath.vec2 (20.0, 15.0),  Math.pi / 2.0, Block ()),
                      (BDDMath.vec2 (20.0, 17.0),  Math.pi / 2.0, Block ()),
                      (BDDMath.vec2 (20.0, 19.0),  Math.pi / 2.0, Block ())]
      in case n 
          of
             1 =>
             let val bs = floor @ ceiling @ leftwall @ rightwall @ 
                     [
                      (BDDMath.vec2 (8.5, 19.3), Math.pi, Lightbulb ()),
                      (BDDMath.vec2 (10.0, 19.3), Math.pi, Lightbulb ()),
                      (BDDMath.vec2 (11.5, 19.3), Math.pi, Lightbulb ())
                     ]

                 val rbs = List.tabulate
                           (20,
                            fn i =>
                               let val v = random_vec ()
                                   val go = random_vec_in ~1.0 1.0 ~1.0 1.0
                               in (v,
                                   0.0,
                                   Moth {health = ref 1.0,
                                         goal = ref (v :+: go),
                                         dna = random_dna () })
                               end)
             in bs @ rbs end
           | 2 =>
             let val bs = floor @ ceiling @ leftwall @ rightwall @ 
                     [
                      (BDDMath.vec2 (19.3, 10.0), Math.pi / 2.0, Lightbulb ()),
                      (BDDMath.vec2 (0.7, 10.0), ~ Math.pi / 2.0, Lightbulb ())
                     ]

                 val rbs = List.tabulate
                           (25,
                            fn i =>
                               let val v = random_vec ()
                                   val go = random_vec_in ~1.0 1.0 ~1.0 1.0
                               in (v,
                                   0.0,
                                   Moth {health = ref 1.0,
                                         goal = ref (v :+: go),
                                         dna = random_dna () })
                               end)
             in bs @ rbs end

           | 3 =>
             let                 
                 val rtot = 0.70710678
                 val bs = floor @ ceiling @ leftwall @ rightwall @ 
                     [
                      (BDDMath.vec2 (10.0, 10.0), ~ Math.pi / 4.0, Block ()),
                      ((BDDMath.vec2 (10.0, 10.0))
                           :+: (BDDMath.vec2 (0.7 * rtot, 0.7 * rtot)),
                       ~ Math.pi / 4.0, Lightbulb ()),
                      ((BDDMath.vec2 (10.0, 10.0))
                           :-: (BDDMath.vec2 (0.7 * rtot, 0.7 * rtot)),
                       3.0 * Math.pi / 4.0, Lightbulb ())

                     ]


                 val rbs = List.tabulate
                           (75,
                            fn i =>
                               let val v = random_vec ()
                                   val go = random_vec_in ~1.0 1.0 ~1.0 1.0
                               in (v,
                                   0.0,
                                   Moth {health = ref 1.0,
                                         goal = ref (v :+: go),
                                         dna = random_dna () })
                               end)
             in bs @ rbs end

           | _ =>
             let                 
                 val rtot = 0.70710678
                 val bs = floor @ ceiling @ leftwall @ rightwall @ 
                     [
                      (BDDMath.vec2 (5.0, 10.0), Math.pi / 2.0, Block ()),
                      (BDDMath.vec2 (5.0, 12.0), Math.pi / 2.0, Block ()),
                      (BDDMath.vec2 (6.0, 13.0), 0.0, Block ()),
                      (BDDMath.vec2 (6.0, 11.0), 0.0, Block ()),
                      (BDDMath.vec2 (6.0, 9.0), 0.0, Block ()),

                      (BDDMath.vec2 (8.5, 10.0), Math.pi / 2.0, Block ()),
                      (BDDMath.vec2 (8.5, 12.0), Math.pi / 2.0, Block ()),
                      (BDDMath.vec2 (10.0, 11.0), ~Math.pi / 3.0, Block ()),
                      (BDDMath.vec2 (11.5, 10.0), Math.pi / 2.0, Block ()),
                      (BDDMath.vec2 (11.5, 12.0), Math.pi / 2.0, Block ()),


                      (BDDMath.vec2 (15.0, 12.3), ~ Math.pi / 8.0, Block ()),
                      (BDDMath.vec2 (15.0, 9.7),  Math.pi / 8.0, Block ()),
                      (BDDMath.vec2 (14.0, 10.0), Math.pi / 2.0, Block ()),
                      (BDDMath.vec2 (14.0, 12.0), Math.pi / 2.0, Block ()),

                      (BDDMath.vec2 (16.0, 11.0), Math.pi / 2.0, Block ()),




                      (BDDMath.vec2 (19.3, 10.0), Math.pi / 2.0, Lightbulb ()),
                      (BDDMath.vec2 (0.7, 10.0), ~ Math.pi / 2.0, Lightbulb ())


                     ]


                 val rbs = List.tabulate
                           (125,
                            fn i =>
                               let val v = random_vec ()
                                   val go = random_vec_in ~1.0 1.0 ~1.0 1.0
                               in (v,
                                   0.0,
                                   Moth {health = ref 1.0,
                                         goal = ref (v :+: go),
                                         dna = random_dna () })
                               end)
             in bs @ rbs end

      end



  (* do_hit b1 b2. b1 hits b2 *)
  fun do_hit b1 (Moth {health, ...}) =
      let val damage = 
              (case b1 of 
                   Moth _ => 0.002
                 | Lightbulb _ => 0.03
                 | Block _ => 0.01
                 | Ball _ => 0.05
              )
      in
          health := ((!health) - damage);
          if !health < 0.0 then health := 0.0 else ()
      end
    | do_hit _ _ = ()


  fun contact_listener c = 
      let
          val (fa, fb) = BDD.Contact.get_fixtures c
          val bda = BDD.Body.get_data (BDD.Fixture.get_body fa)
          val bdb = BDD.Body.get_data (BDD.Fixture.get_body fb)
          val () = (do_hit bda bdb; do_hit bdb bda)
      in () end


  fun setup_level level (constants as CONST {gravity, ...}) pers dna_list = 
      let 
          val ld = get_level_data constants level dna_list
          val num_moths = List.length
                          (List.filter (fn (p, a, b) =>
                                           case b of Moth _ => true
                                                   | _ => false)
                                       ld)
          val need_to_kill = (num_moths * 3) div 4
          val new_world = BDD.World.world (gravity, true)
          val () = populate new_world ld
          val () = BDD.World.set_begin_contact (new_world, contact_listener)
      in GS {world = new_world, level = level, constants = constants,
             killed = 0, need_to_kill = need_to_kill, persistent = pers}
      end



end
