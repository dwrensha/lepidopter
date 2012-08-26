structure Box2d = 
struct
  open Types
  open BDDOps
  infix 6 :+: :-: %-% %+% +++
  infix 7 *: *% +*: +*+ #*% @*:

  val zero = BDDMath.vec2 (0.0, 0.0) 

  fun create_body world (p : BDDMath.vec2) (data as Moth {dna, health, ...}) : unit = 
      let 
          val body = BDD.World.create_body
                         (world,
                          {typ = BDD.Body.Dynamic,
                           position = p,
                           angle = 0.0,
                           linear_velocity = zero,
                           angular_velocity = 0.0,
                           linear_damping = 0.5,
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

    | create_body world (p : BDDMath.vec2) (data as Block ()) : unit = 
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
                           data = data,
                           inertia_scale = 1.0
                         })

          val fixture = BDD.Body.create_fixture_default
                            (body,
                             BDDShape.Polygon
                                 (BDDPolygon.box (0.3, 0.3)),
                             Fix {color = RGB (0.0, 1.0, 1.0), health = ref 1.0},
                             100.0)
          val () = BDD.Fixture.set_restitution (fixture, 1.0)
          val () = BDD.Fixture.set_friction (fixture, 0.4)
      in () end
    | create_body world (p : BDDMath.vec2) (data as Ball ()) : unit = 
      let 
          val body = BDD.World.create_body
                         (world,
                          {typ = BDD.Body.Dynamic,
                           position = p,
                           angle = 0.0,
                           linear_velocity = BDDMath.vec2 (0.0, 0.5),
                           angular_velocity = 0.0,
                           linear_damping = 0.0,
                           angular_damping = 0.0,
                           allow_sleep = false,
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
                             Fix {color = RGB (1.0, 0.2, 0.2), health = ref 1.0},
                             10.0)
          val () = BDD.Fixture.set_restitution (fixture, 1.0)
          val () = BDD.Fixture.set_friction (fixture, 0.4)
      in () end
    | create_body world (p : BDDMath.vec2) (data as Lightbulb ()) : unit = 
      let 
          val body = BDD.World.create_body
                         (world,
                          {typ = BDD.Body.Static,
                           position = p,
                           angle = 0.0,
                           linear_velocity = BDDMath.vec2 (0.0, 0.5),
                           angular_velocity = 0.0,
                           linear_damping = 0.0,
                           angular_damping = 0.0,
                           allow_sleep = false,
                           awake = true,
                           fixed_rotation = false,
                           bullet = true,
                           active = true,
                           data = data,
                           inertia_scale = 1.0
                         })

          val color = RGB (1.0, 1.0, 0.4)
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



  val mt =
      MersenneTwister.initstring (Time.toString (Time.now ()))

  fun random_vec_in left right bottom top =
      let open MersenneTwister
          val x = (Real.fromInt (random_nat mt 1000)) / 1000.0
          val y = (Real.fromInt (random_nat mt 1000)) / 1000.0
      in BDDMath.vec2
             (x * (right - left) + left,
              y * (top - bottom) + bottom)
      end

  fun populate world [] = ()
    | populate world ((pos, body_data)::bodies) =
      let val () = create_body world pos body_data
      in populate world bodies end

  fun get_level_data constants n = 
      let val CONST {left, right, bottom, top, ...} = constants
          fun random_vec () = random_vec_in left right bottom top
      in case n 
          of 1 =>
             let val bs =
                     [(BDDMath.vec2 (10.0, 10.0),
                       Moth {health = ref 1.0,
                             goal = ref (BDDMath.vec2 (15.0, 15.0)),
                             dna = DNA.moth1 }),
                      (BDDMath.vec2 (10.0, 17.0),
                       Moth {health = ref 1.0,
                             goal = ref (BDDMath.vec2 (15.0, 15.0)),
                             dna = DNA.moth2 }),
                      (BDDMath.vec2 (1.0, 7.0),
                       Moth {health = ref 1.0,
                             goal = ref (BDDMath.vec2 (15.0, 15.0)),
                             dna = DNA.random () }),
                      (BDDMath.vec2 (10.0, 1.0), Block ()),
                      (BDDMath.vec2 (15.0, 14.7), Lightbulb ()),
                      (BDDMath.vec2 (5.0, 12.8), Lightbulb ())
                     ]

                 val rbs = List.tabulate
                           (50,
                            fn i =>
                               (random_vec (),
                                Moth {health = ref 1.0,
                                       goal = ref (random_vec()),
                                       dna = DNA.random () }))
             in bs @ rbs end
           | _ => nil
      end



  fun take_hit (Moth {health, ...}) = 
      (health := ((!health) - 0.03);
       if !health < 0.0 then health := 0.0 else ()
      )
    | take_hit _ = ()

  fun contact_listener c = 
      let
          val (fa, fb) = BDD.Contact.get_fixtures c
          val bda = BDD.Body.get_data (BDD.Fixture.get_body fa)
          val bdb = BDD.Body.get_data (BDD.Fixture.get_body fb)
          val () = (take_hit bda; take_hit bdb)
      in () end


  fun setup_level level (constants as CONST {gravity, ...}) pers = 
      let 
          val ld = get_level_data constants level
          val new_world = BDD.World.world (gravity, true)
          val () = populate new_world ld
          val () = BDD.World.set_begin_contact (new_world, contact_listener)
      in GS {world = new_world, level = level, constants = constants, persistent = pers}
      end



end
