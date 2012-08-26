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
                             BDDShape.Circle {radius = 0.5, p = zero},
(*                              BDDShape.Polygon
                                 (BDDPolygon.box (0.3, 0.3)), *)
                             Fix {color = RGB (1.0, 0.0, 1.0), health = ref 1.0},
                             10.0)
          val () = BDD.Fixture.set_restitution (fixture, 1.0)
          val () = BDD.Fixture.set_friction (fixture, 0.4)
      in () end



  val mt =
      MersenneTwister.initstring (Time.toString (Time.now ()))

  fun random_vec left right bottom top =
      let open MersenneTwister
          val x = (Real.fromInt (random_nat mt 1000)) / 1000.0
          val y = (Real.fromInt (random_nat mt 1000)) / 1000.0
      in BDDMath.vec2
             (x * (right - left) + left,
              y * (top - bottom) + bottom)
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


end
