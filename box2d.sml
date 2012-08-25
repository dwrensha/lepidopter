structure Box2d = 
struct
  open Types
  open BDDOps
  infix 6 :+: :-: %-% %+% +++
  infix 7 *: *% +*: +*+ #*% @*:

  val zero = BDDMath.vec2 (0.0, 0.0) 

  fun create_body world (p : BDDMath.vec2) (data as Moth {dna, ...}) : unit = 
      let 
          val body = BDD.World.create_body
                         (world,
                          {typ = BDD.Body.Dynamic,
                           position = p,
                           angle = 0.0,
                           linear_velocity = zero,
                           angular_velocity = 0.0,
                           linear_damping = 0.5,
                           angular_damping = 0.5,
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

          val fixture = BDD.Body.create_fixture_default
                            (body,
                             BDDShape.Polygon
                                 (BDDPolygon.polygon
                                      [BDDMath.vec2 (0.0, 0.0),
                                       BDDMath.vec2 (wing_width, ~half_height),
                                       BDDMath.vec2 (wing_width, half_height)]
                                 ),
                             Fix {color = color},
                             10.0)
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
                             Fix {color = color},
                             10.0)
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
                             Fix {color = RGB (0.0, 1.0, 1.0)},
                             100.0)
          val () = BDD.Fixture.set_restitution (fixture, 1.0)
          val () = BDD.Fixture.set_friction (fixture, 0.4)
      in () end




end
