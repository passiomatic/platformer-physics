module Vector2.Extra exposing
    ( rotateCounterclockwise
    , toString
    , zero
    )

import AltMath.Vector2 as Vector2 exposing (Vec2, vec2)

zero =
    vec2 0 0


{-| Rotate vector 90 degrees counterclockwise.
-}
rotateCounterclockwise : Vec2 -> Vec2
rotateCounterclockwise value =
    vec2 -value.y value.x


toString : Vec2 -> String
toString value =
    String.fromInt (round value.x) ++ ";" ++ String.fromInt (round value.y)
