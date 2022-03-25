module Diagnostic exposing
    ( consIf
    , hitbox
    , vector
    , wall
    )

import AltMath.Vector2 as Vec2 exposing (Vec2, vec2)
import Playground exposing (..)
import Vector2.Extra as Vec2



-- SHAPES


{-| Draw hitbox for given entity.
-}
hitbox : { a | width : Float, height : Float, position : Vec2 } -> Shape
hitbox { width, height, position } =
    rectangle yellow width height
        |> fade 0.6
        |> move position.x position.y


wall : { a | position : Vec2, width : Float, height : Float } -> Shape
wall { position, width, height } =
    rectangle darkGray width height
        |> move position.x position.y


segment : Color -> { a | p1 : Vec2, p2 : Vec2 } -> Shape
segment color { p1, p2 } =
    let
        width =
            Vec2.sub p1 p2
                |> Vec2.length

        angle =
            atan2 (p2.y - p1.y) (p2.x - p1.x) * 180 / pi
    in
    [ rectangle color width 1
    , triangle color 4
    , circle color 2
        |> moveX (width / -2)
    ]
        |> group
        |> move ((p1.x + p2.x) / 2) ((p1.y + p2.y) / 2)
        |> rotate angle


vector : Color -> String -> Vec2 -> Shape
vector color label value =
    let
        angle =
            atan2 value.y value.x * 180 / pi

        width =
            Vec2.length value
    in
    [ [ rectangle color width 3
      , circle color 5
            |> moveLeft (width / 2)
      ]
        -- Show arrow only for non-zero vectors
        |> consIf (width - 0.5 > 0)
            (triangle color 7
                |> rotate -90
                |> moveRight (width / 2)
            )
        |> group
        |> move (value.x / 2) (value.y / 2)
        |> rotate angle
    , words color (label ++ " " ++ Vec2.toString value)
        |> moveDown 50
    ]
        |> group


consIf pred x xs =
    if pred then
        x :: xs

    else
        xs
