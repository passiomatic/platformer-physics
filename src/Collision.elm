module Collision exposing
    ( boundingBox
    , segment
    )

import AltMath.Vector2 as Vector2 exposing (Vec2, vec2)


boundingBox : { a | position : Vec2, width : Float, height : Float } -> { b | position : Vec2, width : Float, height : Float } -> Bool
boundingBox rect1 rect2 =
    let
        x1 =
            startingPoint rect1.position.x rect1.width

        x2 =
            startingPoint rect2.position.x rect2.width

        y1 =
            startingPoint rect1.position.y rect1.height

        y2 =
            startingPoint rect2.position.y rect2.height
    in
    x1 < x2 + rect2.width && x1 + rect1.width > x2 && y1 < y2 + rect2.height && rect1.height + y1 > y2


segment : { a | position : Vec2, width : Float, height : Float } -> { b | p1 : Vec2, p2 : Vec2 } -> Bool
segment rect { p1, p2 } =
    let
        x1 =
            startingPoint rect.position.x rect.width

        y1 =
            startingPoint rect.position.y rect.height
    in
    -- Check if segment is completely outside rect
    not ((p1.x <= x1 && p2.x <= x1) || (p1.y <= y1 && p2.y <= y1) || (p1.x >= x1 + rect.width && p2.x >= x1 + rect.width) || (p1.y >= y1 + rect.height && p2.y >= y1 + rect.height))


startingPoint centerPoint length =
    centerPoint - (length / 2)
