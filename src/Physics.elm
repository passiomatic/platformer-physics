module Physics exposing
    ( Contacts
    , contactsWith
    , simulate
    )

import AltMath.Vector2 as Vector2 exposing (Vec2, vec2)
import Dict exposing (Dict)
import Entity exposing (Entity, Wall)
import Set exposing (Set)
import Vector2.Extra as Vector2


type alias Contacts =
    Set ( Int, Int )


simulate : List Wall -> Float -> Entity -> Entity
simulate walls dt entity =
    let
        v =
            Vector2.scale dt entity.v
    in
    entity
        |> moveX v.x walls
        |> moveY v.y walls


moveX : Float -> List Wall -> Entity -> Entity
moveX amount walls entity =
    let
        newRemainderX =
            entity.remainder.x + amount

        move =
            round newRemainderX
    in
    if move /= 0 then
        moveXExact move walls { entity | remainder = Vector2.setX (newRemainderX - toFloat move) entity.remainder }

    else
        -- Save remainder for the next frame
        { entity | remainder = Vector2.setX newRemainderX entity.remainder }


moveY : Float -> List Wall -> Entity -> Entity
moveY amount walls entity =
    let
        newRemainderY =
            entity.remainder.y + amount

        move =
            round newRemainderY
    in
    if move /= 0 then
        moveYExact move walls { entity | remainder = Vector2.setY (newRemainderY - toFloat move) entity.remainder }

    else
        -- Save remainder for the next frame
        { entity | remainder = Vector2.setY newRemainderY entity.remainder }


moveXExact : Int -> List Wall -> Entity -> Entity
moveXExact move walls entity =
    -- Keep moving?
    if move /= 0 then
        let
            sign =
                direction move

            newEntity =
                { entity | position = Vector2.add (vec2 sign 0) entity.position }
        in
        if touchingWalls newEntity walls then
            -- Hit a wall, stop along the X axis and discard new position
            { entity
                | v = Vector2.setX 0 entity.v
                , lastContact = Vector2.setX (toFloat move) entity.lastContact
            }
                |> clearRemainderX

        else
            moveXExact (move - sign) walls newEntity

    else
        -- No contacts, clear value along X
        { entity | lastContact = Vector2.setX 0 entity.lastContact }


moveYExact : Int -> List Wall -> Entity -> Entity
moveYExact move walls entity =
    -- Keep moving?
    if move /= 0 then
        let
            sign =
                direction move

            newEntity =
                { entity | position = Vector2.add (vec2 0 sign) entity.position }
        in
        if touchingWalls newEntity walls then
            -- Hit a wall, stop along the Y axis and discard new position
            { entity
                | v = Vector2.setY 0 entity.v
                , lastContact = Vector2.setY (toFloat move) entity.lastContact
            }
                |> clearRemainderY

        else
            moveYExact (move - sign) walls newEntity

    else
        -- No contacts, clear value along Y
        { entity | lastContact = Vector2.setY 0 entity.lastContact }


clearRemainderX entity =
    { entity | remainder = Vector2.setX 0 entity.remainder }


clearRemainderY entity =
    { entity | remainder = Vector2.setY 0 entity.remainder }


touchingWalls : Entity -> List Wall -> Bool
touchingWalls entity walls =
    case walls of
        wall :: rest ->
            if intersectsSegment entity wall then
                -- Along or against the wall normal?
                Vector2.dot entity.v wall.normal < 0

            else
                -- Keep checking
                touchingWalls entity rest

        [] ->
            False


contactsWith : Entity -> Dict Int Entity -> Contacts -> Contacts
contactsWith entity others contacts =
    Dict.foldl
        (\_ other accum ->
            if entity.id /= other.id && intersectsBoundingBox entity other then
                -- Hit entity, register contact
                Set.insert ( entity.id, other.id ) accum

            else
                accum
        )
        contacts
        others


intersectsBoundingBox : { a | position : Vec2, width : Float, height : Float } -> { b | position : Vec2, width : Float, height : Float } -> Bool
intersectsBoundingBox rect1 rect2 =
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


intersectsSegment : { a | position : Vec2, width : Float, height : Float } -> { b | p1 : Vec2, p2 : Vec2 } -> Bool
intersectsSegment rect { p1, p2 } =
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


direction value =
    if value == 0 then
        0

    else if value > 0 then
        1

    else
        -1
