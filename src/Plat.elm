module Plat exposing
    ( Platform
    , fromSpawns
    , update
    )

{-| Moving platforms interacting with game entities.
-}

import AltMath.Vector2 as Vector2 exposing (Vec2, vec2)
import Vector2.Extra as Vector2


type alias Platform =
    { remainder : Vec2
    , position : Vec2
    , startPosition : Vec2
    , v : Vec2
    , width : Float
    , height : Float
    , maxOffset : Float
    }


type alias PlatformSpawn =
    { position : Vec2
    , width : Float
    , height : Float
    , maxOffset : Float
    , period : Float
    }


fromSpawns : List PlatformSpawn -> List Platform
fromSpawns spawns =
    List.map
        (\spawn_ ->
            { remainder = Vector2.zero
            , position = spawn_.position
            , startPosition = spawn_.position
            , v = Vector2.scale (1 / spawn_.period) (vec2 0 spawn_.maxOffset)
            , width = spawn_.width
            , height = spawn_.height
            , maxOffset = spawn_.maxOffset
            }
        )
        spawns


update : Float -> Platform -> Platform
update dt platform =
    let
        v =
            if Vector2.distance platform.startPosition platform.position > abs platform.maxOffset then 
                -- Go back
                Vector2.negate platform.v

            else
                platform.v

        newPlatform =
            { platform
                | v = v
            }
    in
    move (Vector2.scale dt v) newPlatform


move : Vec2 -> Platform -> Platform
move amount platform =
    let
        newRemainder =
            Vector2.add platform.remainder amount

        moveX =
            round newRemainder.x

        moveY =
            round newRemainder.y
    in
    if moveX /= 0 || moveY /= 0 then
        { platform | remainder = vec2 (newRemainder.x - toFloat moveX) (newRemainder.y - toFloat moveY) }
            |> moveXExact moveX
            |> moveYExact moveY

    else
        -- Save remainder for the next frame
        { platform | remainder = newRemainder }


moveXExact : Int -> Platform -> Platform
moveXExact exactAmount platform =
    -- Keep moving?
    if exactAmount /= 0 then
        let
            newPlatform =
                { platform | position = Vector2.setX (platform.position.x + toFloat exactAmount) platform.position }
        in
        newPlatform

    else
        platform


moveYExact : Int -> Platform -> Platform
moveYExact exactAmount platform =
    -- Keep moving?
    if exactAmount /= 0 then
        let
            newPlatform =
                { platform | position = Vector2.setY (platform.position.y + toFloat exactAmount) platform.position }
        in
        newPlatform

    else
        platform
