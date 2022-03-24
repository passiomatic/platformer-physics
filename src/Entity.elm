module Entity exposing (Entity, EntityType(..), fromSpawns, update)

import AltMath.Vector2 as Vector2 exposing (Vec2, vec2)
import Dict exposing (Dict)
import Playground exposing (..)
import Vector2.Extra as Vector2


type EntityType
    = Gem
    | Player


type alias Entity =
    { remainder : Vec2
    , position : Vec2
    , v : Vec2
    , width : Float
    , height : Float
    , id : Int
    , type_ : EntityType
    , dir : Float
    }


{-| Spawn point for a game entity.
-}
type alias Spawn =
    { position : Vec2
    , dir : Float
    , type_ : EntityType
    }


gravity =
    -1000


playerMaxRunVelocity =
    270


playerRunAcceleration =
    700


playerJumpSpeed =
    190


playerMaxFallVelocity =
    220


fromSpawns : List Spawn -> Dict Int Entity
fromSpawns spawns =
    List.indexedMap
        (\index spawn_ ->
            spawn spawn_ (index + 1)
        )
        spawns
        |> Dict.fromList


spawn : Spawn -> Int -> ( Int, Entity )
spawn spawn_ nextId =
    let
        entity =
            { id = nextId
            , v = Vector2.zero
            , remainder = Vector2.zero
            , position = spawn_.position
            , dir = spawn_.dir
            , type_ = spawn_.type_
            , width = 10
            , height = 10
            }
    in
    case spawn_.type_ of
        Player ->
            ( 0, initPlayer entity )

        Gem ->
            ( nextId, initGem entity )


initPlayer entity =
    { entity
        | id = 0 -- Hardcode id
        , remainder = Vector2.zero
        , width = 15
        , height = 18
        , type_ = Player
    }


initGem entity =
    { entity
        | width = 10
        , height = 10
        , type_ = Gem
    }


update : Keyboard -> Float -> Entity -> Entity
update keyboard dt entity =
    case entity.type_ of
        Player ->
            let
                dir =
                    inputDirection keyboard entity.dir

                -- Horizontal control
                maxRunVelocity =
                    if keyboard.left then
                        -playerMaxRunVelocity

                    else if keyboard.right then
                        playerMaxRunVelocity

                    else
                        0

                vx =
                    approach entity.v.x maxRunVelocity (playerRunAcceleration * dt)

                -- Vertical control
                vy =
                    -- Jumping?
                    if keyboard.space then
                        playerJumpSpeed

                    else
                        approach entity.v.y playerMaxFallVelocity (gravity * dt)
            in
            { entity
                | v = vec2 vx vy
                , dir = dir
            }

        -- Other entities
        _ ->
            entity


{-| Figure out entity horizontal direction from keyboard status.
-}
inputDirection : Keyboard -> Float -> Float
inputDirection keyboard dir =
    let
        ( newDir, _ ) =
            Playground.toXY keyboard
    in
    if newDir /= 0 then
        newDir

    else
        -- Keep last direction
        dir


{-| Approach a target value increasing or decreasing by maxDelta steps.
-}
approach value target maxDelta =
    if value > target then
        max (value - maxDelta) target

    else
        min (value + maxDelta) target
