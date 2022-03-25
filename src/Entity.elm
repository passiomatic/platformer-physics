module Entity exposing
    ( Entity
    , EntityType(..)
    , PlayerData
    , Wall
    , fromSpawns
    , getPlayer
    , update
    )

import AltMath.Vector2 as Vector2 exposing (Vec2, vec2)
import Dict exposing (Dict)
import Playground exposing (..)
import Vector2.Extra as Vector2


type alias Wall =
    { p1 : Vec2, p2 : Vec2, normal : Vec2 }


type alias PlayerData =
    { lastJumpTime : Int
    }


type EntityType
    = Gem
    | Player PlayerData


type alias Entity =
    { remainder : Vec2
    , position : Vec2
    , v : Vec2
    , lastContact : Vec2
    , width : Float
    , height : Float
    , id : Int
    , type_ : EntityType
    , side : Float
    }


{-| Spawn point for a game entity.
-}
type alias Spawn =
    { position : Vec2
    , side : Float
    , type_ : EntityType
    }


gravity =
    -1100


playerRunVelocity =
    270


playerRunAcceleration =
    650


playerJumpVelocity =
    350


playerFallVelocity =
    430


playerNextJumpInterval =
    150


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
            , lastContact = Vector2.zero
            , position = spawn_.position
            , side = spawn_.side
            , type_ = spawn_.type_
            , width = 10
            , height = 10
            }
    in
    case spawn_.type_ of
        Player _ ->
            ( 0, initPlayer entity )

        Gem ->
            ( nextId, initGem entity )


initPlayer entity =
    { entity
        | id = 0 -- Hardcode id
        , remainder = Vector2.zero
        , width = 13
        , height = 14
    }


getPlayer entities =
    Dict.get 0 entities


initGem entity =
    { entity
        | width = 10
        , height = 10
    }


update : Computer -> Float -> Entity -> Entity
update { keyboard, time } dt entity =
    case entity.type_ of
        Player data ->
            let
                axes =
                    toXY keyboard

                shouldJump =
                    keyboard.space && isOnGround entity

                jumpBoostX =
                    -- If left or right is held at the moment of a jump, horizontal speed boost is applied
                    if shouldJump then
                        30 * Tuple.first axes

                    else
                        0

                -- Horizontal control
                runVelocity =
                    playerRunVelocity * Tuple.first axes

                vx =
                    approach (entity.v.x + jumpBoostX) runVelocity (playerRunAcceleration * dt)

                -- Vertical control
                ( jumpTime, vy ) =
                    if shouldJump then
                        --if keyboard.space && entity.v.y < 0 && (time.now - data.lastJumpTime) > playerNextJumpInterval then
                        ( time.now, playerJumpVelocity )

                    else
                        ( data.lastJumpTime, approach entity.v.y playerFallVelocity (gravity * dt) )
            in
            { entity
                | v = vec2 vx vy
                , side = side keyboard entity.side
                , type_ = Player (PlayerData jumpTime)
            }

        -- Other entities
        _ ->
            entity


isOnGround entity =
    entity.lastContact.y < 0


{-| Figure out entity "side" for render purposes,
-}
side : Keyboard -> Float -> Float
side keyboard dir =
    let
        ( newDir, _ ) =
            Playground.toXY keyboard
    in
    if newDir /= 0 then
        newDir

    else
        -- Keep last direction
        dir


{-| Approach a target value increasing or decreasing by delta steps.
-}
approach value target delta =
    if value > target then
        max (value - delta) target

    else
        min (value + delta) target
