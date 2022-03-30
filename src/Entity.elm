module Entity exposing
    ( Entity
    , EntityType(..)
    , PlayerData
    , Spawn
    , Wall
    , fromSpawns
    , getPlayer
    , respond
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
    800


{-| Gives you slightly less control of horizontal motion in the air.
-}
playerAirAccelerationMult =
    0.8


playerJumpVelocity =
    350


playerFallVelocity =
    430


playerNextJumpInterval =
    350


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
            , width = 16    
            , height = 14
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
        , width = 13
        , height = 14
    }


initGem entity =
    -- Just use default values
    entity


getPlayer entities =
    Dict.get 0 entities


update : Computer -> Float -> Entity -> Entity
update { keyboard, time } dt entity =
    case entity.type_ of
        Player data ->
            let
                axes =
                    toXY keyboard

                onGround =
                    isOnGround entity

                -- Horizontal control
                runVelocity =
                    playerRunVelocity * Tuple.first axes

                runAcceleration =
                    if onGround then
                        playerRunAcceleration

                    else
                        playerRunAcceleration * playerAirAccelerationMult

                vx =
                    approach entity.v.x runVelocity (runAcceleration * dt)

                -- Vertical control
                ( jumpTime, vy ) =
                    if keyboard.space && (onGround || (time.now - data.lastJumpTime) > playerNextJumpInterval) then
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


{-| Contact response logic.
-}
respond : Int -> Int -> { a | entities : Dict Int Entity, gems : Int } -> { a | entities : Dict Int Entity, gems : Int }
respond id1 id2 memory =
    case ( Dict.get id1 memory.entities, Dict.get id2 memory.entities ) of
        ( Just entity1, Just entity2 ) ->
            case ( entity1.type_, entity2.type_ ) of
                -- Pick up gem
                ( Player _, Gem ) ->
                    { memory
                        | gems = memory.gems + 1
                        , entities = Dict.remove entity2.id memory.entities
                    }

                ( _, _ ) ->
                    memory

        ( _, _ ) ->
            memory


{-| Figure out entity "side" for rendering purposes.
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
