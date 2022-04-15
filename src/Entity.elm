module Entity exposing
    ( Contacts
    , Entity
    , EntitySpawn
    , EntityType(..)
    , PlayerData
    , Wall
    , contactsWith
    , fromSpawns
    , getPlayer
    , respond
    , update
    )

{-| Game entities like player, gems, enemies and so on.
-}

import AltMath.Vector2 as Vector2 exposing (Vec2, vec2)
import Collision
import Dict exposing (Dict)
import Plat exposing (Platform)
import Playground exposing (Computer, Keyboard)
import Set exposing (Set)
import Vector2.Extra as Vector2


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


type alias PlayerData =
    { lastJumpTime : Int
    }


type EntityType
    = Gem
    | Player PlayerData


type alias Wall =
    { p1 : Vec2, p2 : Vec2, normal : Vec2 }


type alias EntitySpawn =
    { position : Vec2
    , side : Float
    , type_ : EntityType
    }


{-| Contact records a collision between entities. Used to make gameplay changes.
-}
type alias Contacts =
    Set ( Int, Int )


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


fromSpawns : List EntitySpawn -> Dict Int Entity
fromSpawns spawns =
    List.indexedMap
        (\index spawn_ ->
            spawn spawn_ (index + 1)
        )
        spawns
        |> Dict.fromList


spawn : EntitySpawn -> Int -> ( Int, Entity )
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


{-| Check for user input and do the integration step for the next frame.
-}
update : Computer -> List Wall -> List Platform -> Float -> Entity -> Entity
update computer walls platforms dt entity =
    entity
        |> input computer dt
        |> integrate walls platforms dt


input : Computer -> Float -> Entity -> Entity
input { keyboard, time } dt entity =
    case entity.type_ of
        Player data ->
            let
                axes =
                    Playground.toXY keyboard

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



-- MOVEMENT


integrate : List Wall -> List Platform -> Float -> Entity -> Entity
integrate walls platforms dt entity =
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

        exactAmount =
            round newRemainderX
    in
    if exactAmount /= 0 then
        moveXExact exactAmount walls { entity | remainder = Vector2.setX (newRemainderX - toFloat exactAmount) entity.remainder }

    else
        -- Save remainder for the next frame
        { entity | remainder = Vector2.setX newRemainderX entity.remainder }


moveY : Float -> List Wall -> Entity -> Entity
moveY amount walls entity =
    let
        newRemainderY =
            entity.remainder.y + amount

        exactAmount =
            round newRemainderY
    in
    if exactAmount /= 0 then
        moveYExact exactAmount walls { entity | remainder = Vector2.setY (newRemainderY - toFloat exactAmount) entity.remainder }

    else
        -- Save remainder for the next frame
        { entity | remainder = Vector2.setY newRemainderY entity.remainder }


moveXExact : Int -> List Wall -> Entity -> Entity
moveXExact exactAmount walls entity =
    -- Keep moving?
    if exactAmount /= 0 then
        let
            sign =
                direction exactAmount

            newEntity =
                { entity | position = Vector2.add (vec2 sign 0) entity.position }
        in
        if touchingWalls newEntity walls then
            -- Hit a wall, stop along the X axis and discard new position
            { entity
                | v = Vector2.setX 0 entity.v
                , lastContact = Vector2.setX (toFloat exactAmount) entity.lastContact
            }
                |> clearRemainderX

        else
            moveXExact (exactAmount - sign) walls newEntity

    else
        -- No contacts, clear value along X
        { entity | lastContact = Vector2.setX 0 entity.lastContact }


moveYExact : Int -> List Wall -> Entity -> Entity
moveYExact exactAmount walls entity =
    -- Keep moving?
    if exactAmount /= 0 then
        let
            sign =
                direction exactAmount

            newEntity =
                { entity | position = Vector2.add (vec2 0 sign) entity.position }
        in
        if touchingWalls newEntity walls then
            -- Hit a wall, stop along the Y axis and discard new position
            { entity
                | v = Vector2.setY 0 entity.v
                , lastContact = Vector2.setY (toFloat exactAmount) entity.lastContact
            }
                |> clearRemainderY

        else
            moveYExact (exactAmount - sign) walls newEntity

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
            if Collision.segment entity wall then
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
            if entity.id /= other.id && Collision.boundingBox entity other then
                -- Hit entity, register contact
                Set.insert ( entity.id, other.id ) accum

            else
                accum
        )
        contacts
        others


{-| Approach a target value increasing or decreasing by delta steps.
-}
approach value target delta =
    if value > target then
        max (value - delta) target

    else
        min (value + delta) target


direction value =
    if value == 0 then
        0

    else if value > 0 then
        1

    else
        -1


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
