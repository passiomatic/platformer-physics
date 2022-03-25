module Main exposing (..)

import AltMath.Vector2 as Vector2 exposing (Vec2, vec2)
import Diagnostic
import Dict exposing (Dict)
import Entity exposing (Entity, EntityType(..))
import Levels exposing (level1)
import Playground exposing (..)
import Set exposing (Set)
import Vector2.Extra as Vector2


type alias Memory =
    { entities : Dict Int Entity
    , gems : Int
    , debug : Bool
    , lastLogTime : Int
    , lastVelocity : Vec2
    }


type alias Wall =
    { position : Vec2, width : Float, height : Float }


type alias Contacts =
    Set ( Int, Int )


initialModel : Memory
initialModel =
    let
        entities =
            Entity.fromSpawns level1.spawns
    in
    { entities = entities
    , gems = 0
    , debug = True
    , lastLogTime = 0
    , lastVelocity = Vector2.zero
    }


viewScale =
    2


logTimeInterval =
    100



-- ms


view : Computer -> Memory -> List Shape
view computer { debug, lastVelocity, entities } =
    [ renderBackground computer.screen
    , renderScene debug level1.walls entities
        |> scale viewScale
    , Diagnostic.vector green lastVelocity
        |> moveDown 100
    ]


renderScene : Bool -> List Wall -> Dict Int Entity -> Shape
renderScene debug walls entities =
    let
        walls_ =
            List.map Diagnostic.wall walls

        entities_ =
            Dict.map
                (\_ entity ->
                    case entity.type_ of
                        Player _ ->
                            renderPlayer debug entity

                        Gem ->
                            renderGem debug entity
                )
                entities
                |> Dict.values
    in
    List.append entities_ (renderOrigin :: walls_)
        |> group


renderPlayer debug entity =
    [ Diagnostic.body entity
    , polygon blue
        [ ( -5, 7 )
        , ( 5, 7 )
        , ( 8, 0 )
        , ( 5, 0 )
        , ( 5, -7 )
        , ( -5, -7 )
        ]
        |> scaleX entity.side
        |> move entity.position.x entity.position.y
    ]
        |> group


renderGem debug entity =
    [ Diagnostic.body entity
    , polygon red
        [ ( -13, 5 )
        , ( -5, 10 )
        , ( 5, 10 )
        , ( 13, 5 )
        , ( 0, -9 )
        ]
        |> move entity.position.x entity.position.y
    ]
        |> group


renderOrigin =
    circle lightBlue 2


renderBackground screen =
    rectangle black screen.width screen.height


message text =
    words black text
        |> move 0 -150



-- UPDATE


fixedDeltaTime =
    1 / 60


update : Computer -> Memory -> Memory
update computer memory =
    let
        ( lastVelocity, lastLogTime ) =
            if computer.time.now - memory.lastLogTime > logTimeInterval then
                ( Dict.get 0 memory.entities
                    |> Maybe.map .v
                    |> Maybe.withDefault Vector2.zero
                , computer.time.now
                )

            else
                ( memory.lastVelocity, memory.lastLogTime )

        -- TODO fix dt
        dt =
            min fixedDeltaTime (toFloat computer.time.delta / 1000)

        --Debug.log "dt" <| min fixedDeltaTime (toFloat computer.time.delta / 1000)
        newEntities =
            Dict.map
                (\_ entity ->
                    entity
                        |> Entity.update computer dt
                        |> simulate level1.walls dt
                 --|> contactWithEntities
                )
                memory.entities
    in
    -- TODO respond to contacts
    { memory | entities = newEntities, lastVelocity = lastVelocity, lastLogTime = lastLogTime }


simulate : List Wall -> Float -> Entity -> Entity
simulate walls dt entity =
    let
        v =
            Vector2.scale dt entity.v
    in
    entity
        |> moveX v.x walls
        |> moveY v.y walls



-- HELPERS


moveX : Float -> List Wall -> Entity -> Entity
moveX amount walls entity =
    let
        newRemainderX =
            entity.remainder.x + amount

        move =
            round newRemainderX
    in
    if move /= 0 then
        moveXExact move { entity | remainder = Vector2.setX (newRemainderX - toFloat move) entity.remainder } walls

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
        moveYExact move { entity | remainder = Vector2.setY (newRemainderY - toFloat move) entity.remainder } walls

    else
        -- Save remainder for the next frame
        { entity | remainder = Vector2.setY newRemainderY entity.remainder }


moveXExact : Int -> Entity -> List Wall -> Entity
moveXExact move entity walls =
    -- Keep moving?
    if move /= 0 then
        let
            sign =
                direction move

            newEntity =
                { entity | position = Vector2.add (vec2 sign 0) entity.position }
        in
        if isCollidingWithWalls newEntity walls then
            -- Hit a wall, stop and discard new position
            { entity | v = Vector2.setX 0 entity.v }
                |> clearRemainderX

        else
            moveXExact (move - sign) newEntity walls

    else
        entity


moveYExact : Int -> Entity -> List Wall -> Entity
moveYExact move entity walls =
    -- Keep moving?
    if move /= 0 then
        let
            sign =
                direction move

            newEntity =
                { entity | position = Vector2.add (vec2 0 sign) entity.position }
        in
        if isCollidingWithWalls newEntity walls then
            -- Hit a wall, stop and discard new position
            { entity | v = Vector2.setY 0 entity.v }
                |> clearRemainderY

        else
            moveYExact (move - sign) newEntity walls

    else
        entity


clearRemainderX entity =
    { entity | remainder = vec2 0 entity.remainder.y }


clearRemainderY entity =
    { entity | remainder = vec2 entity.remainder.x 0 }


isCollidingWithWalls : Entity -> List Wall -> Bool
isCollidingWithWalls entity walls =
    case walls of
        wall :: rest ->
            if intersectsBoundingBox entity wall then
                True

            else
                -- Keep checking
                isCollidingWithWalls entity rest

        [] ->
            False


contactWithEntities : Entity -> List Entity -> Contacts -> Contacts
contactWithEntities entity others contacts =
    List.foldl
        (\other accum ->
            -- Skip self
            if entity.id /= other.id then
                if intersectsBoundingBox entity other then
                    -- Hit entity, register contact
                    Set.insert ( entity.id, other.id ) accum

                else
                    accum

            else
                accum
        )
        contacts
        others


intersectsBoundingBox : { a | position : Vec2, width : Float, height : Float } -> { b | position : Vec2, width : Float, height : Float } -> Bool
intersectsBoundingBox rect1 rect2 =
    let
        startingPoint centerPoint length =
            centerPoint - (length / 2)

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


direction value =
    if value == 0 then
        0

    else if value > 0 then
        1

    else
        -1



-- ENTRY POINT


main =
    Playground.game view update initialModel
