module Main exposing (..)

import AltMath.Vector2 as Vector2 exposing (Vec2, vec2)
import Diagnostic
import Dict exposing (Dict)
import Entity exposing (Entity, EntityType(..), Spawn, Wall)
import Levels exposing (level1)
import Physics exposing (Contacts)
import Playground exposing (..)
import Vector2.Extra as Vector2


type alias Memory =
    { entities : Dict Int Entity
    , gems : Int
    , camera : Vec2
    , debug : Bool
    , lastLogTime : Int
    , lastVelocity : Vec2
    , lastContact : Vec2
    }


initialModel : Memory
initialModel =
    let
        entities =
            Entity.fromSpawns level1.spawns
    in
    { entities = entities
    , gems = 0
    , camera = findCameraPosition level1.spawns
    , debug = True
    , lastLogTime = 0
    , lastVelocity = Vector2.zero
    , lastContact = Vector2.zero
    }


viewScale =
    2


logTimeInterval =
    100


minDistanceFromEdge =
    90


cameraSpeed =
    1.8


view : Computer -> Memory -> List Shape
view computer memory =
    [ renderBackground computer.screen
    , renderScene memory.debug level1.walls memory.entities
        |> move (-memory.camera.x * viewScale) -(memory.camera.y * viewScale)
    , renderPhysics memory
    ]


renderPhysics : Memory -> Shape
renderPhysics memory =
    (if memory.debug then
        [ Diagnostic.vector green "v" memory.lastVelocity
            |> moveDown 100
            |> moveLeft 150
        , Diagnostic.vector yellow "lastContact" memory.lastContact
            |> moveDown 100
            |> moveRight 150
        ]

     else
        []
    )
        |> group


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
    List.append walls_ entities_
        |> group
        |> scale viewScale


renderPlayer debug entity =
    [ polygon blue
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
        |> Diagnostic.consIf debug (Diagnostic.hitbox entity)
        |> group


renderGem debug entity =
    [ polygon red
        [ ( -13, 5 )
        , ( -5, 10 )
        , ( 5, 10 )
        , ( 13, 5 )
        , ( 0, -9 )
        ]
        |> move entity.position.x entity.position.y
    ]
        |> Diagnostic.consIf debug (Diagnostic.hitbox entity)
        |> group



-- renderOrigin =
--     circle lightBlue 2


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
        -- TODO fix dt
        dt =
            min fixedDeltaTime (toFloat computer.time.delta / 1000)

        --Debug.log "dt" <| min fixedDeltaTime (toFloat computer.time.delta / 1000)
        newEntities =
            Dict.map
                (\_ entity ->
                    entity
                        |> Entity.update computer dt
                        |> Physics.simulate level1.walls dt
                 --|> contactWithEntities
                )
                memory.entities
    in
    -- TODO respond to contacts
    { memory
        | entities = newEntities
    }
        |> updateCamera computer dt
        |> logValues computer


logValues computer memory =
    if memory.debug && computer.time.now - memory.lastLogTime > logTimeInterval then
        case Entity.getPlayer memory.entities of
            Just player ->
                { memory
                    | lastVelocity = player.v
                    , lastContact = player.lastContact
                    , lastLogTime = computer.time.now
                }

            Nothing ->
                memory

    else
        memory


updateCamera : Computer -> Float -> Memory -> Memory
updateCamera computer dt memory =
    case Entity.getPlayer memory.entities of
        Just player ->
            { memory
                | camera = follow computer.screen dt player.position memory.camera
            }

        Nothing ->
            memory


{-| Attempt to init camera with player spawn position.
-}
findCameraPosition : List Spawn -> Vec2
findCameraPosition spawns =
    let
        maybeSpawn =
            List.filter
                (\spawn ->
                    case spawn.type_ of
                        Player _ ->
                            True

                        _ ->
                            False
                )
                spawns
                |> List.head
    in
    case maybeSpawn of
        Just spawn ->
            spawn.position

        Nothing ->
            Vector2.zero


{-| Follow a given target if it goes beyond a "safe area" centered on screen.
-}
follow : Screen -> Float -> Vec2 -> Vec2 -> Vec2
follow { width, height } dt target camera =
    let
        newX =
            -- Check if target is moving torwards left/right edges
            if target.x < (camera.x - width * 0.5 / viewScale + minDistanceFromEdge) then
                target.x

            else if target.x > (camera.x + width * 0.5 / viewScale - minDistanceFromEdge) then
                target.x

            else
                camera.x

        newY =
            -- Check if target is moving torwards top/bottom edges
            if target.y > (camera.y + height * 0.5 / viewScale - minDistanceFromEdge) then
                target.y

            else if target.y < (camera.y - height * 0.5 / viewScale + minDistanceFromEdge) then
                target.y

            else
                camera.y

        t =
            Vector2.sub (vec2 newX newY) camera
                |> Vector2.scale (dt * cameraSpeed)
    in
    -- Move camera to new position along t vector
    Vector2.add camera t



-- ENTRY POINT


main =
    Playground.game view update initialModel
