module Main exposing (..)

import AltMath.Vector2 as Vector2 exposing (Vec2, vec2)
import Diagnostic
import Dict exposing (Dict)
import Entity exposing (Entity, EntityType(..), Spawn, Wall)
import Levels exposing (level1)
import Physics exposing (Contacts)
import Playground exposing (..)
import Set exposing (Set)
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
    { entities = Entity.fromSpawns level1.spawns
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
    , renderMessage (String.fromInt memory.gems ++ " GEMS")
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
        [ ( -10, 5 )
        , ( -4, 8 )
        , ( 4, 8 )
        , ( 10, 5 )
        , ( 0, -7 )
        ]
        |> move entity.position.x entity.position.y
    ]
        |> Diagnostic.consIf debug (Diagnostic.hitbox entity)
        |> group


renderBackground screen =
    rectangle black screen.width screen.height


renderMessage text =
    words white text
        |> move 0 250



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
                )
                memory.entities

        -- Figure out contacts between player and other entities since we
        --   are not interested in generic entities vs. entities interactions
        newContacts =
            case Entity.getPlayer newEntities of
                Just player ->
                    Physics.contactsWith player newEntities Set.empty

                Nothing ->
                    Set.empty
    in
    { memory
        | entities = newEntities
    }
        |> resolveContacts newContacts
        |> updateCamera computer dt
        |> logValues computer


resolveContacts contacts memory =
    Set.foldl
        (\( id1, id2 ) accum ->
            Entity.respond id1 id2 accum
        )
        memory
        contacts


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


{-| Follow a given target if it goes beyond a rectangular area centered on screen.
-}
follow : Screen -> Float -> Vec2 -> Vec2 -> Vec2
follow { top, left, bottom, right } dt target camera =
    let
        newX =
            -- Check if target is moving toward left/right edges
            if target.x < (camera.x + left / viewScale + minDistanceFromEdge) then
                target.x

            else if target.x > (camera.x + right / viewScale - minDistanceFromEdge) then
                target.x

            else
                camera.x

        newY =
            -- Check if target is moving toward top/bottom edges
            if target.y > (camera.y + top / viewScale - minDistanceFromEdge) then
                target.y

            else if target.y < (camera.y + bottom / viewScale + minDistanceFromEdge) then
                target.y

            else
                camera.y

        movement =
            Vector2.sub (vec2 newX newY) camera
                |> Vector2.scale (dt * cameraSpeed)
    in
    Vector2.add camera movement



-- ENTRY POINT


main =
    Playground.game view update initialModel
