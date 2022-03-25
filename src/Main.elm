module Main exposing (..)

import AltMath.Vector2 as Vector2 exposing (Vec2, vec2)
import Diagnostic
import Dict exposing (Dict)
import Entity exposing (Entity, EntityType(..), Wall)
import Levels exposing (level1)
import Physics exposing (Contacts)
import Playground exposing (..)
import Vector2.Extra as Vector2


type alias Memory =
    { entities : Dict Int Entity
    , gems : Int
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
    , debug = True
    , lastLogTime = 0
    , lastVelocity = Vector2.zero
    , lastContact = Vector2.zero
    }


viewScale =
    2


logTimeInterval =
    100


view : Computer -> Memory -> List Shape
view computer memory =
    [ renderBackground computer.screen
    , renderScene memory.debug level1.walls memory.entities
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



-- ENTRY POINT


main =
    Playground.game view update initialModel
