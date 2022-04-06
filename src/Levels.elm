module Levels exposing (..)

import AltMath.Vector2 exposing (Vec2, vec2)
import Entity exposing (EntityType(..), PlayerData)


level1 =
    { spawns = [ { position = vec2 576 64, side = 1, type_ = Gem }, { position = vec2 208 112, side = 1, type_ = Gem }, { position = vec2 400 112, side = 1, type_ = Gem }, { position = vec2 304 48, side = 1, type_ = Player (PlayerData 0) }, { position = vec2 48 64, side = 1, type_ = Gem } ]
    , walls = [ { p1 = vec2 176 16, p2 = vec2 432 16, normal = vec2 0 1 }, { p1 = vec2 32 48, p2 = vec2 160 48, normal = vec2 0 1 }, { p1 = vec2 448 48, p2 = vec2 608 48, normal = vec2 0 1 }, { p1 = vec2 144 96, p2 = vec2 272 96, normal = vec2 0 1 }, { p1 = vec2 336 96, p2 = vec2 464 96, normal = vec2 0 1 } ]
    , platforms = [ { position = vec2 480 256, width = 128, height = 32, maxOffset = 100, period = 4 } ]
    }
