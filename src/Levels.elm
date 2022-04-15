
module Levels exposing (..)

import AltMath.Vector2 exposing (Vec2, vec2)
import Entity exposing (EntityType(..), PlayerData)

level1 = {
     spawns = [ { position = vec2 544 64, side = 1, type_ = Gem}, { position = vec2 224 336, side = 1, type_ = Gem}, { position = vec2 416 336, side = 1, type_ = Gem}, { position = vec2 96 64, side = 1, type_ = Gem}, { position = vec2 320 336, side = 1, type_ = Player (PlayerData 0)} ]
     , walls = [ { p1 = vec2 0 0, p2 = vec2 640 0, normal = vec2 0 1}, { p1 = vec2 64 48, p2 = vec2 192 48, normal = vec2 0 1}, { p1 = vec2 432 48, p2 = vec2 576 48, normal = vec2 0 1}, { p1 = vec2 144 96, p2 = vec2 272 96, normal = vec2 0 1}, { p1 = vec2 368 96, p2 = vec2 496 96, normal = vec2 0 1}, { p1 = vec2 640 0, p2 = vec2 640 480, normal = vec2 -1 0}, { p1 = vec2 176 320, p2 = vec2 464 320, normal = vec2 0 1}, { p1 = vec2 0 480, p2 = vec2 0 0, normal = vec2 1 0} ]
     , platforms = [ { position = vec2 544 136, width = 128, height = 16, maxOffset = 100, period = 3}, { position = vec2 96 280, width = 128, height = 16, maxOffset = -100, period = 4} ]
    }
