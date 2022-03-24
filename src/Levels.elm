
module Levels exposing (..)

import AltMath.Vector2 exposing (Vec2, vec2)
import Entity exposing (EntityType(..))

level1 = {
     spawns = [ { position = vec2 120 40, dir = 1, type_ = Player}, { position = vec2 704 120, dir = 1, type_ = Gem}, { position = vec2 268 68, dir = 1, type_ = Gem}, { position = vec2 520 120, dir = 1, type_ = Gem} ]
     , walls = [ { position = vec2 120 4, width = 240, height = 8}, { position = vec2 360 44, width = 240, height = 8}, { position = vec2 600 92, width = 240, height = 8} ]
    }