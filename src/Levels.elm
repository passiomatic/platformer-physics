
module Levels exposing (..)

import AltMath.Vector2 exposing (Vec2, vec2)
import Entity exposing (EntityType(..), PlayerData)

level1 = {
     spawns = [ { position = vec2 120 40, side = 1, type_ = Player (PlayerData 0)}, { position = vec2 704 120, side = 1, type_ = Gem}, { position = vec2 268 68, side = 1, type_ = Gem}, { position = vec2 520 120, side = 1, type_ = Gem} ]
     , walls = [ { p1 = vec2 4 -4, p2 = vec2 232 -4, normal = vec2 0 1}, { p1 = vec2 232 -4, p2 = vec2 232 36, normal = vec2 -1 0}, { p1 = vec2 232 36, p2 = vec2 388 36, normal = vec2 0 1}, { p1 = vec2 388 36, p2 = vec2 388 80, normal = vec2 -1 0} ]
    }
