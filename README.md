# Platformer physics in Elm

This is a proof-of-concept application to explore several concepts described by Maddy Thorson's post [_Celeste and TowerFall Physics_][c].

All the physics in the system are handled by two types: `Wall` and `Entity`. Walls are the collidable level geometry. Entities are physics objects, such as players, arrows, monsters, treasure chests, etc. 

Anything that has to move and interact with the level geometry is an entity. The system has a few simple constraints:

* All colliders are axis-aligned bounding boxes (AABBs)
* All collider positions, widths, and heights are integer numbers
* Walls do not interact with other walls

For this demo the game graphics and logic have been kept to a minimun to focus on entity movement and collision detection/response. 

If you want richer graphics while using the [WebGL Playground][w] package take a look at [Sunny Land][sl].

## Run locally

Clone the repo and run Elm's `reactor` command:

```
elm reactor
```

Then point your browser to <http://localhost:8000/src/Main.elm>

## Other references 

The sample Beef code modeled after the Celeste physics can be found in the [Strawberry game engine][4]. In particular look at the [`Actor`][a] and [`Player`][p] classes. 

Other interesting blog posts have been studied to complete the implementation:

* [_Integration basics_][1] and [_Fix your timestep!_][2] by Glenn Fiedler
* [_Collision detection: rectangle vs rectangle_][3] by Jeff Thompson

[c]: https://maddythorson.medium.com/celeste-and-towerfall-physics-d24bd2ae0fc5
[1]: https://gafferongames.com/post/integration_basics/
[2]: https://gafferongames.com/post/fix_your_timestep/
[3]: http://www.jeffreythompson.org/collision-detection/rect-rect.php
[4]: https://github.com/MaddyThorson/StrawberryBF
[p]: https://github.com/MaddyThorson/StrawberryBF/blob/bae5798f070e10f11bb633e5bc206e33859df928/SampleGame/src/Entities/Player.bf
[a]: https://github.com/MaddyThorson/StrawberryBF/blob/bae5798f070e10f11bb633e5bc206e33859df928/SampleGame/src/Physics/Actor.bf
[w]: https://package.elm-lang.org/packages/justgook/webgl-playground/latest/
[sl]: https://github.com/passiomatic/sunny-land