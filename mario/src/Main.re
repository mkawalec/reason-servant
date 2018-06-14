open Webapi.Dom;

[%%bs.raw {|
  require('../../../src/style.less');
|}];

module Canvas = {
  [@bs.deriving abstract]
  type context = pri {
    mutable fillStyle: string,
    mutable strokeStyle: string
  };

  [@bs.send] external getContext2d: (Dom.element, [@bs.as "2d"] _) => context = "getContext";
  [@bs.send] external fillRectFloat: (context, float, float, float, float) => unit = "fillRect";
  [@bs.send] external fillRectInt: (context, int, int, int, int) => unit = "fillRect";

  [@bs.send] external clearRect: (context, int, int, int, int) => unit = "";
  [@bs.send] external strokeRect: (context, int, int, int, int) => unit = "";
};

module ArrayUtil = {
  let iterRange = (start: int, end_: int, arr: array('a), fn: ((int, 'a) => unit)): unit => {
    Belt.Array.slice(arr, start, end_ - start) |. Belt.Array.forEachWithIndex(fn);
  };
};

type world = {
  env: array(array(envElement)),
  hero: fourAxisElement(float),
  viewport: fourAxisElement(float),
  scene: scene,
  lastAnimationTime: option(int)
}
and envElement = {
  coordinate: int,
  elementType: elementType
}
and elementType = Floor | Stone | Active
and fourAxisElement('a) = {
  x: 'a,
  y: 'a,
  vx: 'a,
  vy: 'a
}
and scene = {
  ctx: Canvas.context,
  node: Dom.element,
  w: int,
  h: int,
  leftClicked: bool,
  rightClicked: bool
};

let canHeroJump = (world: world) => {
  world.hero.vy === 0.0;
}


let generateRandomEnvironment = (length: int): array(array(envElement)) => {
  let baseElement = {coordinate: 0, elementType: Floor};
  Array.make_matrix(length, 1, baseElement);
};

let tileSize = 40.0;
let heroSize = tileSize /. 2.0;

let paint = (world: world): unit => {
  open Canvas;

  let envPaintStart = int_of_float(world.viewport.x /. tileSize);
  let xPaintOffset = world.viewport.x /. tileSize -. float_of_int(envPaintStart);
  let envPaintEnd = int_of_float(float_of_int(world.scene.w) /. tileSize) + envPaintStart;

  /* Draw the sky */
  fillStyleSet(world.scene.ctx, "#008AC5");
  Canvas.fillRectInt(world.scene.ctx, 0, 0, world.scene.w, world.scene.h);

  /* Draw the ground */
  fillStyleSet(world.scene.ctx, "#97FF29");
  ArrayUtil.iterRange(envPaintStart, envPaintEnd, world.env, (idx, tiles) => {
    let paintIdx = idx - envPaintStart;
    Array.iter(tile => {
      world.scene.ctx |. Canvas.fillRectFloat(
        (float_of_int(paintIdx) -. xPaintOffset) *. tileSize,
        float_of_int(world.scene.h) -. (
          float_of_int(tile.coordinate) *. tileSize -. world.viewport.y
        ),
        tileSize,
        tileSize);
    }, tiles);
  });

  /* Draw our hero */
  fillStyleSet(world.scene.ctx, "#00deff");
  world.scene.ctx |. Canvas.fillRectFloat(
    world.hero.x -. xPaintOffset *. tileSize,
    float_of_int(world.scene.h) -. (world.hero.y -. world.viewport.y),
    heroSize,
    tileSize);
};

module Date = {
  type t;
  [@bs.new] external create: unit => t = "Date";
  [@bs.send] external getTime: t => int = "";
};

let findCollisions = (world: world, newX: float, newY: float):
  option((int, envElement)) => {
    let beginningTile = int_of_float(newX /. tileSize);
    let endTiles = if (newX /. tileSize -. float_of_int(beginningTile) > 0.5) {
      [beginningTile + 1];
    } else {
      [];
    }
    let columnsToCheck = List.append([beginningTile], endTiles);

    List.fold_left((collided, column) => {
      if (Js_option.isSome(collided)) {
        collided;
      } else {
        Array.fold_left((collided_, tile) => {
          if (int_of_float(newY) <= (tile.coordinate + 1) * int_of_float(tileSize)) {
            Some((column, tile));
          } else {
            collided_;
          }
        }, None, Array.get(world.env, column));
      }
    }, None, columnsToCheck);
};

let dV = 0.005;
let vDamping = dV /. 10.0;
let maxV = 0.2;

let step = (world: world): world => {
  open Js_math;

  let currentTime = Date.create() |> Date.getTime;

  let dT = switch(world.lastAnimationTime) {
    | None => 1000.0 /. 60.0;
    | Some(time) => float_of_int(currentTime - time);
  };

  /* TODO: make nicer */
  let newVY = world.hero.vy -. 0.00981;
  let leftDV = if (world.scene.leftClicked) {
    dT *. dV;
  } else {
    0.0;
  };
  let rightDV = if (world.scene.rightClicked) {
    dT *. dV;
  } else {
    0.0;
  };
  let vX' = world.hero.vx +. rightDV -. leftDV;
  let vX'' = -1.0 *. sign_float(vX') *. vDamping *. dT +. /* V damping */
    if (abs_float(vX') > maxV) { sign_float(vX') *. maxV } else { vX' };
  let newVX = if (abs_float(vX'') <= vDamping *. dT) { 0.0 } else { vX'' };
  let newViewPortY = if (world.hero.y > 200.0) { world.viewport.y +. newVY *. dT } else {world.viewport.y};
  let newViewPortX = if (world.hero.x > 200.0) { world.viewport.x +. newVX *. dT } else {world.viewport.x};
  let newY = world.hero.y +. newVY *. dT;

  let boundLeft = 0.;
  let boundRight = float_of_int(world.scene.w) -. heroSize;
  let newX = max(boundLeft, min(boundRight, world.hero.x +. newVX *. dT));

  /* detect collisions */
  switch (findCollisions(world, newX, newY)) {
    | None => {...world, hero: {
      x: newX,
      y: newY,
      vx: newVX,
      vy: newVY
    }};
    | Some((_, collidedTile)) => {
      {...world, hero: {
        x: newX,
        y: float_of_int(collidedTile.coordinate + 1) *. tileSize,
        vx: newVX,
        vy: 0.0
      },
      viewport: {
        x: newViewPortX,
        y: newViewPortY,
        vx: 0.0,
        vy: 0.0
    }};
    };
  };
};

let mainLoop = (world: world) => {
  let currentWorld = ref(world);

  HtmlElement.focus(Element.unsafeAsHtmlElement(world.scene.node));
  Element.addKeyDownEventListener((e) => {
    open KeyboardEvent;

    /* if key up jump, if right, keep adding speed until max */
    /* TODO: use some kind of smart typesafe collection */
    if (key(e) === "ArrowUp") {
      if (canHeroJump(currentWorld^)) {
        currentWorld := {...currentWorld^, hero: {...currentWorld^.hero,
          vy: 0.4
        }};
      }
    } else if (key(e) === "ArrowRight") {
      currentWorld := {...currentWorld^, scene: {...currentWorld^.scene,
        rightClicked: true,
        leftClicked: false
      }};
    } else if (key(e) === "ArrowLeft") {
      currentWorld := {...currentWorld^, scene: {...currentWorld^.scene,
        leftClicked: true,
        rightClicked: false
      }};
    };
  }, world.scene.node);

  Element.addKeyUpEventListener((e) => {
    open KeyboardEvent;

    if (key(e) === "ArrowRight") {
      currentWorld := {...currentWorld^, scene: {...currentWorld^.scene,
        rightClicked: false
      }};
    } else if (key(e) === "ArrowLeft") {
      currentWorld := {...currentWorld^, scene: {...currentWorld^.scene,
        leftClicked: false
      }};
    };
  }, world.scene.node);

  let rec requestFrame = () => {
    Webapi.requestAnimationFrame((_) => {
      let newWorld = step(currentWorld^);
      paint(newWorld);
      currentWorld := newWorld;

      requestFrame();
    });
  };

  requestFrame();
};

/*
   TODO:
   - listen to keyboard events
*/

let initialize = (): option(world) => {
  switch (Document.getElementById("scene", document)) {
    | None => None;
    | Some(scene) => {
      let canvasHeight = Element.clientHeight(scene);
      let canvasWidth = Element.clientWidth(scene);

      scene |> Element.setAttribute("height", string_of_int(canvasHeight) ++ "px");
      scene |> Element.setAttribute("width", string_of_int(canvasWidth) ++ "px");

      Some({
        env: generateRandomEnvironment(80),
        hero: {x: 80.0, y: 80.0, vx: 0.0, vy: 0.0},
        viewport: {x: 0.0, y: -80.0, vx: 0.0, vy: 0.0},
        scene: {
          ctx: scene |. Canvas.getContext2d,
          node: scene,
          w: canvasWidth,
          h: canvasHeight,
          leftClicked: false,
          rightClicked: false
        },
        lastAnimationTime: None
      });
    }
  };
};

switch (initialize()) {
  | None => Js.log("initialize failed");
  | Some(world) => mainLoop(world);
};
