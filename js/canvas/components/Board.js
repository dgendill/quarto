import * as C from '../Config'
import * as Game from '../Game'

export const parts = {
  spaces : null,
  base : null,
  board : null,
  pieceOffset : null
}


export const rpoints = {
  pieceOffset : { x : -10, y : 80 }
}

let boardDrawn = false;

let listeners = [];

function registerEventListeners(fn) {
  listeners.push(fn);
}

function notifyEventListeners(item, event) {
    listeners.forEach(function(l) {
      l(item, event);
    })
}

// (Event -> Eff _ Unit) -> Eff (board :: BOARD) Unit
export function drawBoard(eventListener) {
  if (eventListener) {
    registerEventListeners(eventListener);
  }

  if (boardDrawn) return;
  boardDrawn = true;

  parts.base = drawBoardBase();
  parts.spaces = drawBoardSpaces(function(item, event) {
    notifyEventListeners(item, event);
  });

  let board = new Group([parts.base].concat(parts.spaces));
  board.name = "board"

  board.transform(new Matrix(1.0, 0.0, 0.3, 1.0, 0.0, 0.0))
  board.transformContent = false;
  board.position = new Point(500, 250)
  board.rotation = 146.98

  parts.board = board;

  Game.onFrame(animateSpaces);
  disable();

}

export function resetSpaces() {
    parts.spaces.forEach(function(space) {
      space.disabled = false;
      space.played = false;
    })
}

export function disable() {
  parts.spaces.forEach((space) => {
    space.disabled = true;
    space.mouseover = false;
    space.mousedown = false;
  })
}

export function enable() {
  parts.spaces.forEach((space) => {
    space.disabled = false;
  })
}

export function animateSpaces(event) {
  let spaces = parts.spaces;

  spaces.forEach(function(space) {
      if (space.mouseover) {
        if (space.mousedown) {
          var size = (C.gameSpaceRadius + C.animationRadius) * 2;
        } else {
          var time = event.time * 1000;
          var animationLength = 2000;
          var extrasize = Math.abs(C.animationRadius * Math.sin(time * 2 * Math.PI / animationLength))
          var size = (C.gameSpaceRadius * 2) + extrasize;
        }

        space.size = new Size(size);
      } else {
        space.size = new Size(C.gameSpaceRadius * 2);
      }
  });

}

export function drawBoardBase() {
  var size = new Size(C.boardwidth, C.boardheight);
  var point = new Point (0, 0);
  var board = new Shape.Rectangle(point, size);
  board.style = {
    fillColor : "#502D16"
  }
  return board;
}

export function drawBoardSpaces(eventListener) {

  function drawSpace(x, y, radius) {
    var iwidthhalf = radius;
    var iheighthalf = radius;
    var divisionsx = 4
    var divisionsy = 4
    var spacingx = (C.boardwidth - 2.0 * divisionsx * iwidthhalf) / (divisionsx + 1.0)
    var spacingy = (C.boardheight - 2.0 * divisionsy * iheighthalf) / (divisionsy + 1.0)
    var xn = x;// - 1.0
    var yn = y;// - 1.0
    var translateX = spacingx + iwidthhalf + ((spacingx + 2.0 * iwidthhalf ) * xn)
    var translateY = spacingy + iheighthalf + ((spacingy + 2.0 * iheighthalf) * yn)


    var shape = Shape.Circle(new Point(translateX, translateY), radius)
    shape.style = {
      fillColor: "#2B1100"
    }
    return shape;

  }

  var spaces = [];

  for(var y = 0; y < 4; y++) {
    for(var x = 0; x < 4; x++) {
      var space = drawSpace(x, y, C.gameSpaceRadius);
      space.name = (x+1) + "," + (y+1)

      space.on('mouseenter', function(event) {
        if (!this.disabled && !this.played) {
          this.mouseover = true;
          eventListener(this, event);
        }
      })

      space.on('mouseleave', function(event) {
        if (!this.disabled && !this.played) {
          this.mouseover = false;
          this.mousedown = false;
          eventListener(this, event);
        }
      })

      space.on('mousedown', function(event) {
        if (!this.disabled && !this.played) {
          this.mousedown = true;
          eventListener(this, event);
        }
      })

      space.on('mouseup', function(event) {
        if (!this.disabled && !this.played) {
          this.mousedown = false;
          eventListener(this, event);

          // Game.playPiece(this.name)
        }
      });

      spaces.push(space);

    }
  }
  parts.spaces = spaces;

  return spaces;
}
