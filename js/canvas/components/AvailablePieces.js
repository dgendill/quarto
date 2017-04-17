import * as Game from '../Game'
import { assets } from '../Assets'
import * as Util from '../Util'

export const parts = {
  text : null,
  textbg : null,
  anytext : null,
  area : null,
  pieces : null,
  minipieces : null,
  overlay : null
};

const rpoints = Util.deepFreeze({
  pieces : { x : 65, y : 140 },
  piecesArea : { x : 20, y : 60},
  piecesText : { x : 20, y : 40}
});

export function showMessage(message) {
  parts.anytext.visible = true;
  parts.anytext.content = message;
  parts.textbg.visible = true;
}
export function hideMessage() {
  parts.anytext.visible = false;
  parts.textbg.visible = false;
}
export const showGivePieceText = showMessage.bind(this, "Choose a piece to give your opponent.");
export const hideGivePieceText = hideMessage;
export const showPlayPieceText = showMessage.bind(this, "Play your piece")
export const hidePlayPieceText = hideMessage;

export function drawLayout() {
  const atb = new Shape.Rectangle(Util.toPoint(rpoints.piecesArea), new Size(240,500));
  atb.style = {
    strokeColor:'#333',
    strokeWidth: 5,
    fillColor: "#efefef"
  }

  const textbg = new Shape.Rectangle(
    Util.toPoint({ x : rpoints.piecesText.x - 3, y : rpoints.piecesText.y - 18}),
    new Size(360, 25)
  );
  textbg.opacity = 1;
  textbg.visible = true;
  textbg.style = {
    fillColor : 'white',
    strokeWidth : 1,
    strokeColor : 'black'
  }

  const anytext = new PointText(Util.toPoint(rpoints.piecesText));
  anytext.style = {
    fillColor : "black",
  }
  anytext.content = "Play your piece.";
  anytext.pivot = anytext.bounds.topLeft;
  anytext.fontSize = 18
  anytext.visible = false;


  const piecesArea = new Group([atb]);
  piecesArea.name = "piecesArea";

  const overlay = new Shape.Rectangle(
    atb.bounds.topLeft,
    atb.size
  );
  overlay.opacity = 0;
  overlay.visible = false;
  overlay.style = {
    strokeColor : 'red',
    fillColor : 'transparent',
    strokeWidth : 8
  }

  parts.textbg = textbg;
  parts.anytext = anytext;
  parts.area = piecesArea;
  parts.overlay = overlay;


}

export function disable() {
  parts.area.opacity = .5;
  project.getItem({name : 'availablePieces'}).opacity = .5;
  parts.overlay.visible = true;
}

export function enable() {
  project.getItem({name : 'availablePieces'}).opacity = 1;
  parts.area.opacity = 1;
  parts.overlay.visible = false;
}

export function isShort(pieceId) {
  return pieceId.indexOf('Short') != -1
}

export let piecesDrawn = false;

let listener = function() {};

function notifyEventListeners(item, event) {
  listener(item, event);
}

function setEventListener(fn) {
  listener = fn
}

export function addMiniPiecesTo (project_) {
  if (project_.minisSetup) return;
  project_.minisSetup = true;
  var minipieces = [];

  parts.pieces.forEach(function(piece) {
    var mini = piece.clone();
    mini.visible = false;
    mini.name = piece.name + "-mini";
    mini.scale(0.6);
    minipieces.push(mini);
  })

  var layer = new Layer({
    children : minipieces
  });

  layer.name = "minipieces";

  project_.addLayer(layer);

}

export function drawAvailablePieces(eventListener) {
  if (eventListener) { setEventListener(eventListener); }
  if (piecesDrawn) return;
  piecesDrawn = true;

  var rowAdders = assets.ids.map(function(id, i) {
    if (i == 0) return 0;
    if (i % 4 == 0 && isShort(id)) {
      return 120;
    }  else if ((i % 4) == 0) {
      return 120;
    } else {
      return 0;
    }
  })

  var pieces = [];


  var yPos = 0;
  assets.ids.forEach(function(id, i) {
    var r = new Raster(assets[id]);

    if (isShort(id)) {
      r.pivot = r.bounds.topLeft;
      r.position = new Point(0, 0);
      var rect = new Path.Rectangle({
        point : new Point(0,0),
        size : new Size(64, 128-66)
      });
      rect.pivot = rect.bounds.topLeft;
      rect.position = new Point(0,54);
      var p = new Group([rect, r]);
      p.style = {
        strokeWidth: 2,
        strokeColor: 'red'
      }
      p.pivot = rect.bounds.center;
      p.clipped = true;

    } else {
      r.pivot = r.bounds.topLeft;
      r.position = new Point(0, 0);
      var rect = new Path.Rectangle({
        point : new Point(0,0),
        size : new Size(64, 128 - 20)
      });
      rect.pivot = rect.bounds.topLeft;
      rect.position = new Point(0,10);
      var p = new Group([rect, r]);
      p.style = {
        strokeWidth: 2,
        strokeColor: 'red'
      }
      p.pivot = rect.bounds.center;
      p.clipped = true;
    }

    p.name = id;
    p.gameEventListener = function() {
      notifyEventListeners.apply(this, arguments)
    }
    pieces.push(p);


  });

  setDefaultPositions(pieces);
  enableAllPieceEvents(pieces, eventListener);
  parts.pieces = pieces;

  const piecesGroup = new Group(pieces);
  piecesGroup.name = "availablePieces";
  piecesGroup.moveBelow(parts.overlay);

  return pieces;
}

export function enableAllPieceEvents(pieces) {
  pieces.forEach(function(p) {
    if (p.eventsAdded) return;

    var ch = function(event) {
      this.gameEventListener(this, event);
    };

    p.on('click', ch);

    p.gameRemoveAllListeners = function() {
      p.eventsAdded = false;
      p.off('click', ch);
      p.off('mouseenter', this.cbMouseEnter);
      p.off('mouseleave', this.cbMouseLeave);

    }

    p.downScale = function() {
      if (!this.scaled) return;
      this.scale(1 / 1.1, 1 / 1.1);
      this.scaled = false;
    }

    p.upScale = function() {
      if (this.scaled) return;
      this.scale(1.1, 1.1);
      this.scaled = true;
    }

    p.cbMouseLeave = function(event) {
      this.downScale();
      this.gameEventListener(this, event);
    }

    p.cbMouseEnter = function(event) {
      this.upScale();
      this.gameEventListener(this, event);
    }

    p.on('mouseenter', p.cbMouseEnter);
    p.on('mouseleave', p.cbMouseLeave);

    p.eventsAdded = true;

  })
}

export function getDefaultPositions(pieces) {

    var positions = [];

    var pieceIndex = pieces.reduce(function(acc, piece) {
      acc[piece.name] = piece;
      return acc;
    }, {})

    var rowAdders = assets.ids.map(function(id, i) {
      if (i == 0) return 0;
      if (i % 4 == 0 && isShort(id)) {
        return 120;
      }  else if ((i % 4) == 0) {
        return 120;
      } else {
        return 0;
      }
    })

    var yPos = 0;

    assets.ids.forEach(function(id, i) {
      yPos += rowAdders[i];
      if (pieceIndex[id]) {
        positions.push(
          new Point(((i % 4) * 50) + rpoints.pieces.x, yPos + rpoints.pieces.y)
        )
      }
    });

    return positions;
}

export function setDefaultPositions(pieces) {
  var positions = getDefaultPositions(pieces);
  positions.forEach(function(position, i) {
    var piece = pieces[i];
    piece.pivot = piece.bounds.center;
    piece.position = position;

  });

}


export function resetPieces() {

  var pieces = parts.pieces;
  var availablePieces = project.getItem({name : "availablePieces"});

  pieces.forEach(function(piece) {
    piece.remove();
    availablePieces.addChild(piece);
  })

  setDefaultPositions(pieces);
  enableAllPieceEvents(pieces);
  enable();


}
