import * as AvailablePieces from './components/AvailablePieces';
import * as paper from 'paper'
import * as Assets from './Assets'
import * as Board from './components/Board'
import * as C from './Config'
import * as Util from './Util'
import * as Shapes from './Shapes'
import * as Demo from './Demo'
import * as Actions from './Actions'
import * as Animation from './Animation'

export {Board, Assets, AvailablePieces, Shapes, Demo, Actions, Animation};

paper.install(window);

let identity = function(x) { return x; };

var state = {
  playedPieces : ["id", "id"],
  availablePieces : ["id", "id"]
}

let canvas, winningScreenCanvas;

export function onFrame(fn) {
  Animation.frameHandlers.push(fn);
}

export function itemName(item) {
  return item.name;
}

export function newGame() {
  Board.resetSpaces();
  AvailablePieces.resetPieces();
}

export function setup() {

  let onBoard = new Layer()
  onBoard.name = "onBoard";

  let baseLayer = new Layer();
  baseLayer.name = "baseLayer";

  var playPieceText = new PointText(new Point(0,0));
  playPieceText.name = "playPieceText";
  playPieceText.content = "Play this piece.";
  playPieceText.pivot = playPieceText.bounds.center;
  playPieceText.fontSize = 18
  playPieceText.style = {
    fillColor : "black"
  }
  playPieceText.visible = false;

  var playedPieces = new Layer();
  playedPieces.name = "playedPieces";
  var layers = [
    new Layer(),
    new Layer(),
    new Layer(),
    new Layer(),
    new Layer(),
    new Layer(),
    new Layer(),
  ]
  layers.forEach((l, i)=> {
    l.name = "layer" + i;
    playedPieces.addChild(l);
    l.bringToFront();
  });

  let onDeck = new Layer();

  onDeck.name = "onDeck";
  playedPieces.addChild(onDeck);
  onDeck.bringToFront();

  playedPieces.bringToFront();

  baseLayer.activate();

}

export function init(cb) {
  console.log('Game.js init');
  
  window.onload = function() {

    canvas = document.getElementById('board-base');
    canvas.width = 800; //window.innerWidth;
    canvas.height = 630; // window.innerHeight;
    
    paper.setup('board-base');

    view.onFrame = function(event) {
      Animation.frameHandlers.forEach(function(fh) {
        fh(event);
      })
      view.draw();
    }

    setup();
    cb();
  }
}


export function loadAssets(success, error) {
  Assets.load(success);
}


export function affSuccess() {
  return function(success, error) {
    return success();
  }
}

// Todo Enabled Audio For the Blind
// https://boardgamegeek.com/thread/24226/game-solvable

export function showMainMenu() {
  let menu = document.getElementById('menu');
  showElement(menu);
}

export function hideMainMenu() {
  let menu = document.getElementById('menu');
  hideElement(menu);
}

export function showGame() {
  let canvas = document.getElementById('board-base');
  project.layers.forEach(function(l) {
    l.visible = true;
  })
}

export function hideGame() {
  let canvas = document.getElementById('board-base');
  project.layers.forEach(function(l) {
    l.visible = false;
  })
}

export function createMainMenu(callback) {
    let menu = document.getElementById('menu');
    showElement(menu);

    let elements = [
      "new-game", "how-to-play", "new-game-remote", "new-game-single-player"
    ]

    elements.forEach( elementName => {
      let element = menu.querySelector("." + elementName);

      let listener = function() {
        callback(elementName);
      }

      let removeEventListener = function() {
        element.removeEventListener('click', listener);
      }

      element.addEventListener('click', listener);

    });

}

function showElement(element) {
  element.style.display = 'block';
  element.setAttribute('aria-hidden', false);
}

function hideElement(element) {
  element.style.display = 'none';
  element.setAttribute('aria-hidden', true);
}
