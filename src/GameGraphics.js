'use strict';

exports.init = function(success, error) {
  Game.init(success);
}

exports.newGame = function(success, error) {
    Game.newGame();
    success({});
}

exports.drawBoard_ = function() {
  return function(success, error) {
    Game.Board.drawBoard();
    success({});
  }
}

exports.listenToBoard_ = function(fn) {
  return function(success, error) {
    Game.Board.drawBoard(function(shape, event) {
      var cb = fn(shape, event);
      cb(function() {}, function() {});
    });
    success({});
  }
}

exports.showMessage = function(message) {
  return function(success, error) {
    Game.AvailablePieces.showMessage(message);
    success({});
  }
}

exports.hideMessage = function(success, error) {
  Game.AvailablePieces.hideMessage();
  success({});
}

exports.createMainMenu_ = function(callback) {
  return function(success, error) {
    Game.createMainMenu(function(eventType) {
      callback(eventType)(function() {});
    });
    success({});
  }
}

exports.itemName_ = function() {
  return Game.itemName.apply(this, arguments);
}

exports.loadAssets_ = function(srcs, ids, success, error) {
  return function() {
    Game.Assets.load(srcs, ids, function(images) {
      success(images)();
    }, error);
  }
}

exports.listenToAvailablePieces_ = function(fn) {
  return function (success, error) {
    Game.AvailablePieces.drawAvailablePieces(function(item, event) {
      var cb = fn(item, event);
      cb(function() {});
    });
    success({});
  }
}

exports.drawAvailablePieces_ = function() {
  return function (success, error) {
    Game.AvailablePieces.drawAvailablePieces();
    success({});
  }
}

exports.drawAvailablePiecesLayout = function (success, error) {
  Game.AvailablePieces.drawLayout();
  success({});
}

exports.getPaperItem_ = function(project_, name, just, nothing) {
  var item = project_.getItem({ name : name });
  if (item) {
    return just(item);
  } else {
    return nothing;
  }
}

exports.getPaperProject = function(canvas) {
  var activeProject = project;
  var newProject = new Project(canvas);
  activeProject.activate();
  return newProject;
}

exports.showPaperItem = function(item) {
  return function(success, error) {
    item.visible = true;
    success();
  }
}

exports.hidePaperItem = function(item) {
  return function(success, error) {
    item.visible = false;
    success();
  }
}

exports.movePaperItem_ = function(x, y, item) {
  return function(success, error) {
    item.position = new Point(x, y);
    success();
  }
}

exports.movePaperItemPivot_ = function(pivot, x, y, item) {
  return function(success, error) {
    var xx, yy;
    var width = item.bounds.topRight.x - item.bounds.topLeft.x;
    var height = item.bounds.bottomLeft.y - item.bounds.topLeft.y;

    var halfw = width / 2;
    var halfh = height / 2;

    if (pivot == "topLeft") {
      xx = x - halfw;
      yy = y + halfh;
    } else if (pivot == "topRight") {
      xx = x + halfw;
      yy = y + halfh;
    } else if (pivot == "bottomLeft") {
      xx = x - halfw;
      yy = y - halfh;
    } else if (pivot == "bottomRight") {
      xx = x + halfw;
      yy = y - halfh;
    } else if (pivot == "leftCenter") {
      xx = x - halfw;
      yy = y;
    } else if (pivot == "topCenter") {
      xx = x;
      yy = y + halfh;
    } else if (pivot == "rightCenter") {
      xx = x + halfw;
      yy = y;
    } else if (pivot == "bottomCenter") {
      xx = x;
      yy = y - halfh;
    } else {
      xx = x;
      yy = y;
    }

    item.position = new Point(xx, yy);
    success();
  }
}


exports.addMiniPiecesTo = function(project_) {
  return function(success, error) {
    Game.AvailablePieces.addMiniPiecesTo(project_);
    success();
  }
}
