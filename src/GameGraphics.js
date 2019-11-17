'use strict';

const _id = function() {};
const _cancelSuccess = function (cancelError, cancelerError, cancelerSuccess) {
  cancelerSuccess();
};

exports.init_ = function (onError, onSuccess) {
  console.log(onError, onSuccess);
  Game.init(onSuccess);
  return _cancelSuccess;
};


exports.newGame_ = function(error, success) {
    Game.newGame();
    success();
}

exports.drawBoard_ = function(onError, onSuccess) {
  Game.Board.drawBoard();
  onSuccess();
  return _cancelSuccess;
}

exports.listenToBoard_ = function(fn, affHandler) {
  return function(onError, onSuccess) {
    Game.Board.drawBoard(function(shape, event) {
      var cb = fn(shape, event);
      affHandler(cb)();
    });
    onSuccess();

    return _cancelSuccess;

  }
}

exports.showMessage_ = function(message) {
  return function(error, success) {
    Game.AvailablePieces.showMessage(message);
    success();
  }
}

exports.hideMessage_ = function(error, success) {
  Game.AvailablePieces.hideMessage();
  success();
}

exports.createMainMenu_ = function(callback, affHandler) {
  return function(onError, onSuccess) {
    Game.createMainMenu(function(eventType) {
      var r = callback(eventType);
      affHandler(r)();
    });
    onSuccess();
    return _cancelSuccess;
  }
}

exports.itemName_ = function(item) {
  return Game.itemName.apply(this, [item]);
}

exports.loadAssets_ = function(srcs, ids) {
  return function(onError, onSuccess) {

    Game.Assets.load(srcs, ids, function(images) {
      console.log('Game.Assets.Load');
      onSuccess(images);
    });
    return _cancelSuccess;
  }
}

exports.listenToAvailablePieces_ = function(fn, affHandler) {
  return function (error, success) {
    Game.AvailablePieces.drawAvailablePieces(function(item, event) {

      var cb = fn(item, event);
      affHandler(cb)();

    });
    success();
  }
}

exports.drawAvailablePieces_ = function (onError, onSuccess) {
  Game.AvailablePieces.drawAvailablePieces();
  onSuccess();
  return _cancelSuccess;
}


exports.drawAvailablePiecesLayout_ = function(onError, onSuccess) {
  Game.AvailablePieces.drawLayout();
  onSuccess();
  return _cancelSuccess;
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
  return function(onError, onSuccess) {
    item.visible = true;
    onSuccess();
    return _cancelSuccess;
  }
}

exports.hidePaperItem = function(item) {
  return function(onError, onSuccess) {
    item.visible = false;
    onSuccess();
    return _cancelSuccess;
  }
}

exports.movePaperItem_ = function(x, y, item) {
  return function(onError, onSuccess) {
    item.position = new Point(x, y);
    onSuccess();
    return _cancelSuccess;
  }
}

exports.movePaperItemPivot_ = function(pivot, x, y, item) {
  return function(error, success) {
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


exports.addMiniPiecesTo_ = function(project_) {
  return function(error, success) {
    Game.AvailablePieces.addMiniPiecesTo(project_);
    success();
  }
}
