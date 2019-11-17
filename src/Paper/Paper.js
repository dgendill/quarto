exports.setup = function(canvasId) {
    return function() {
        paper.install(window);    
        paper.setup(canvasId);

        view.onFrame = function(event) {
            Animation.frameHandlers.forEach(function(fh) {
              fh(event);
            })
            view.draw();
        }

    }
}

