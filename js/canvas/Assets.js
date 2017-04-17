var assets = {};

export var assets = assets;

export function load(srcs, ids, success, error) {
  var images = {};
  var assetCount = srcs.length;
  var loadCount = 0;
  srcs.forEach(function(src, i) {
    var img = new Image(src);
    images[ids[i]] = img;
    img.onload = function() {
      loadCount++;
      if (loadCount == assetCount) {
        assets = images;
        assets.ids = ids;
        success(images);
      }
    }

    img.onerror = function(e) {
      error(e)
    }

    img.src = src;
  })
}
