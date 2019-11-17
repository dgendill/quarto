export const assets = {};

export function load(srcs, ids, success, error) {
  var assetCount = srcs.length;
  var loadCount = 0;
  srcs.forEach(function(src, i) {
    var img = new Image(src);
    assets[ids[i]] = img;
    img.onload = function() {
      loadCount++;
      if (loadCount == assetCount) {
        assets.ids = ids;
        success(assets);
      }
    }

    img.onerror = function(e) {
      error(e)
    }

    img.src = src;
  })
}
