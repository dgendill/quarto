exports.pointtext_ = function(point, config) {
    var text = new PointText(point);
    Object.assign(text, config);
    return text;
}