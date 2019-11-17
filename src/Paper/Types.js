exports.size_ = function(width, height) {
    return new Size(width, height);
}

exports.point_ = function(x, y) {
    return new Point (x, y);
}

exports.rectangle_ = function(point, size) {
    return new Rectangle(point, size);
}
