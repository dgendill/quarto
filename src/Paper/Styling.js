exports.fillColor_ = function(shape, color) {
    shape.style.fillColor = color;
}

exports.style_ = function(item, style) {
    Object.assign(item.style, style);
    return item;
}