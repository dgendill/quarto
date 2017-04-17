
export function arrow(name, x ,y, tailLength) {
  let length = tailLength == null ? 50 : tailLength;
  var p = project.getItem({name : name});
  if (p) { p.remove(); }
  p = new Path([
    new Point(x, y), new Point(x, y+10), new Point(x+length, y+10), new Point(x+length, y+20),
    new Point(x, y + 20), new Point(x, y + 30), new Point(x - 20, y + 15), new Point(x, y)
  ]);
  p.pivot = new Point(x,y);
  p.name = name;
  return p;
}


export function labelArrow(config) {
  var name, x, y, content, width;
  ({name, x, y, content, width} = config);

  var t = tooltip({
    name : "labelArrowTooltip" + name,
    padding : 1,
    x : x,
    y : y,
    width : width,
    height : 30,
    content : content,
    justification : 'center'
  });


  var a = arrow("labelArrowArrow" + name, x - 2,y + 22, width + 3);
  a.fillColor = '#fff';
  a.strokeColor = '#000'

  return new Group([t, a]);
}

export function text(props) {
  var name, x, y, fillColor, fontFamily;
  ({name, x, y, fillColor, fontFamily} = props);

  var justification = props.justification || 'left';
  var textContent = props.content;

  if (name) { text = project.getItem({name : name}); }
  if (text) { text.remove(); }

  var text = new PointText(new Point(x + 5, y + 20));
  text.style = {
    fillColor : fillColor,
    fontSize : 18
  }
  text.justification = justification;
  text.content = textContent;
  text.visible = true;
  text.fontFamily = fontFamily;

  var r = text.rasterize(72);
  r.position = new Point(x + r.width / 2, y + r.height / 2);
  r.name = name;
  text.remove();
  return r;
}

export function move(name, x, y) {
  var item = project.getItem({ name : name});
  item.position = new Point(x, y);
}

export function removeGraphicItem(item) {
  item.remove();
}

export function removeGraphicItemByName(name) {
  var item = project.getItem({ name : name});
  item.remove();
}

export function tooltip(props) {
  var name, x, y, width, height;
  ({name, x, y, width, height} = props);

  var justification = props.justification || 'left';
  var textContent = props.content;
  var padding = props.padding || 10;
  var rect;

  if (name) {
    rect = project.getItem({name : name + "rect"})
  }

  if (!rect) {
    rect = new Shape.Rectangle(new Point(x, y), new Size(width, height));
    rect.applyMatrix = true;
    rect.style = {
      fillColor : 'rgba(45, 0, 0,.8)',
      strokeColor : "#000"
    };
    if (name) { rect.name = name + "rect"; }
  } else {
    rect.position = new Point(x + width / 2, y + height / 2);
    rect.size = new Size(width, height);
  }

  var t = text({
    name : name,
    x : x,
    y : y,
    fillColor : "#fff",
    content : textContent,
    justification : justification,
    fontFamily : '"Proxima Nova Regular","Segoe UI",Roboto,"Droid Sans","Helvetica Neue",Arial,sans-serif'
  });

  if (justification == 'left') {
    t.position = new Point(
      rect.bounds.topLeft.x + (t.width / 2) + padding,
      rect.bounds.topLeft.y + (t.height / 2) + padding
    )
  } else if (justification == 'center') {
    t.position = new Point(
      rect.bounds.center.x,
      rect.bounds.topLeft.y + (t.height / 2) + padding
    )
  }

  return new Group([rect, t]);

}
