import {arrow, tooltip, labelArrow} from './Shapes'
import {animatePieceToPosition, animatePieceToHome} from './Animation'

// // var arrow = Game.Shapes.arrow;
// // var tooltip = Game.Shapes.tooltip;
// // var labelArrow = Game.Shapes.labelArrow;
//
// let text2 = `Each game piece falls into one of
// the following categories:
//
//   - Dark or Light
//   - Tall or Short
//   - Square or Circle
//   - Hollow or Solid`;
//
//
// export function slide1(callback) {
//   let slide = [
//     labelArrow("la1", 310, 97, 'Tall Hollow', 116),
//     labelArrow("la2", 310, 215, 'Short Hollow', 116),
//     labelArrow("la3", 310, 340, 'Tall Solid', 116),
//     labelArrow("la4", 310, 460, 'Short Solid', 116),
//
//     tooltip({
//       name : 'slide1',
//       x : 500,
//       y : 100,
//       width : 295,
//       height : 185,
//       content : text2
//      }),
//     nextButton(770, 260, goSlide2)
//
//   ]
//
//   function goSlide2() {
//     slide.forEach(i => { i.remove(); });
//     callback();
//   }
//
// }
//
// export function slide2() {
//   animatePieceToPosition("id-Dark-Tall-Square-Hollow", "3,4");
//   animatePieceToPosition("id-Dark-Tall-Circle-Hollow", "3,3");
//   animatePieceToPosition("id-Dark-Short-Square-Hollow", "3,2");
//   animatePieceToPosition("id-Dark-Tall-Square-Solid", "3,1");
//
//   var text2 = `The goal of the game is to get
// four similar pieces in a row. For
// instance, four dark pieces in a
// row would win...`;
//
//   var slide = [
//     tooltip({
//       name : 'slide2',
//       x : 500,
//       y : 100,
//       width : 285,
//       height : 135,
//       content : text2
//     }),
//     nextButton(760, 210, goSlide3)
//   ]
//
//   function goSlide3() {
//     slide.forEach(i => { i.remove(); });
//     animatePieceToHome("id-Dark-Tall-Square-Hollow");
//     animatePieceToHome("id-Dark-Tall-Circle-Hollow");
//     animatePieceToHome("id-Dark-Short-Square-Hollow");
//     animatePieceToHome("id-Dark-Tall-Square-Solid");
//     slide3();
//   }
//
// }
//
// export function slide3() {
//   animatePieceToPosition("id-Dark-Tall-Square-Hollow", "1,3");
//   animatePieceToPosition("id-Light-Tall-Square-Hollow", "2,3");
//   animatePieceToPosition("id-Dark-Short-Square-Hollow", "3,3");
//   animatePieceToPosition("id-Dark-Tall-Square-Solid", "4,3");
// }

export function nextButton(name, x, y, content, callback) {
  content = content || "NEXT";
  var a = arrow(name + "arrow", x, y, 0)
  a.style = { fillColor : "#ffa500", strokeColor : '#fff', strokeWidth : 0 }
  a.scale(-1,1);
  a.scale(.8,1);
  a.scale(.5,.5)

  var t = Game.Shapes.text({
    x : 0, y : 0,
    name : name + "text", content : content,
    fillColor : '#ffa500', fontFamily : '"Proxima Nova Regular","Segoe UI",Roboto,"Droid Sans","Helvetica Neue",Arial,sans-serif'
  })
  a.position = new Point(x + t.width + 4, y)
  t.position = new Point(x + t.width / 2, y - 4 + t.height / 2);

  var group = new Group([a,t]);
  group.name = name;
  
  var onClick = function() {
    group.off(mouseEnter);
    group.off(mouseLeave);
    group.off(onClick);
    group.remove();
    callback();
  }

  var mouseEnter = () => {
    document.body.style.cursor = "pointer";
  }

  var mouseLeave = () => {
    document.body.style.cursor = "default";
  }

  group.on('click', onClick);
  group.on('mouseenter', mouseEnter);
  group.on('mouseleave', mouseLeave);

  return group;

}

export function prevButton(name, x, y, callback) {
  var a = arrow(name + 'arrow', x, y, 0)
  a.style = { fillColor : "#ffa500", strokeColor : '#fff', strokeWidth : 0 }
  a.scale(1,1);
  a.scale(.8,1);
  a.scale(.5,.5)

  var t = Game.Shapes.text({
    x : x + 2, y : y - 4,
    name : name + "text", content : "PREV",
    fillColor : '#ffa500', fontFamily : '"Proxima Nova Regular","Segoe UI",Roboto,"Droid Sans","Helvetica Neue",Arial,sans-serif'
  })
  a.position = new Point(x + 8, y);
  t.position = new Point(x + 8 + 2 + t.width / 2, y - 4 + t.height / 2);

  var group = new Group([a,t]);
  group.name = name;

  var onClick = function() {
    group.off(onClick);
    group.off(mouseEnter);
    group.off(mouseLeave);
    group.remove();
    callback();
  }

  var mouseEnter = () => {
    document.body.style.cursor = "pointer";
  }

  var mouseLeave = () => {
    document.body.style.cursor = "default";
  }

  group.on('click', onClick);
  group.on('mouseenter', mouseEnter);
  group.on('mouseleave', mouseLeave);

  return group;

}
