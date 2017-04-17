export function deepFreeze (o) {
  Object.freeze(o);

  Object.getOwnPropertyNames(o).forEach(function (prop) {
    if (o.hasOwnProperty(prop)
    && o[prop] !== null
    && (typeof o[prop] === "object" || typeof o[prop] === "function")
    && !Object.isFrozen(o[prop])) {
      deepFreeze(o[prop]);
    }
  });

  return o;
};


export function removeItem(array, item) {
  for (var i = array.length - 1; i >= 0; i--) {
    if (array[i] === item) {
        array.splice(i, 1);
    }
  }
}

export function toPoint(p) {
  return new Point(p.x, p.y);
}

export function toPositionId(p) {
  return p.x + "," + p.y;
}

export function getPointLayer(x, y) {
  let l1 = [[1,4]];
  let l2 = [[2,4],[1,3]];
  let l3 = [[3,4],[2,3],[1,2]];
  let l4 = [[4,4],[3,3],[2,2],[1,1]];
  let l5 = [[4,3],[3,2],[2,1]];
  let l6 = [[4,2],[3,1]];
  let l7 = [[4,1]];

  var layer;

  [l1, l2, l3, l4, l5, l6, l7].find(function(set, index) {
    var found = set.find(function(pair) {
      if (pair[0] == x && pair[1] == y) {
        return true;
      }
    });

    if (found) {
      layer = project.getItem({ name : "layer" + index })
      return true;
    }
  });

  return layer;
}


export function addVectors(v1, v2) {
  return new Point(v1.x + v2.x, v1.y + v2.y);
}

export function subtractVectors(v1, v2) {
  return new Point(v1.x - v2.x, v1.y - v2.y);
}

export function reduceVector(v1, val) {
  return new Point(v1.x / val, v1.y / val);
}
