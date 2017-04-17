'use strict';

// DOMElement -> DOMElement -> Aff (dom :: DOM) Unit
exports.clickToCopyElementValue_ = function (clickElement, valueElement) {
  return function (success, fail) {
    clickElement.addEventListener('click', function() {
      var result = copyText(valueElement.value);
      if (result) {
        success({});
      } else {
        fail(new Error('Content could not be copied'));
      }
    })
  }

}

// DOMElement -> Eff (dom :: DOM) Boolean
// Attempt to select all content in an element and return
// true if the browser supports either document.selection or
// window.getSelection().  Otherwise return false.
exports.selectElementText_ = function(element) {
  return function() {
    return selectElementText(element);
  }
}

// DOMElement -> PretendEff (dom :: DOM) Boolean
function selectElementText(element) {
  if (document.selection) {
    var range = document.body.createTextRange();
    range.moveToElementText(element);
    range.select();
    return true;
  } else if (window.getSelection) {
    var range = document.createRange();
    range.selectNode(element);
    window.getSelection().removeAllRanges();
    window.getSelection().addRange(range);
    return true;
  } else {
    return false;
  }
}

// String -> Boolean
// Accepts text to copy, and returns a boolean indicating
// whether the text has actually been copied via document.execCommand.
function copyText(text){
  var element = document.createElement('DIV');
  element.textContent = text;
  document.body.appendChild(element);
  var selectionWorked = selectElementText(element);
  var copyWorked = document.execCommand('copy');
  element.remove();
  return selectionWorked == true && copyWorked == true;
}
