/* Range slider binding */

$(document).on("input", ".range-slider__range", function(evt) {

  // evt.target is the button that was clicked
  var el = $(evt.target);

  // Raise an event to signal that the value changed
  el.trigger("change");
  console.log(el);
});

var rangeBinding = new Shiny.InputBinding();
$.extend(rangeBinding, {
  find: function(scope) {
    return $(scope).find(".range-slider__range");
  },
  getValue: function(el) {
    return parseFloat($(el).val());
  },
  setValue: function(el, value) {
    $(el).val(value);
  },
  subscribe: function(el, callback) {
    $(el).on("change.range-slider__range", function(e) {
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off(".range-slider__range");
  }
});

Shiny.inputBindings.register(rangeBinding);
