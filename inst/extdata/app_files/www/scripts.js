// Adds and removes a specific class to the body element
// Targets that class to alter the bg color for a specific panel (Methods)

$(document).on('shown.bs.tab', function(e) {

  var activePane = $('.tab-pane.active');
  var panelValue = activePane.attr('data-value');
  
  if (panelValue === 'Methods') {
    $('body').addClass('methods-active');
    $('').addClass('fade-in');
  } else {
    $('body').removeClass('methods-active');
    $('').removeClass('fade-in');
  }
  
});