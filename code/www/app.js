$(document).ready(function() {

// Hide or show app status infos based on tab selected

$('#toggle-saved-queries').on('click', function () {
  $('#app-status-infos').addClass('hide');
});

$('#toggle-coverages').on('click', function () {
  $('#app-status-infos').removeClass('hide');
});

$('#toggle-data-count').on('click', function () {
  $('#app-status-infos').removeClass('hide');
});

// END OF INIT
});
