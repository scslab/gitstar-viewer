$(function() {
  window.parent.postMessage($(document).height().toString(), "*");

  /* Hide commit message */
  $("#commit-message").hide();

  /* Hide/show commit message based on clicks. */
  $('#commit-message-show').click(function() {
    $('#commit-message').slideDown('slow');
  });
  $('#commit-message-hide').click(function() {
    $('#commit-message').slideUp('slow');
  });

});
