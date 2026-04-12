// Prevent duplicate registration
if (!window.loadingHandlersRegistered) {

Shiny.addCustomMessageHandler('showDownloadLoading', function(message) {
  var button = $('#' + message.button_id);
  var originalText = button.html();

  button.data('original-text', originalText);

  var newText = message.text || "Loading...";
  button.html('<i class="fa fa-spinner fa-spin"></i> ' + newText);
  button.prop('disabled', true);
});

  Shiny.addCustomMessageHandler('hideDownloadLoading', function(message) {
    var buttonId = message.button_id;
    var button = $('#' + buttonId);
    var originalText = button.data('original-text');
    button.html(originalText || 'Download Excel Form');
    button.prop('disabled', false);
  });

  Shiny.addCustomMessageHandler('updateDownloadProgress', function(message) {
    var percent = message.percent;
    $('#progress_bar').css('width', percent + '%').html(Math.round(percent) + '%');
    if (percent === 0 || percent === 100) {
      setTimeout(() => $('#progress_container').hide(), 1000);
    } else {
      $('#progress_container').show();
    }
  });

  Shiny.addCustomMessageHandler('hideDownloadProgress', function(message) {
    $('#progress_container').hide();
    $('#progress_bar').css('width', '0%').html('0%');
  });

  window.loadingHandlersRegistered = true;
}