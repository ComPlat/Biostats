Shiny.addCustomMessageHandler('updateField', function(message) {
  var result = message.message;
  $('#output').append(result + '<br>');
});