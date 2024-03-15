Shiny.addCustomMessageHandler('downloadText', function(message) {
  var divId = message.ID;
  var div = document.getElementById(divId);
  inner = div.innerHTML;
  if (!div) {
      console.error("Div with ID '" + divId + "' not found.");
      return;
  }
  var imgElement = div.getElementsByTagName('img')[0];
  var src = imgElement.getAttribute('src');
  if (src.startsWith('data:image/png;base64,')) {
      var base64Data = src.split(',')[1];
      console.log(base64Data);
      Shiny.setInputValue('pngData', base64Data);
  } else {
      console.error("Invalid src format. It should start with 'data:image/png;base64,'");
  }
});