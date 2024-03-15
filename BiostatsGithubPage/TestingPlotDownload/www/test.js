Shiny.addCustomMessageHandler('test', function(message) {
   var divId = message.ID;
   var div = document.getElementById(divId);
   div = div.innerHTML;
   var ImageBase64 = div.split(",")[1];
   ImageBase64 = ImageBase64.split("\"")[0];
   var a = document.createElement("a"); 
   a.href = "data:image/png;base64," + ImageBase64; 
   a.download = "Image.png"; 
   a.click(); 
});


Shiny.addCustomMessageHandler('testOldPlot', function(message) {
   var ImageBase64 = message.Base64String;
   var a = document.createElement("a"); 
   a.href = "data:image/png;base64," + ImageBase64; 
   a.download = "Image.png"; 
   a.click(); 
});