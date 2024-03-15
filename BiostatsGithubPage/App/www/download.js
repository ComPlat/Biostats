Shiny.addCustomMessageHandler('DownloadPlot', function(message) {
   var ImageBase64 = message.Base64String;
   var a = document.createElement("a"); 
   a.href = "data:image/png;base64," + ImageBase64; 
   a.download = "Image.png"; 
   a.click(); 
});

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

/*
Shiny.addCustomMessageHandler('downloadZip', function(message) {
  var files = message.files;
  var FileContent = message.FileContent;
  console.log(files);
  console.log(files.length);
  var zip = new JSZip();
  for (i in FileContent) {
    var fileData = files[i];
    var fileName = FileContent[i];
    zip.file(fileName, fileData);
  }
  zip.generateAsync({type:'blob'})
  .then(function(content) {
    saveAs(content, 'download.zip');
  });
});
*/

Shiny.addCustomMessageHandler('downloadZip', function(message) {
  var FileContent = message.FileContent;
  if( (typeof FileContent) == "string") {
    var zip = new JSZip();
    var fileName = 'file' + 1 + '.txt'; 
    zip.file(fileName, FileContent);
    zip.generateAsync({type: 'blob'}).then(function(content) {
      saveAs(content, 'download.zip');
    });
  } else {
    var zip = new JSZip();
    for (var i = 0; i < FileContent.length; i++) {
      var fileName = 'file' + (i + 1) + '.txt'; 
      zip.file(fileName, FileContent[i]);
    }
    zip.generateAsync({type: 'blob'}).then(function(content) {
      saveAs(content, 'download.zip');
    });
  }
});