Shiny.addCustomMessageHandler('downloadZip', function(message) {
  var FileContent = message.FileContent;
  if( (typeof FileContent) == "string") {
    if (FileContent.startsWith("data:image")) {
      var fileName = 'file' + (i + 1) + '.png'; 
      var zip = new JSZip();
      var imageData = atob(FileContent.split(',')[1]);
      var byteArray = new Uint8Array(imageData.length);
      for (var i = 0; i < imageData.length; i++) {
        byteArray[i] = imageData.charCodeAt(i);
      }
      zip.file(fileName, byteArray, {binary: true});
      zip.generateAsync({type: 'blob'}).then(function(content) {
        saveAs(content, 'download.zip');
      });
    } else {
      var zipText = new JSZip();
      var fileNameText = 'file' + 1 + '.txt'; 
      zipText.file(fileNameText, FileContent);
      zipText.generateAsync({type: 'blob'}).then(function(content) {
        saveAs(content, 'download.zip');
      });
    }
  } else {
    var zip = new JSZip();
    for (var i in FileContent) {
      if (FileContent[i].startsWith("data:image")) {
        var fileName = 'file' + (i + 1) + '.png'; 
        var imageData = atob(FileContent[i].split(',')[1]);
        var byteArray = new Uint8Array(imageData.length);
        for (var i = 0; i < imageData.length; i++) {
          byteArray[i] = imageData.charCodeAt(i);
        }
        zip.file(fileName, byteArray, {binary: true});
      } else {
        var fileName = 'file' + (i + 1) + '.txt'; 
        zip.file(fileName, FileContent[i]); 
      }
    }
    zip.generateAsync({type: 'blob'}).then(function(content) {
      saveAs(content, 'download.zip');
    });
  }
});