Shiny.addCustomMessageHandler('downloadZip', function(message) {
  var files = message.files;
  var filenames = message.filenames;
  var zip = new JSZip();
  for(i in filenames) {
    var fileData = files[i];
    var fileName = filenames[i];
    zip.file(fileName, fileData);
  }
  zip.generateAsync({type:'blob'})
  .then(function(content) {
    saveAs(content, 'download.zip');
  });
});