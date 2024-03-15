Shiny.addCustomMessageHandler('downloadPlot', function(message) {
  var link = document.createElement('a');
  link.href = message.path; 
  link.download = 'plot.png'; 
  document.body.appendChild(link);
  link.click();
  document.body.removeChild(link); 
});