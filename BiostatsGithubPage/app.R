library(shiny)

ui <- fluidPage(
  titlePanel("Custom Message Handler Demo with Download"),
  mainPanel(
    textInput("user_message", "Enter your message:"),
    actionButton("trigger_button", "Trigger Custom Message Handler"),
    tags$head(
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jszip/3.7.1/jszip.min.js"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/FileSaver.js/2.0.5/FileSaver.min.js")
    ),
    tags$script(
      "
      Shiny.addCustomMessageHandler('updateField', function(message) {
        console.log(message.message);
        var result = message.message;
        $('#output').append(result + '<br>');
      });
      
      Shiny.addCustomMessageHandler('downloadText', function(message) {
        var text = message.text;
        var filename = message.file;
        var element = document.createElement('a');
        element.setAttribute('href', 'data:text/plain;charset=utf-8,' + encodeURIComponent(text));
        element.setAttribute('download', filename);
        element.style.display = 'none';
        document.body.appendChild(element);
        element.click();
        document.body.removeChild(element);
      });
      
       Shiny.addCustomMessageHandler('downloadZip', function(message) {
          var files = message.files;
          var filenames = message.filenames;
          console.log(files.length);
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
      "
    ),
    div(id = "output", style = "margin-top: 10px;"),
    br(),
    actionButton("download_button", "Download Appended Messages"),
    actionButton("download_button2", "Download Appended Messages")
  )
)

server <- function(input, output, session) {
  observeEvent(input$trigger_button, {
    user_message <- input$user_message
    session$sendCustomMessage("updateField", list(message = user_message))
  })
  
  observeEvent(input$download_button, {
    session$sendCustomMessage(type = "downloadText", list(text = "test", file = "bla.csv"))
  })
  
  observeEvent(input$download_button2, {
    files <- c("File content 1", "File content 2")
    filenames <- c(tempfile(fileext = ".txt"), tempfile(fileext = ".txt"))
    session$sendCustomMessage(type = "downloadZip", list(files = files, filenames = filenames))
  })
}

shinyApp(ui, server)
