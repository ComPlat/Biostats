library(shiny)

ui <- fluidPage(
  # JavaScript to initialize the popover with interactive content on click
  tags$script(HTML("
    $(document).ready(function() {
      // Initialize the popover on click instead of hover
      $('#aovTest').popover({
        trigger: 'click', // Opens on click
        html: true,
        content: '<button id=\"popoverBtn\" class=\"btn btn-link\">Go to ANOVA Info</button>',
        placement: 'bottom'
      });

      // Set up click event for the button inside the popover
      $(document).on('click', '#popoverBtn', function() {
        Shiny.setInputValue('go_to_tab', 'infoTab');
        $('#aovTest').popover('hide'); // Hide the popover after clicking the button
      });

      // Close the popover if clicking outside of it
      $(document).on('click', function(e) {
        if (!$(e.target).closest('#aovTest').length) {
          $('#aovTest').popover('hide');
        }
      });
    });
  ")),
  
  # UI with tab layout
  tabsetPanel(
    id = "mainTabset",
    tabPanel("Main",
             # Action button with popover for interactive tooltip
             actionButton(
               "aovTest", "ANOVA"
             ),
             p("This is the Main tab.")
    ),
    tabPanel("Info", 
             value = "infoTab",  # This is the ID to match in JavaScript
             h3("ANOVA Information"),
             p("Here are more details about ANOVA...")
    )
  )
)

server <- function(input, output, session) {
  # Observe the JavaScript input and update the selected tab
  observeEvent(input$go_to_tab, {
    updateTabsetPanel(session, "mainTabset", selected = input$go_to_tab)
  })
}

shinyApp(ui, server)

