# a minimal example of my issues

library(shiny) # for app
library(bslib) # for most recent recommended UI options
library(lorem) # for placeholder text

app_theme <- bs_theme(bg="ivory", fg = "#222222")

# user interface
ui <- fluidPage(
  
  # colour in the background to illustrate the issue
  theme = app_theme,
  
  # a card
  card(card_header("A card"),
       card_body(class = "align-items-center",
                 imageOutput("plot_schematic", height="auto", width = "50%")),
       card_body(lorem::ipsum(paragraphs = 1))
  )
)

# Server logic
server <- function(input, output) {
  output$plot_schematic <- renderImage({
    list(
      src = file.path("sad_puppy.png"),
      contentType = "image/png",
      height = 100,
      width = 242
    )
  }, deleteFile = FALSE)
}

# Complete app with UI and server components
shinyApp(ui, server)
  