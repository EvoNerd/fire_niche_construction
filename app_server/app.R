# a shiny app to explore bistability in the context of ecological feedbacks
#   this is the main file. It loads dependencies then runs the app.
#author: Ana-Hermina Ghenu
#date: 2025-07-01

# load the environment
source("fire_funs.R")
source("fire_text.R")

# I'm getting an error message "unable to open connection to X11 display"
library(Cairo) # this should fix it but it seems that Cairo isn't installed on the server
options(bitmapType = "cairo") # set headless graphics device

library(shiny) # for app
library(bslib) # for most recent recommended UI options
library(thematic) # for converting R plots to have consistent theme as from bslib

# set the theme for bslib objects
app_theme <- bs_theme(bootswatch = "minty", # set simplex theme
                      version = 5,
                      primary = substring(colours_species["arbour"], first=1, last=7), # set the primary: this matches colour used for trees
                      secondary = colour_fire) # secondary colour used for fire

###################################
# define the app
###################################


# call thematic before launching the shiny
  # this will integrate the theme from bslib to how the plots are displayed
thematic_shiny(font = font_spec("auto"))

# user interface
ui <- fluidPage(

  theme = app_theme,
  
  # set all cards to have a white background
  tags$head(
    tags$style(HTML("
      .card {
        background-color: white !important;
      }
    "))
  ),
  # reduce the font size of slider controls
  tags$style(HTML("
    .control-label {
      font-size: 14px;
    }
  ")),
  # I tried to reduce the size of the slider but I don't think it does anything
  tags$style(HTML("
    .js-range-slider {
      height: 0px !important;
    }
  ")),
  # reduce the font size of card headers
  tags$style(HTML("
    .card-header {
      font-size: 14px;
    }
  ")),
  
  # typeset math using latex code
  withMathJax(),
  # allow in-line LaTeX via $ in mathjax.
  tags$div(HTML("<script type='text/x-mathjax-config' >
            MathJax.Hub.Config({
            tex2jax: {inlineMath: [['$','$'], ['\\\\(','\\\\)']]}
            });
            </script >
            ")),
  
  HTML("<br>"),
  
  navset_card_pill(# use pill-shaped buttons to navigate
    # explain what the tabs on this card do
    title = "Select a topic:",
    
    nav_panel("Introduction",
              markdown(text_intro)),

    nav_panel("App How-to & Things to Try", markdown(text_howto)),
    
    nav_panel("What's going on?", markdown(text_huh)),
    
    nav_panel("Key Insight", markdown(text_feedback)),
    
    #nav_panel("Things to try", markdown(text_try)),
    
    #nav_panel("Optional: code", markdown(text_code)),
    
    nav_panel("Summary", markdown(text_summary))
  ),
  
  # Top row with sliders
  fluidRow(
    column(4,
           sliderInput(inputId = "muA_slider",
                       label = "Trees Lost per Fire $(L)$",
                       value = 0.07, 
                       min = 0.01, max = 0.19,
                       step = 0.02)
    ),
    column(4,
           sliderInput(inputId = "A0_slider",
                       label = "Initial Tree Density $(T_0)$",
                       value = 0.1, 
                       min = 0.02, max = 0.98,
                       step = 0.08)
    ),
    column(4,
           sliderInput(inputId = "time_slider",
                       label = "Number of Years",
                       value = 100, 
                       min = 50, max = 500,
                       step = 50)
    )
  ),
  
  # Middle and bottom rows replaced laid out as cards on a responsive grid
  layout_columns(#NOTE that the sm setting of breakpoints makes it difficult to set the total height of 2-column layout here
                 gap = "0rem", # removes the whitespace between cards
    card(
      card_header("1. Model schematic & Equation"),
      card_body(imageOutput("plot_schematic", height = 55, width="75%"),
                uiOutput("dynamEq", height = 30),
      height="160px",
      class = "align-items-center")
    ),
    card(
      card_header("3. Fire frequency over Time"),
      plotOutput("plot_fire_time"),
      height="200px"
    ),
    card(
      card_header("2. Change in Tree density"),
      plotOutput("plot_A_vs_dA"),
      height="200px"
    ),
    card(
      card_header("4. Trees & Grasses over Time"),
      plotOutput("plot_species_time"),
      height="200px"
    ),
    col_widths = breakpoints(
      #sm = rep(12, times = 4), # for portrait-mode phones, display in a single column
      sm = c(7, 5, 7, 5), # left column is wider than right column
      lg = rep(6, times=4) # columns are equally sized when it gets wide enough
    )
  )
)


# Server logic
server <- function(input, output) {
  # fix the environmental frequency of fire 
  # be careful to choose a value that does *NOT* result in neutral system
  E <- 0.09
  #E <- 0.55 # increase climatic rate of fire for the test question
  
  ######################
  # expressions
  ######################
  
  # a reactive expression to define the parameters
  curr_params <- reactive(c(epsilon = E,
                            mu_A = input$muA_slider))
  
  # a reactive expression to get dA as a function of tree frequency (A)
  dA_df <- reactive(get_dA_by_A(params = curr_params()))
  
  # a reactive expression to get the long-term model outcome
  outcome <- reactive(get_steady_state(params = curr_params(),
                              dA.df = dA_df()))
  
  # a reactive expression to simulate evolution of the system over time
  sims <- reactive(sim_forward_time(time = 0:input$time_slider,
                                    init = c(A = input$A0_slider),
                                    params = curr_params()))
  
  # a reactive expression to simulate fire frequency in the system over time
  fires <- reactive(get_fire_time(params = curr_params(),
                                  sim_df = sims()))
  
  ######################
  # outputs
  ######################
  # typeset dynamic differential equation
  output$dynamEq <- renderUI({
    temp_parameters <- curr_params()
    muA <- unname(temp_parameters["mu_A"])
    Ep <- unname(temp_parameters["epsilon"])
    
    # first define the equation by colouring the dynamic variables with their appropriate colours
    the_eqn <- paste0("$$\\scriptsize{ \\frac{\\delta T}{\\delta t} = \\overbrace{0.1 \\color{", colours_species["arbour"],
                      "}{T(1-T)}}^{\\text{biomass growth}} - \\overbrace{ \\color{", colour_fire,
                      "}{%.02f} \\color{", colours_species["arbour"],
                      "}{T}\\underbrace{(%.02f + \\color{", substring(colours_species["grass"], first=1, last=7),
                      "}{(1-T)})(1-\\color{", colours_species["arbour"],
                      "}{T})}_{\\text{frequency of fire}}}^{\\text{loss of biomass due to fire}}}$$")
       
    # then typeset the equation using MathJax
    withMathJax(sprintf(the_eqn, muA, Ep))
  })

  # render Group Frequencies over Time plot
  output$plot_species_time <- renderPlot({ggplot_t_finiteANDoutcome(sim_df = sims(),
                                                                    steady_state = outcome())})
  
  # render Fire Frequencies over Time plot
  output$plot_fire_time <- renderPlot({ggplot_fire_finiteANDoutcome(params = curr_params(),
                                                                   sim_df = sims(),
                                                                   fire_df = fires(),
                                                                   steady_state = outcome())})
  
  # render T vs dT plot
  output$plot_A_vs_dA <- renderPlot({plot_dA_by_A(dA.df = dA_df(),
                                                  steady_state = outcome())})

  # render model schematic from saved png file
  output$plot_schematic <- renderImage({
   list(
     src = file.path("model_schematics",
                     paste0("schemL-", substring(as.character(input$muA_slider), 3, 4), ".png")),
     contentType = "image/png",
     height = 75, # 100,
     width = 250 # 364
   )
  }, deleteFile = FALSE)
}

# Complete app with UI and server components
shinyApp(ui, server)
