# a shiny app to explore bistability in the context of ecological feedbacks
#   this is the main file. It loads dependencies then runs the app.
#author: Ana-Hermina Ghenu
#date: 2025-07-01

# load the environment
source("fire_funs.R")
source("fire_text.R")
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
thematic_shiny(font = font_spec("auto", scale = 1.53))

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
  
  # typeset math using latex code
  withMathJax(),
  # allow in-line LaTeX via $ in mathjax.
  tags$div(HTML("<script type='text/x-mathjax-config' >
            MathJax.Hub.Config({
            tex2jax: {inlineMath: [['$','$'], ['\\\\(','\\\\)']]}
            });
            </script >
            ")),

  # Title
  titlePanel("Ecosystem restructuring by fire"),
  
  # a subtitle / summary can be added here:
  markdown(text_subtitle),
  
  navset_card_pill(# use pill-shaped buttons to navigate
    # explain what the tabs on this card do
    title = "Select a topic:",
    
    nav_panel("Introduction",
              markdown(text_intro)),

    nav_panel("How to use the app", markdown(text_howto)),
    
    nav_panel("What's going on?", markdown(text_huh)),
    
    nav_panel("Key Insight", markdown(text_feedback)),
    
    nav_panel("Things to try", markdown(text_try)),
    
    nav_panel("Optional: code", markdown(text_code)),
    
    nav_panel("Summary", markdown(text_summary))
  ),
  
  # Top row with sliders
  fluidRow(
    column(4,
           sliderInput(inputId = "muA_slider",
                       label = "Trees Lost per fire $(L)$",
                       value = 0.07, 
                       min = 0.01, max = 0.19,
                       step = 0.02)
    ),
    column(4,
           sliderInput(inputId = "A0_slider",
                       label = "Initial Tree Density $(T_0)$",
                       value = 0.08, 
                       min = 0.02, max = 0.98,
                       step = 0.02)
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
  layout_columns(height=580, 
    card(card_header("1. Model schematic & Equation"),
         card_body(class = "align-items-center",
                   imageOutput("plot_schematic", height = 100, width="75%")),
         card_body(uiOutput("dynamEq"), height=100, fill=FALSE)
    ),
    card(
      card_header("3. Fire frequency over Time"),
      plotOutput("plot_fire_time")
    ),
    card(
      card_header("2. Change in Tree density"),
      plotOutput("plot_A_vs_dA")
    ),
    card(
      card_header("4. Tree & Grass density over Time"),
      plotOutput("plot_species_time")
    ),
    col_widths = c(7, 5, 7, 5)  # left column is wider than right column
  )
)


# Server logic
server <- function(input, output) {
  # fix the environmental frequency of fire 
  # be careful to choose a value that does *NOT* result in neutral system
  E <- 0.09
  #E <- 0.55 # increase climatic rate of fire for the test question
  static_schematic <- plot_schem_static()
  
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
  
  # reactive arrows for the schematic
  LF_arrows <- reactive(plot_schem_arrows(params = curr_params()))
  
  ######################
  # outputs
  ######################
  # typeset dynamic differential equation
  output$dynamEq <- renderUI({
    temp_parameters <- curr_params()
    muA <- unname(temp_parameters["mu_A"])
    Ep <- unname(temp_parameters["epsilon"])
    
    # first define the equation by colouring the dynamic variables with their appropriate colours
    the_eqn <- paste0("$$\\small{ \\frac{\\delta T}{\\delta t} = \\overbrace{0.1 \\color{", colours_species["arbour"],
                      "}{T(1-T)}}^{\\text{biomass growth}} - \\overbrace{ \\color{", colour_fire,
                      "}{%.02f} \\color{", colours_species["arbour"],
                      "}{T}\\underbrace{(%.02f + \\color{", substring(colours_species["grass"], first=1, last=7),
                      "}{(1-T)})(1-\\color{", colours_species["arbour"],
                      "}{T})}_{\\text{frequency of fire}}}^{\\text{loss of biomass due to fire}}}$$")
    
    # # here's another version of the equation with the Ep parameter coloured in red
    # the_eqn <- paste0("$$\\small{ \\frac{\\delta T}{\\delta t} = \\overbrace{0.1 \\color{", colours_species["arbour"],
    #                   "}{T(1-T)}}^{\\text{biomass growth}} - \\overbrace{ %.02f \\color{", colours_species["arbour"],
    #                   "}{T}\\underbrace{( \\color{", colour_fire,
    #                   "}{%.02f} + \\color{", substring(colours_species["grass"], first=1, last=7),
    #                   "}{(1-T)})(1-\\color{", colours_species["arbour"],
    #                   "}{T})}_{\\text{frequency of fire}}}^{\\text{loss of biomass due to fire}}}$$")
    
    # then typeset the equation using MathJax
    withMathJax(sprintf(the_eqn, muA, Ep))
  })
  # render model schematic natively:
    # THIS WAS REMOVED FOR APP DEPLOYMENT bc it varies with participant window size
  #output$plot_schematic <- renderPlot({static_schematic + LF_arrows()})
  # render model schematic from saved png file
  output$plot_schematic <- renderImage({
    list(
      src = file.path("model_schematics",
                      paste0("schemL-", substring(as.character(input$muA_slider), 3, 4), ".png")),
      contentType = "image/png",
      height = 100,
      width = 364
    )
  }, deleteFile = FALSE)
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
}

# Complete app with UI and server components
shinyApp(ui, server)
