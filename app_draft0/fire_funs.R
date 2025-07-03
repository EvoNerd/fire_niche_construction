# fire shiny app dependency for app.R
#   simulation and plotting functions are defined here
#author: Ana-Hermina Ghenu
#date: 2025-07-01

library(tidyverse) # for pipe syntax
library(deSolve)   # for solving ordinary differential equations
library(ggplot2)   # for plotting the time series
library(patchwork) # for combining ggplots in the time series
library(ggarrow)   # for ridiculously complex fancy arrows
library(png)       # for fire emoji
library(grid)      # also for fire emoji


# define colour scheme for species
# use rgb to set transparency (alpha = 0.75) # note that max value for alpha is 255 so 0.75*255 ~ 191
colours_species <- c(arbour = rgb(0, 102, 0, maxColorValue = 255, alpha = 191),  # Forest green
                     grass = rgb(244, 212, 48, maxColorValue = 255, alpha = 191)) # Gold
colour_dA <- "#2D1C56"
colour_fire <- "#FF0000"

###################################
# define simulation functions
###################################

# ordinary differential equation: a function to get the rate of change over time
ODE <- function(time, init, params){
  with (as.list(c(time, init, params)), {
    # define the differential equations:
    dA_dt = 0.1*A*(1-A) - mu_A*A*(epsilon + (1-A))*(1-A)
    return(list(dA = dA_dt))
  })
}

# a function to simulate the community over time
sim_forward_time <- function(time, init, params) {
  # solve the system of ODE's for the requested time points:
  sim <- ode(func = ODE, y = init, parms = params, times = time, method="ode23")
  # make a long data.frame with trees and grass over time
  return(data.frame(time = rep(sim[,1], times=2),
                    species = rep(c("arbour", "grass"), each=length(time)),
                    # recall that fraction of grass = 1 - fraction of trees
                    density = c(sim[,2], 1-sim[,2]))
         )
}

# a function to get dA as a function of A
get_dA_by_A <- function(params){
  # Initialize a vector of tree fractions from 0 to 1
  A_values <- seq(0, 1, length.out = 1002)
  # remove 0 and 1 because they're always equilibrium points for this model
  A_values <- A_values[2:1001]
  # compute dA
  dA_values <- sapply(A_values,
                      function(x)
                        ODE(time = 0, init = c(A=x), params = params)$dA)
  # return a data.frame with A and dA values
  return(data.frame(A = A_values, dA = dA_values))
}

# a function to get instantaneous fire probability
fire_prob <- function(params, A_density)
  unname((params["epsilon"] + (1-A_density))*(1-A_density))

# a function to get the frequency of fire over time
get_fire_time <- function(params, sim_df){
  # Filter for tree species only
  tree_df <- sim_df[sim_df$species == "arbour", ]
  
  # Extract time and tree density
  time_pts <- tree_df$time
  A_vals <- tree_df$density
  
  # calculate the instantaneous fire frequency
  return(data.frame(time = time_pts,
                    fire = sapply(A_vals,
                                  function(x) fire_prob(params, x))))
}

# a function indicating the long-term steady state
get_steady_state <- function(params, dA.df){
  # some presets for plotting the arrows
  min_arrow_length = 0.07
  arrow_horizontal_whitespace = 0.08
  
  # 0 and 1 are always equilibrium points for this system
  # calculate the 3rd equilibrium point
  equil_pt <- unname(((1+params["epsilon"])*params["mu_A"]-0.1)/params["mu_A"])
  
  # when the 3rd equilibrium point is negative,
  if(equil_pt < 0) {
    # then the system is going to fix for trees
    
    # draw the arrows above the y=0 at a quarter of the smallest max dA values
    y_val = max(abs(dA.df$dA))/4
    # increase the whitespace of the arrow
    arrow_horizontal_whitespace <- 3*arrow_horizontal_whitespace
    arrow1_length <- 1-2*arrow_horizontal_whitespace
    output <- list(outcome = "A=1",
                   stable_pts = 1,
                   arrow1 = c(y0 = y_val, x0 = arrow_horizontal_whitespace,
                              y1 = y_val, x1 = arrow_horizontal_whitespace + arrow1_length)
                   )
    
  # when the 3rd equilibrium point is larger than 1,
  } else if(equil_pt > 1) {
    # then the system is going to fix for grass
    
    # draw the arrows above the y=0 at a quarter of the smallest max dA values
    y_val = min(dA.df$dA)/4
    # increase the whitespace of the arrow
    arrow_horizontal_whitespace <- 3*arrow_horizontal_whitespace
    arrow1_length <- 1-2*arrow_horizontal_whitespace
    output <- list(outcome = "A=0",
                   stable_pts = 0,
                   arrow1 = c(y0 = y_val, x0 = arrow_horizontal_whitespace + arrow1_length,
                              y1 = y_val, x1 = arrow_horizontal_whitespace)
    )
    
  # when the 3rd equilibrium point is between 0 and 1,
  } else {
    # then the system is bistable at 0, 1 and the 3rd equilibrium point is the unstable one
    
    # if the unstable equilibrium point is in the middle
    if(equil_pt > 0.2 & equil_pt < 0.8){
      # draw the arrows symmetrically about x=0 at a quarter of the *SMALLEST* max dA values
      y_val = min(abs(min(dA.df$dA)), abs(max(dA.df$dA)))/4
      # x values for left arrow (1): horizontally in middle of the 2 equil pts
      arrow1_length <- max(equil_pt-2*arrow_horizontal_whitespace, min_arrow_length)
      # x values for right arrow (2): horizontally in middle of the 2 equil pts
      arrow2_length <- max(1-equil_pt-2*arrow_horizontal_whitespace, min_arrow_length)
      
    # otherwise if the unstable equilibrium point is too much on one side
    } else {
      # draw the arrows symmetrically about x=0 at over a fifth of the *OVERALL* max dA values
      y_val = max(abs(dA.df$dA))/7
      # increase the whitespace of the arrow
      arrow_horizontal_whitespace <- 3*arrow_horizontal_whitespace
      # x values for left arrow (1): horizontally in middle of the 2 equil pts
      arrow1_length <- max(equil_pt-2*arrow_horizontal_whitespace, min_arrow_length)
      # x values for right arrow (2): horizontally in middle of the 2 equil pts
      arrow2_length <- max(1-equil_pt-2*arrow_horizontal_whitespace, min_arrow_length)
    }
    
    output <- list(outcome = "bistable",
                    stable_pts = c(0, 1),
                    unstable_pt = equil_pt,
                    arrow1 = c(y0 = -y_val, x0 = equil_pt - arrow_horizontal_whitespace,
                               y1 = -y_val, x1 = equil_pt - arrow_horizontal_whitespace - arrow1_length),
                    arrow2 = c(y0 = y_val, x0 = equil_pt + arrow_horizontal_whitespace,
                               y1 = y_val, x1 = equil_pt + arrow_horizontal_whitespace + arrow2_length)
                   )
  }
  return(output)
}



###################################
# define visualization functions
###################################


# Function to generate coordinates for just under 3/4 circle
generate_circle <- function(center = c(0, 0), radius = 1, npoints = 50) {
  theta <- seq(0.55 * pi, 2 * pi, length.out = npoints)
  x <- center[1] + radius * cos(theta)
  y <- center[2] + radius * sin(theta)
  data.frame(x = x, y = y)
}

# plot the bottom layers of the schematic
  # this gets called one time at the very beginning
plot_schem_static <- function(){
  # default white space btw node & line segment
  whitespace <- 0.2
  # default arrow & line segment thickness
  thicc <- 0.7
  
  # define the width and height of the filled rectangles
  rect_HALFwidth <- 1.2
  rect_HALFheight <- 0.35
  # define the width and height of the white rectangles
  white_FIREwidth <- 0.55
  white_CLIMATEwidth <- 1.12
  white_HALFheight <- 0.25
  
  # fire
  fire_emoji <- rasterGrob(readPNG("fire_emoji.png"), width = unit(1, "npc"), height = unit(1, "npc"))
  
  # Create a data.frame to define the rectangles and text
  rect_coords <- data.frame(
    rect_fill = c(colours_species, rep("white", 2)),
    x_center = c(1, 1, 4, 7), # 3 evenly spaced columns
    y_center = c(4.5, 1, 2.75, 3.625) # fire in middle of trees & grass. Climate in middle of trees & fire.
  )
  # name the rows by the "compartments"
  row.names(rect_coords) = c("Trees", "Grasses", "Fire", "Climate")
  # use the center points and rectangle dimensions to get edge coordinates
  rect_coords$left  <-  rect_coords$x_center - c(rep(rect_HALFwidth, 2), white_FIREwidth, white_CLIMATEwidth)
  rect_coords$right  <- rect_coords$x_center + c(rep(rect_HALFwidth, 2), white_FIREwidth, white_CLIMATEwidth)
  rect_coords$bottom <- rect_coords$y_center - rep(c(rect_HALFheight, white_HALFheight), each=2)
  rect_coords$top  <-   rect_coords$y_center + rep(c(rect_HALFheight, white_HALFheight), each=2)
  
  # add a fire emoji on the bottom layer
  schematic <- ggplot() +
                annotation_custom(fire_emoji, xmin = 3.7, xmax = 4.9, ymin = 2.35, ymax = 4.15)
  
  # begin making the plot. Put the rectangles on the lowest layer
  schematic <- schematic +
    # Add rectangles
    geom_rect(data = rect_coords,
              aes(xmin = left, xmax = right, ymin = bottom, ymax = top), 
              fill = rect_coords$rect_fill, color = rep(c("black", "white"), each=2),
              linewidth=thicc/5) +
    # Add text labels
    geom_text(data = rect_coords,
              aes(x = x_center, y = y_center, label = row.names(rect_coords)), 
              size = 8) +
    
    # add circle arrow for trees --| trees
    geom_arrow(data = generate_circle(center = c(rect_coords["Trees", "left"], rect_coords["Trees", "bottom"]-0.1),
                                      radius = rect_HALFheight*1.15),
               aes(x=x, y=y),
               arrow_head = arrow_head_line(angle = 90, lineend = "square"),
               length_head = unit(4, "mm"),
               linewidth = thicc) +
    
    # add straight arrow for trees --| grasses
    geom_arrow(data = data.frame(x = c(rect_coords["Trees", "x_center"], rect_coords["Grasses", "x_center"]),
                                 y = c(rect_coords["Trees", "bottom"] - whitespace, rect_coords["Grasses", "top"] + whitespace)),
               aes(x=x, y=y),
               arrow_head = arrow_head_line(angle = 90, lineend = "square"),
               length_head = unit(4, "mm"),
               linewidth = thicc)
              
  
  # finally set limits and theme
  schematic <- schematic +
    scale_x_continuous(limits = c(min(rect_coords$left)-0.4, max(rect_coords$right))) +
    scale_y_continuous(limits = c(min(rect_coords$bottom), max(rect_coords$top))) +
    theme_void() +
    coord_fixed()
  
  return(schematic)
}

# add just the remaining arrows to the schematic
plot_schem_arrows <- function(params){
  # muA arrow
  # epsilon arrow
  geom_text(data = data.frame(x=0.1, y=0.7, val=paste0(params, collapse = "")), aes(x=x, y=y, label=val))
}

# # a function to plot the community over finite time
# plot_t_finite <- function(sim_df){
#   # Set up an empty plot
#   plot(sim_df$time, sim_df$arbour,
#        type = "n",
#        ylim = c(-0.01, 1.01), yaxs = "i", # remove y-axis padding
#        xlab = "Time (years)",
#        ylab = "Species density",
#        yaxt = "n"  # Suppress default y-axis tick labels
#   )
#   
#   # Add custom y-axis tick labels
#   axis(2, at = seq(from=0, to=1, by=0.25), labels = c("0", "", "0.5", "", "1"))
#   
#   # Add lines for each species
#   lines(sim_df$time, sim_df$arbour, col = colours_species["arbour"], lwd = 7)
#   lines(sim_df$time, sim_df$grass, col = colours_species["grass"], lwd = 7)
#   
# }

# a GGPLOT function to plot the community over finite time
ggplot_t_finite <- function(sim_df){
  # get x-axis labels
  tickwidth_x <- floor(max(sim_df$time)/4)
  labs_x <- seq(from=tickwidth_x, to=3*tickwidth_x, by=tickwidth_x)
  
  # create the plot
  finit_plot <- ggplot(sim_df,
                       aes(x = time, y = density, colour = species, linewidth = 2.5)) +
                  geom_line() +
                  scale_colour_manual(values = colours_species) +
                  scale_y_continuous(limits = c(-0.01, 1.01), expand = c(0, 0),
                                     breaks = seq(from=0, to=1, by=0.25),
                                     labels = c(0, "", 0.5, "", 1)) +
                  scale_x_continuous(limits = c(min(sim_df$time), max(sim_df$time)), expand = c(0, 0),
                                     breaks = labs_x,
                                     labels = labs_x) +
                  labs(x="Time (years)", y="Density of each Group") +
                  theme_bw() + 
                  theme(text = element_text(size=22),
                       axis.text=element_text(size=18),
                       axis.line.y=element_blank(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       legend.position = "null",
                       plot.margin = margin(t=3, r=2, b=0, l=0)
                       )
  
  return(finit_plot)
}

# a GGPLOT function to plot the long-term outcome of the community
ggplot_t_inf <- function(sim_df, steady_state){
  # make a dataframe for plotting: inf_df
  
  # if the system has 2 attractors
  if (steady_state$outcome == "bistable"){
    # we need to check the initial conditions as compared to the unstable equilibrium point
    index_stable_pt <- ifelse(sim_df$density[1] < steady_state$unstable_pt,
                              1, 2)
    # create the appropraite df
    inf_df <- data.frame(Group = c("arbour", "grass"),
                         Value = c(steady_state$stable_pts[index_stable_pt],
                                   1 - steady_state$stable_pts[index_stable_pt]))
  
  # if the system has only 1 attractor
  } else {
    # we can add the lines directly from the steady state
    inf_df <- data.frame(Group = c("arbour", "grass"),
                         Value = c(steady_state$stable_pts,
                                   1 - steady_state$stable_pts))
  }
  # create the plot
  inf_plot <- ggplot() +
    geom_hline(data = inf_df, aes(yintercept = Value, color = Group),
               linewidth = 2.5) +
    scale_x_continuous(limits=c(0.5, 1.5), breaks = c(1), labels = "outcome") +
    scale_colour_manual(values = colours_species,
                        labels = c("Trees", "Grasses")) +
    scale_y_continuous(limits = c(-0.01, 1.01), expand = c(0, 0)) +
    labs(x="", colour="Group") +
    theme_bw() + theme(text = element_text(size=22),
                       axis.text=element_text(size=18),
                       axis.line.y=element_blank(),
                       axis.text.y=element_blank(),
                       axis.ticks.y=element_blank(),
                       axis.title.y=element_blank(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       plot.margin = margin(t=3, r=2, b=0, l=0),
                       legend.position = "null"
                       )
  return(inf_plot)
}

# a GGPLOT function to combine the finite and infinite time plots
ggplot_t_finiteANDoutcome <- function(sim_df, steady_state)
  wrap_plots(ggplot_t_finite(sim_df),
             ggplot_t_inf(sim_df, steady_state),
             ncol = 2, widths = c(3,1))


# # a function to plot the outcome of the community after infinite time
# plot_t_inf <- function(sim_df, steady_state){
#   # set up an empty plot
#   plot(x=1, y=1,
#        type = "n",
#        xlim = c(-1,1),
#        ylim = c(-0.01, 1.01), yaxs = "i", # remove y-axis padding
#        xlab = "", ylab = "",
#        xaxt="n", yaxt = "n"  # Suppress default axis tick labels
#   )
#   
#   # Add a custom x-axis tick label
#   axis(1, at = 0, labels = "outcome")
#   
#   # if the system has 2 attractors
#   if (steady_state$outcome == "bistable"){
#     # we need to check the initial conditions as compared to the unstable equilibrium point
#     index_stable_pt <- ifelse(sim_df$arbour[1] < steady_state$unstable_pt,
#                               1, 2)
#     # plot the lines for the appropriate stable point
#     abline(h = steady_state$stable_pts[index_stable_pt], lwd = 7, col = colours_species["arbour"])
#     abline(h = 1 - steady_state$stable_pts[index_stable_pt], lwd = 7, col = colours_species["grass"])
#     
#   } else { # if the system has only 1 attractor
#     # we can add the lines directly from the steady state
#     abline(h = steady_state$stable_pts, lwd = 7, col = colours_species["arbour"])
#     abline(h = 1 - steady_state$stable_pts, lwd = 7, col = colours_species["grass"])
#   }
#   
# }
# 
# # a function to combine the finite and infinite time plots
# plot_t_finiteANDoutcome <- function(sim_df, steady_state){
#   
#   # define a new plotting area
#   par(cex=0.7,
#       cex.lab = 2, # increase size of axis labels
#       cex.axis = 1.5, # increase size of tick mark labels 
#       mar = c(3.9, 4.4, 1, 0.1), # decrease the borders
#       fig=c(0,0.8,0,1)) # next plot will appear on the left side (80% width and 100% height)
#   plot_t_finite(sim_df)
#   # add a plot on top of the existing one & define area where to print it
#   par(new=TRUE,
#       mar = c(3.9, 0.1, 1, 0.1), # decrease the borders
#       fig=c(0.81, 1, 0, 1)) # next plot will appear on right side (19% width and 100% height)
#   plot_t_inf(sim_df, steady_state)
#   
#   # record the plot as a variable
#   the.plot <- recordPlot()
#   # reset the device
#   dev.off()
#   
#   return(the.plot)
# }

# a function to plot dA as a function of A
plot_dA_by_A <- function(dA.df, steady_state){
  # change the graphing settings
  par(cex.lab = 2, # increase size of axis labels
      cex.axis = 1.5, # increase size of tick mark labels 
      mar = c(4.0, 4.4, 2.4, 0.85)) # decrease the borders
  
  # when there's an unstable equilibrium point that is off to one side
  if(steady_state$outcome == "bistable"){
    if(!(steady_state$unstable_pt > 0.2 & steady_state$unstable_pt < 0.8)){
      # the y-axis limits of the plot will have to be expanded bc the arrows exceed the range of dA
      # and the shorter arrow will have to be moved
      
      # check if it's the minimum y-value that needs to be expanded
      if(which.min(c(min(dA.df$dA), steady_state$arrow1["y0"])) == 2){
        # expand the limits
        limits_y <- c(2*steady_state$arrow1["y0"], max(dA.df$dA))
        # in this case arrow 1 will have to be changed
        arrow1 <- steady_state$arrow1
        arrow1["x0"] <- steady_state$unstable_pt
        arrow1["x1"] <- 0
        # arrow 2 can remain the same
        arrow2 <- steady_state$arrow2
      }
      
      # check if it's the maximum y-value that needs to be expanded
      if(which.max(c(max(dA.df$dA), steady_state$arrow2["y0"])) == 2){
        # expand the limits
        limits_y <- c(min(dA.df$dA), 2*steady_state$arrow2["y0"])
        # arrow 1 can remain the same
        arrow1 <- steady_state$arrow1
        # in this case arrow 2 will have to be changed
        arrow2 <- steady_state$arrow2
        arrow2["x0"] <- steady_state$unstable_pt
        arrow2["x1"] <- 1
      }
      
      
    # otherwise the system is bistable with the unstable equilibrium near the middle  
    } else {
      limits_y <- range(dA.df$dA)
      arrow1 <- steady_state$arrow1
      arrow2 <- steady_state$arrow2
    }
    
  # otherwise the system has just 1 attractor
  } else {
    limits_y <- range(dA.df$dA)
    arrow1 <- steady_state$arrow1
  }

  
  # plot the points
  plot(x = dA.df$A, y = dA.df$dA,
       pch = 16,
       col = colour_dA, 
       ylab = expression("Change in tree density (" * delta * "T)"),
       xlab = "Tree density (T)",
       ylim = limits_y,
       xlim = c(-0.01, 1.01), xaxs = "i",
       xaxt = "n"  # Suppress default x-axis tick labels
  )
  
  # Add custom x-axis tick labels
  axis(1, at = seq(from=0, to=1, by=0.25), labels = c("0", "", "0.5", "", "1"))
  # Add a dotted vertical line at y = 0
  abline(h = 0, lty = 3, lwd = 0.6, col = "black")
  
  # plot the arrows and the attractors
  if(steady_state$outcome == "A=1"){
        arrows(x0 = arrow1["x0"],
               y0 = arrow1["y0"],
               x1 = arrow1["x1"],
               y1 = arrow1["y1"],
               length=0.2)
        #plot attractor
        points(x=1, y=0, pch=23, bg=substring(colours_species["arbour"], first=1, last=7),
               col="white", cex=1.2, lwd=2)
        # Add the legend for the attractor
        legend("bottomright", "Attractor", pch=18, col=colours_species["arbour"],
               cex=1.6, inset=c(0,0.95), xpd=TRUE, horiz=TRUE, bty="n"
              )
    
  } else if(steady_state$outcome == "A=0"){
        arrows(x0 = arrow1["x0"],
               y0 = arrow1["y0"],
               x1 = arrow1["x1"],
               y1 = arrow1["y1"],
               length=0.2)
        #plot attractor
        points(x=0, y=0, pch=23, bg=substring(colours_species["grass"], first=1, last=7),
               col="white", cex=1.2, lwd=2)
        # Add the legend for the attractor
        legend("bottomright", "Attractor", pch=18, col=colours_species["grass"],
               cex=1.6, inset=c(0,0.95), xpd=TRUE, horiz=TRUE, bty="n"
              )
        
  } else if(steady_state$outcome == "bistable"){
    arrows(x0 = arrow1["x0"],
           y0 = arrow1["y0"],
           x1 = arrow1["x1"],
           y1 = arrow1["y1"],
           length=0.2)
    arrows(x0 = arrow2["x0"],
           y0 = arrow2["y0"],
           x1 = arrow2["x1"],
           y1 = arrow2["y1"],
           length=0.2)
    #plot attractors
    points(x=1, y=0, pch=23, bg=substring(colours_species["arbour"], first=1, last=7),
           col="white", cex=1.2, lwd=2)
    points(x=0, y=0, pch=23, bg=substring(colours_species["grass"], first=1, last=7),
           col="white", cex=1.2, lwd=2)
    # Add a legend with just the point for the grass attractor
    legend("bottomright", "Attractors", text.col="white", pch=18, col=colours_species["grass"],
           cex=1.6, inset=c(0.03,0.95), xpd=TRUE, horiz=TRUE, bty="n"
          )
    # Add the legend for the tree attractor
    legend("bottomright", "Attractors", pch=18, col=colours_species["arbour"],
           cex=1.6, inset=c(0,0.95), xpd=TRUE, horiz=TRUE, bty="n"
          )
  }
  
}


# a GGPLOT function to plot fire over finite time
ggplot_fire_finite <- function(params, fire.df, ylims){
  # get x-axis labels
  tickwidth_x <- floor(max(fire.df$time)/4)
  labs_x <- seq(from=tickwidth_x, to=3*tickwidth_x, by=tickwidth_x)
  
  # create the plot
  finit_plot <- ggplot(fire.df,
                       aes(x = time, y = fire, linewidth = 2.5, colour=colour_fire)) +
    geom_line() +
    scale_x_continuous(limits = c(min(fire.df$time), max(fire.df$time)), expand = c(0, 0),
                       breaks = labs_x,
                       labels = labs_x) +
    scale_y_continuous(limits = ylims + c(-0.01, 0.01), expand = c(0, 0)) +
    labs(x="Time (years)", y="Frequency of Fire") +
    theme_bw() + 
    theme(text = element_text(size=22),
          axis.text=element_text(size=18),
          axis.line.y=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "null",
          plot.margin = margin(t=3, r=2, b=0, l=0)
    )
  
  return(finit_plot)
}

# a GGPLOT function to plot the long-term fire outcome
ggplot_fire_inf <- function(params, sim_df, fire_df, steady_state, ylims){
  
  # if the system has 2 attractors
  if (steady_state$outcome == "bistable"){
    # we need to check the initial conditions as compared to the unstable equilibrium point
    index_stable_pt <- ifelse(sim_df$density[1] < steady_state$unstable_pt,
                              1, 2)
    # use the appropriate stable state value
    inf_df <- data.frame(Value = fire_prob(params, steady_state$stable_pts[index_stable_pt]))
    
    # if the system has only 1 attractor
  } else {
    # we can add the lines directly from the steady state
    inf_df <- data.frame(Value = fire_prob(params, steady_state$stable_pts))
  }
  # create the plot
  inf_plot <- ggplot() +
    geom_hline(data = inf_df, aes(yintercept = Value, color = colour_fire),
               linewidth = 2.5) +
    scale_x_continuous(limits=c(0.5, 1.5), breaks = c(1), labels = "outcome") +
    scale_y_continuous(limits = ylims + c(-0.01, 0.01), expand = c(0, 0)) +
    labs(x="") +
    theme_bw() + theme(text = element_text(size=22),
                       axis.text=element_text(size=18),
                       axis.line.y=element_blank(),
                       axis.text.y=element_blank(),
                       axis.ticks.y=element_blank(),
                       axis.title.y=element_blank(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       plot.margin = margin(t=3, r=2, b=0, l=0),
                       legend.position = "null"
    )
  return(inf_plot)
}

# a GGPLOT function to combine the finite and infinite time plots FIRE
ggplot_fire_finiteANDoutcome <- function(params, sim_df, fire_df, steady_state){
  # get the y limits
  ylims <- c(fire_prob(params, 1),
             fire_prob(params, 0))
  # make the plot
  wrap_plots(ggplot_fire_finite(params, fire_df, ylims),
             ggplot_fire_inf(params, sim_df, fire_df, steady_state, ylims),
             ncol = 2, widths = c(3,1))
}
