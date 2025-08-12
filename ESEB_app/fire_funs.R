# fire shiny app dependency for app.R
#   simulation and plotting functions are defined here
#author: Ana-Hermina Ghenu
#date: 2025-07-01

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
generate_circle <- function(center = c(0, 0),
                            radius = 1,
                            start.end = c(0, 2), # in radians!
                            npoints = 50) {
  theta <- seq(start.end[1] * pi, start.end[2] * pi, length.out = npoints)
  x <- center[1] + radius * cos(theta)
  y <- center[2] + radius * sin(theta)
  data.frame(x = x, y = y)
}

# a function to resize circle data.frame by a factor of x in the y (left or right) direction
resize_circle <- function(circle_df, resize, direction="right") {
  if (direction == "right"){
    x_start <- min(circle_df$x)
    new_x <- resize * (circle_df$x - x_start) + x_start
    return(data.frame(x=new_x, y=circle_df$y))
    
  } else if (direction == "left") {
    x_start <- max(circle_df$x)
    new_x <- resize * (circle_df$x - x_start) + x_start
    return(data.frame(x=new_x, y=circle_df$y))
  }
}

# plot the bottom layers of the schematic
  # this gets called one time at the very beginning
plot_schem_static <- function(){
  # default white space btw node & line segment
  whitespace <- 0.2
  # default arrow & line segment thickness
  thicc <- 0.7
  
  # define the width and height of the filled rectangles
  rect_HALFwidth <- 2.5
  rect_HALFheight <- 0.6
  # define the width and height of the white rectangles
  white_FIREwidth <- 0.95
  white_CLIMATEwidth <- 2
  white_HALFheight <- 0.5
  
  # fire
  fire_emoji <- rasterGrob(readPNG("fire_emoji.png"), width = unit(1, "npc"), height = unit(1, "npc"))
  
  # Create a data.frame to define the rectangles and text
  rect_coords <- data.frame(
    rect_fill = c(colours_species, rep("white", 2)),
    x_center = c(1, 1, 6.8, 11.1), # 3 unevenly spaced columns
    y_center = c(4.5, 1, 2.75, 4.6-rect_HALFheight) # fire in middle of trees & grass. Climate at bottom edge of trees.
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
                annotation_custom(fire_emoji, xmin = 5.6, xmax = 8.05, ymin = 2, ymax = 5)
  
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
              size = 9) +
    
    # add circle arrow for trees --| trees
    geom_arrow(data = generate_circle(center = c(rect_coords["Trees", "left"],
                                                 rect_coords["Trees", "bottom"] - whitespace),
                                      radius = rect_HALFheight*1,
                                      start.end = c(0.55, 2.01)),
               aes(x=x, y=y),
               arrow_head = arrow_head_line(angle = 90),
               length_head = unit(4, "mm"),
               linewidth = thicc) +
    
    # add straight arrow for trees --| grasses
    geom_arrow(data = data.frame(x = c(rect_coords["Trees", "x_center"], rect_coords["Grasses", "x_center"]),
                                 y = c(rect_coords["Trees", "bottom"] - whitespace, rect_coords["Grasses", "top"] + whitespace)),
               aes(x=x, y=y),
               arrow_head = arrow_head_line(angle = 90),
               length_head = unit(4, "mm"),
               linewidth = thicc) +
    
    # add circle arrow for grasses --> fire
    geom_arrow(data = resize_circle(generate_circle(center = c(rect_coords["Grasses", "right"] + whitespace,
                                                 rect_coords["Fire", "bottom"] - rect_HALFheight/4),
                                      radius = 1.2,
                                      start.end = c(1.5, 2.03)),
                                    resize = 1.9,
                                    direction = "right"),
               aes(x=x, y=y),
               arrow_head = arrow_head_wings(offset=45, inset=33),
               length_head = unit(3, "mm"),
               linewidth = thicc) +
    
    # add circle arrow for trees --| fire
    geom_arrow(data = resize_circle(generate_circle(center = c(rect_coords["Grasses", "right"] + whitespace,
                                                               rect_coords["Fire", "top"] + rect_HALFheight/4),
                                                    radius = 1.2,
                                                    start.end = c(0.5, 0.15)),
                                    resize = 1.9,
                                    direction = "right"),
               aes(x=x, y=y),
               arrow_head = arrow_head_line(angle = 90),
               length_head = unit(3, "mm"),
               linewidth = thicc) +
    
    # Add a text label for the L variable
    annotate(
      "text", label = list('italic("L")'), parse = TRUE,
      x = 4.1, y = 2.5, size = 8, colour = colour_fire
    ) # +
    
    # # Add a text label for the F variable
    # annotate(
    #   "text", label = list('italic("F")'), parse = TRUE,
    #   x = 10, y = 2.5, size = 8, colour = colour_fire
    # )
  
  # finally set limits and theme
  schematic <- schematic +
               scale_x_continuous(limits = c(min(rect_coords$left)-0.59, max(rect_coords$right))) +
               scale_y_continuous(limits = c(min(rect_coords$bottom), max(rect_coords$top))) +
               theme_void() +
               coord_fixed()
  
  return(schematic)
}

# add just the remaining arrows to the schematic
plot_schem_arrows <- function(params){
  # rescale the parameter values
  scaling_L <- params["mu_A"] * 16
  scaling_F <- params["epsilon"] / (0.09)
  
  # epsilon arrow: straight arrow for climate --> fire
  F_arrow <- geom_arrow(data = data.frame(x = c(11.1, # rect_coords["Climate", "x_center"]
                                                7.95), # rect_coords["Fire", "right"] + whitespace
                                          y = c(3.4, # rect_coords["Climate", "bottom"] - whitespace/2
                                                2.75) # rect_coords["Fire", "y_center"]
                                          ),
                        aes(x=x, y=y),
                        arrow_head = arrow_head_wings(offset=45, inset=33),
                        length_head = unit(2 + scaling_F, "mm"),
                        linewidth = 0.7 * scaling_F) # thicc * scaling_F
  
  # muA arrow: circle arrow for fire --| trees
  L_arrow <- geom_arrow(data = resize_circle(
                                generate_circle(
                                  center = c(5.65, # rect_coords["Fire", "left"] - whitespace
                                             3.9), # rect_coords["Trees", "bottom"] 
                                  radius = 1.2,
                                  start.end = c(1.5, 1.08)
                                  ),
                                resize = 1.9,
                                direction = "left"),
             aes(x=x, y=y),
             arrow_head = arrow_head_line(angle = 90),
             length_head = unit(4 * scaling_L, "mm"),
             linewidth = 0.7 * scaling_L) # thicc * scaling_L
  
  return(list(F_arrow = F_arrow,
              L_arrow = L_arrow))
}

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
                  labs(x="Time (years)", y="Density") +
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
    scale_x_continuous(limits=c(0.5, 1.5), breaks = c(1), labels = "long-\nterm") +
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
       ylab = expression("Change in Trees (" * delta * "T)"),
       xlab = "Tree density (T)",
       ylim = limits_y,
       xlim = c(-0.01, 1.01), xaxs = "i", # set a fixed padding for x axis
       xaxt = "n", yaxt = "n"  # Suppress default tick labels for both x and y axes
  )
  
  # Add custom x-axis tick labels
  axis(1, at = seq(from=0, to=1, by=0.25), labels = c("0", "", "0.5", "", "1"))
  
  # custom y-axis tick labels
    # find which side of plot is furthest from 0
  if (which.max(abs(range(dA.df$dA))) == 1){ # bottom side furthest from 0
    # add tick label on bottom and at 0
    axis(2, at = c(min(dA.df$dA), 0),
         labels = signif(c(min(dA.df$dA), 0), digits = 1))
    
    # top side furthest from 0
  } else { 
    # add tick label at 0 and top
    axis(2, at = c(0, max(dA.df$dA)),
         labels = signif(c(0, max(dA.df$dA)), digits = 1))
  }
    
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
  
  # get y-axis labels
  ticks_y <- seq(from = ylims[1], to = signif(ylims[2], digits = 2), length.out = 5)
  
  # create the plot
  finit_plot <- ggplot(fire.df,
                       aes(x = time, y = fire, linewidth = 2.5)) +
    geom_line(colour = colour_fire) +
    scale_x_continuous(limits = c(min(fire.df$time), max(fire.df$time)), expand = c(0, 0),
                       breaks = labs_x,
                       labels = labs_x) +
    scale_y_continuous(limits = ylims + c(-0.01, 0.01), expand = c(0, 0),
                       breaks = ticks_y,
                       labels = c(ticks_y[1], "", ticks_y[3], "", ticks_y[5])) +
    labs(x="Time (years)", y="Fire Frequency") +
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
    geom_hline(data = inf_df, aes(yintercept = Value), color = colour_fire,
               linewidth = 2.5) +
    scale_x_continuous(limits=c(0.5, 1.5), breaks = c(1), labels = "long-\nterm") +
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
