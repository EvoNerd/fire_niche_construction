library(ggplot2)
library(ggarrow)

# default arrow & line segment thickness
thicc <- 0.7
# default white space btw node & line segment
whitespace <- 0.2
# default length of arrowheads & inhibition
arrowhead_len <- 0.3*2

# TO DO:
# THESE VALUES WILL COME FROM THE SLIDERS
muA_scale <- 0.5
epsilon_scale <- 0.5
# TO DO: RENAME "rect_coords"

# define the width and height of the filled rectangles
rect_HALFwidth <- 4.2/2
rect_HALFheight <- 0.7/2
# define the width and height of the white rectangles
white_FIREwidth <- 2/2
white_CLIMATEwidth <- 3/2
white_HALFheight <- 0.3/2

# Create a data.frame to define the rectangles and text
fire_rect_coords <- data.frame(
  rect_fill = c("yellow", "forestgreen", "lightgrey", "lightgrey"),
  x_center = c(2, 2, 4, 7), # 3 evenly spaced columns
  y_center = c(1, 5, 3, 4) # fire in middle of trees & grass. Climate in middle of trees & fire.
)
# name the rows by the "compartments"
row.names(fire_rect_coords) = c("Grasses", "Trees", "Fire", "Climate")
# use the center points and rectangle dimensions to get edge coordinates
fire_rect_coords$left  <-  fire_rect_coords$x_center - c(rep(rect_HALFwidth, 2), white_FIREwidth, white_CLIMATEwidth)
fire_rect_coords$right  <- fire_rect_coords$x_center + c(rep(rect_HALFwidth, 2), white_FIREwidth, white_CLIMATEwidth)
fire_rect_coords$bottom <- fire_rect_coords$y_center - rep(c(rect_HALFheight, white_HALFheight), each=2)
fire_rect_coords$top  <-   fire_rect_coords$y_center + rep(c(rect_HALFheight, white_HALFheight), each=2)

# TO DO: ADD THE FIRE IMAGE ON THE LOWEST LAYER!

# begin making the plot. Put the rectangles on the lowest layer
schematic <- ggplot() +
  # Add rectangles
  geom_rect(data = fire_rect_coords,
            aes(xmin = left, xmax = right, ymin = bottom, ymax = top), 
            fill = fire_rect_coords$rect_fill, color = rep(c("black", "white"), each=2),
            linewidth=thicc/5) +
  # Add text labels
  geom_text(data = fire_rect_coords,
            aes(x = x_center, y = y_center, label = row.names(fire_rect_coords)), 
            size = 6)

# add the arrows
schematic <- schematic +
  # arrow for Trees ---| Grasses
  geom_arrow(aes(x = fire_rect_coords["Trees", "x_center"],
                 xend = fire_rect_coords["Trees", "x_center"],
                 y = fire_rect_coords["Trees", "bottom"] - whitespace,
                 yend = fire_rect_coords["Grasses", "top"] + whitespace),
                # weight of arrow is fixed
                linewidth = thicc) #+
  # geom_segment(aes(x = fire_rect_coords["Trees", "x_center"] - arrowhead_len,
  #                  xend = fire_rect_coords["Trees", "x_center"] + arrowhead_len,
  #                  y = fire_rect_coords["Grasses", "top"] + whitespace,
  #                  yend = fire_rect_coords["Grasses", "top"] + whitespace),
  #              # weight of arrow is fixed
  #              linewidth = thicc) +
  
  # # arrow for Climate ---> Fire
  # geom_segment(aes(x = fire_rect_coords["Climate", "left"] - whitespace,
  #                  xend = fire_rect_coords["Fire", "right"] + whitespace,
  #                  y = fire_rect_coords["Climate", "bottom"],
  #                  yend = fire_rect_coords["Fire", "top"]),
  #              lineend = "butt", linejoin = "mitre",
  #              arrow = arrow(length = unit(arrowhead_len/3, "npc")),
  #              # weight of arrow is fixed
  #              linewidth = thicc) +
  # 
  # # arrow for Grasses ---> Fire
  # geom_curve(aes(x = fire_rect_coords["Grasses", "right"] - whitespace,
  #                xend = fire_rect_coords["Fire", "left"],
  #                y = fire_rect_coords["Grasses", "y_center"],
  #                yend = fire_rect_coords["Fire", "bottom"] - whitespace),
  #           curvature = 0.2,
  #           lineend = "butt",
  #           arrow = arrow(length = unit(arrowhead_len/3, "npc")),
  #           # weight of arrow is fixed
  #           linewidth = thicc) +
  # 
  # # arrow for Trees ---| Fire
  # geom_curve(aes(x = fire_rect_coords["Trees", "right"] - whitespace,
  #                xend = fire_rect_coords["Fire", "left"],
  #                y = fire_rect_coords["Trees", "y_center"],
  #                yend = fire_rect_coords["Fire", "top"] - whitespace),
  #            curvature = -0.2,
  #            # weight of arrow is fixed
  #            linewidth = thicc) +
  # geom_segment(aes(x = fire_rect_coords["Fire", "left"] - arrowhead_len/2,
  #                  xend = fire_rect_coords["Fire", "left"] + arrowhead_len/2,
  #                  y = fire_rect_coords["Fire", "top"] - whitespace  - arrowhead_len/2,
  #                  yend = fire_rect_coords["Fire", "top"] + whitespace  + arrowhead_len/2),
  #              # weight of arrow is fixed
  #              linewidth = thicc)


# finally set limits and theme
schematic <- schematic +
              xlim(min(fire_rect_coords$left) - whitespace, max(fire_rect_coords$right) + whitespace) +
              ylim(min(fire_rect_coords$bottom) - whitespace, max(fire_rect_coords$top) + whitespace) +
              theme_void()
