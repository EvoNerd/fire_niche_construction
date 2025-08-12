# fire shiny app dependency for app.R
#   tutorial text that appears on the top of the app is defined here
#author: Ana-Hermina Ghenu
#date: 2025-07-01

# use the same colours as used for the bslib theme
html_col1 <- sprintf("<span style='color: %s;'>", colours_species["arbour"])
html_col2 <- sprintf("<span style='color: %s;'>", colour_fire)
html_col3 <- sprintf("<span style='color: %s;'>", substring(colours_species["grass"], first=1, last=7))


# subtitle text (appears underneath the app title)
text_subtitle <- paste0(html_col1, "When the fraction of trees lost per fire is sufficiently high, this creates a system with two attractors where the long-term outcome depends on the initial tree density.</span>")




# Introduction
text_intro <- paste0("Imagine two vegetation groups with different responses to fire, fire-prone grasses and fire-resistant trees.

In a climate with frequent fires (once per decade, fire frequency: $F = 0.09$), you might expect that grasses will always dominate. But this changes when we consider the functional traits of the ecosystem members:

- ",html_col1,"**Trees:**</span> grow slowly (max. 0.1 biomass increase per year) and limit their own growth by $(1 - \\text{tree density})$. Their biomass *decreases* the frequency of fires by $F(1 - \\text{tree density})$. 
<br>The **fraction of trees lost per fire ($L$)** depends on a slider-controlled value.

- ",html_col3,"**Grasses:**</span> grow quickly to fill up any available space $(\\text{grass density}=1-\\text{tree density})$. Their biomass is flammable and *increases* the frequency of fires by $F + \\text{grass density}$.

Here you will explore how the fraction of trees lost per fire event determines whether grasses will dominate, or not.

",html_col2,"*Play with the app using the sliders below or select another topic using the buttons above.*</span>")




# How to
text_howto <- paste0("#### Slider 1. Fraction of trees lost per fire ($L$)

Adjust how susceptible tree biomass is to fire damage.

- $L < 0.092$: little tree biomass is lost. **This is a system with one attractor.**

- $L > 0.092$: a lot of tree biomass is lost. **This is a system with two attractors.**

*",html_col2,"Try sliding $L$ to see how the fire-susceptibility of trees changes the schematic and equation (1. top left).</span>
<br>What other plots or equations change with $L$?*

#### Slider 2. Initial tree density ($T_0$)

Adjust the initial fraction of tree biomass $T_0$ at the start of the simulation.

*",html_col2,"Try sliding $T_0$ to see how the tree density changes over time (4. bottom right).</span>
<br>Do any other plots change with $T_0$?*

#### Slider 3. Number of years

Adjust the number of years to simulate over time.

",html_col2,"*Which parts of the plots on the right change when you adjust the third slider?*</span>")




# What's going on?
text_huh <- paste0("A key equation (1. middle left) shows how the tree density, $T$, changes over time.
<br>We also visualize this in two plots:

- **2. Bottom left:** Rate of change of tree density ($\\delta T$) as a function of current tree density ($T$).
<br>Shows in which direction (arrow) and how fast (y-axis) the tree density is changing.
<br>Coloured diamonds show the stable equilbrium points or **\"attractors\"**. These determine the long-term outcome of the ecosystem.

- **4. Bottom right:** Tree density $T$ over Time.
<br>Shows how the tree density increases, or decreases, each year. The right side shows the long-term **outcome** after many years.

*",html_col2,"Play with the sliders to explore how the fraction of trees lost per fire ($L$) and initial conditions ($T_0$) shape the long-term outcome.</span>
<br>Can trees dominate if they are initially rare? When does this happen?*")




# Fire feedback
text_feedback <- paste0("There is an ecological feedback between the density of trees and the frequency of fires (1. top left plot). Fires occur rarely when tree density is high, but often when tree density is low.

The equation (1. middle left) shows the frequency of fires as a function of tree density. You can visualize it by sliding initial tree density ($T_0$) and seeing how the initial fire frequency at time 0 changes in plot 3 (top right).

",html_col2,"**Key insight:**</span> When trees are rare ($T$ is small), grasses and fires are common. Rare trees can only increase in density if few are lost per fire!")




# Things to try
text_try <- paste0("- ",html_col2,"Set $L$ near 0.05:</span> trees always take over.
<br>This is a system with only one attractor.

- ",html_col2,"Set $L$ above 0.09:</span> trees can spread only when they start from a sufficiently common initial density $T_0$.
<br>This is a system with two attractors.

- ",html_col2,"Vary the initial tree density ($T_0$):</span> *How does the starting point change the long-term outcome?*")




# A glimpse at the code
text_code <- "The differential equation in the app below describes how $T$, the tree density, changes over time. We can write it in math like this:
$$ \\frac{dT}{dt} = 0.1 T (1-T) - L \\cdot T (F + (1-T)) (1-T)$$
We can also code it using the R programming language like this:
```
# ordinary differential equation: a function to get the rate of change over time
ODE <- function(time, init, params){
  # pass the variables as a list
  with (as.list(c(init, params)), {
    # define the differential equation:
    dT_dt = 0.1*T*(1-T) - L*T*(F + (1-T))*(1-T)
    return(list(dT = dT_dt))
  })
}
```"




# Summary
text_summary <- "Even if grasses increase the frequency of fires, they might not dominate. The fate of grasses depends on:

- How susceptible tree biomass is to fire damage ($L$), and

- How common trees are to start with ($T_0$).

When the fraction of trees lost per fire is sufficiently high, ecological feedback creates a system with two attractors where the long-term outcome depends on the initial tree density."