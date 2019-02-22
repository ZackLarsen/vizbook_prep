##################################################
## Project: Dataviz
## Script purpose: Make a custom LOVE plot
## Date: February 22, 2019
## Author: Zack Larsen
##################################################

library(pacman)
p_load(tidyverse, magrittr, rbokeh, plotly, ggthemes, ggbeeswarm, cowplot,
       ggalt, scales, ggcorrplot, ggExtra, RColorBrewer, randomcoloR, MASS,
       d3heatmap, ggridges, r2d3, gridExtra, viridis, ggrepel, lubridate,
       gapminder, superheat, ggthemr)





# example data ------------------------------------------------------------

# First, create random data
psData <- data.frame("variable_name" = c("gender","age","ccs_level","ipa_cnt_bsln"),
                     "adjusted" = c(0.09,0.07,0.16,0.08),
                     "unadjusted" = c(0.14,0.13,0.23,0.24))

psData %>% 
  mutate(difference = unadjusted - adjusted) -> psData

psData











# ggplot2 -----------------------------------------------------------------

#https://rkabacoff.github.io/datavis/Time.html

p1 <- ggplot(psData, 
       aes(y = reorder(variable_name, unadjusted),
           x = unadjusted,
           xend = adjusted)) +  
  geom_dumbbell(size = 0.5,
                size_x = 2.5, 
                size_xend = 2.5,
                colour = "black", 
                colour_x = "blue", 
                colour_xend = "red") +
  geom_vline(aes(xintercept=0.1), colour = "red", size = 1, linetype = "dashed") +
  labs(title = "Imbalance Reduction After Matching",
       subtitle = "Adjusted vs. Unadjusted",
       x = "",
       y = "")

p1


p1 + theme_economist_white()



## After customizing with ggthemeassist
p1 + theme(plot.subtitle = element_text(vjust = 1), 
    plot.caption = element_text(vjust = 1), 
    panel.grid.minor = element_line(colour = "honeydew4", 
        size = 0.25, linetype = "dashed"), plot.title = element_text(family = "serif", 
        size = 15)) +labs(x = NULL, y = NULL, caption = "Image design by Zack Larsen")





# plotly ------------------------------------------------------------------

p <- plot_ly(psData, color = I("gray80")) %>%
  add_segments(x = ~adjusted, xend = ~unadjusted, y = ~variable_name, yend = ~variable_name, showlegend = FALSE) %>%
  add_markers(x = ~adjusted, y = ~variable_name, name = "adjusted", color = I("pink")) %>%
  add_markers(x = ~unadjusted, y = ~variable_name, name = "unadjusted", color = I("blue")) %>%
  layout(
    title = "Covariate balance improvement",
    xaxis = list(title = "Absolute Mean Differences"),
    margin = list(l = 65)
  )
p
