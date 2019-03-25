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
match_data <- data.frame("variable_name" = c("gender",
                                       "age",
                                       "ccs_level",
                                       "total medical net paid",
                                       "professional net paid",
                                       "admission counts",
                                       "professional visits",
                                       "BH total net paid",
                                       "BH professional",
                                       "BH outpatient",
                                       "ALOS"),
                         "adjusted" = c(0.09,0.17,0.29,0.07,0.09,0.06,0.13,0.11,0.08,0.08,0.14),
                         "unadjusted" = c(0.14,0.15,0.37,0.13,0.24,0.39,0.12,0.2,0.18,0.31,0.4))

match_data <- match_data %>% 
  mutate(difference = unadjusted - adjusted) 

match_data





# ggplot2 -----------------------------------------------------------------

#https://rkabacoff.github.io/datavis/Time.html

p1 <- ggplot(match_data, 
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


p1 + theme_economist()



## After customizing with ggthemeassist
p1 + 
  theme(
  axis.line = element_line(size = 2, colour = "grey80"),
  panel.border = element_rect(linetype = "dashed", fill = NA),
  plot.subtitle = element_text(vjust = 1), 
  plot.caption = element_text(vjust = 1), 
  panel.background = element_rect(fill = "white", colour = "grey50"),
  panel.grid.minor = element_line(
    colour = "honeydew4", 
    size = 0.25, 
    linetype = "dashed"
    ), 
  plot.title = element_text(
    family = "serif", 
    size = 15
    )
  ) + 
  labs(x = NULL, y = NULL, caption = "Image design by Zack Larsen")







match_data

ggplot(match_data, 
       aes(y = reorder(variable_name, unadjusted),
           x = unadjusted,
           xend = adjusted)) +  
  geom_dumbbell(size = 1.2,
                size_x = 3, 
                size_xend = 3,
                colour = "grey", 
                colour_x = "red", 
                colour_xend = "blue") +
  geom_vline(aes(xintercept=0.1, color = "red", linetype="dashed")) +
  theme_minimal() + 
  labs(title = "Change in Balance",
       subtitle = "Before and after matching",
       x = "Life Expectancy (years)",
       y = "") +
  theme(legend.position = "none")



























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
