
# This script is for testing out different visualizations on datasets
# that come preloaded with r
# The hope is to include all of these in the R gitbook called 'vizbook'


library(pacman)
p_load(tidyverse, magrittr, rbokeh, plotly, ggthemes, ggbeeswarm, cowplot)


# data --------------------------------------------------------------------

View(diamonds)

View(mpg)

View(mtcars)

View(airquality)

View(midwest)



# Histograms and density plots --------------------------------------------

diamonds$carat
diamonds$depth
diamonds$price


h1 <- ggplot(data = diamonds,
            mapping = aes(x = price))
h1 + geom_histogram(fill = "red", color = "black")



h2 <- ggplot(data = diamonds,
            mapping = aes(x = price, fill = cut))
h2 + geom_histogram(alpha = 0.4, bins = 20)




d1 <- ggplot(data = diamonds,
            mapping = aes(x = price, fill = cut, color = cut))
d1 + geom_density(alpha = 0.3)


# Proportionally scaled density map
d2 <- ggplot(data = diamonds,
             mapping = aes(x = price, fill = cut, color = cut))

d2 + geom_density(alpha = 0.3, mapping = aes(y = ..scaled..))



# Boxplots / violin plots -------------------------------------------------

x1 <- ggplot(data = diamonds,
            mapping = aes(x = cut, y = price)) +
  theme_dark() +
  theme_classic()

x1 + geom_boxplot(fill = "lightpink") + 
  coord_flip()

x1 + geom_tufteboxplot(median.type = "line", 
                      whisker.type = 'line',
                      hoffset = 0, width = 3)



x2 <- ggplot(data = diamonds,
             mapping = aes(x = cut, y = price)) +
  theme_economist() + 
  scale_fill_economist()

x2 + geom_boxplot(fill = "lightpink") + 
  coord_flip()





# Beeswarm ----------------------------------------------------------------


b1 <- ggplot(diamonds, 
             aes(x = cut,
                 y = price)) 

b1 + geom_beeswarm(color = "red", 
                   groupOnX = FALSE) +
  labs(title ="Beeswarm Plot of Cut")




# Scatterplots / bubble charts --------------------------------------------

p <- ggplot(data = diamonds,
            mapping = aes(x = depth, y = price))

p + geom_point()

p + geom_point() +
  annotate(geom = "rect", xmin = 70, xmax = 80,
           ymin = 15000, ymax = 20000, fill = "red", alpha = 0.2) + 
  annotate(geom = "text", x = 70, y = 14000,
           label = "A surprisingly high \n recovery rate.", hjust = 0)







# Correlation / correlograms ----------------------------------------------




# Bar charts --------------------------------------------------------------

b1 <- ggplot(data = ,
            mapping = aes(x = , y = , fill = ))

b1 + geom_bar(position = "dodge", stat = "identity") + 
  theme(legend.position = "top")



# cumulative empirical distribution ---------------------------------------






# ridgeline plots ---------------------------------------------------------




# qq-plots ----------------------------------------------------------------










# Line charts -------------------------------------------------------------




# Trendlines / smoothing --------------------------------------------------






# Heatmaps ----------------------------------------------------------------




# Density / hexbins -------------------------------------------------------






# Treemaps ----------------------------------------------------------------





# Geographic plots --------------------------------------------------------







# Network graphs ----------------------------------------------------------






# Love (dot) plots --------------------------------------------------------




# Parallel coordinates ----------------------------------------------------





# Sankey diagrams ---------------------------------------------------------





# Maps --------------------------------------------------------------------





# Choropleths -------------------------------------------------------------




# Cartograms --------------------------------------------------------------




