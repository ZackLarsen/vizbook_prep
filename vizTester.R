
# This script is for testing out different visualizations on datasets
# that come preloaded with r
# The hope is to include all of these in the R gitbook called 'vizbook'

library(pacman)
p_load(tidyverse, magrittr, rbokeh, plotly, ggthemes, ggbeeswarm, cowplot,
       ggalt, scales, ggcorrplot, ggExtra)


# data --------------------------------------------------------------------

View(diamonds)

View(mpg)

View(mtcars)

View(airquality)

View(midwest)




# Resources (links) -------------------------------------------------------

#http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html#Dot%20Plot



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





p <- ggplot(diamonds, aes(depth, fill = cut(depth, 100))) +
  geom_histogram(show.legend = FALSE) +
  theme_minimal() +
  labs(x = "Price", y = "n") +
  ggtitle("Histogram of diamond depth")

p + scale_fill_discrete(h = c(180, 360), c = 150, l = 80)







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







# Simulate data
set.seed(170513)
n <- 2000
d <- data.frame(a = rnorm(n))
d$b <- -(d$a + rnorm(n, sd = 2))
# Add first principal component
d$pc <- predict(prcomp(~a+b, d))[,1]
# Add density for each point
d$density <- fields::interp.surface(
  MASS::kde2d(d$a, d$b), d[,c("a", "b")])
# Plot
ggplot(d, aes(a, b, color = pc, alpha = 1/density)) +
  geom_point(shape = 16, size = 5, show.legend = FALSE) +
  theme_minimal() +
  scale_color_gradient(low = "#32aeff", high = "#f2aeff") +
  scale_alpha(range = c(.25, .6))



# Correlation / correlograms ----------------------------------------------

# Correlation matrix
data(mtcars)
corr <- round(cor(mtcars), 1)
# Plot
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of mtcars", 
           ggtheme=theme_bw)


# Bar charts --------------------------------------------------------------

b1 <- ggplot(data = ,
            mapping = aes(x = , y = , fill = ))

b1 + geom_bar(position = "dodge", stat = "identity") + 
  theme(legend.position = "top")




theme_set(theme_bw())  
# Data Prep
data("mtcars")  # load data
mtcars$`car name` <- rownames(mtcars)  # create new column for car names
mtcars$mpg_z <- round((mtcars$mpg - mean(mtcars$mpg))/sd(mtcars$mpg), 2)  # compute normalized mpg
mtcars$mpg_type <- ifelse(mtcars$mpg_z < 0, "below", "above")  # above / below avg flag
mtcars <- mtcars[order(mtcars$mpg_z), ]  # sort
mtcars$`car name` <- factor(mtcars$`car name`, levels = mtcars$`car name`)  # convert to factor to retain sorted order in plot.
# Diverging Barcharts
ggplot(mtcars, aes(x=`car name`, y=mpg_z, label=mpg_z)) + 
  geom_bar(stat='identity', aes(fill=mpg_type), width=.5)  +
  scale_fill_manual(name="Mileage", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="Normalised mileage from 'mtcars'", 
       title= "Diverging Bars") + 
  coord_flip()

# cumulative empirical distribution ---------------------------------------






# ridgeline plots ---------------------------------------------------------




# qq-plots ----------------------------------------------------------------










# Line charts -------------------------------------------------------------




# Trendlines / smoothing --------------------------------------------------






# Heatmaps ----------------------------------------------------------------


# Calendar heatmaps


# Density / hexbins -------------------------------------------------------






# Treemaps ----------------------------------------------------------------


p_load(treemapify)
proglangs <- read.csv("https://raw.githubusercontent.com/selva86/datasets/master/proglanguages.csv")

# plot
treeMapCoordinates <- treemapify(proglangs,
                                 area = "value",
                                 fill = "parent",
                                 label = "id",
                                 subgroup = "parent")

treeMapPlot <- ggplot(treeMapCoordinates) + 
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_brewer(palette = "Dark2")

print(treeMapPlot)



# Geographic plots --------------------------------------------------------







# Network graphs ----------------------------------------------------------






# Love (dot) plots --------------------------------------------------------




# Dumbbell plots ----------------------------------------------------------
theme_set(theme_classic())

health <- read.csv("https://raw.githubusercontent.com/selva86/datasets/master/health.csv")
health$Area <- factor(health$Area, levels=as.character(health$Area))  # for right ordering of the dumbells

health

# health$Area <- factor(health$Area)
gg <- ggplot(health, aes(x=pct_2013, xend=pct_2014, y=Area, group=Area)) + 
  geom_dumbbell(color="#a3c4dc", 
                size=0.75, 
                point.colour.l="#0e668b") + 
  scale_x_continuous() + 
  labs(x=NULL, 
       y=NULL, 
       title="Dumbbell Chart", 
       subtitle="Pct Change: 2013 vs 2014", 
       caption="Source: https://github.com/hrbrmstr/ggalt") +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())
plot(gg)

# Parallel coordinates ----------------------------------------------------





# Sankey diagrams ---------------------------------------------------------





# Maps --------------------------------------------------------------------





# Choropleths -------------------------------------------------------------




# Cartograms --------------------------------------------------------------





# Marginal histogram/boxplot ----------------------------------------------

data(mpg, package="ggplot2")
# Scatterplot
theme_set(theme_bw())  # pre-set the bw theme.
mpg_select <- mpg[mpg$hwy >= 35 & mpg$cty > 27, ]
g <- ggplot(mpg, aes(cty, hwy)) + 
  geom_count() + 
  geom_smooth(method="lm", se=F)

ggMarginal(g, type = "histogram", fill="transparent")
ggMarginal(g, type = "boxplot", fill="transparent")
ggMarginal(g, type = "density", fill="transparent")

# Seasonal plot -----------------------------------------------------------

p_load(forecast)
theme_set(theme_classic())
# Plot
ggseasonplot(AirPassengers) + labs(title="Seasonal plot: International Airline Passengers")
