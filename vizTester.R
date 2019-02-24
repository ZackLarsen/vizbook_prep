
##################################################
## Project: Vizbook_prep
## Script purpose: Use R datasets for dataviz
## Date: February 9, 2019
## Author: Zack Larsen
##################################################

library(pacman)
p_load(tidyverse, magrittr, rbokeh, plotly, ggthemes, ggbeeswarm, cowplot,
       ggalt, scales, ggcorrplot, ggExtra, RColorBrewer, randomcoloR, MASS,
       d3heatmap, ggridges, r2d3, gridExtra, viridis, ggrepel, lubridate,
       gapminder, superheat, ggthemr, networkD3)

R.Version()

# Remember: cufflinks is a great binding for pandas and plotly,
# and yellowbrick road is great for pandas and scikit-learn
# http://www.scikit-yb.org/en/latest/

# data --------------------------------------------------------------------

View(diamonds)

View(mpg)

View(mtcars)

View(airquality)

View(midwest)




# Resources (links) -------------------------------------------------------

#http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html#Dot%20Plot
# http://www.sthda.com/english/wiki/be-awesome-in-ggplot2-a-practical-guide-to-be-highly-effective-r-software-and-data-visualization
# https://uc-r.github.io/lollipop

# Histograms and density plots --------------------------------------------

# stat_bin() is suitable only for continuous x data. If your x data is discrete, you probably want to use stat_count()

diamonds$carat
diamonds$depth
diamonds$price



h1 <- ggplot(data = diamonds,
            mapping = aes(x = price))

h1 + geom_histogram(fill = "black", color = "red")
h1 + geom_histogram(fill = "blue", color = "black", binwidth = 500)
h1 + geom_histogram(fill = "red", color = "black", binwidth = 250)
h1 + geom_histogram(fill = "pink", color = "black", bins = 10)

h1 + geom_histogram(fill = "pink", color = "black", bins = 30) +
  labs(title="Pink histogram", y="Count of points in bin", x="Diamond price",
       caption="Data source: diamonds") + 
  theme(plot.title=element_text(size=30, 
                              face="bold", 
                              family="American Typewriter",
                              color="grey",
                              hjust=0.5,
                              lineheight=1.5),  # title
      plot.subtitle=element_text(size=15, 
                                 family="American Typewriter",
                                 face="bold",
                                 hjust=0.5),  # subtitle
      plot.caption=element_text(size=10),  # caption
      axis.title.x=element_text(vjust=-2,  
                                size=15),  # X axis title
      axis.title.y=element_text(size=15),  # Y axis title
      axis.text.x=element_text(size=10, 
                               angle=30,
                               vjust=0.5),  # X axis text
      axis.text.y=element_text(size=10))  # Y axis text


# To make it easier to compare distributions with very different counts,
# put density on the y axis instead of the default count
ggplot(diamonds, aes(price, stat(density), colour = cut)) +
  geom_freqpoly(binwidth = 500)



h2 <- ggplot(data = diamonds,
            mapping = aes(x = price, fill = cut))

h2 + geom_histogram(alpha = 0.4, bins = 20)
h2 + geom_histogram(alpha = 0.6, bins = 40)
h2 + geom_histogram(alpha = 0.8, bins = 20)




h3 <- ggplot(diamonds, aes(price, fill = cut(price, 100))) +
  geom_histogram(show.legend = FALSE, bins = 50) +
  theme_minimal() +
  labs(x = "Price", y = "n")

h3 + scale_fill_discrete(h = c(180, 360), c = 150, l = 80)

h3 + scale_fill_discrete(h = c(180, 360), c = 150, l = 80) +
  labs(title="Rad histogram") +
  theme(plot.background=element_rect(fill="salmon"), 
        plot.margin = unit(c(1, 2, 1, 1), "cm"),
        plot.title=element_text(size=30, 
                        face="bold", 
                        family="American Typewriter",
                        color="cyan",
                        hjust=0.5,
                        lineheight=1.5))


# Faceting with CUSTOM binwidth function
ggplot(data = diamonds, aes(x = price)) +
  geom_histogram(binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3))) +
  facet_wrap(~cut)
# Using density instead of count for a more even comparison across facets:
ggplot(data = diamonds, aes(x = price, stat(density))) +
  geom_histogram(fill = "tomato", color = "black",
                 binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3))) +
  facet_wrap(~cut)










# Bokeh histogram
# 600 X 400 is a good figure to start with for the Viewer, but you
# might want more than that if you are going to zoom because it will
# not automatically adjust like ggplot
figure(width = 600, height = 400, legend_location = "top_right") %>%
  ly_hist(price, data = diamonds, breaks = 40, freq = FALSE) %>%
  ly_density(price, data = diamonds)

figure(width = 600, height = 400, legend_location = "top_right") %>%
  ly_hist(price, data = diamonds, breaks = 40, freq = FALSE, hover = (price)) %>%
  ly_density(price, data = diamonds)







# Plotly histogram

plot_ly(x = ~diamonds$price,
        type = "histogram",
        histnorm = "probability",
        alpha = 0.5)










# Density -----------------------------------------------------------------

d1 <- ggplot(data = diamonds,
            mapping = aes(x = price, fill = cut, color = cut))
d1 + geom_density(alpha = 0.3)


# Proportionally scaled density map
d2 <- ggplot(data = diamonds,
             mapping = aes(x = price, fill = cut, color = cut))

d2 + geom_density(alpha = 0.3, mapping = aes(y = ..scaled..))






# rbokeh
figure(width = 600, height = 400, legend_location = "top_right") %>%
#  ly_hist(price, data = diamonds, breaks = 40, freq = FALSE, hover = (price)) %>%
  ly_density(price, data = diamonds, color = "red")






# Boxplots -------------------------------------------------

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





# rbokeh

figure(ylab = "Height (inches)", width = 600) %>%
  ly_boxplot(voice.part, height, data = lattice::singer)

figure(data = lattice::singer) %>%
  ly_points(catjitter(voice.part), jitter(height), color = "black") %>%
  ly_boxplot(voice.part, height, with_outliers = FALSE)





# plotly
p <- plot_ly(midwest, x = ~percollege, color = ~state, type = "box")
p

p <- plot_ly(y = ~rnorm(50), type = "box") %>%
  add_trace(y = ~rnorm(50, 1))
p

p <- plot_ly(x = ~rnorm(50), type = "box") %>%
  add_trace(x = ~rnorm(50, 1))
p



p <- plot_ly(ggplot2::diamonds, y = ~price, color = ~cut, type = "box")
p





# Crossbar ----------------------------------------------------------------

# violin plots ------------------------------------------------------------

p <- ggplot(mtcars, aes(factor(cyl), mpg))
p + geom_violin(fill = "grey80", colour = "#3366FF")
p + geom_violin() + geom_jitter(height = 0, width = 0.1)

p + geom_violin(aes(fill = cyl))




data(Boston)
dt.long <- gather(Boston, "variable",
                  "value", crim:medv)

col <- colorRampPalette(c("red", "blue"))(14)
# col.bp <- brewer.pal(9, "Set1") # brewer.pal only has a max of 9 colors
col.rc <- as.vector(distinctColorPalette(14))

ggplot(dt.long,aes(factor(variable), value))+
  geom_violin(aes(fill=factor(variable)))+
  geom_boxplot(alpha=0.3, color="black", width=.1)+
  labs(x = "", y = "")+
  theme_bw()+
  theme(legend.title = element_blank())+
  facet_wrap(~variable, scales="free")















# rbokeh













# plotly
df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/violin_data.csv")

p <- df %>%
  plot_ly(
    x = ~day,
    y = ~total_bill,
    split = ~day,
    type = 'violin',
    box = list(
      visible = T
    ),
    meanline = list(
      visible = T
    )
  ) %>% 
  layout(
    xaxis = list(
      title = "Day"
    ),
    yaxis = list(
      title = "Total Bill",
      zeroline = F
    )
  )

p




# Beeswarm ----------------------------------------------------------------

ggplot(iris,aes(Species, Sepal.Length)) + geom_beeswarm()





# rbokeh




# plotly




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




# rbokeh
figure() %>%
  ly_points(Sepal.Length, Sepal.Width, data = iris, color = Species)




idx <- split(1:150, iris$Species)
figs <- lapply(idx, function(x) {
  figure(width = 300, height = 300) %>%
    ly_points(Sepal.Length, Sepal.Width, data = iris[x, ],
              hover = list(Sepal.Length, Sepal.Width))
})

# 1 row, 3 columns
grid_plot(figs)












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

b1 <- ggplot(data = diamonds,
            mapping = aes(x = cut, y = price, fill = color))

b2 <- b1 + geom_bar(position = "dodge", stat = "identity") + 
  theme(legend.position = "top")

# Make the above ggplot graph interactive with simple call to ggplotly():
ggplotly(b2)





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





# rbokeh

# total yield per variety
figure() %>%
  ly_bar(variety, yield, data = lattice::barley, hover = TRUE) %>%
  theme_axis("x", major_label_orientation = 90)






# plotly

p <- ggplot(data = diamonds, aes(x = cut, fill = clarity)) +
  geom_bar(position = "dodge")
ggplotly(p)






# PCA ---------------------------------------------------------------------


pcaCars <- princomp(mtcars, cor = TRUE)

# proportion of variance explained
summary(pcaCars)

# scree plot
plot(pcaCars, type = "l")

# cluster cars
carsHC <- hclust(dist(pcaCars$scores), method = "ward.D2")

# dendrogram
plot(carsHC)

# cut the dendrogram into 3 clusters
carsClusters <- cutree(carsHC, k = 3)

# add cluster to data frame of scores
carsDf <- data.frame(pcaCars$scores, "cluster" = factor(carsClusters))
carsDf <- transform(carsDf, cluster_name = paste("Cluster",carsClusters))




# ggplot2
p1 <- ggplot(carsDf,aes(x=Comp.1, y=Comp.2)) +
  theme_classic() +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_vline(xintercept = 0, color = "gray70") +
  geom_point(aes(color = cluster), alpha = 0.55, size = 3) +
  xlab("PC1") +
  ylab("PC2") + 
  xlim(-5, 6) + 
  ggtitle("PCA Clusters from Hierarchical Clustering of Cars Data") 

p1 + geom_text(aes(y = Comp.2 + 0.25, label = rownames(carsDf)))


# ggplot2 with ggrepel
p1 + geom_text_repel(aes(y = Comp.2 + 0.25, label = rownames(carsDf))) 







# plotly

p <- plot_ly(carsDf, x = carsDf$Comp.1 , y = carsDf$Comp.2, 
             text = rownames(carsDf),
             mode = "markers", 
             color = carsDf$cluster_name, 
             marker = list(size = 11)) 

p <- layout(p, title = "PCA Clusters from Hierarchical Clustering of Cars Data", 
            xaxis = list(title = "PC 1"),
            yaxis = list(title = "PC 2"))

p



# Proportional bar chart --------------------------------------------------



# rbokeh
# proportional bars
figure() %>%
  ly_bar(variety, yield, color = year,
         data = lattice::barley, position = "fill", width = 1) %>%
  theme_axis("x", major_label_orientation = 90) %>%
  set_palette(discrete_color = pal_color(c("red", "blue")))
# swap axes and use different palette
figure() %>%
  ly_bar(yield, variety, color = year,
         data = lattice::barley, position = "fill") %>%
  set_palette(discrete_color = pal_color(c("red", "blue")))


# cumulative empirical distribution ---------------------------------------






# ridgeline plots ---------------------------------------------------------

# https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html

ggplot(lincoln_weather, aes(x = `Mean Temperature [F]`, y = `Month`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Temperatures in Lincoln NE in 2016')

ggplot(iris, aes(x=Sepal.Length, y=Species, fill=factor(..quantile..))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = c(0.025, 0.975)) +
  scale_fill_manual(
    name = "Probability", values = c("#FF0000A0", "#A0A0A0A0", "#0000FFA0"),
    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")
  )

ggplot(iris, aes(x=Sepal.Length, y=Species, fill=factor(..quantile..))) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")

ggplot(diamonds, aes(x = price, y = cut, fill = cut)) + 
  geom_density_ridges(scale = 4) + 
  scale_fill_cyclical(values = c("blue", "green"))


ggplot(iris, aes(x = Sepal.Length, y = Species)) + geom_density_ridges(scale = 0.9)
ggplot(iris, aes(x = Sepal.Length, y = Species)) + geom_density_ridges(scale = 1)
ggplot(iris, aes(x = Sepal.Length, y = Species)) + geom_density_ridges(scale = 5)


ggplot(iris, aes(x = Sepal.Length, y = Species, height = ..density..)) + 
  geom_density_ridges(stat = "binline", bins = 20, scale = 0.95, draw_baseline = FALSE)


ggplot(iris, aes(x = Sepal.Length, y = Species)) + geom_density_ridges() + theme_ridges()

ggplot(iris, aes(x = Sepal.Length, y = Species, fill = Species)) + 
  geom_density_ridges(alpha=0.6, bandwidth=4) + 
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ridges(center_axis_labels = TRUE) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0))




# qq-plots ----------------------------------------------------------------










# Line charts -------------------------------------------------------------




# Trendlines / smoothing --------------------------------------------------






# Heatmaps ----------------------------------------------------------------

# Generate random date ranges:
start_date <- as.Date("2014/1/1")
end_date <- as.Date("2014/12/31")

seq(start_date, end_date, "days")
sample(seq(start_date, end_date, by="day"), 6)

seq(start_date, start_date + 6, "days")

# Make a vector of 100 start dates:
start_dates <- sample(seq(start_date, end_date, by="day"), 100)
start_dates

# Make a vector of 100 end dates:
end_dates <- sample(seq(start_date, end_date, by="day"), 100)
end_dates

# Make a data frame with 2 columns: id, time period / interval:
people <- 1:100

df <- data.frame(people)
df

df$start <- start_dates
df

# Add random number of days to the start dates:
ndays <- sample.int(100, 100)
ndays

df$end <-df$start + ndays
df

glimpse(df)


# Truncate end dates
df %>% 
  filter(end > as.Date("2014-12-31"))
# NOTE: we are using dplyr's if_else instead of base's ifelse() because
# it gives a less surprising return type, and preserves S3 vectors like dates"
df$end <- if_else(df$end > as.Date("2014/12/31"), as.Date("2014/12/31"), df$end)
df

# Create interval column
df$interval <- interval(df$start, df$end)
typeof(df$interval) # double
df %>% 
  head()

floor_date(as.POSIXct("2014-09-11"), "month")
ceiling_date(as.POSIXct("2014-09-11"), "month")

# Beginning of year
boy <- as.POSIXct("2014-01-01")

df %>% 
  head()

df_copy <- df
df_copy %>% 
  head()

colnames(df_copy)

# Try to create empty columns in df pertaining to week number,
# then for each member in the dataframe, loop through all
# 52 weeks to test if the week is %within% the interval
# for that member. If so, put a 1 or TRUE for that member's week value:

# Try to mutate in a loop with strings for variable names:
# https://stackoverflow.com/questions/26003574/dplyr-mutate-use-dynamic-variable-names

for(i in 0:51){
  varname <- paste0('week',i+1)
  df_copy <- df_copy %>%
    mutate(!!varname := as.integer(
      as.interval(
        boy + weeks(i),boy + weeks(i+1))
      %within%interval))
}

df_copy

# Get the sum of the members with coverage in that week:
df_copy %>% 
  summarise_at(vars(week1:week52), funs(sum))













# https://community.rstudio.com/t/counting-overlapping-records-per-day-using-dplyr/4289/2
memberships <- tibble(
  memberID     = c("A", "A", "A", "B"),
  membershipID = 1:4 %>% as.factor,
  start_date   = ymd(c(20100101, 20101220, 20120415, 20110605)),
  end_date     = ymd(c(20101231, 20111231, 20130430, 20120531)),
  mo_dur       = interval(start_date, end_date) %>% 
    as.duration() / dyears() * 12)

ggplot(memberships) + 
  geom_segment(aes(x = start_date, xend = end_date,
                   y = membershipID, yend = membershipID)) +
  geom_text(vjust = -0.5, hjust=0, size = 3,
            aes(x = start_date, y = membershipID, 
                label = paste(round(mo_dur, 2), "months")))

memberships <- tibble::rowid_to_column(memberships)

overlaps <- purrr::map(memberships$rowid, function(id) {
  if (id == nrow(memberships)) {
    NA
  } else {
    row <- memberships[memberships[["rowid"]] == id, ]
    intv <- lubridate::interval(row$start_date, row$end_date)
    # these are the id's of the rows following id
    other_ids <- (id):(nrow(memberships))
    ol <- purrr::map_int(other_ids, function(other_id) {
      other_row <- memberships[memberships[["rowid"]] == other_id, ]
      # either on end is inside of interval or start and end span it
      if (other_row$start_date %within% intv |
          other_row$end_date %within% intv |
          (other_row$start_date <= row$start_date &
           other_row$end_date >= row$end_date)) {
        as.integer(other_row$rowid)
      } else {
        NA_integer_
      }
    })
    # drop the NA's
    ol <- ol[!is.na(ol)]
    # if nothing overlapped return NA
    if (length(ol > 0)) {
      ol
    } else {
      NA
    }
  }
})

# make it a tibble so youcan bind it
overlaps <- tibble::tibble(following_overlaps = overlaps)
# add as column
memberships <- dplyr::bind_cols(memberships, overlaps)

memberships









# Create dataset with three columns: year, month, and count of something:
df <- as.data.frame(airquality)
df
res <- ggplot(df, aes(Day, Month)) + geom_tile(aes(fill = Ozone),colour = "white") +
  scale_fill_gradient(low = "#d8e1cf", high = "#438484") +  
  guides(fill=guide_legend(title="Total Ozone")) +
  labs(title = "Atmospheric Ozone by Month and Day",
       x = "Day", y = "Month") +
  theme_minimal() 
res

ggplotly(res)















# https://rkabacoff.github.io/datavis/Other.html#biplots
# load data
data(gapminder, package="gapminder")

# subset Asian countries
asia <- gapminder %>%
  filter(continent == "Asia") %>%
  dplyr::select(year, country, lifeExp)

# convert to long to wide format
plotdata <- spread(asia, year, lifeExp)

# save country as row names
plotdata <- as.data.frame(plotdata)
row.names(plotdata) <- plotdata$country
plotdata$country <- NULL

# row order
sort.order <- order(plotdata$"2007")

# color scheme
colors <- rev(brewer.pal(5, "Blues"))

# create the heat map
superheat(plotdata,
          scale = FALSE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,
          heat.pal = colors,
          order.rows = sort.order,
          title = "Life Expectancy in Asia")











# rbokeh













# plotly

vals <- unique(scales::rescale(c(volcano)))
o <- order(vals, decreasing = FALSE)
cols <- scales::col_numeric("Blues", domain = NULL)(vals)
colz <- setNames(data.frame(vals[o], cols[o]), NULL)
p <- plot_ly(z = volcano, colorscale = colz, type = "heatmap")
p







# d3
d3heatmap(mtcars, scale="column", colors="Blues", dendrogram = "none")




dropcols <- c("start","end","interval")
mydf <- df_copy %>% 
  dplyr::select(-one_of(dropcols))
mydf

d3heatmap(mydf, scale="column", colors="Blues", dendrogram = "none")
# It works!! Victory!








# Calendar heatmaps


# Density / hexbins -------------------------------------------------------

figure() %>% ly_hexbin(rnorm(10000), rnorm(10000))




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

#http://www.rebeccabarter.com/blog/2017-04-20-interactive/
forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Group = "group", opacity = 0.9, Nodesize = 3, 
             linkDistance = 100, fontSize = 20)

# Dot plots ---------------------------------------------------------------

ggplot(diamonds, aes(x = price)) +
  geom_dotplot() + 
  labs(title = "Proportion by price",
       y = "Proportion",
       x = "Price")

ggplot(diamonds, aes(x = price)) +
  geom_dotplot(binwidth = 500) + 
  labs(title = "Proportion by price",
       y = "Proportion",
       x = "Price")



# geom_point --------------------------------------------------------------

ggplot(mtcars, aes(wt, mpg)) +
  geom_point(shape = 21, colour = "black", fill = "white", size = 5, stroke = 5)









# https://socviz.co/maps.html






# pointrange --------------------------------------------------------------







# Love (lollipop) plots --------------------------------------------------------

# https://rpubs.com/ageek/ggplot-adv-part2
mpg %>%
  group_by(manufacturer) %>%
  summarise(Mileage=mean(cty)) %>%
  #convert to factor and re-order using forcats:fct_order method at one go
  mutate(Make=fct_reorder(manufacturer, Mileage))%>%
  ggplot(aes(x=Make, y=Mileage)) + 
  geom_point(size=4, color="tomato3") + 
  geom_segment(aes(x=Make, 
                   xend=Make, 
                   y=0, 
                   yend=Mileage)) + 
  #print value for each bar as well
  geom_text(color="purple", size=4, vjust=-0.8, 
            aes(label=sprintf("%0.1f", round(Mileage, digits = 2))))+
  labs(title="Lollipop Chart",
       subtitle="Make Vs Avg. Mileage") + 
  # change default limit to 30 else geom_text for largest point 24.4              prints halfway only
  ylim(0, 27)+
  theme(axis.text.x = element_text(angle=65, vjust=0.7, color="tomato3"))



# This example might be useful in examining matching imbalance,
# by showing which variable decreased or increased in imbalance
# instead of showing the absolute amount:

# https://www.r-graph-gallery.com/302-lollipop-chart-with-conditional-color/
x=seq(0, 2*pi, length.out=100)
data=data.frame(x=x, y=sin(x) + rnorm(100, sd=0.2))
# Add a column with your condition for the color
data <- data %>% 
  mutate(mycolor = ifelse(y>0, "type1", "type2")) %>% 
  arrange(desc(y))
data$x <- as.character(round(x, digits = 2))

# plot
l1 <- ggplot(data, aes(x=reorder(x,-y), y=y)) +
  geom_segment(aes(x=x, xend=x, y=0, yend=y, color=mycolor), 
               size=1.3, alpha=0.9) +
  theme_light() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
  ) +
  xlab("") +
  ylab("Value of Y")

l1

# Swap axes
l1 + coord_flip()

# Add in annotation:
l1 +
  coord_flip() +
  annotate("text", x = 3, y = -1, label = "Above Average", 
           color = "#00BFC4", size = 3, hjust = -0.1, vjust = .75) +
  annotate("text", x = 3, y = -1, label = "Below Average", 
           color = "#F8766D", size = 3, hjust = -0.1, vjust = -.1) +
  geom_segment(aes(x = 3, xend = 4 , y = -1, yend = -0.5),
               arrow = arrow(length = unit(0.2,"cm")), color = "#00BFC4") +
  geom_segment(aes(x = 3, xend = 2 , y = -1, yend = -0.5),
               arrow = arrow(length = unit(0.2,"cm")), color = "#F8766D")

# Have to change the x and xend for the geom_segment arrows to whatever
# factor levels are at the center of the plot






# Dumbbell plots ----------------------------------------------------------

#https://rud.is/b/2016/04/17/ggplot2-exercising-with-ggalt-dumbbells/


# https://rpubs.com/ageek/ggplot-adv-part2
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


ggplot(health, aes(x=pct_2013, xend=pct_2014, y=Area)) + 
  #create a thick line between x and xend instead of using defaut 
  #provided by geom_dubbell
  geom_segment(aes(x=pct_2013, 
                   xend=pct_2014, 
                   y=Area, 
                   yend=Area), 
               color="#b2b2b2", size=1.5)+
  geom_dumbbell(color="light blue", 
                size_x=3.5, 
                size_xend = 3.5,
                #Note: there is no US:'color' for UK:'colour' 
                # in geom_dumbbel unlike standard geoms in ggplot()
                colour_x="#edae52", 
                colour_xend = "#9fb059")+
  labs(x=NULL, y=NULL, 
       title="Dumbbell Chart", 
       subtitle="Pct Change: 2013 vs 2014")+
  geom_text(color="black", size=2, hjust=-0.5,
            aes(x=pct_2013, label=pct_2013))+
  geom_text(aes(x=pct_2014, label=pct_2014), 
            color="black", size=2, hjust=1.5) +
  geom_vline(aes(xintercept=0.1, color = "black"))













#https://rkabacoff.github.io/datavis/Time.html
# create dumbbell plot
# First, create random data
psData <- data.frame("variable_name" = c("gender","age","ccs_level"),
                     "adjusted" = c(0.09,0.07,0.26),
                     "unadjusted" = c(0.14,0.13,0.39))
psData %>% 
  mutate(difference = unadjusted - adjusted) -> psData
psData


data(gapminder, package = "gapminder")
gapminder
# subset data
plotdata_long <- gapminder %>% 
  filter(continent == "Americas" & year %in% c(1952, 2007)) %>% 
  dplyr::select(country, year, lifeExp)

# convert data to wide format
plotdata_wide <- spread(plotdata_long, year, lifeExp)
names(plotdata_wide) <- c("country", "y1952", "y2007")
plotdata_wide

# create dumbbell plot
ggplot(plotdata_wide, aes(y = country,
                          x = y1952,
                          xend = y2007)) +  
  geom_dumbbell()


# Different look
ggplot(plotdata_wide, 
       aes(y = reorder(country, y1952),
           x = y1952,
           xend = y2007)) +  
  geom_dumbbell(size = 1.2,
                size_x = 3, 
                size_xend = 3,
                colour = "grey", 
                colour_x = "blue", 
                colour_xend = "red") +
  geom_vline(aes(xintercept=40, color = "blue")) +
  theme_minimal() + 
  labs(title = "Change in Life Expectancy",
       subtitle = "1952 to 2007",
       x = "Life Expectancy (years)",
       y = "")



# Similar to above but with example relevant to matching:
# Dataset should look like:
# varname | pre | post
match_data <- data.frame("varname" = c("gender",
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
                         "adjusted" = c(0.09,0.17,0.29,0.07,0.19,0.26,0.3,0.19,0.24,0.08,0.14),
                         "unadjusted" = c(0.14,0.15,0.37,0.13,0.24,0.39,0.12,0.2,0.18,0.31,0.4))

match_data <- match_data %>% 
  mutate(diff = unadjusted - adjusted)
match_data

dumbbell <- ggplot(match_data, 
       aes(y = reorder(varname, adjusted),
           x = unadjusted,
           xend = adjusted)) +  
  geom_dumbbell(size = 1.2,
                size_x = 3, 
                size_xend = 3,
                colour = "grey", 
                colour_x = "blue", 
                colour_xend = "red") +
  geom_vline(aes(xintercept=0.1),linetype='dashed') +
  theme_minimal() + 
  labs(title = "Reduction in Imbalance",
       subtitle = "Pre vs. Post",
       x = "Mean absolute difference",
       y = "",
       fontface = "bold")

dumbbell + theme(axis.text.y = element_text(colour = "darkblue")) +labs(y = NULL)

# https://data-se.netlify.com/2018/05/23/playing-around-with-dumbbell-plots/
dumbbell2 <- dumbbell + 
  geom_text(aes(y = varname, label = diff),
          x = max(match_data$unadjusted)*1.2, hjust = 1, vjust = -0.5,
          color = "blue", fontface = "bold") +
  annotate(x = max(match_data$unadjusted)*1.2, y = "professional visits", label = "Diff",
           geom = "text", vjust = -3,
           fontface = "bold",
           hjust = 1) +
  theme_economist() +
  scale_color_manual(name = "", values = c("red", "blue") )

dumbbell2


dumbbell +
  annotate(geom = "rect",
         xmin = .25,
         xmax = 1,
         ymin = as.numeric(d$country[d$country =="Region I"]) - 0.3,
         ymax = as.numeric(d$country[d$country =="Region I"]) + 0.3,
         alpha = .3,
         fill = "firebrick")










# plotly
# https://plot.ly/r/dumbbell-plots/

s <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/school_earnings.csv")
# order factor levels by men's income (plot_ly() will pick up on this ordering)
s$School <- factor(s$School, levels = s$School[order(s$Men)])
s
# School Women Men Gap
p <- plot_ly(s, color = I("gray80")) %>%
  add_segments(x = ~Women, xend = ~Men, y = ~School, yend = ~School, showlegend = FALSE) %>%
  add_markers(x = ~Women, y = ~School, name = "Women", color = I("pink")) %>%
  add_markers(x = ~Men, y = ~School, name = "Men", color = I("blue")) %>%
  layout(
    title = "Gender earnings disparity",
    xaxis = list(title = "Annual Salary (in thousands)"),
    margin = list(l = 65)
  )
p



# Data should look like:
# variable_name adjusted unadjusted difference
psData <- data.frame("variable_name" = c("gender",
                                         "age",
                                         "ccs_level",
                                         "total medical net paid",
                                         "BH net paid",
                                         "BH professional",
                                         "ALOS"),
                     "adjusted" = c(0.09,0.07,0.26,0.3,0.19,0.08,0.14),
                     "unadjusted" = c(0.14,0.13,0.39,0.12,0.2,0.31,0.4))
psData %>% 
  mutate(difference = unadjusted - adjusted) -> psData
psData

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


# p <- plot_ly(psData, color = I("gray80")) %>%
#   add_segments(x = ~adjusted, xend = ~unadjusted, y = ~variable_name, yend = ~variable_name, showlegend = FALSE) %>%
#   
#   # Trying to dd vertical line here for ALL y-axis
#   add_segments(x = 0.1, xend = 0.1, y = 0, yend = 1, showlegend = FALSE) %>% 
#   
#   add_markers(x = ~adjusted, y = ~variable_name, name = "adjusted", color = I("pink")) %>%
#   add_markers(x = ~unadjusted, y = ~variable_name, name = "unadjusted", color = I("blue")) %>%
#   layout(
#     title = "Covariate balance improvement",
#     xaxis = list(title = "Absolute Mean Differences"),
#     margin = list(l = 65)
# )
# 
# p




# Parallel coordinates ----------------------------------------------------





# Sankey diagrams ---------------------------------------------------------





# Maps --------------------------------------------------------------------

gmap(lat = 40.74, lng = -73.95, zoom = 11,
     width = 600, height = 600,
     map_style = gmap_style("blue_water"))



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

ggMarginal(g, type = "histogram", fill="pink")
ggMarginal(g, type = "boxplot", fill="transparent")
ggMarginal(g, type = "density", fill="transparent")

# Seasonal plot -----------------------------------------------------------

p_load(forecast)
theme_set(theme_classic())
# Plot
ggseasonplot(AirPassengers) + labs(title="Seasonal plot: International Airline Passengers")

# Policy Coverage ---------------------------------------------------------
memberships <- tibble(
  memberID     = c("A", "A", "A", "B"),
  membershipID = 1:4 %>% as.factor,
  start_date   = ymd(c(20100101, 20101220, 20120415, 20110605)),
  end_date     = ymd(c(20101231, 20111231, 20130430, 20120531)),
  mo_dur       = interval(start_date, end_date) %>% 
    as.duration() / dyears() * 12)

ggplot(memberships) + 
  geom_segment(aes(x = start_date, xend = end_date,
                   y = membershipID, yend = membershipID)) +
  geom_text(vjust = -0.5, hjust=0, size = 3,
            aes(x = start_date, y = membershipID, 
                label = paste(round(mo_dur, 2), "months")))

