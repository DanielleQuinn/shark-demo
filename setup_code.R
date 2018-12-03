# This code sets up functions to be used
# with Creative Computing / Skype a Scientist 
# classroom demonstrations

# ---- Packages ----
library(dplyr)
library(ggplot2)
library(tidyr)

# ---- Data ----
data <- read.csv("sharkdata.csv")
floordata <- data %>%
  arrange(desc(depth)) %>%
  mutate(rank = 1:nrow(.)) %>%
  dplyr::select(rank, depth) %>%
  add_row(rank = max(.$rank), depth = 1500, .before = 1)
data <- left_join(data, floordata)

# ---- Pie chart theme ----
pietheme <- theme_minimal()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_blank(),
        axis.text.x = element_blank(),
        strip.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16))

# ---- Colours for each species ----
mycols <- data.frame(species = c("Black Dogfish","Deep-Water Catshark","Greenland Shark", "Spinytail Skate"),
                     colour = c("olivedrab3","darkorange","purple","firebrick3"))

# ---- Function: display table ----
count_sharks <- function()
{
  return(table(data$species))
}

# ---- Function: draw bar plot ----
bargraph_sharks <- function(x = 18)
{
  bargraph1 <<- ggplot(data) +
    geom_bar(aes(x = species, fill = species), col = "black") +
    scale_fill_manual(values = c("olivedrab3","darkorange","purple","firebrick3")) +
    theme_bw(x) + ylab("Count") + xlab("") +
    theme(legend.position = "none")
  return(bargraph1)
}
# Check: setting consistent colours via scale_colour_manual

# ---- Function: draw pie chart ----
piechart_sharks <- function()
{
  data$fac <- 1
  piechart1 <<- ggplot(data) +
    geom_bar(aes(x = fac, fill = species), col = 'black') +
    pietheme +
    coord_polar("y") +
    scale_fill_manual(values = c("olivedrab3","darkorange","purple","firebrick3"), name = "Species")
  return(piechart1)
}

# ---- Function: draw ocean ----
draw_ocean<-function()
{
  ocean <<- ggplot(floordata) +
    geom_rect(xmax = 150, xmin = -max(floordata$rank), ymin = -500, ymax = 0, fill = "deepskyblue2") +
    geom_rect(xmax = 150, xmin = -max(floordata$rank), ymin = -1000, ymax = -500, fill = "deepskyblue3") +
    geom_rect(xmax = 150, xmin = -max(floordata$rank), ymin = -1500, ymax = -1000, fill = "deepskyblue4") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    ylab("Depth") + xlab("") +
    geom_polygon(aes(y = -depth, x = -rank), col = 'black', fill = 'burlywood4') +
    theme(axis.ticks = element_blank(), axis.text.x = element_blank()) +
    theme_bw(15)
  return(ocean)
}

# ---- function: add sharks ----
add_sharks<-function()
{
  plot1 <<- ocean + 
    geom_point(aes(y = jitter(-depth) + 40, x = -rank + 100, fill = species), col = 'black', shape = 21, size = 5, data = data) +
    facet_wrap(~species) +
    scale_fill_manual(values=c("olivedrab3","darkorange","purple","firebrick3"))
  return(plot1)
}

# ---- function: add sharks by season ----
by_season<-function(Species)
{
  plot2<<-ocean + geom_point(aes(y = jitter(-depth) + 40,
                                 x = -rank + 100), fill=mycols$colour[mycols$species == Species],
                             col = 'black', shape = 21, size = 5,
                             data = data %>% filter(species == Species)) +
    facet_wrap(~season) + ggtitle(Species) +
    theme(legend.position = "none")
  return(plot2)
}

# Note: Spinytail skate is a good example for facetting by season