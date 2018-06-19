# Source this code into demo or run beforehand
# ---- Packages ----
library(dplyr)
library(ggplot2)

# ---- Data ----
data <- read.csv("shark-data.csv")
studyspecies <- c("Spinytail Skate", "Deep-Water Catshark", "Greenland Shark", "Black Dogfish")
studydata <- droplevels(data %>% filter(year > 1995 & species %in% studyspecies))
studydata <- studydata[-which(studydata$species == "Black Dogfish" & studydata$depth > 600),]
studydata <- studydata[-which(studydata$species == "Spinytail Skate" & studydata$season=="Fall" & studydata$depth < 500),]
oceandata <- studydata[with(studydata, order(-depth)),]
oceandata$rank <- 1:nrow(oceandata)
floordata <- rbind(data.frame(rank = max(oceandata$rank), depth = 1500),oceandata %>% select(rank, depth))
oceandata_sub <- droplevels(oceandata %>% filter(species %in% studyspecies))

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

# ---- Function: draw bar plot ----
bargraph_sharks <- function()
{
  barplot1 <<- ggplot(oceandata_sub) +
    geom_bar(aes(x = species, fill = species), col = "black") +
    scale_fill_manual(values = c("olivedrab3","darkorange","purple","firebrick3")) +
    theme_bw(18) + ylab("Count") + xlab("") +
    theme(legend.position = "none")
  return(barplot1)
}

# ---- Function: draw pie chart ----
piechart_sharks <- function()
{
  oceandata_sub$fac <- 1
  piechart1 <<- ggplot(oceandata_sub) +
    geom_bar(aes(x = fac, fill = species), col = 'black') +
    pietheme +
    coord_polar("y") +
    scale_fill_manual(values = c("olivedrab3","darkorange","purple","firebrick3"), name = "Species")
  return(piechart1)
}

# ---- Function: draw ocean ----
draw_ocean<-function()
{
  ocean<<-ggplot(oceandata)+
    geom_rect(xmax=150, xmin=-max(oceandata$rank), ymin=-500, ymax=0, fill="deepskyblue2")+
    geom_rect(xmax=150, xmin=-max(oceandata$rank), ymin=-1000, ymax=-500, fill="deepskyblue3")+
    geom_rect(xmax=150, xmin=-max(oceandata$rank), ymin=-1500, ymax=-1000, fill="deepskyblue4")+
    geom_hline(yintercept=0, linetype="dashed")+ylab("Depth")+theme_bw(15)
  return(ocean)
}

# ---- function: add ocean floor ----
add_floor<-function()
{
  ocean<<-ocean+
    geom_polygon(aes(y=-depth, x=-rank), data=floordata, col='black', fill='burlywood4')+
    xlab("")+
    theme(axis.ticks=element_blank(), axis.text.x=element_blank())
  return(ocean)
}

# ---- function: add boat ----
add_boat<-function()
{
  ocean<<-ocean+geom_rect(xmin=-1100, xmax=-600, ymin=-10, ymax=100, col='black', fill="grey80")+
    geom_text(aes(x=-850, y=50, label="trawler"), size=5, col='black')
  return(ocean)
}

# ---- function: add sharks ----
add_sharks<-function()
{
  plot1<<-ocean+geom_point(aes(y=jitter(-depth)+40, x=-rank+100, fill=species), col='black',shape=21, size=5)+
    facet_wrap(~species)+
    scale_fill_manual(values=c("olivedrab3","darkorange","purple","firebrick3"))
  return(plot1)
}

# ---- function: add sharks by season ----
by_season<-function(Species)
{
  sub<-oceandata_sub %>% filter(species==Species)
  plot2<<-ocean+geom_point(aes(y=jitter(-depth)+40, x=-rank+100), fill=mycols$colour[mycols$species==Species], col='black', shape=21, size=5, data=sub)+
    facet_wrap(~season)+ggtitle(Species)+
    theme(legend.position = "none")
  return(plot2)
}
