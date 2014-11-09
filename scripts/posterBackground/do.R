library(ggplot2)
library(grid)
library(RColorBrewer)

load('data/wind_year.Rda')
wind <- wind.year
load("data/bathy.Rdata")
load("data/night.Rdata")
load("data/red_sea_inside.Rdata")
load("data/red_sea_outside.Rdata")

x_min <- 31
x_max <- 45
y_min <- 10
y_max <- 31

#----- Axis layer ----#

axis_p <-ggplot(wind, aes(x=long, y=lat))
axis_p <- axis_p + coord_cartesian(ylim=c(y_min,y_max),xlim=c(x_min,x_max))

#----- Theme layer -----#

theme_p <- theme(axis.line=element_blank(),
                 axis.text.x=element_blank(),
                 axis.text.y=element_blank(),
                 axis.ticks=element_blank(),
                 axis.title.x=element_blank(),
                 axis.title.y=element_blank(),
                 legend.position="none",
                 panel.background=element_blank(),
                 panel.border=element_blank(),
                 panel.grid.major=element_blank(),
                 panel.grid.minor=element_blank(),
                 plot.background=element_blank(),
                 plot.margin = unit(c(-.1, -.1, -1, -1), "lines"))

# ----- Bathymetry layer ------#

bathy_p <-stat_contour(data=bathy_df, aes(x, y, z=topo1),
                       color="black", breaks=c(-30), size=.15) 
# color="#00BAFFFF", breaks=c(-30), size=.15) 

#------ Red Sea layer -----#

red.sea.outside_p <- geom_polygon(data=red.sea.outside, 
                                  aes(x=long, y=lat, group=group), 
                                  colour="#1c5c6b", fill="magenta", size=.5)

#------ Wind Layer ------#

wind_p <- axis_p + geom_tile(aes(fill = class50), height=0.1)
wind_p <- wind_p + scale_fill_manual(values=rev(brewer.pal(7, "RdYlBu")), 
                                     labels=seq(7))
wind_p <- wind_p + bathy_p + red.sea.outside_p + theme_p 
ggsave(filename="results/posterfig/wind.png", plot=wind_p, width=568.3, height=826, dpi=300, 
       units="mm")

#------ Night Layer ------#

night_p <- axis_p + geom_raster(data=night_df, 
                                aes(x = x, y = y, fill = nightearth))
night_p <- night_p + scale_fill_gradient(low='black', high='white', 
                                         trans='log', limits=c(.1,5000), 
                                         na.value='black')
night_p <- night_p + theme_p
ggsave(filename="results/posterfig/night.png", plot=night_p, units="mm")
