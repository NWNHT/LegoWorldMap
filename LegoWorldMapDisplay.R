# Script to display the maps
library(tidyverse)

## Load one of the maps below, it will save to dataframe "worldmap" ##

# Just the land
load("./CompletedMaps/worldmapLand.RData")

# The land and a shadow to the east of any land tile
load("./CompletedMaps/worldmapShadowEast.Rdata")

# The land and any tile directly next to a land tile in a cardinal direction
load("./CompletedMaps/worldmapAdjacent4.Rdata")

# The land and any tile directly next to a land tile(not just the cardinal directions) 
load("./CompletedMaps/worldmapAdjacent8.Rdata")

# The land and any tile within 6 studs of the land(I think I made this function work for any distance in the LegoWorldMapBorderPattern file)
load("./CompletedMaps/worldmapAdjacentDist6.Rdata")



## Run the custom colours and grid pattern and plot with or without grid ##

# Set the colours for the map
colours <- c("1" = "white","2" = "#0A3463", "3" = "#36AEBF", "4" = "#467083", "5" = "#4B9F4A", "6"= "#BBE90B", "7" = "#E4CD9E", "8" = "#F8BB3D", "9" = "#FE8A18", "10" = "#FF698F", "16" = "black")

# Generate grid pattern
xsample <- seq(from = 16.5, by = 16, length.out = 7)
ysample <- seq(from = 16.5, by = 16, length.out = 4)


# Plot the map without grid
worldmap %>% ggplot(aes(x = xco, y = yco, col = tile)) + 
  geom_point(size = 3) + 
  scale_color_manual(values = colours) + 
  theme(panel.background = element_rect(fill = 'black', colour = 'black'), panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# Plot the map with grid
worldmap %>% ggplot(aes(x = xco, y = yco, col = tile)) + 
  geom_point(size = 3) + 
  geom_hline(yintercept = ysample, col = "red") + 
  geom_vline(xintercept = xsample, col = "red") +
  scale_color_manual(values = colours) + 
  theme(panel.background = element_rect(fill = 'black', colour = 'black'), panel.grid.major = element_blank(), panel.grid.minor = element_blank())








