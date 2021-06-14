# Script to generate patterns for the Lego World Map #

# Lucas Nieuwenhout


library(tidyverse)

# Load full land dataframe
load("/Users/lucasnwnht/Library/Mobile Documents/com~apple~CloudDocs/AAAFiles/AA_School/DataScience/WorldMap/worldmapLand.RData")

# Plot the map
worldmap %>% ggplot(aes(x = xco, y = yco, col = tile)) + geom_point()

# scale_color_manual(values = c())


# for all points x, y, if land, then next, if not land, check if x +/- 1 and y +/- 1 is land

# Set the adjacent array to hold the translations.
adjacent8 <- array(c(c(1,1,1,0,0,-1,-1,-1), c(-1, 0, 1, -1, 1, -1, 0, 1)), dim = c(8,2))
adjacent4 <- array(c(c(1,-1,0,0), c(0,0,1,-1)), dim = c(4,2))
maxx <- 128
maxy <- 80


# For each stud, if it is not land, check if the stud pointed at by the translation in the adjacent vector is land.
# If it is land then make it colour #2
for (i in 1:128) {
  for (j in 1:80) {
    if (!worldmap$land[(worldmap$xco == i) & (worldmap$yco == j)]) {
      for (k in 1:8) {
        if (worldmap$land[(worldmap$xco == min(max((i + adjacent8[k,1]),1),maxx)) & (worldmap$yco == min(max((j + adjacent8[k,2]), 1),maxy))]) {
          worldmap$tile[(worldmap$xco == i) & (worldmap$yco == j)] <- 2
          break
        }
      }
    }
  }
}

# For adjacent4
for (i in 1:128) {
  for (j in 1:80) {
    if (!worldmap$land[(worldmap$xco == i) & (worldmap$yco == j)]) {
      for (k in 1:4) {
        if (worldmap$land[(worldmap$xco == min(max((i + adjacent4[k,1]),1),maxx)) & (worldmap$yco == min(max((j + adjacent4[k,2]), 1),maxy))]) {
          worldmap$tile[(worldmap$xco == i) & (worldmap$yco == j)] <- 2
          break
        }
      }
    }
  }
}



colours <- c("1" = "white", "2" = "deepskyblue2", "16" = "black")


# Plot the map
worldmap %>% ggplot(aes(x = xco, y = yco, col = tile)) + geom_point() + scale_color_manual(values = colours) + theme(panel.background = element_rect(fill = 'black', colour = 'black'))


# for i in 1:8, if xco/yxo + adjacent[i,1] and adjacent[i,2] is land then make it a certain colour









