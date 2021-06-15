# Script to generate patterns for the Lego World Map #

# Lucas Nieuwenhout


library(tidyverse)

# Load full land dataframe
load("./CompletedMaps/worldmapLand.RData")

# Plot the map
worldmap %>% ggplot(aes(x = xco, y = yco, col = tile)) + geom_point()

# scale_color_manual(values = c())


## Assign based on proximity to land ##

# Set the adjacent arrays to hold the translations.
adjacent4 <- array(c(c(1,-1,0,0), c(0,0,1,-1)), dim = c(4,2))
adjacent8 <- array(c(c(1,1,1,0,0,-1,-1,-1), c(-1, 0, 1, -1, 1, -1, 0, 1)), dim = c(8,2))
adjacent4Level2 <- array(c(c(-1, 0, 1, -2, -1, 1, 2, -2, 2, -2, -1, 1, 2, -1, 0, 1), c(2, 2, 2, 1, 1, 1, 1, 0, 0, -1, -1, -1, -1, -2, -2, -2)), dim = c(16, 2))
adjacent8Level2 <- array(c(c(-2, -1, 0, 1, 2, -2, 2, -2, 2, -2, 2, -2, -1, 0, 1, 2), c(2, 2, 2, 2, 2, 1, 1, 0, 0, -1, -1, -2, -2, -2, -2, -2)), dim = c(16, 2))
shadowEast <- array(c(-1, 0), dim = c(1, 2))

# Set the limits of the frame
maxx <- 128
maxy <- 80


# For each stud, if it is not land, check if the stud pointed at by the translation in the adjacent vector is land.
# If it is land then change assign the colour.



# For adjacent4
for (i in 1:128) { # For all x
  for (j in 1:80) { # For all y
    if (!worldmap$land[(worldmap$xco == i) & (worldmap$yco == j)]) { # If land then ignore
      for (k in 1:4) { # Cycle through adjacent array to check cells
        if (worldmap$land[(worldmap$xco == min(max((i + adjacent4[k,1]),1),maxx)) & (worldmap$yco == min(max((j + adjacent4[k,2]), 1),maxy))]) {
          worldmap$tile[(worldmap$xco == i) & (worldmap$yco == j)] <- 3
          break
        }
      }
    }
  }
}

save(worldmap, file = "./CompletedMaps/worldmapAdjacent4.Rdata")

# For Adjacent4Level2
for (i in 1:128) { # For all x
  for (j in 1:80) { # For all y
    if (!worldmap$land[(worldmap$xco == i) & (worldmap$yco == j)]) { # If land then ignore
      for (k in 1:16) { # Cycle through adjacent array to check cells
        if (worldmap$land[(worldmap$xco == min(max((i + adjacent4Level2[k,1]),1),maxx)) & (worldmap$yco == min(max((j + adjacent4Level2[k,2]), 1),maxy))]) {
          worldmap$tile[(worldmap$xco == i) & (worldmap$yco == j)] <- 4
          break
        }
      }
    }
  }
}





# For adjacent8
for (i in 1:128) { # For all x
  for (j in 1:80) { # For all y
    if (!worldmap$land[(worldmap$xco == i) & (worldmap$yco == j)]) { # If land then ignore
      for (k in 1:8) { # Cycle through adjacent array to check cells
        if (worldmap$land[(worldmap$xco == min(max((i + adjacent8[k,1]),1),maxx)) & (worldmap$yco == min(max((j + adjacent8[k,2]), 1),maxy))]) {
          worldmap$tile[(worldmap$xco == i) & (worldmap$yco == j)] <- 3
          break
        }
      }
    }
  }
}

save(worldmap, file = "./CompletedMaps/worldmapAdjacent8.Rdata")

# For Adjacent8Level2
for (i in 1:128) { # For all x
  for (j in 1:80) { # For all y
    if (!worldmap$land[(worldmap$xco == i) & (worldmap$yco == j)]) { # If land then ignore
      for (k in 1:16) { # Cycle through adjacent array to check cells
        if (worldmap$land[(worldmap$xco == min(max((i + adjacent8Level2[k,1]),1),maxx)) & (worldmap$yco == min(max((j + adjacent8Level2[k,2]), 1),maxy))]) {
          worldmap$tile[(worldmap$xco == i) & (worldmap$yco == j)] <- 4
          break
        }
      }
    }
  }
}



# For shadowEast(Could make any other direction)
for (i in 1:128) { # For all x
  for (j in 1:80) { # For all y
    if (!worldmap$land[(worldmap$xco == i) & (worldmap$yco == j)]) { # If land then ignore
      for (k in 1:1) { # Cycle through adjacent array to check cells
        if (worldmap$land[(worldmap$xco == min(max((i + shadowEast[k,1]),1),maxx)) & (worldmap$yco == min(max((j + shadowEast[k,2]), 1),maxy))]) {
          worldmap$tile[(worldmap$xco == i) & (worldmap$yco == j)] <- 2
          break
        }
      }
    }
  }
}

save(worldmap, file = "./CompletedMaps/worldmapShadowEast.Rdata")




# Plot the map with custom colours
colours <- c("1" = "white","2" = "#0A3463", "3" = "#36AEBF", "4" = "#467083", "5" = "#4B9F4A", "6"= "#BBE90B", "7" = "#E4CD9E", "8" = "#F8BB3D", "9" = "#FE8A18", "10" = "#FF698F", "16" = "black")
worldmap %>% ggplot(aes(x = xco, y = yco, col = tile)) + 
  geom_point(size = 3) + scale_color_manual(values = colours) + 
  theme(panel.background = element_rect(fill = 'black', colour = 'black'), panel.grid.major = element_blank(), panel.grid.minor = element_blank())




# Useful for counting the number of each tile required.
worldmap %>% group_by(tile) %>% summarize(tiles = sum(!land))
