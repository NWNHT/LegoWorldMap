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
shadowEast <- array(c(-1, 0), dim = c(1, 2))

# Set the limits of the frame
maxx <- 128
maxy <- 80


# For each stud, if it is not land, check if the stud pointed at by the translation in the adjacent vector is land.
# If it is land then make it colour #2

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
colours <- c("1" = "white","2" = "blue4", "3" = "deepskyblue2", "16" = "black")
worldmap %>% ggplot(aes(x = xco, y = yco, col = tile)) + 
  geom_point() + scale_color_manual(values = colours) + 
  theme(panel.background = element_rect(fill = 'black', colour = 'black'), panel.grid.major = element_blank(), panel.grid.minor = element_blank())










