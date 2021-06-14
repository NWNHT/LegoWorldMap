# Script to convert image of Lego World Map to a "worldmap" dataframe #
# Relies on cropped image of black and white Lego world map
# Lucas Nieuwenhout - 2021-06-13
# First R project/script


library(tidyverse)
library(imager)

# Load full land dataframe
load("./CompletedMaps/worldmapLand.RData")

# Create empty map dataframe
worldmap <- data.frame(tile = factor(16, levels = (1:16)),land = NA, yco = rep(1:80, each = 128), xco = rep(1:128, times = 80))

# Plot worldmap dataframe
worldmap %>% ggplot(aes(x = xco, y = yco, col = tile)) + geom_point()

# Load in white and black image
im <- load.image("./mono.jpg")

# Resize image for fun I guess, I don't think this actually helped, it likely didn't
imscale <- 17
im <- resize(im, size_x = imscale*128, size_y = imscale*80)

# Create temporary dataframe for image
imdfTemp <- as.data.frame(im)

# Create formatted dataframe for image with separete RGB
imdf <- data.frame(x = imdfTemp %>% filter(cc == 1) %>% pull(x), 
                   y = imdfTemp %>% filter(cc == 1) %>% pull(y),
                   R = imdfTemp %>% filter(cc == 1) %>% pull(value),
                   G = imdfTemp %>% filter(cc == 2) %>% pull(value),
                   B = imdfTemp %>% filter(cc == 3) %>% pull(value))

# Create white variable in imdf from RGB values
thres <- 0.7 # A wide range of thresholds will work
imdf <- imdf %>% mutate(white = ifelse((imdf$R > thres & imdf$G > thres & imdf$G > thres), TRUE, FALSE))



# Plot monochrome map image from imdf dataframe and index edges(takes a while)
imdf %>% ggplot(aes(x, -y, col = white)) + geom_point() +
 geom_hline(yintercept = -87, col = "red") +
 geom_hline(yintercept = -1349, col = "red") +
 geom_vline(xintercept = 3, col = "red") +
 geom_vline(xintercept = 2174, col = "red")

# Create vectors for samples, values for beginning and end are as determined above
#     Note that the length of ysample is 75, top 5 rows are blank, this shows up elsewhere
ysample = round(seq(87, 1349, length.out = 75))
xsample = round(seq(3, 2174, length.out = 128)) # Previously 8, 3 was like 90%, 1 is not good

# Plot sample grid(takes a while)
imdf %>% ggplot(aes(x, -y, col = white)) + geom_point() +
  geom_hline(yintercept = -ysample, col = "red") +
  geom_vline(xintercept = xsample, col = "red")

# For all points in xsample and ysample check if intersection is white and write to worldmap
for (i in 1:128) {
  for (j in 1:75) {
    if (sum(imdf$white[imdf$x == xsample[i] & imdf$y == ysample[j]]) > 0) {
      # If white assign as land
      worldmap$tile[(worldmap$xco == i) & (worldmap$yco == 81 - (j + 5))] <- 1
      worldmap$land[(worldmap$xco == i) & (worldmap$yco == 81 - (j + 5))] <- TRUE
    }else {
      # else assign as blank
      worldmap$tile[(worldmap$xco == i) & (worldmap$yco == 81 - (j + 5))] <- 16
    }
  }
}

# Plot worldmap dataframe
worldmap %>% ggplot(aes(x = xco, y = yco, col = tile)) + geom_point()


# -- #
# Up to this point the generated map accounts for ~95% of studs.  It is extremely accurate East of -
# Florida it begins to miss some points as the indexing of the xsample is slightly off.  This is - 
# likely because my indexing is slightly off there and I can't quite tune it correctly.  It might - 
# also be affected by a little skewing of the image as it is not orthographic.
# --#


# Manual edits to complete map, separated for convenience, could be combined into single set.
# Antarctica
xcoordant = as.numeric(c(4, 7:14, 8:14, 7:13, 9:13))
ycoordant = c(1, rep(1, times = 8), rep(2, times = 7), rep(3, times = 7), rep(4, times = 5))

for (i in 1:length(xcoordant)) {
  worldmap$tile[(worldmap$xco == xcoordant[i]) & (worldmap$yco == ycoordant[i])] <- 1
  worldmap$land[(worldmap$xco == xcoordant[i]) & (worldmap$yco == ycoordant[i])] <- TRUE
}

# America
xcoordam = as.numeric(c(17, 20, 20, 21, 22, 24, 24, 24, 25, 23, 24, 25, 26, 27))
ycoordam = c(56, 52, 53, 52, 52, 51, 52, 53, 53, 49, 49, 49, 49, 51)

for (i in 1:length(xcoordam)) {
  worldmap$tile[(worldmap$xco == xcoordam[i]) & (worldmap$yco == ycoordam[i])] <- 1
  worldmap$land[(worldmap$xco == xcoordam[i]) & (worldmap$yco == ycoordam[i])] <- TRUE
}

# Canada
xcoordcan = as.numeric(c(20, 24, 17, 1, 3, 4, 3, 4, 16, 16))
ycoordcan = c(68, 68, 72, 50, 49, 48, 63, 64, 71, 72)

for (i in 1:length(xcoordcan)) {
  worldmap$tile[(worldmap$xco == xcoordcan[i]) & (worldmap$yco == ycoordcan[i])] <- 1
  worldmap$land[(worldmap$xco == xcoordcan[i]) & (worldmap$yco == ycoordcan[i])] <- TRUE
}

#Alaska
xcoordal = as.numeric(c(2:16, 4:16, 4:16, 2:16, 4, 5, 6, 5, 1, 2))
ycoordal = c(rep(66, times = 15), rep(67, times = 13), rep(68, times = 13), rep(69, times = 15), 70, 70, 70, 65, 68, 68)

for (i in 1:length(xcoordal)) {
  worldmap$tile[(worldmap$xco == xcoordal[i]) & (worldmap$yco == ycoordal[i])] <- 1
  worldmap$land[(worldmap$xco == xcoordal[i]) & (worldmap$yco == ycoordal[i])] <- TRUE
}

# Test plot with grid
worldmap %>% ggplot(aes(x = xco, y = yco, col = tile)) + geom_point() + geom_vline(xintercept = seq(16, 128, 16)) + geom_hline(yintercept = seq(16, 64, 16))

# Convert all land = NA to FALSE as not previously assigned.  Could be made obsolete by defining as false
worldmap$land <- ifelse(is.na(worldmap$land), FALSE, worldmap$land)

# Admire the work
colours <- c("1" = "white", "2" = "deepskyblue2", "16" = "black")
worldmap %>% ggplot(aes(x = xco, y = yco, col = tile)) + 
  geom_point() + scale_color_manual(values = colours) + 
  theme(panel.background = element_rect(fill = 'black', colour = 'black'), panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# Save the worldmap dataframe
save(worldmap, file = "/Users/lucasnwnht/Library/Mobile Documents/com~apple~CloudDocs/AAAFiles/AA_School/DataScience/WorldMap/worldmapLand.RData")
