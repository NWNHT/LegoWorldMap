library(tidyverse)
library(imager)

# Load in the image
im <- load.image("./ReferenceImages/rainbowApple.png")

plot(im)

# Convert the image to a dataframe
imdf <- as.data.frame(im)

# Convert format of dataframe to RGB
imdf <- data.frame(x = imdf %>% filter(cc == 1) %>% pull(x), 
                   y = imdf %>% filter(cc == 1) %>% pull(y),
                   R = imdf %>% filter(cc == 1) %>% pull(value),
                   G = imdf %>% filter(cc == 2) %>% pull(value),
                   B = imdf %>% filter(cc == 3) %>% pull(value))

# Define the sample frequency/dimension
imageRatio <- max(imdf$y) / max(imdf$x)
xwidth <- 32
ywidth <- round(xwidth * imageRatio)


# Create sample sequence
xsample <- round(seq(1, max(imdf$x), length.out = xwidth))
ysample <- round(seq(1, max(imdf$y), length.out = ywidth))

# Create iamge dataframe
imageMap <- data.frame(x = rep(1:xwidth, times = ywidth), y = rep(1:ywidth, each = xwidth), tile = factor(16, levels = (1:16)), R = 0, G = 0, B = 0)


# Parse through
for (i in 1:xwidth) {
  for (j in 1:ywidth) {
    imageMap$R[imageMap$x == i & imageMap$y == j] <- imdf$R[imdf$x == xsample[i] & imdf$y == ysample[j]]
    imageMap$G[imageMap$x == i & imageMap$y == j] <- imdf$G[imdf$x == xsample[i] & imdf$y == ysample[j]]
    imageMap$B[imageMap$x == i & imageMap$y == j] <- imdf$B[imdf$x == xsample[i] & imdf$y == ysample[j]]
  }
}

# Rainbow Apple Logo Mapping to existing tiles
imageMap$tile[((imageMap$G < 0.734) & (imageMap$G > 0.733))] <- 6
imageMap$tile[((imageMap$G < 0.722) & (imageMap$G > 0.721))] <- 8
imageMap$tile[((imageMap$R < 0.961) & (imageMap$R > 0.960))] <- 9
imageMap$tile[((imageMap$R < 0.879) & (imageMap$R > 0.878))] <- 10
imageMap$tile[((imageMap$R < 0.589) & (imageMap$R > 0.588))] <- 2
imageMap$tile[((imageMap$G < 0.616) & (imageMap$G > 0.615))] <- 3



# Plot the image with real colour using the rgb() and scale_color_identity() function
imageMap %>% ggplot(aes(x, -y, col = rgb(R, G, B))) + geom_point(size = 4) + scale_color_identity()


colours <- c("1" = "white","2" = "#0A3463", "3" = "#36AEBF", "4" = "#467083", "5" = "#4B9F4A", "6"= "#BBE90B", "7" = "#E4CD9E", "8" = "#F8BB3D", "9" = "#FE8A18", "10" = "#FF698F", "16" = "black")
imageMap %>% ggplot(aes(x = x, y = -y, col = tile)) + 
  geom_point(size = 3) + scale_color_manual(values = colours) + 
  theme(panel.background = element_rect(fill = 'black', colour = 'black'), panel.grid.major = element_blank(), panel.grid.minor = element_blank())






