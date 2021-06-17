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
imageMap <- data.frame(x = rep(1:xwidth, times = ywidth), y = rep(1:ywidth, each = xwidth), R = 0, G = 0, B = 0)


# Parse through
for (i in 1:xwidth) {
  for (j in 1:ywidth) {
    imageMap$R[imageMap$x == i & imageMap$y == j] <- imdf$R[imdf$x == xsample[i] & imdf$y == ysample[j]]
    imageMap$G[imageMap$x == i & imageMap$y == j] <- imdf$G[imdf$x == xsample[i] & imdf$y == ysample[j]]
    imageMap$B[imageMap$x == i & imageMap$y == j] <- imdf$B[imdf$x == xsample[i] & imdf$y == ysample[j]]
  }
}

# Plot the image with real colour using the rgb() and scale_color_identity() function
imageMap %>% ggplot(aes(x, -y, col = rgb(R, G, B))) + geom_point(size = 4) + scale_color_identity()









