library(tidyverse)
library(imager)

# Load in the image
im <- load.image("./ReferenceImages/CanadianFlag.png")

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
xsample <- round(seq(0, max(imdf$x), length.out = xwidth))
ysample <- round(seq(0, max(imdf$y), length.out = ywidth))

# Create iamge dataframe
imageMap <- data.frame(x = rep(1:xwidth, times = ywidth), y = rep(1:ywidth, each = xwidth))




for (i in 1:xwidth) {
  for (j in 1:ywidth) {
    
  }
}












