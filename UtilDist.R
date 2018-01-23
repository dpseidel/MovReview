# Home Range analysis and comparison - MovReview
# date updated: 01/22/2018
# author: D.P. Seidel

# libraries
library(tidyverse)
library(sf)
library(sp)
library(adehabitatHR)

# read cleaned data  - already formated as a ltraj dataframe. 
AG256 <- read_csv("AG256_2010.csv")
head(AG256)

# isolate lat and long points for adehabitatHR home range metrics
AG_sf <- st_as_sf(AG256, coords=c("x","y"), na.fail=FALSE, crs = 32733) # UTM Zone 33S
AG_points <- AG_sf %>% na.omit %>% st_coordinates %>% SpatialPoints() # Adehabitat requires Spatial objects. 

# plot
plot(AG_points)

# Kernel UD -- using adehabitatHR package
AG_kernel <- kernelUD(AG_points)
AG_UD 

# LoCoh - k 
## Create convex hulls from the maximum number of nearest neighbors such that
## the sum of their distances is less than or equal to a
# using package t-locoh with s=0 is much faster than adehabitatHR `locoh.a` function
AG_coords <- AG_sf %>% na.omit %>% st_coordinates 
AG_time <- AG_sf %>% na.omit %>% pull(date)

AG_lxy <- xyt.lxy(xy= AG_coords, dt= AG_time, id="AG256", proj4string=CRS("+proj=utm +south +zone=33 +ellps=WGS84"))
AG_lxy <- lxy.nn.add(AG_lxy, s=0, k= 50)

summary(AG_lxy)
# testing k's 
# creating 6 different hullsets with *k* values of 9, 12, 15, 18, 21, and 24
# we are still setting s=0 because we are not considering the temporal component.

AG_lhs <- lxy.lhs(AG_lxy, k=3*3:15, s=0)
plot(AG_lhs, hulls=TRUE, figs.per.page=6) # look for no swiss cheese?

# or create isos look for asymptoting area?
AG_isos <- lhs.iso.add(AG_lhs)

lhs.plot.isoarea(AG_isos)



