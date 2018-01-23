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

# Kernel UD -- quick!
AG_kernel <- kernelUD(AG_points)
AG_UD 

# LoCoh - a
## Create convex hulls from the maximum number of nearest neighbors such that
## the sum of their distances is less than or equal to a
AG_LoCoh <- LoCoH.k(AG_points, a = 500, unin = "m", unout = "m2")  # algorithm takes a little, be patient


