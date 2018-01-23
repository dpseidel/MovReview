# Behavioral Change Point analysis - MovReview
# date updated: 01/22/2018
# author: D.P. Seidel

# libraries
library(tidyverse)
library(sf)
library(sp)
library(bcpa)

# read cleaned data  - already formated as a ltraj dataframe. 
AG256 <- read_csv("AG256_2010.csv")
head(AG256)

# isolate lat and long points for adehabitatHR home range metrics
AG_sf <- st_as_sf(AG256, coords=c("x","y"), na.fail=FALSE, crs = 32733) # UTM Zone 33S
