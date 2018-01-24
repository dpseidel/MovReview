# Behavioral Change Point analysis - MovReview
# date updated: 01/22/2018
# author: D.P. Seidel

# libraries
library(tidyverse)
library(sf)
library(moveHMM)

# read cleaned data  - already formated as a ltraj dataframe. 
AG256 <- read_csv("AG256_2010.csv") 
# make it a spatial object
AG_sf <- st_as_sf(AG256, coords=c("x","y"), na.fail=FALSE, crs = 32733) # UTM Zone 33S
head(AG_sf)

AG_coords <- st_coordinates(AG_sf) %>% as.tibble(.)/1000 # convert to km for interpretability

data <- prepData(AG_coords, type="UTM", coordNames =  c("X","Y"))

hist(data$step)
hist(data$angle)
summary(data)
plot(data)

# fit HMM
mu0 <- c(0.1,1) # step mean (two parameters: one for each state)
sigma0 <- c(0.1,1) # step SD
zeromass0 <- c(0.1,0.05) # step zero-mass
stepPar0 <- c(mu0,sigma0,zeromass0)

angleMean0 <- c(pi,0) # angle mean
kappa0 <- c(1,1) # angle concentration
anglePar0 <- c(angleMean0,kappa0)

m <- fitHMM(data=data, nbStates=2, stepPar0=stepPar0, anglePar0=anglePar0, formula=~1)

m
plot(m)

plotStates(m) # need to subset to make this valueable. 
