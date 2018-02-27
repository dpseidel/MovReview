# Home Range analysis and comparison - MovReview
# date updated: 01/22/2018
# author: D.P. Seidel

# libraries
library(tidyverse)
library(sf)
library(sp)
library(adehabitatHR)
library(tlocoh)

############# Data Prep #############
# read cleaned data  - already formated as a ltraj dataframe. 
AG256 <- read_csv("AG256_2010.csv")
head(AG256)

# isolate lat and long points for adehabitatHR home range metrics
AG_sf <- st_as_sf(AG256, coords=c("x","y"), 
                  na.fail=FALSE, crs = 32733) # UTM Zone 33S

# filter to just the western range
AG_wet <- AG_sf %>% 
  filter(date < dmy("15-04-2010"), date > dmy("15-02-2010"))

AG_points <- AG_wet %>% 
  na.omit %>% 
  st_coordinates %>% 
  SpatialPoints() # Adehabitat requires Spatial objects. 

# plot
plot(AG_points)

########### MCP -- using adehabitatHR package ##############
AG_mcp_100 <- mcp(AG_points, 100)
plot(AG_mcp_100)
points(AG_points, col = "green", cex= .3)

############## Kernel UD -- using adehabitatHR package ##############
AG_kernel <- kernelUD(AG_points)
image(AG_kernel, col=grey(seq(1,0,length=15)))
#plot(getverticeshr(AG_kernel, 95), add=TRUE, lwd=2)
AG_kud_95 <- getverticeshr(AG_kernel, 95)
plot(AG_kud_95)
points(AG_points, col = "green", cex= .3)


################# LoCoh - a #######################
# using package t-locoh with s=0 is much faster than adehabitatHR `locoh.a` function
AG_coords <- AG_wet %>% na.omit %>% st_coordinates 
AG_time <- AG_wet %>% na.omit %>% pull(date)

AG_lxy <- xyt.lxy(xy= AG_coords, dt= AG_time, id="AG256", 
                  proj4string=CRS("+proj=utm +south +zone=33 +ellps=WGS84"))
summary(AG_lxy)

AG_lxy_a <- lxy.nn.add(AG_lxy, s=0, a = 80000)
AG_lhs_a <- lxy.lhs(AG_lxy_a, a=seq(30000,80000,5000), s=0)
AG_isos_a <- lhs.iso.add(AG_lhs_a)
lhs.plot.isoarea(AG_isos_a)
lhs.plot.isoear(AG_isos_a)

AG_lhs_a75000 <- lhs.select(AG_lhs_a, a=75000)
AG_iso_a75000 <- lhs.iso.add(AG_lhs_a75000)
AG_lhs_a75000_95poly <- AG_iso_a75000[[1]]$isos[[1]]$polys[5]
plot(AG_lhs_a75000_95poly)


##################### Figures #####################
# all together plot
plot(AG_kud_95, col="orange")
#points(AG_points, col = "grey", cex= .05)
plot(AG_mcp_100, add=TRUE, col = )
#points(AG_points, col = "grey", cex= .05)
plot(AG_lhs_a75000_95poly, add = TRUE, col = "blue")
points(AG_points, col = "grey", cex= .05)

# individual plots
hr <- st_as_sf(AG_mcp_100) %>% st_set_crs(32733)
kud <- st_as_sf(AG_kud_95) %>% st_set_crs(32733)
locoh <- st_as_sf(AG_lhs_a75000_95poly)

### gridless
#png(file = "Figures/MCP.png")
plot(st_geometry(hr), col="#9ecae1")
points(st_coordinates(AG_wet), cex=.1)
#dev.off()

#png(file = "Figures/KUD.png")
plot(st_geometry(kud), col="#9ecae1")
points(st_coordinates(AG_wet), cex=.1)
#dev.off()

#png(file = "Figures/LoCoh.png")
plot(st_geometry(locoh), col="#9ecae1")
points(st_coordinates(AG_wet), cex=.1)
#dev.off()
