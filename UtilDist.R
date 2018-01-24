# Home Range analysis and comparison - MovReview
# date updated: 01/22/2018
# author: D.P. Seidel

# libraries
library(tidyverse)
library(sf)
library(sp)
library(adehabitatHR)
library(tlocoh)


# read cleaned data  - already formated as a ltraj dataframe. 
AG256 <- read_csv("AG256_2010.csv")
head(AG256)

# isolate lat and long points for adehabitatHR home range metrics
AG_sf <- st_as_sf(AG256, coords=c("x","y"), na.fail=FALSE, crs = 32733) # UTM Zone 33S
AG_points <- AG_sf %>% na.omit %>% st_coordinates %>% SpatialPoints() # Adehabitat requires Spatial objects. 

# plot
plot(AG_points)

# MCP
AG_mcp_100 <- mcp(AG_points, 100)
plot(AG_mcp_100)
points(AG_points, col = "green", cex= .3)

# Kernel UD -- using adehabitatHR package
AG_kernel <- kernelUD(AG_points)
image(AG_kernel, col=grey(seq(1,0,length=15)))
#plot(getverticeshr(AG_kernel, 95), add=TRUE, lwd=2)
AG_kud_95 <- getverticeshr(AG_kernel, 95)
plot(AG_kud_95)
points(AG_points, col = "green", cex= .3)



# LoCoh - k 
## Create convex hulls from the maximum number of nearest neighbors such that
## the sum of their distances is less than or equal to a
# using package t-locoh with s=0 is much faster than adehabitatHR `locoh.a` function
AG_coords <- AG_sf %>% na.omit %>% st_coordinates 
AG_time <- AG_sf %>% na.omit %>% pull(date)

AG_lxy <- xyt.lxy(xy= AG_coords, dt= AG_time, id="AG256", proj4string=CRS("+proj=utm +south +zone=33 +ellps=WGS84"))
AG_lxy <- lxy.nn.add(AG_lxy, s=0, k = 45)

summary(AG_lxy)
# testing k's 
# creating different hullsets with ascending *k* values 9-45 by 3. 
# we are still setting s=0 because we are not considering the temporal component.
AG_lhs <- lxy.lhs(AG_lxy, k=3*3:15, s=0)
plot(AG_lhs, hulls=TRUE, figs.per.page=6) # look for no swiss cheese?

# or create isos look for asymptoting area, make sure no big jumps.
AG_isos <- lhs.iso.add(AG_lhs)
lhs.plot.isoarea(AG_isos)
# look for k's that don't have high edge to area ration. 
lhs.plot.isoear(AG_isos)

# Ok, let's pick 30 to be conservative 
AG_lhs_k30 <- lhs.select(AG_lhs, k=30)
AG_iso_k30 <- lhs.iso.add(AG_lhs_k30)

# pick out the 95% polygon.
AG_lhs_k30_95poly <- AG_iso_k30$AG256.pts10791.k30.s0.kmin0$isos$`iso.srt-area.iso-q.h10791.i5`$polys[5,]
plot(AG_lhs_k30_95poly)

# compared to Locoh.a, 
AG_lxy_a <- lxy.nn.add(AG_lxy, s=0, a = 40000)
AG_lhs_a <- lxy.lhs(AG_lxy_a, a=seq(20000,40000,2000), s=0)
AG_isos_a <- lhs.iso.add(AG_lhs_a)
lhs.plot.isoarea(AG_isos_a)
lhs.plot.isoear(AG_isos_a)

AG_lhs_a36000 <- lhs.select(AG_lhs_a, a=36000)
AG_iso_a36000 <- lhs.iso.add(AG_lhs_a36000)
AG_lhs_a36000_95poly <- AG_iso_a36000$AG256.pts10791.a36000.s0.kmin0$isos$`iso.srt-nep.iso-q.h10791.i5`$polys[5,]
plot(AG_lhs_a36000_95poly)

# For figure

plot(AG_kud_95, col="orange")
#points(AG_points, col = "grey", cex= .05)

plot(AG_mcp_100, add=TRUE, col = )
#points(AG_points, col = "grey", cex= .05)

plot(AG_lhs_a36000_95poly, add = TRUE, col = "blue")
points(AG_points, col = "grey", cex= .05)

# ggplots
hr <- st_as_sf(AG_mcp_100) %>% st_set_crs(32733)
kud <- st_as_sf(AG_kud_95) %>% st_set_crs(32733)
locoh <- st_as_sf(AG_lhs_a36000_95poly)

my_theme <- theme_bw() + theme(panel.ontop=TRUE, panel.background=element_blank())

ggplot(hr) + 
  geom_sf(fill="goldenrod") + 
  theme_light() + 
  geom_sf(data=AG_sf, size = .1) 
ggsave("MCP.png")

ggplot() + 
  geom_sf(data = kud, fill="goldenrod") + 
  theme_light() + 
  geom_sf(data=AG_sf, size = .1)
ggsave("KUD.png")

ggplot() + 
  geom_sf(data = locoh, fill = "goldenrod") + 
  theme_light() + 
  geom_sf(data=AG_sf, size = .1)
ggsave("Locoh.png")


