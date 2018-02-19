#########################################################
# Code for Reproducible SSF example for Seidel et al. 2018
# author: Eric R. Dougherty
# January 2018
#########################################################

### Set Up ###
library(raster)
library(lme4)
library(sf)
library(tidyverse)
library(adehabitatLT)
library(mapview)
library(fasterize)

### Load Data ###
zeb <- read_csv(all.pts, "AG256_2010.csv")

########## Variable Extraction ###############
#### External Data Files not provided #####

dist_roads <- raster('ENP_Predictors/Dist_PrimaryRoads.tif') %>%
crop(zebra.ext)
dist_water <- raster('ENP_Predictors/Dist_Water.tif') %>%
crop(zebra.ext)
veg_raster <- raster('ENP_Predictors/Vegetation_Crop.tif') %>%
resample(dist_roads, method='ngb') %>%
crop(zebra.ext)
NDVI_10 <- raster('ENP_Predictors/Mean_NDVI_2010.tif') %>%
resample(dist_roads, method='ngb') %>%
crop(zebra.ext)
Green_10 <- raster('ENP_Predictors/Mean_Greenness_2010.tif') %>%
resample(dist_roads, method='ngb') %>%
crop(zebra.ext)
Wet_10 <- raster('ENP_Predictors/Mean_Wetness_2010.tif') %>%
resample(dist_roads, method='ngb') %>%
crop(zebra.ext)

pred_stack_10 <- stack(dist_roads, dist_water, veg_raster, NDVI_10, Green_10, Wet_10)

#####################################################

zeb.sp <- as(zeb, "Spatial")
zeb.ltraj <- as.ltraj(zeb.sp@coords, date=zeb$date, id=zeb$ID)

# Function to sample available points based on empirical steps
# steps.ltraj - an ltraj object of the movement path
# n.avail.per.pt - number of available points to select per used point
SSF.sampling <- function(steps.ltraj, n.avail.per.pt) {
    ltraj.df <- steps.ltraj[[1]]
    step.lengths <- ltraj.df$dist
    turning.angles <- ltraj.df$rel.angle
    avail.pts <- data.frame(matrix(0,nrow(ltraj.df)*n.avail.per.pt,2))
    for (i in 1:nrow(ltraj.df)) {
        used.pt <- ltraj.df[i, 1:2]
        for (j in 1:n.avail.per.pt) {
            rand.rows <- round(runif(2,0.5,(nrow(ltraj.df)+0.5)))
            temp.step.length <- step.lengths[rand.rows[1]]
            temp.turning.angle <- turning.angles[rand.rows[2]]
            dx <- cos(temp.turning.angle) * temp.step.length
            dy <- sin(temp.turning.angle) * temp.step.length
            new.x <- used.pt[,1] + dx
            new.y <- used.pt[,2] + dy
            avail.pts[(i-1)*n.avail.per.pt+j,1] <- new.x
            avail.pts[(i-1)*n.avail.per.pt+j,2] <- new.y
        }
    }
    return(avail.pts)
}

avail.pts <- SSF.sampling(zeb.ltraj, 5) %>%
mutate(binom = 0)
used.pts <- data.frame(zeb.sp@coords) %>%
mutate(binom = 1)
colnames(used.pts) <- c("X1", "X2", "binom")

all.pts <- rbind(used.pts, avail.pts)
all.pts <- all.pts[!is.na(all.pts[,1]),]
colnames(all.pts) <- c('x', 'y', 'binom')

######################################################

all.sp <- SpatialPoints(all.pts[!is.na(all.pts[,1]), 1:2])
zeb.extract <- data.frame(raster::extract(pred_stack_10, all.sp))

all.pts <- cbind(all.pts, zeb.extract)
# write_csv(all.pts, "AG256_2010_Extract.csv")

# all.pts <- read_csv(all.pts, "AG256_2010_Extract.csv")

######################################################

zebra.model <- glm(binom ~ Dist_PrimaryRoads + Dist_Water + 
                   Mean_NDVI_2010 + Mean_Greenness_2010 + Mean_Wetness_2010,
                   data=all.pts, family=binomial(link='logit'))

summary(zebra.model)

zebra.model2 <- glm(binom ~ Dist_PrimaryRoads + Dist_Water +
                   Mean_Greenness_2010 + Mean_Wetness_2010,
                   data=all.pts, family=binomial(link='logit'))

summary(zebra.model2)

######################################################

pred10_final <- stack(dist_roads, dist_water, Green_10, Wet_10)
predictions <- raster::predict(object=pred10_final, model=zebra.model2, fun=predict)
predictions <- exp(predictions)

#pdf("AG256_Predictive_Map.pdf", width=10, height=8)
plot(predictions)
points(zeb.sp, cex=0.3, pch=19)
dev.off()


