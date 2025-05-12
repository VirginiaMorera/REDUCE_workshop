# 0. House keeping ####
rm(list = ls())
library(inlabru)
library(INLA)
library(tidyverse)
library(ggplot2)
library(sf)
library(terra)
library(tidyterra)
library(patchwork)

# 1. Load and explore data ####
data("gorillas_sf")
names(gorillas_sf)

## 1.1. Nest data ####
str(gorillas_sf$nests)

ggplot() + 
  geom_sf(data = gorillas_sf$nests, aes(col = group)) + 
  theme_bw()

## 1.2 Boundary ####
str(gorillas_sf$boundary)

ggplot() + 
  geom_sf(data = gorillas_sf$boundary, fill = "lightgray") +
  geom_sf(data = gorillas_sf$nests, aes(col = group)) + 
  theme_bw()

## 1.3 Sampling locations ####
names(gorillas_sf$plotsample)

ggplot() + 
  geom_sf(data = gorillas_sf$boundary, fill = "lightgray") +
  geom_sf(data = gorillas_sf$nests) + 
  geom_sf(data = gorillas_sf$plotsample$plots, fill = NA, col = "red") +
  geom_sf(data = gorillas_sf$plotsample$nests, col = "orange") +
  geom_sf(data = gorillas_sf$plotsample$counts, aes(col = count)) +
  theme_bw()

ggplot() + 
  geom_sf(data = gorillas_sf$boundary, fill = "lightgray") +
  # geom_sf(data = gorillas_sf$nests) + 
  geom_sf(data = gorillas_sf$plotsample$plots, fill = NA, col = "red") +
  geom_sf(data = gorillas_sf$plotsample$nests, col = "orange") +
  # geom_sf(data = gorillas_sf$plotsample$counts, aes(col = count)) +
  theme_bw()

## 1.4 Covars ####
# library(spatstat)
# data("gorillas")
# elev <- rast(gorillas.extra$elevation)
# aspect <- rast(gorillas.extra$aspect)
# heat <- rast(gorillas.extra$heat)
# slopeangle <- rast(gorillas.extra$slopeangle)
# slopetype <- rast(gorillas.extra$slopetype)
# vegetation <- rast(gorillas.extra$vegetation)
# waterdist <- rast(gorillas.extra$waterdist)
# 
# covars <- c(elev, aspect, heat, slopeangle, slopetype, vegetation, waterdist)
# names(covars) <- c("elev", "aspect", "heat", "slopeangle", "slopetype",
#                     "vegetation", "waterdist")
# crs(covars) <- "epsg:32632"
# covars <- project(covars, crs(gorillas_sf$nests))
# writeRaster(covars, file = "Data/covars.grd",  overwrite = T)

covars <- rast("Data/covars.grd")
plot(covars)


ggplot() + 
  geom_spatraster(data = covars, aes(fill = elev)) +
  scale_fill_viridis_c() +
  geom_sf(data = gorillas_sf$nests) + 
  theme_bw()


## 1.5 Mesh ####
ggplot() + 
  gg(gorillas_sf$mesh) + 
  geom_sf(data = gorillas_sf$boundary, col = "red", fill = NA) + 
  theme_bw()

# 2. Build a better mesh ####

mesh_better <- fm_mesh_2d_inla(
  boundary = gorillas_sf$boundary,
  max.edge = c(0.1,0.5),  # this controls the size of the triangles
  crs = st_crs(gorillas_sf$nests)) #inlabru is very particular with crs

ggplot() + 
  gg(mesh_better) + 
  geom_sf(data = gorillas_sf$boundary, col = "red", fill = NA) + 
  theme_bw()

# 3. Model ####

## 3.1 Very simple model ####

### Formula ####
form1 <- geometry ~  Intercept(1)  +
  Eff.elevation(covars$elev, model = "linear") 

### Run model ####
m1 <- bru(components = form1,
          data = gorillas_sf$plotsample$nests,
          family = "cp", 
          domain =  list(geometry = mesh_better))

summary(m1)

### Set up dataset for predictions ####
newdf <- fm_pixels(mesh_better,
                   dims = c(100, 100),
                   mask = gorillas_sf$boundary,
                   format = "sf")

### Predict ####
pred1 <- predict(m1, newdata = newdf, 
                 ~ exp(Intercept + Eff.elevation))

ggplot() + 
  gg(data = pred1, aes(fill = q0.5), geom = "tile") +
  # geom_sf(data = gorillas_sf$nests) +
  # geom_sf(data = gorillas_sf$plotsample$nests, col = "orange") +
  scale_fill_viridis_c() +
  theme_bw()

## 3.2 Model with samplers ####

form1 <- geometry ~  Intercept(1)  +
  Eff.elevation(covars$elev, model = "linear") 

m1.2 <- bru(components = form1,
          data = gorillas_sf$plotsample$nests,
          family = "cp", 
          samplers = gorillas_sf$plotsample$plots,
          domain =  list(geometry = mesh_better))

summary(m1.2)

pred1.2 <- predict(m1.2, newdata = newdf, 
                   ~ exp(Intercept + 
                           Eff.elevation))

ggplot() + 
  gg(data = pred1.2, aes(fill = q0.5), geom = "tile") +
  scale_fill_viridis_c() +
  geom_sf(data = gorillas_sf$nests) +
  geom_sf(data = gorillas_sf$plotsample$nests, col = "orange") +
  theme_bw()

### Compare with previous ####

ggplot() + 
  gg(data = pred1.2, aes(fill = q0.5), geom = "tile") +
  scale_fill_viridis_c() +
  theme_bw() + 

ggplot() + 
  gg(data = pred1, aes(fill = q0.5), geom = "tile") +
  scale_fill_viridis_c() +
  theme_bw() 

## 3.3 Model with factor ####

form2 <- geometry ~  Intercept(1)  +
  Eff.elevation(covars$elev, model = "linear") + 
  Eff.vegetation(covars$vegetation, model = "factor")

m2 <- bru(components = form2,
          data = gorillas_sf$plotsample$nests,
          family = "cp", 
          samplers = gorillas_sf$plotsample$plots,
          domain =  list(geometry = mesh_better))

summary(m2)

pred2 <- predict(m2, newdata = newdf, 
                 ~ exp(Intercept + 
                         Eff.elevation + 
                         Eff.vegetation))

ggplot() + 
  gg(data = pred2, aes(fill = q0.5), geom = "tile") +
  scale_fill_viridis_c() +
  geom_sf(data = gorillas_sf$nests) +
  geom_sf(data = gorillas_sf$plotsample$nests, col = "orange") +
  theme_bw()

## 3.3 Model with  spde ####

### Define SPDE priors ####

pcmatern <- inla.spde2.pcmatern(mesh_better, 
                                prior.range = c(5, 0.01),                                
                                prior.sigma = c(0.1, 0.01)) 

### Run model ####

form3 <- geometry ~  Intercept(1)  +
  Eff.elevation(covars$elev, model = "linear") + 
  # Eff.vegetation(covars$vegetation, model = "factor") + 
  Eff.spde(geometry, model = pcmatern)

m3 <- bru(components = form3,
          data = gorillas_sf$plotsample$nests,
          family = "cp", 
          samplers = gorillas_sf$plotsample$plots,
          domain =  list(geometry = mesh_better))

summary(m3)

### Predict ####

# OVerall prediction
pred3 <- predict(m3, newdata = newdf,
                 ~ exp(Intercept + 
                         Eff.elevation + 
                         # Eff.vegetation + 
                         Eff.spde))

ggplot() + 
  gg(data = pred3, aes(fill = q0.5), geom = "tile") +
  scale_fill_viridis_c() +
  geom_sf(data = gorillas_sf$nests) +
  geom_sf(data = gorillas_sf$plotsample$nests, col = "orange") +
  theme_bw()

# SPDE effect
pred3.1 <- predict(m3, newdata = newdf,
                 ~ exp(Eff.spde))

ggplot() + 
  gg(data = pred3, aes(fill = q0.5), geom = "tile") +
  scale_fill_viridis_c() +
  # geom_sf(data = gorillas_sf$nests) +
  # geom_sf(data = gorillas_sf$plotsample$nests, col = "orange") +
  theme_bw() + ggtitle("Prediction") +

ggplot() + 
  gg(data = pred3.1, aes(fill = q0.5), geom = "tile") +
  scale_fill_viridis_c() +
  # geom_sf(data = gorillas_sf$nests) +
  # geom_sf(data = gorillas_sf$plotsample$nests, col = "orange") +
  theme_bw() + ggtitle("SPDE effect")


## 3.4 Make covariate effect non-linear ####

### Mesh for elevation ####
mesh1D_elev <- inla.mesh.1d(seq(min(covars$elev[], na.rm = T)-1, 
                                max(covars$elev[], na.rm = T)+1, 
                                length.out = 20),
                            degree = 2)

### Priors for elevation ####
diff(range(covars$elev[], na.rm = T))/3
pcmatern1d <- inla.spde2.pcmatern(mesh1D_elev,
                                  prior.range = c(100, 0.1), # 1 third range mesh
                                  prior.sigma = c(2, 0.1))

### New priors for SPDE
pcmatern2d <- inla.spde2.pcmatern(mesh_better,
                                  prior.range = c(2, 0.1), # 1 third range mesh
                                  prior.sigma = c(1, 0.01))

### Run model ####
form4 <- geometry ~  Intercept(1)  +
  Eff.elevation(covars$elev, model = pcmatern1d) + 
  # Eff.vegetation(covars$vegetation, model = "factor") + 
  Eff.spde(geometry, model = pcmatern2d)

m4 <- bru(components = form4,
          data = gorillas_sf$plotsample$nests,
          family = "cp",
          samplers = gorillas_sf$plotsample$plots,
          domain =  list(geometry = mesh_better))

summary(m4)

### Predict from model ####

# Spatial prediction
pred4 <- predict(m4, newdata = newdf,
                 ~ exp(Intercept + 
                         Eff.elevation +
                         # Eff.vegetation +
                         Eff.spde))

ggplot() + 
  gg(data = pred4, aes(fill = q0.5), geom = "tile") +
  scale_fill_viridis_c() +
  # geom_sf(data = gorillas_sf$nests) +
  # geom_sf(data = gorillas_sf$plotsample$nests, col = "orange") +
  theme_bw()

# Evaluate covariate effect 

elev.pred <- predict(
  m4,
  n.samples = 100,
  newdata = data.frame(elevation_var = seq(min(covars$elev[], na.rm = T), 
                                           max(covars$elev[], na.rm = T),
                                           length.out = 100)),
  formula = ~ Eff.elevation_eval(elevation_var)) 

eval.elev <- ggplot(elev.pred) +
  geom_line(aes(x = elevation_var, y = q0.5)) +
  geom_ribbon(aes(x = elevation_var,
                  ymin = q0.025,
                  ymax = q0.975),
              alpha = 0.2) + 
  labs(x = "Elevation", y = "Effect") + 
  theme_bw()

# count predicted number of nests

pred_nests <- predict(m4, fm_int(mesh_better, gorillas_sf$boundary),
                      ~ sum(weight*exp(Intercept + 
                              Eff.elevation +
                              # Eff.vegetation +
                              Eff.spde)))

nrow(gorillas_sf$nests)
