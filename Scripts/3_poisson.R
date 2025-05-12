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

## 1.3 Sampling locations ####
ggplot() + 
  geom_sf(data = gorillas_sf$boundary, fill = "lightgray") +
  geom_sf(data = gorillas_sf$nests, alpha = 0.2) + 
  geom_sf(data = gorillas_sf$plotsample$counts, aes(col = count), size = 3) +
  theme_bw()

## 1.4 Covars ####
covars <- rast("Data/covars.grd")
plot(covars)

ggplot() + 
  geom_spatraster(data = covars, aes(fill = elev)) +
  scale_fill_viridis_c() +
  geom_sf(data = gorillas_sf$nests) + 
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
form1 <- count ~  Intercept(1)  + # change of response variable here! 
  Eff.elevation(covars$elev, model = "linear") 

### Run model ####
m1 <- bru(components = form1,
          data = gorillas_sf$plotsample$counts,
          family = "poisson", # change of family here!!
          domain =  list(geometry = mesh_better)) # no samplers as all sampled

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


## 3.3 Model with factor ####

form2 <- count ~  Intercept(1)  +
  Eff.elevation(covars$elev, model = "linear") + 
  Eff.vegetation(covars$vegetation, model = "factor")

m2 <- bru(components = form2,
          data = gorillas_sf$plotsample$counts,
          family = "poisson", 
          domain =  list(geometry = mesh_better))

summary(m2)

pred2 <- predict(m2, newdata = newdf, 
                 ~ exp(Intercept + 
                         Eff.elevation + 
                         Eff.vegetation))

ggplot() + 
  gg(data = pred2, aes(fill = q0.5), geom = "tile") +
  scale_fill_viridis_c() +
  theme_bw()

## 3.3 Model with  spde ####

### Define SPDE priors ####

pcmatern <- inla.spde2.pcmatern(mesh_better, 
                                prior.range = c(2, 0.1),                                
                                prior.sigma = c(1, 0.01)) 

### Run model ####

form3 <- count ~  Intercept(1)  +
  # Eff.elevation(covars$elev, model = "linear") + 
  Eff.vegetation(covars$vegetation, model = "factor_full") +
  Eff.spde(geometry, model = pcmatern)

m3 <- bru(components = form3,
          data = gorillas_sf$plotsample$counts,
          family = "poisson", 
          domain =  list(geometry = mesh_better))

summary(m3)

### Predict ####

# OVerall prediction
pred3 <- predict(m3, newdata = newdf,
                 ~ exp(Intercept + 
                         # Eff.elevation + 
                         Eff.vegetation +
                         Eff.spde))

ggplot() + 
  gg(data = pred3, aes(fill = q0.5), geom = "tile") +
  scale_fill_viridis_c() +
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


# Evaluate covariate effect 

veg.pred <- predict(
  m3,
  n.samples = 100,
  newdata = data.frame(veg_var = seq(1:6)),
  formula = ~ Eff.vegetation_eval(veg_var)) 

eval.veg <- ggplot(veg.pred) +
  geom_point(aes(x = veg_var, y = q0.5)) +
  geom_errorbar(aes(x = veg_var,
                  ymin = q0.025,
                  ymax = q0.975)) + 
  scale_x_continuous(breaks = 1:6, 
                     labels = c("Colonising", "Disturbed", "Grassland", 
                                "Primary", "Secondary", "Transition")) +
  labs(x = "Vegetation type", y = "Effect") + 
  theme_bw()

pred_nests <- predict(m3, fm_int(mesh_better, gorillas_sf$boundary),
                      ~ sum(weight*exp(Intercept + 
                                         # Eff.elevation +
                                         Eff.vegetation +
                                         Eff.spde)))
nrow(gorillas_sf$nests)
