# 0. Housekeeping ####
rm(list = ls())
library(INLA)
library(inlabru)
library(ggplot2)
library(sf)

# 1. Load and explore data
data(shrimp)
boundary <- st_read("Data/shrimp_boundary.shp")

## 1.1 Fisheries data ####
str(shrimp$hauls)

ggplot(shrimp$hauls) + 
  geom_point(aes(x = catch, y = landing)) + 
  theme_bw()

ggplot(shrimp$hauls) + 
  geom_point(aes(x = depth, y = catch)) + 
  theme_bw()


ggplot() + 
  geom_sf(data = shrimp$hauls, aes(col = depth, size = catch)) + 
  theme_bw()

## 1.2 Mesh ####

ggplot() + 
  gg(shrimp$mesh) + 
  geom_sf(data = boundary, fill = "orange", alpha = 0.2) +
  geom_sf(data = shrimp$hauls, aes(col = depth, size = catch)) + 
  theme_bw()


# 2. Modelling process ####

## 2.1 Setting up spde parameters ####

matern <- inla.spde2.pcmatern(shrimp$mesh, 
                              # alpha = 3/2,
                              prior.range = c(60, 0.1),
                              prior.sigma = c(0.2, 0.1))

maternB <- inla.spde2.pcmatern(shrimp$mesh,
                              # alpha = 3/2,
                              prior.range = c(100, 0.1),
                              prior.sigma = c(2, 0.1))

cmp = ~ spde(geometry, model = matern) +
  spdeB(geometry, model = maternB) +
  scaling(1,prec.linear=1,marginal=bru_mapper_marginal(qexp,rate = 1))+
  Eff.depth(depth, model = "linear") + 
  lgcpIntercept(1) +
  Intercept(1)

# The catch data has some non-integer value, so Poisson is inappropriate
# Using the pseudo-model "xpoisson" that allows non-integer observations instead
lik1 <- like(data =  shrimp$hauls,
             family = "xpoisson",
             formula = catch ~ spde + Eff.depth + Intercept)

lik2 <- like(data =  shrimp$hauls,
             family = "cp",
             domain = list(geometry = shrimp$mesh),
             formula = geometry ~ scaling * spde + spdeB + lgcpIntercept)

# Even though both likelihoods are log-linear, INLA needs some
# assistance in finding the modes; bru_max_iter = 3 seems sufficient here, but
# allowing more iterations is safer; this iteration stops at 4.
fit <- bru(components = cmp,
           lik1,
           lik2,
           options = list(bru_max_iter = 20,
                          bru_verbose = 4))

summary(fit)

newdf <- fm_pixels(shrimp$mesh,
                   dims = c(100, 100),
                   mask = boundary,
                   format = "sf")

fish.intensity <- predict(fit, newdf, ~ exp(spde + Intercept + Eff.depth + 
                                            lgcpIntercept + spdeB + scaling))

ggplot() +
  gg(fish.intensity, aes(fill = q0.5), geom = "tile") +
  gg(shrimp$hauls, size = 0.5) + 
  scale_fill_viridis_c() + 
  theme_bw()
