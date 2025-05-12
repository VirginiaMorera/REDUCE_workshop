# 0. Housekeeping ####
rm(list = ls())
library(INLA)
library(inlabru)
library(mgcv) #install this if you don't have it already
library(ggplot2)
library(fmesher)
library(patchwork)

# 1. Load and explore data ####

data(Poisson2_1D)
cd <- countdata2
head(cd)

ggplot(cd) + 
  geom_point(aes(x = x, y = count)) + 
  theme_bw()

# 2. GAM approach ####

## 2.1 Fit ####
fit.gam <- gam(count ~ s(x, k = 10),
               family = poisson(),
               data = cd)

summary(fit.gam)

## 2.2 Predict ####
newdata <- data.frame(x = seq(0, 50, length = 100))

pred.gam <- predict(fit.gam, newdata = newdata, type = "response", se.fit = T)
dat4pred <- cbind(newdata, gam = pred.gam)

ggplot(dat4pred) +
  geom_line(aes(x = x, y = gam.fit), lty = 2) +
  geom_ribbon(aes(x = x, 
                  ymin = gam.fit-gam.se.fit, 
                  ymax = gam.fit+gam.se.fit), 
              fill = "gray", alpha = 0.5) +
  geom_point(data = cd, aes(x = x, y = count), col = "orange")  + 
  theme_bw()

# 3. INLA approach ####

## 3.1 Mesh ####
x <- seq(-10, 60, by = 1) # this sets mesh points - try others if you like
mesh1D <- fm_mesh_1d(x, boundary = "free")

ggplot() +
  gg(mesh1D) + 
  geom_point(data = cd, aes(x = x, y = 0), col = "orange")

## 3.2 SPDE priors ####
the_spde <- inla.spde2.pcmatern(mesh1D,
                                prior.range = c(1, 0.01),
                                prior.sigma = c(2, 0.01)
)

## 3.3 Fit ####
fit2.bru <- bru(
  count ~ Intercept(1, prec.linear = 1 / 2^2) + field(x, model = the_spde), 
  data = cd, 
  family = "poisson"
)

summary(fit2.bru)

## 3.4 Predict ####
pred2.bru <- predict(fit2.bru,
                     newdata,
                     formula =  ~ exp(field + Intercept),
                     n.samples = 1000
)


ggplot() +
  gg(pred2.bru) +
  geom_point(data = cd, aes(x = x, y = count), size = 2, col = "orange") +
  geom_line(data = dat4pred, aes(x = x, y = gam.fit), lty = 2) +
  geom_ribbon(data = dat4pred, 
              aes(x = x, 
                  ymin = gam.fit-gam.se.fit, 
                  ymax = gam.fit+gam.se.fit), 
              fill = "blue", alpha = 0.5) +
  labs(x = "x", y = "Intensity") + 
  theme_bw()
  