rm(list = ls())
library(INLA)
library(inlabru)
library(ggplot2)

data(shrimp)

mesh <- shrimp$mesh

hauls <- shrimp$hauls


ggplot() + 
  gg(mesh) + 
  geom_sf(data = hauls, aes(col = catch)) + 
  theme_bw()


matern <- inla.spde2.pcmatern(mesh, 
                              alpha = 3/2,
                              prior.range = c(30, 0.1),
                              prior.sigma = c(1, 0.1))

maternB <- inla.spde2.pcmatern(mesh, 
                              alpha = 3/2,
                              prior.range = c(100, 0.1),
                              prior.sigma = c(5, 0.1))

cmp = ~ spde(geometry, model = matern) +
  spdeB(geometry, model = maternB) +
  scaling(1,prec.linear=1,marginal=bru_mapper_marginal(qexp,rate = 1))+
  lgcpIntercept(1) +
  Intercept(1)

# The catch data has some non-integer value, so Poisson is inappropriate
# Using the pseudo-model "xpoisson" that allows non-integer observations instead
lik1 <- like(data =  shrimp$hauls,
             family = "xpoisson",
             formula = catch ~ spde + Intercept)

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
           options = list(bru_max_iter = 30,
                          bru_verbose = 4))

summary(fit)

pxl <- fm_int(shrimp$mesh)
fish.intensity <- predict(fit, pxl, ~ exp(spde + Intercept))

ggplot() +
  gg(fish.intensity, aes(col = q0.5)) +
  gg(shrimp$hauls, size = 0.5)
