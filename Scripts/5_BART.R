# 0. House keeping ####
rm(list = ls())
library(embarcadero)
library(tidyverse)
library(sf)
library(terra)
library(predicts)
library(tidyterra)
'%!in%' <- function(x,y)!('%in%'(x,y))

# 1. Load data ####

## 1.1 Bird data ####
ven_df <- read.csv("Data/Veneguera_wintering.csv") 

ven_sf <- ven_df %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326)

map <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf")

ggplot() + 
  geom_sf(data = map, fill = "lightgray", col = "darkgray") + 
  geom_sf(data = ven_sf) + 
  theme_bw()


## 1.2 Covariates ####
covars <- rast("Data/BART_covars.grd")

plot(covars)

ggplot() + 
  geom_spatraster(data = covars, aes(fill = bati)) +
  geom_sf(data = map, fill = "lightgray", col = "darkgray") + 
  geom_sf(data = ven_sf) + 
  scale_fill_viridis_c() +
  coord_sf(xlim = ext(covars)[1:2], ylim = ext(covars)[3:4]) +
  theme_bw()

## 1.3 Pseudoabsences ####

# I strongly suggest using an equal number of presences and absences in your 
# training data. You can experiment with the demo data by changing “nrow(ticks)” 
# to “5000” below if you want to see some odd model behavior

absences_df <- as.data.frame(backgroundSample(covars$bati, nrow(ven_sf)))

absences_sf <- absences_df %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326)

ggplot() + 
  geom_spatraster(data = covars, aes(fill = bati)) +
  geom_sf(data = map, fill = "lightgray", col = "darkgray") + 
  geom_sf(data = absences_sf, col = "orange") + 
  geom_sf(data = ven_sf) + 
  scale_fill_viridis_c() +
  coord_sf(xlim = ext(covars)[1:2], ylim = ext(covars)[3:4]) +
  theme_bw()

## 1.4 Extract covars

covars_ven <- extract(covars, ven_sf)

str(covars_ven)

pres <- bind_cols(ven_df, covars_ven) %>% 
  select(-ID) %>% 
  mutate(pres = 1)

str(pres)

covars_abs <- extract(covars, absences_sf)

abs <- bind_cols(absences_df, covars_abs) %>% 
  select(-ID) %>% 
  mutate(pres = 0)

model_df <- bind_rows(pres, abs) %>% 
  drop_na()


# 2. Run model  ####

# 2.1 Select needed covars ####
xvars <- names(model_df)[c(3,4,5)]
  
# 2.2 Run model and step-wise variable selection in one step ####
# pick better values for iterations and steps

sdm <-bart.step(x.data = model_df[,xvars],
                y.data = model_df %>% select(pres),
                full = TRUE,
                iter.step = 10, 
                iter.plot = 10, 
                tree.step = 2)
summary(sdm)

# 3. Obtain outputs  ####

## 3.1 Predicted presence probability ####
prediction <- predict(object = sdm,
                      x.layers = raster::stack(covars),
                      quantiles =c(0.025, 0.5, 0.975),
                      splitby = 20,
                      quiet = TRUE)
# plot prediction
plot(prediction)
names(prediction) <- c("Mean", "q2.5%", "median", "q97.5%")
prediction <- rast(prediction)
plot(prediction)

# plot binary pres/absence model 
mean_pred <- prediction$Mean

# here use cutoff value from model summary to optimise, or pick a value according to your project goal
mean_pred <- mean_pred %>% 
  mutate(Mean_bin = if_else(Mean > 0.5477886 , 1, 0))

ggplot() + 
  geom_spatraster(data = mean_pred, aes(fill = Mean_bin))  + 
  scale_fill_viridis_c()  + 
  theme_bw() + 
  coord_equal()

## 3.2 Variable importance ####

varimp <- varimp(sdm, plots = T)


## 3.3 Partial response ####

partial_plots <- partial(sdm,
                         trace = FALSE,
                         ci = TRUE,
                         equal = TRUE,
                         smooth = 10, 
                         panels = F)

partial_plots[[1]] + partial_plots[[2]] + 
  partial_plots[[3]] + 
  plot_layout(ncol = 2)
saveRDS(partial_plots, file = "Data/Partial_plots.RDS")

## 3.4 2D partial response ####
p <- pd2bart(sdm,
             xind = c("bati", "sst"),
             pl = TRUE)

## 3.5 Spatial partial response ####
sp.dep.sst <- spartial(sdm, stack(covars), x.vars = 'sst', equal = TRUE)

plot(rast(sp.dep.sst))
