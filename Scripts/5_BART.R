# 0. House keeping ####
rm(list = ls())
library(embarcadero)
library(tidyverse)
library(sf)
library(terra)
library(predicts)

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

# Select needed covars
xvars <- names(model_df)[c(3,4,5)]
  
sdm <-bart.step(x.data = model_df[,xvars],
                y.data = model_df[,"pres"],
                full = TRUE,
                quiet = F, 
                iter.step = 10, 
                iter.plot = 10, 
                tree.step = 2)
summary(sdm)

# 3. Obtain outputs  ####

prediction <- predict(object = sdm,
                      x.layers = raster::stack(covars),
                      quantiles =c(0.025, 0.975),
                      splitby = 20,
                      quiet = TRUE)
plot(prediction)
names(prediction) <- c("Mean", "q2.5%", "q97.5%")
prediction <- rast(prediction)
plot(prediction)

## Variable importance ####

#' We can now generate a plot of the importance of the included variables

varimp_list <- list()
for(i in seq_along(model_list)) {
  # i = 1
  
  varimp <- varimp(model_list[[i]], plots = F)
  
  varimp_ordered <- varimp %>% 
    arrange(-varimps) %>% 
    mutate(iteration = names(model_list)[i])
  
  varimp_list[i] <- list(varimp_ordered)
}

varimp_pops <- map_df(varimp_list, bind_rows)
row.names(varimp_pops) <- NULL

varimp_pops <- varimp_pops %>% 
  dplyr::select(iteration, Variable = names, Importance = varimps) %>% 
  mutate(Variable = recode(Variable, sst = "Sea Surface Temperature", chla_var = "Chlorophyll A Variability", 
                           logchla_lag3 = "Chlorophyll A", bati = "Bathimetry", 
                           sst_grad = "Sea Surface Temperature gradient", sal = "Salinity", 
                           slope = "Slope2"))

write.csv(varimp_pops, 
            file = "embarcadero_SDMs/final_outputs/colony_models/colony_varimp.csv")
  

varimp_pops %>% 
  group_by(Variable) %>% 
  summarise(mean = mean(Importance), 
            sd = sd(Importance)) %>% 
  ggplot() + 
  geom_point(aes(x = Variable, y = mean)) + 
  geom_errorbar(aes(x = Variable, ymin = mean-sd, ymax = mean+sd), width=.2) + 
  theme_bw()


## Partial response ####
#' These plots show the mean effect of each variable when measured at all other values of the other predictors, for all the values of the variable of interest. This will show us at which values of the covariate the dependence is higher.
#' With CI you can add credible intervals, and with trace = TRUE you can see the dependence plot for each of the trees (remember BARTs are regression trees), with the thicker line representing the mean of all the trees. 

for(i in seq_along(model_list)) {
  # i = 1
  sdm <- model_list[[i]]
  
  tmp_partial <- partial(sdm,
                         trace = FALSE,
                         ci = TRUE,
                         equal = TRUE,
                         smooth = 10, 
                         panels = F)
  
  saveRDS(tmp_partial, 
          file = paste0("embarcadero_SDMs/outputs/partial_plots_", names(model_list)[i], ".RDS"))
}


