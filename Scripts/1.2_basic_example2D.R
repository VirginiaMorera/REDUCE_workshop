# 0. Housekeeping ####
rm(list = ls())

library(rnaturalearth)
library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
library(predicts)
library(corrplot)
library(GGally)
library(gstat)

# 1. Load data ####

## 1.1. Load and clean ####
data <- read.csv("Data/2D_example_data.csv") 
map <- ne_countries(scale = "medium", returnclass = "sf")
covars <- rast("Data/preds_2D_example.grd")
names(covars) <- c("Chl", "Sal", "SST")

data_sf <-  data %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326) %>% 
  mutate(pres = as.factor(pres))

ggplot() + 
  geom_sf(data = map, fill = "lightgray", col ="darkgray") +
  geom_spatraster(data = covars, aes(fill = SST)) + 
  geom_sf(data = data_sf, aes(col = pres)) +
  scale_fill_viridis_c() +
  coord_sf(xlim = st_bbox(data_sf)[c(1,3)], st_bbox(data_sf)[c(2,4)], expand = TRUE) +
  theme_bw()

## 1.2 explore ####
testRes = cor.mtest(data[,c(1:3)], conf.level = 0.95)
M=cor(data[,c(4:6)])
corrplot(M,  order = 'hclust', addCoef.col = "black")


my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=loess, fill="red", color="red", ...) +
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}

g = ggpairs(data[,c(4:6)], lower = list(continuous = my_fn))
g

# 2. Model with glm ####

## 2.1 Run model ####
m1<- glm(pres ~ Chl + Sal + SST, 
         data = data,
         family="binomial")

summary(m1)

## 2.2 Predict ####
pglm<-predict(covars,
              m1,
              type='response')

ggplot() + 
  geom_spatraster(data = pglm, aes(fill = lyr1)) + 
  geom_sf(data = data_sf, aes(col = pres)) + 
  scale_fill_viridis_c() +
  theme_bw()

# 3. Evaluate and check spatial autocorrelation ####

## 3.1 Extract model residuals and predictions at points  ####
data_pred <- data %>% 
  mutate(pred = extract(pglm, data_sf)$lyr1, 
         res = resid(m1))  %>% 
  drop_na()

head(data_pred)

## 3.2 Prediction power ####
cor(data_pred$pres, data_pred$pred, method="spearman")

ggplot(data_pred) + 
  geom_boxplot(aes(x = factor(pres), y = pred, group = factor(pres))) + 
  theme_bw()

## 3.3 Bubble plot ####
ggplot(data_pred) + 
  geom_point(aes(x = x, y = y, size = abs(res), col = factor(pres))) + 
  coord_equal() + 
  theme_bw()

## 3.4 variogram ####
data_sf <- data_sf %>% 
  mutate(res = resid(m1))
variogram_obj <- variogram(res ~ 1, data = data_sf)
plot(variogram_obj, main = "Empirical Variogram of GAM Residuals")

## 3.5 Compare with model with lon lat ####
m2 <- glm(pres ~ Chl + Sal + SST + x + y, 
          data = data,
          family="binomial")

data_sf <- data_sf %>% 
  mutate(res2 = resid(m2))
variogram_obj <- variogram(res2 ~ 1, data = data_sf)
plot(variogram_obj, main = "Empirical Variogram of GAM Residuals")

# Can you run this with inlabru and compare? ####

