#-----------------------------------------------------------------------------------------
# fun_habitat_plot      Suite of common functions for plotting habitat model data
#-----------------------------------------------------------------------------------------
# Functions developed in March et al. 2021(https://doi.org/10.1038/s41598-021-01700-w)
#
# filterGridAbs     Filter Gridded absences
# gridPresAbs       Grid presence and absence using a common grid
# landMask          Funcion to check if a track falls into the oceanMask
# plot.prediction   Plot prediction map
# plot_Missing      Plot missing data
# spt_overlap       Spatio-temporal overlap of grid cells



#-------------------------------------------------------------
# filterGridAbs     Filter Gridded absences
#-------------------------------------------------------------
filterGridAbs <- function(gpres, gabs, grid, temporal_thrs){
  
  #--------------------------------------------------------------------------
  # 4. FILTER OUT ABSENCES BY SPATIAL AND TEMPORAL CRITERIA
  # We overlap absence with the presence bins. If there was a presence, we remove such absence
  # separate non-overlapping absence and presence locations
  #--------------------------------------------------------------------------
  
  # for each absence, check if there is a presence in adjacent cells within the temporal period defined
  # if there is a match, remove absence. if not, keep it.
  # Note: This part of the code is computer intensive and time consuming. Using parallel works fine.
  keep <- foreach(k=1:nrow(gabs), .packages=c("dplyr", "raster")) %do% {
    spt_overlap(abs_cell = gabs$cell[k], abs_date = gabs$date[k],
                pres_df = gpres, temporal_thrs, grid)
  }
  
  # filter out absences that match presences
  gabs$keep <- unlist(keep)
  gabs <- filter(gabs, keep == TRUE) %>%
    dplyr::select(-keep)
  
  # return data
  return(gabs)
}
#-------------------------------------------------------------  

#-------------------------------------------------------------
# gridPresAbs     Grid presence and absence using a common grid
#-------------------------------------------------------------
gridPresAbs <- function(pres, abs, res){
  
  #---------------------------------------------------------------
  # 1. Generate grid
  #---------------------------------------------------------------
  # Create a ocean mask to grid all observations
  # It is based on the following parameters:
  # res: resolution
  # ext: extent estimated from the data
  
  # define bounding box
  xmin <- floor(min(min(pres$longitude), min(abs$longitude)))
  xmax <- ceiling(max(max(pres$longitude), max(abs$longitude)))
  ymin <- floor(min(min(pres$latitude), min(abs$latitude)))
  ymax <- ceiling(max(max(pres$latitude), max(abs$latitude)))
  ext <- extent(xmin, xmax, ymin, ymax)
  
  #create grid
  grid <- raster(ext, res = res, crs = crs("+proj=longlat +datum=WGS84"))
  
  
  #---------------------------------------------------------------
  # 2. Generate presences
  #---------------------------------------------------------------
  
  # Extract cell ID from raster for each observation
  pres$cell <- cellFromXY(grid, cbind(pres$longitude, pres$latitude))
  
  # Transform observation to presence by cell and date per individual
  cpres <- pres %>%
    dplyr::group_by(organismID, cell, date) %>%
    dplyr::summarize(occ = 1,
                     n = n())
  
  # Get raster coordinates
  xy <- xyFromCell(grid, cpres$cell)
  cpres$longitude <- xy[,"x"]
  cpres$latitude <- xy[,"y"]
  
  
  #---------------------------------------------------------------
  # 3. Generate absences
  #---------------------------------------------------------------
  
  # Extract cell ID from raster for each observation
  abs$cell <- cellFromXY(grid, cbind(abs$longitude, abs$latitude))
  
  # Transform observation to absence by cell and date
  cabs <- abs %>%
    dplyr::group_by(organismID, cell, date) %>%
    dplyr::summarize(occ = 0,
                     n = n())
  
  # Get raster coordinates
  xy <- xyFromCell(grid, cabs$cell)
  cabs$longitude <- xy[,"x"]
  cabs$latitude <- xy[,"y"]
  
  
  # return data
  l <- list(gridPres = cpres, gridAbs = cabs, grid = grid)
  return(l)
}
#-------------------------------------------------------------  


#----------------------------------------------------------------------------------------------
# landMask    Funcion to check if a track falls into the oceanMask
#----------------------------------------------------------------------------------------------
## Create function to check if point is on land
## It requires same inputs than gshhsMask()
landMask <- function(tm, pt){
  xy <- cbind(pt[[1]],pt[[2]])
  ov <- raster::extract(oceanmask, xy)
  if(is.na(ov)) ov <- 1  # if point is outside study area classify as land (1)
  ov == 0  # returns a logical indicating whether the point is at sea (TRUE) or on land (FALSE)
}
#----------------------------------------------------------------------------------------------



#---------------------------------------------------------
# plot.prediction       Plot prediction map
#---------------------------------------------------------
plot.prediction <- function(r, land, title, subtitle){
  
  # transform raster to data.frame
  rdf <- as.data.frame(r, xy = TRUE) %>% drop_na()
  rdf <- setDT(melt(rdf, id.vars = c("x", "y")))
  
  # set scale range
  zl <- c(0, 1)
  
  # set xy limits
  yl <- range(rdf$y)
  xl <- range(rdf$x)
  
  # plot
  p <- ggplot()+
    geom_raster(aes(x = x, y = y, fill = value), data=rdf) +
    geom_sf(fill = grey(0.8), size = 0.2, data = land) +
    scale_fill_viridis_c('Habitat suitability', limits = zl) +
    coord_sf(xlim = xl, ylim = yl, expand=c(0,0)) +
    labs(title = title, subtitle = subtitle, x = '', y = '') +
    theme_article() +
    theme(legend.position = "bottom",
          legend.key.height=grid::unit(0.6,"cm"),
          legend.key.width=grid::unit(1.6,"cm"),
          legend.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0),
          axis.title.x = element_text(margin = ggplot2::margin(t = 0, r = 0, b = -2, l = 0))) +
    guides(fill = guide_colorbar(title.position = "top",
                                 title.hjust = 0.5,
                                 frame.colour = "black",
                                 ticks = TRUE))
  
  
  return(p)
  
  
}
#---------------------------------------------------------


#------------------------------------------------------------------------------------
# plot_Missing    Plot missing data
#------------------------------------------------------------------------------------
plot_Missing <- function(data_in, title = NULL){
  # https://www.kaggle.com/notaapple/detailed-exploratory-data-analysis-using-r
  temp_df <- as.data.frame(ifelse(is.na(data_in), 0, 1))
  temp_df <- temp_df[,order(colSums(temp_df))]
  data_temp <- expand.grid(list(x = 1:nrow(temp_df), y = colnames(temp_df)))
  data_temp$m <- as.vector(as.matrix(temp_df))
  data_temp <- data.frame(x = unlist(data_temp$x), y = unlist(data_temp$y), m = unlist(data_temp$m))
  ggplot(data_temp) + geom_tile(aes(x=x, y=y, fill=factor(m))) + scale_fill_manual(values=c("grey80", "grey10"), name="Missing\n(0=Yes, 1=No)") + theme_light() + ylab("") + xlab("") + ggtitle(title)
}
#------------------------------------------------------------------------------------


#------------------------------------------------------------------------------------
# spt_overlap     Spatio-temporal overlap of grid cells
#------------------------------------------------------------------------------------
spt_overlap <- function(abs_cell, abs_date, pres_df, temporal_thrs, grid){
  # Given a cell number and date for an absence, this function checks the overlap with presences.
  # For the temporal criteria, it first filter all presence within the temporal threshold (in days).
  # Then, for the filtered cells, the function checks if they are adhjacent to the target cell.
  #
  # absence and presence cell number have to be originated from the same raster (grid)
  
  library(dplyr)
  library(raster)
  
  # check if there are presence for a given date, considering temporal window
  ipres <- dplyr::filter(pres_df, date >= abs_date - temporal_thrs, date <= abs_date + temporal_thrs)
  if(nrow(ipres)==0) keep <- TRUE
  
  # check if there are adjacent cells
  if(nrow(ipres)>0){
    adj <- adjacent(grid, cells = as.numeric(abs_cell), directions = 8, pairs = FALSE,
                    include = TRUE, target = ipres$cell)
    
    if(length(adj)==0) keep <- TRUE
    if(length(adj)>0) keep <- FALSE
  }
  
  return(keep)
}
#------------------------------------------------------------------------------------

