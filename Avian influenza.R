

install.packages("GSODR")
install.packages("MODIStsp")

library(sf)
library(dplyr)
setwd(" ")
lab7data <- st_read(" ")
glimpse(lab7data)

library("MODIStsp")

library("GSODR")
library(foreach)
library(tidyverse)
library("stars")
library(tmaptools)
library(tmap)

tm_shape(lab7data) +
  tm_polygons() +
  tm_basemap(server = "OpenStreetMap")


library(dplyr)
library(stringr)

freshwater_lakes <- lab7data %>%
  filter(
         str_detect(t(waterbody), "lake|lk|Reserv|Jord|Lake")
         ) 

##Create a map of these fresh water lakes and reservoirs over the state of North Carolina.##

library(tmap)
library(tmaptools)


lakesmap <- tmap_mode("view")

tm_basemap("Esri.WorldTopoMap") + 
  tm_shape(freshwater_lakes) +
  tm_borders(col = "green") +
  tm_fill(col = "yellow")


library(sf)
library(foreach)

library(dplyr)

freshwater_lakes <- select(freshwater_lakes,-onemap_sde,-descrip_cl, -comment)


freshwater_grid <- st_make_grid(freshwater_lakes, cellsize = 800, what="centers")
freshwater_grid <-  freshwater_grid %>% st_intersection(freshwater_lakes)




lakesmap2 <- tmap_mode("view")

tm_basemap("Esri.WorldTopoMap") + 
  tm_shape(freshwater_grid) +
 tm_symbols(col = "green",size = 0.2)






date_start <- "2019.02.01"
date_end <- "2019.04.30"




MODIStsp(
  gui = FALSE,
  out_folder = " ",
  selprod = "Surf_Temp_Daily_1Km (M*D11A1)",
  bandsel = "LST_Day_1km",
  user = "mstp_test" ,
  password = "",
  start_date = date_start,
  end_date = date_end,
  verbose = TRUE,
  parallel = FALSE,
  reprocess = TRUE,
  start_x = 11,
  end_x = 11,
  start_y = 5,
  end_y = 5,
  delete_hdf = TRUE
)



##Load the separate RData files for TERRA and for AQUA (requires the raster##
##package, but you should immediately convert to a stars object using the st_as_stars() function).##


load(".RData")

terrastars <- st_as_stars(raster_ts)



load(".RData")

aquastars <- st_as_stars(raster_ts)


##	For each satellite, use ## 

##… %>% st_apply(c("x","y"), function(q) median(q,na.rm=TRUE))## 

##to calculate the median temperature value across all days within each grid cell.## 
##This should yield a stars object only spatial dimension (i.e., no time dimension).##
##Convert the reported temperature (which is in a very odd unit system) to Celsius ##
##by dividing by 50 and then subtracting 273.15.  Concatenate the results for the ##
##two satellites into a joint stars objects with two attributes (one for each satellite). ##


median_raster <- terrastars %>%
  st_apply(c("x", "y"), function(q) median(q, na.rm = TRUE)) %>%
  st_as_stars()

medianterra <- (median_raster/ 50) - 273.15

median_raster2 <- aquastars %>%
  st_apply(c("x", "y"), function(q) median(q, na.rm = TRUE)) %>%
  st_as_stars()


# Convert AQUA median temperature to Celsius
aquamedian <- ( median_raster2 / 50) - 273.15

library(stars)
library(dplyr)


jointstars <- st_set_crs(jointstars, st_crs(freshwater_lakes))
joint_crs <- st_crs(jointstars)

jointstars <- st_transform(r12_sf, freshwater_crs)

# Check the structure of the joint stars object
st_dimensions(jointstars)

r12 <- c(medianterra, aquamedian)
r12_sf <- st_as_sf(r12)

mycolors <- c(rgb(1, 0.8, 0.4, 1), rgb(1, 0.6, 0.2, 1), rgb(1, 0.4, 0, 1), rgb(0.8, 0.3, 0, 1), rgb(0.6, 0.2, 0, 1))
plot(aquamedian, col=mycolors)


mycolors <- c(rgb(1, 0.8, 0.4, 1), rgb(1, 0.6, 0.2, 1), rgb(1, 0.4, 0, 1), rgb(0.8, 0.3, 0, 1), rgb(0.6, 0.2, 0, 1))
plot(medianterra, col=mycolors)

library("spData")
states_polygons <- us_states
ncpoly <- states_polygons %>% filter(NAME=="North Carolina")
plot(ncpoly)
st_crs(ncpoly)
st_crs(medianterra)





##set the crs of ncpoly to the crs of terramedian##
ncpoly_crs <- st_crs(medianterra)
nccrs <- st_transform(ncpoly, ncpoly_crs)


 terracropped <- st_crop(medianterra, nccrs)
aquacropped <- st_crop(aquamedian, nccrs)



 mycolors <- c(rgb(1, 0.8, 0.4, 1), rgb(1, 0.6, 0.2, 1), rgb(1, 0.4, 0, 1), rgb(0.8, 0.3, 0, 1), rgb(0.6, 0.2, 0, 1))
 plot(terracropped, col=mycolors)

 
 mycolors <- c(rgb(1, 0.8, 0.4, 1), rgb(1, 0.6, 0.2, 1), rgb(1, 0.4, 0, 1), rgb(0.8, 0.3, 0, 1), rgb(0.6, 0.2, 0, 1))
 plot(aquacropped, col=mycolors)
 


##transforming terramedian crs to freshwater crs##

freshwater_crs <- st_crs(freshwater_lakes)


terracropped_transformed <- st_transform(terracropped, freshwater_crs)

aquacropped_transformed <- st_transform(aquacropped, freshwater_crs)

       
map1lab7 <- tmap_mode("plot")
 tm_basemap("Esri.WorldTopoMap") + 
   tm_shape(freshwater_lakes) +
   tm_polygons() + 
   tm_shape(terracropped_transformed,
            raster.warp=FALSE) +
   tm_raster(
             palette="-RdYlGn",
   )  + tm_shape(freshwater_lakes) +
   tm_borders() 
 

 map2lab7 <- tmap_mode("plot")
 tm_basemap("Esri.WorldTopoMap") + 
   tm_shape(freshwater_lakes) +
   tm_polygons() + 
   tm_shape(aquacropped_transformed,
            raster.warp=FALSE) +
   tm_raster(
     palette="-inferno",
   )  + tm_shape(freshwater_lakes) +
   tm_borders() 
 




install.packages("rnoaa")
library("rnoaa")
date_start <- "2019-02-01"
date_end <- "2019-04-30"


date_start <- as.Date(date_start, format = "%Y-%m-%d")
date_end <- as.Date(date_end, format = "%Y-%m-%d")

dates <- seq(date_start,date_end, by = "day")
data_list <- lapply(dates, function(date) {
  ncdc(datasetid = 'GHCND',
       datatypeid = "TAVG",
       locationid = 'FIPS:37',
       limit=1000,
       startdate = date,
       enddate = date,
       token = " ")
})



##printing each element in the list ##

for (i in 1:length(data_list)) {
  print(data_list[[i]])
}

#extracting one element data and station id details ##

stationiddata <- print(data_list[[1]], n = 38 , na.print = "")
stationid <- stationiddata$data$station


##combining into a single dataframe ##

library(data.table)

combined_df <- rbindlist(lapply(data_list, function(x) data.frame(x$data)))

median_df <- combined_df %>% 
  group_by(station) %>% 
  summarise(median_temp = median(value, na.rm = TRUE)) %>% 
  mutate(median_temp_divided = median_temp/10) %>% 
  select(-median_temp)



# Create an empty list to store the metadata for each station
metadata_list <- list()

# Loop over each station ID and download its metadata
for (i in 1:length(stationid)) {
  
  # Get the station ID
  station <- stationid[i]
  
  # Download the station metadata
  station_metadata <- ncdc_stations(datasetid = 'GHCND', 
                                    stationid = station, 
                                    startdate = as.character(date), 
                                    enddate = as.character(date), 
                                    limit=1000,
                                    token =  " ")
  
  # Store the metadata for this station in the list
  metadata_list[[i]] <- station_metadata
}

# Combine all of the metadata into a single dataframe
metadata_df <- do.call(rbind, metadata_list)


# Changing the list into a single df ##

station_df <- rbindlist(lapply(metadata_list, function(x) data.frame(x$data)))

#combining the median temp data and the station metadata and transforming to lakes crs ##

colnames(station_df)[colnames(station_df) == "id"] <- "station"



merged_df <- merge(median_df, station_df, by = "station")

stationsf <- st_as_sf(merged_df, coords = c("longitude", "latitude"), crs = 4326)

stationsf <- st_transform(stationsf, st_crs(freshwater_lakes))


 map2lab7 <- tmap_mode("plot")
 
   tm_basemap("Esri.WorldTopoMap") + 
      tm_shape(freshwater_lakes) +
       tm_polygons() + 
       tm_shape(terracropped_transformed, raster.warp=FALSE) +
       tm_raster(palette="-RdYlGn") +
       tm_shape(freshwater_lakes) +
       tm_borders() +
       tm_shape(stationsf) +
       tm_dots(col = "red", size = 0.2)


   map2lab7 <- tmap_mode("plot")
   
   tm_basemap("Esri.WorldTopoMap") + 
     tm_shape(freshwater_lakes) +
     tm_polygons() + 
     tm_shape(aquacropped_transformed, raster.warp=TRUE) +
     tm_raster(palette="-inferno") +
     tm_shape(freshwater_lakes) +
     tm_borders() +
     tm_shape(stationsf) +
     tm_dots(col = "green", size = 0.2)  
   
library(sf)
# join the two objects

   
   jointstars <- st_set_crs(jointstars, st_crs(freshwater_lakes))
   joint_crs <- st_crs(jointstars)
   
   jointstars <- st_transform(r12_sf, freshwater_crs)
   
downscaler_station <- st_join(stationsf , jointstars) %>% cbind(.,st_coordinates(.)) 

downscalerstation <- downscaler_station %>% rename(terra = MOD11A1_LST_Day_1km_2019_032,
                                                            aqua = MYD11A1_LST_Day_1km_2019_032)

crs_joints <- st_crs(jointstars)
freshwater_grid <- st_transform(freshwater_grid, crs_joints)
freshwater_sf <- st_as_sf(freshwater_grid)

downscaler_lakes <- st_join(freshwater_sf , jointstars) %>% cbind(.,st_coordinates(.))


downscalerlakes <- downscaler_lakes %>%  rename(terra = MOD11A1_LST_Day_1km_2019_032,
                                                          aqua = MYD11A1_LST_Day_1km_2019_032) 
 
install.packages("RSpectra")

library(raster)
library(stars)
library("ncdf4")
library(broom)
library(elevatr)
library(spaMM)
library(lubridate)
library(foreach)
library(tidyverse)
library(tidycensus)
library(RAQSAPI)
library(MODIStsp)
library(stars)
library("RSpectra")
library(elevatr)



downscaler1 <- fitme(median_temp_divided ~ terra + aqua , 
                             data = downscalerstation) 

downscaler2 <-  fitme(median_temp_divided ~ terra + aqua  + Matern(1|X+Y), 
                      data = downscalerstation) 


downscaler3 <- fitme(median_temp_divided ~ terra + aqua + Matern(0+terra|X+Y) + Matern(0+aqua|X+Y), 
                           data = downscalerstation) 



downscaled_final <- downscalerlakes %>% 
  mutate( `mediantemptq` = predict(downscaler1,newdata = downscalerlakes %>% st_drop_geometry()),
          `mediantemptqkrig` = predict(downscaler2,newdata = downscalerlakes %>% st_drop_geometry()),
          `mediantemptqspace` = predict(downscaler3,newdata = downscalerlakes %>% st_drop_geometry()))



downscaled_results <- downscaled_final %>%
  gather(starts_with("mediantemp"),
         key="Model",
         value="Predictedmediantemp")

save(file="lab7results.RData",downscaled_results)
load(file="lab7results.RData")

#	Calculating and presenting the predicted fraction of fresh water
#lake/reservoir area in the state of North Carolina that had a 
#median temperature ≤ 10oC under each model. 

num_cold_lakes <- sum(downscaled_final$mediantemptq <= 10)
total_lakes <- nrow(downscaled_final)
fraction_cold_lakes <- num_cold_lakes / total_lakes

## 0.6507


num_cold_lakes <- sum(downscaled_final$mediantemptqkrig <= 10)
total_lakes <- nrow(downscaled_final)
fraction_cold_lakes <- num_cold_lakes / total_lakes

##0.497

num_cold_lakes <- sum(downscaled_final$mediantemptqspace <= 10)
total_lakes <- nrow(downscaled_final)
fraction_cold_lakes <- num_cold_lakes / total_lakes

##0.563

################################################################################
### Step 23: Plot the results of all the downscalers for a specific day.     ###                                                              ###       
################################################################################



lab8final_list <- split(downscaled_results, f = downscaled_results$Model)

lab8final_lmodel <- lab8final_list$mediantemptq %>% st_rasterize()
  
lab8final_krigmodel <- lab8final_list$mediantemptqkrig %>% st_rasterize()

lab8final_spmodel <- lab8final_list$mediantemptqspace  %>% st_rasterize()





map <- tmap_mode("view")

tm1 <- tm_basemap("Esri.WorldTopoMap") + 
  tm_shape(ncpoly_sf) +
  tm_borders() +   
  
  tm_shape(lab8final_lmodel) + 
  tm_raster(palette = "-RdYlGn", col = "Predictedmediantemp")

tm_layout(legend.position = c("right", "top"),
          legend.bg.color = "white")

print(tm1)




map <- tmap_mode("view")

tm2 <- tm_basemap("Esri.WorldTopoMap") + 
  tm_shape(ncpoly_sf) +
  tm_borders() +   
  
  tm_shape(lab8final_krigmodel) + 
  tm_raster(palette = "-RdYlGn", col = "Predictedmediantemp")

tm_layout(legend.position = c("right", "top"),
          legend.bg.color = "white")

print(tm2)

#5.1.1

delayx <- filter(flights, arr_delay >= 2)
delayx


top10_delays <- flights %>%
  mutate(delay_rank = row_number(desc(dep_delay))) %>%
  filter(delay_rank <= 10)





map <- tmap_mode("view")

tm3 <- tm_basemap("Esri.WorldTopoMap") + 
  tm_shape(ncpoly_sf) +
  tm_borders() +   
  
  tm_shape(lab8final_spmodel) + 
  tm_raster(palette = "-RdYlGn", col = "Predictedmediantemp" )

tm_layout(title = "Spatial model", legend.position = c("right", "top"),
          legend.bg.color = "white")

print(tm3)


map <- tmap_mode("view")

tm3 <- tm_basemap("Esri.WorldTopoMap") + 
  tm_shape(ncpoly_sf) +
  tm_borders() +   
  
  tm_shape(lab8final_spmodel) + 
  tm_raster(palette = "-RdYlGn", col = "Predictedmediantemp")

tm_layout(title = "Spatial model", title.position = c("center", "top"), 
          legend.position = c("right", "top"), legend.bg.color = "white")

print(tm3)

