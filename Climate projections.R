install.packages("rnaturalearth")
install.packages("USAboundaries")



library(tidyverse)
library(stars)     
library(sf)       
library(tmap)
library(tmaptools)
library(rnaturalearth) 
library(USAboundaries)

###1)	Download the projections for the “IP” model at 2.5 arc-second resolution for the period##
##2061-2080 under RCP4.5 and RCP8.5.##

setwd(" ")

raster::getData(name = 'worldclim', 
                var = 'bio', 
                res = 2.5,
                path = " ")

raster::getData(name = 'CMIP5', 
                var = 'bio', #grabs 19 most popular climate variables 
                res = 2.5, #spatial resolution in minutes
                rcp = 45, #RCP4.5.  
                model = "IP", # one particular climate model called IPSL-CM5A-MR
                year = 70, # years 2061-2080
                path = " ")


raster::getData(name = 'CMIP5', 
                var = 'bio', #grabs 19 most popular climate variables
                res = 2.5, #spatial resolution in minutes
                rcp = 85,  #RCP8.5.  
                model = "IP", # one particular climate model called IPSL-CM5A-MR
                year = 70, # years 2061-2080
                path = " ")
##2)Load the raster for the average temperature variable (“bio1”) for both of these projections,##
##and convert to tenths of a degree.##

annual_T <- stars::read_stars("wc2-5\\bio1.bil")
annual_T <- annual_T/10
crs_hist <- st_crs(annual_T)

annual_T_70_IP_4.5 <- stars::read_stars("cmip5\\2_5m\\ip45bi701.tif")
annual_T_70_IP_4.5 <- annual_T_70_IP_4.5/10


annual_T_70_IP_8.5 <- stars::read_stars("cmip5\\2_5m\\ip85bi701.tif")
annual_T_70_IP_8.5 <- annual_T_70_IP_8.5/10

crs_rcp <- st_crs(annual_T_70_IP_4.5)  ##extracting the crs projection of annual_T_70_IP_4.5 ##


##downloading colorado map##


install.packages("spData")
library("spData")
states_polygons <- us_states
copoly <- states_polygons %>% filter(NAME=="Colorado")  ##extracting the polygons of coloradostate##

plot(copoly)##plotting copoly##

comaprcp <- copoly %>% 
  st_transform(crs_rcp) ##assigning crs of crs_rcp to copoly##

st_crs(annual_T)##printing the crs of annual_T
st_crs(copoly)## "" ##
st_crs(crs_rcp)## "" ##
st_crs(copolytrans)
copolytrans <- st_transform(copoly, st_crs(annual_T))##transforming copoly crs to that of annual_T##

annualtcomap <- annual_T %>% 
  st_crop(copolytrans)##cropping annual_T dataset to that of copolytrans##


annualT70IP4.5comap <- annual_T_70_IP_4.5 %>% 
  st_crop(comaprcp) %>%
  st_transform(crs_hist)  

st_dimensions(annualtcomap)
st_dimensions(annualT70IP4.5comap)

st_dimensions(annualT70IP4.5comap) <- st_dimensions(annualtcomap)


annualt70IP8.5comap <- annual_T_70_IP_8.5 %>% 
  st_crop(comaprcp) %>%
  st_transform(crs_hist)
st_dimensions(annualt70IP8.5comap) <- st_dimensions(annualtcomap)




##not used##
library(raster)
r <- raster(extent(copoly), ncol=ncol(copoly), nrow=nrow(copoly), crs=st_crs(copoly))
coloradomap <- rasterize(copoly, r, field="AREA")
st_crs(coloradomap)
projection(coloradomap) <- "+init=epsg:4326"
dataset_crs <- st_crs(annualT70IP4.5comap)
coloradocrs <- st_transform(coloradomap, st_crs(annual_T))
# Transform the CRS of the dataset to match the coordinate system of the raster file
coloradocrs <- st_transform(dataset_crs, crs(coloradomap))
##not used##




cotmap1 <- tmap_mode("plot")
tm_basemap("Esri.WorldTopoMap") + 
tm_shape(copoly) +
  tm_polygons() + 
  tm_shape(c(annualtcomap,
             annualT70IP4.5comap,
             annualt70IP8.5comap,
             along=list(Source=c("1970-2000",
                                 "RCP4.5 2061-2080",
                                 "RCP8.5 2061-2080"))),
           raster.warp=FALSE) +
  tm_raster(title="Average\nTemperature (C)",
            midpoint=0,
            palette="-RdYlGn",
            ) +
  tm_facets(nrow=1, as.layers = TRUE) +
  tm_shape(copoly) +
  tm_borders() 



cotmap1 <- tmap_mode("plot")

tm_basemap("Esri.WorldTopoMap") + 
  tm_shape(copoly) +
  tm_polygons() + 
  tm_shape(c(annualtcomap,
             annualT70IP4.5comap,
             annualt70IP8.5comap,
             annualt70IP8.5comap - annualT70IP4.5comap , # subtract one raster layer from the other
             along=list(Source=c("1970-2000",
                                 "RCP4.5 2061-2080",
                                 "RCP8.5 2061-2080",
                                 "RCP8.5 - RCP4.5 2061-2080"))),
           raster.warp=FALSE) + # set raster.warp=FALSE to prevent warping
  tm_raster(title="Average\nTemperature (C)",
            midpoint=0,
            breaks = c(-5,-3,-2,-1,0,1,2,3,4,5,7,9,11,13,15,17,19,20),
            palette="-RdYlGn") +
  tm_facets(nrow=1) +
  tm_shape(copoly) +
  tm_borders()




##For precipitation projections##

annual_T1 <- stars::read_stars("wc2-5\\bio12.bil")
crs_hist <- st_crs(annual_T1)
annualt1comap <- annual_T1 %>% 
  st_crop(copolytrans)
#RCP4.5 temperature
# file name convention is [model][rcp][variable set][central year][variable number].tif
annual_Tpp45 <- stars::read_stars("cmip5\\2_5m\\ip45bi7012.tif")


#RCP8.5 temperature
# file name convention is [model][rcp][variable set][central year][variable number].tif
annual_Tpp85 <- stars::read_stars("cmip5\\2_5m\\ip85bi7012.tif")

crs_rcp1 <- st_crs(annual_Tpp45)



comaprcp <- copoly %>% 
  st_transform(crs_rcp)

st_crs(annual_T)
st_crs(copoly)
st_crs(crs_rcp)

copolytrans <- st_transform(copoly, st_crs(annual_T))

annualtcomap <- annual_T %>% 
  st_crop(copolytrans)


annual_Tpp45comap <- annual_Tpp45 %>% 
  st_crop(comaprcp) %>%
  st_transform(crs_hist)  

st_dimensions(annualtcomap)
st_dimensions(annual_Tpp45comap)

st_dimensions(annual_Tpp45comap) <- st_dimensions(annualtcomap)

annual_Tpp85comap <- annual_Tpp85 %>% 
  st_crop(comaprcp) %>%
  st_transform(crs_hist)
st_dimensions(annual_Tpp85comap) <- st_dimensions(annualtcomap)



cotmap2 <- tmap_mode("plot")
tm_basemap("Esri.WorldTopoMap") + 
  tm_shape(copoly) +
  tm_polygons() + 
  tm_shape(c(annualt1comap,
             annual_Tpp45comap,
             annual_Tpp85comap,
             annual_Tpp85comap - annual_Tpp45comap,
             along=list(Source=c("1970-2000",
                                 "RCP4.5 2061-2080",
                                 "RCP8.5 2061-2080",
                                 "RCP8.5 - RCP4.5 2061-2080"))),
           raster.warp=FALSE) +
  tm_raster(title="Averageprecipitation(mm)",
            midpoint=0,
            breaks = c(-200,-150,-100,0,25,50,100,150,200,400,600,800,1000,1200)
            
  ) +
  tm_facets(nrow=2, as.layers = TRUE) +
  tm_shape(copoly) +
  tm_borders() 




data_path <- " "
 


   # Set the list of climate models
   models <- c("AC", "BC", "CC", "CE", "CN", "GF", "GD", "GS", "HD", "HG", "HE", "IN", "IP", "MI", "MR", "MC", "MP", "MG", "NO")
   
   # Loop over the models, scenarios, and years to download the data
   for (model in models) {

     
  # Construct the filename
  
    raster::getData(name = 'CMIP5', 
     var = 'bio', #grabs 19 most popular climate variables 
    res = 2.5, #spatial resolution in minutes
     rcp = 45, #RCP4.5.  
    model = model, # one particular climate model called IPSL-CM5A-MR
    year = 70, # years 2061-2080
   path = data_path)
                        
                  }
           
     
   filename <- paste0("worldclim_", model, "_", scenario, "_20", sprintf("%02d", year), ".tif")


   for (model in models) {
     
     filename <- paste0("worldclim_", model, "_", scenario, "_20", sprintf("%02d", year), ".tif")
     # Construct the filename
     
     raster::getData(name = 'CMIP5', 
                     var = 'bio', #grabs 19 most popular climate variables 
                     res = 2.5, #spatial resolution in minutes
                     rcp = 85, #RCP4.5.  
                     model = model, # one particular climate model called IPSL-CM5A-MR
                     year = 70, # years 2061-2080
                     path = data_path)
     
   }  
   
   
  
##load raster projections and crop for denver county for all the models using for loop ## 
   setwd(" ")    
   
   annualb1den <- stars::read_stars("wc2-5\\bio1.bil")
   annualb1den <- annualb1den/10
   crs_hist <- st_crs(annualb1den)
   plot(annualb1den)
   
   ##load denver map and plot##
   
       install.packages("tigris") 
       library("tigris")
       
       denmap <- counties(state = "Colorado",
                          
                          class = "sf") %>%
         
         filter(NAME=="Denver") %>%
         
         st_as_sf(.)
       plot(denmap)    
       
 ##crop the recent climate data to denver map and plot##
       
denmaprecent <- st_transform(denmap, st_crs(annualb1den)) 
annualb1denmap <- annualb1den %>% 
  st_crop(denmaprecent)
plot(annualb1denmap)
 

denmap3 <- tmap_mode("view")
tm_basemap("Esri.WorldTopoMap") 
  tm_shape(denmap) +
  tm_polygons() + 
  tm_shape(c(annualb1denmap
),
           raster.warp=FALSE) +
  tm_raster(title="Average\nTemperature (C)",
            midpoint=0,
            palette="-RdYlGn",
  ) +
  
  tm_shape(denmap) +
  tm_borders() 

##extracting rasters of the temperature projections for 2061 to 2080 for rcp 4.5 emissions##


       annualT45denmap_list <- list()
       models <- c("AC", "BC", "CC", "CE", "CN", "GF", "GD", "GS", "HD", "HG", "HE", "IN", "IP", "MI", "MR", "MC", "MP", "MG", "NO")
       
       for (model in models) {
         annual_T_all_IP_4.5 <- stars::read_stars(paste0("cmip5\\2_5m\\", tolower(model), "45bi701",".tif"))
         annual_T_all_IP_4.5 <- annual_T_all_IP_4.5 / 10
         denmaprcp <- st_transform(denmap, st_crs(annual_T_all_IP_4.5)) 
         denvercrs_valid <- st_make_valid(denmaprcp)
         annualT45denmap <- annual_T_all_IP_4.5 %>% 
           st_crop(denvercrs_valid)  %>% st_transform(crs_hist) 
         annualT45denmap_list[[paste0(model)]] <- annualT45denmap
       }   
       
  
       
      
   
## extracting rasters for temperature projections for th year 2061  to 2080 for rcp 8.5 emissions##
       
annualT85denmap_list <- list()
models <- c("AC", "BC", "CC", "CE", "CN", "GF", "GD", "GS", "HD", "HG", "HE", "IN", "IP", "MI", "MR", "MC", "MP", "MG", "NO")
        
 for (model in models) {
 file_path <- paste0("cmip5\\2_5m\\", tolower(model), "85bi701",".tif")
  tryCatch({
 annual_T_all_8.5 <- stars::read_stars(file_path)
annual_T_all_8.5 <- annual_T_all_8.5 / 10
denmaprcp <- st_transform(denmaprcp, st_crs(annual_T_all_8.5)) 
denvercrs_valid <- st_make_valid(denmaprcp)
annualT85denmap <- annual_T_all_8.5 %>% 
st_crop(denvercrs_valid)  %>% st_transform(crs_hist) 
annualT85denmap_list[[paste0(model)]] <- annualT85denmap
}, error = function(e) {
 warning(sprintf("File not found: %s. Skipping to next iteration", file_path))
})}


## Calculating median temperature values for the produced temperature##
##projection rasters one for each climate model for both emissions ##
  
 # create an empty list
median_values <- list()
   
# iterate over each object in annualT45denmap_list
for (i in seq_along(annualT45denmap_list)) {
           
 # get the variable names that end with ".tif"
tif_vars <- grep("\\.tif$", names(annualT45denmap_list[[i]]), value = TRUE)
            
 # calculate the median value of each variable that ends with ".tif"
 tif_medians <- sapply(annualT45denmap_list[[i]][tif_vars], median, na.rm = TRUE)
              
 # add the object name and median values to the list
median_values[[i]] <- c(names(annualT45denmap_list)[i], tif_medians)}
# convert the list to a data frame
median_values <- as.data.frame(do.call(rbind, median_values))
   
View(median_values)
  

median_values85 <- list()

# iterate over each object in annualT45denmap_list
for (i in seq_along(annualT85denmap_list)) {
  
  # get the variable names that end with ".tif"
  tif_vars <- grep("\\.tif$", names(annualT85denmap_list[[i]]), value = TRUE)
  
  # calculate the median value of each variable that ends with ".tif"
  tif_medians <- sapply(annualT85denmap_list[[i]][tif_vars], median, na.rm = TRUE)
  
  # add the object name and median values to the list
  median_values85[[i]] <- c(names(annualT85denmap_list)[i], tif_medians)}
# convert the list to a data frame
median_values85 <- as.data.frame(do.call(rbind, median_values85))


## merging both the dataframs based on the climate model, with NA for missing values.##
merged_lab6 <- merge(median_values, median_values85, by = "V1", all = TRUE)

merged_lab6
names(merged_lab6)[2:3] <- c("rcp45", "rcp85")

##pivoting the data into long data##(tidyr package)

library(tidyr)

long_data <- pivot_longer(merged_lab6, 
cols = c("rcp45", "rcp85"), 
names_to = "Scenario", 
values_to = "Temperature")

# Print the long-format data frame
print(long_data)

#boxplot for rcp vs temp ##

long_data$Temperature <- as.numeric(as.character(long_data$Temperature))
 ggplot(long_data, aes(x = Scenario, y = Temperature , color = Scenario)) +
       geom_boxplot() +
       xlab("Scenario") +
       ylab("Temperature") +
       ylim(0, 20)

 ##regression model##
 
 model1lab6 <- lm(Temperature ~ Scenario + V1, data = long_data) %>% broom::tidy(conf.int=TRUE)
summary(model1lab6)


library(openxlsx)
write.xlsx(model1lab6, file = "summarylab6.xlsx")
