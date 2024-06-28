
##Download the data and change the names##

lab3 <- read.csv("lab3.csv")

names(lab3)[names(lab3) == "created_at"] <- "date"
names(lab3)[names(lab3) == "Monitor.Number"] <- "monitor"
names(lab3)[names(lab3) == "PM2.5_ATM_ug.m3"] <- "pm25"

##creating a graph that illustrates the time series of smoke concentrations from all monitors##

library(tidycensus)
library(ggplot2)
library(tidyverse)
library(sf)
library(tmaptools)
lab3$date <- as.Date(lab3$date)
ggplot(lab3, aes(x = date, y = pm25)) + 
  geom_line() + 
  labs(title = "Smoke Concentration from All Monitors", 
       x = "Date", 
       y = "pm25concentrations")




##creating a map of Fort Collins with locations of the monitors and a base-map displayed##


install.packages("spaMM")
install.packages("stars")
install.packages("leaflet")
install.packages("elevatr")
install.packages("leafem")
library(spaMM)
library(stars)
library(spaMM)
library(leaflet)
library(leafem)
library(elevatr)
library(lubridate)
library(tmap)
library(tmaptools)
library(sf)
library(dplyr)
library(tidyverse)
library(raster)
##converting to the sf object ##
lab3sf <- lab3 %>%  
    st_as_sf(coords=c("Y","X")) %>%
    st_set_crs("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
    st_transform("+proj=longlat +datum=WGS84") 




## Get unique set of dates##

days <- lab3sf$date %>% 
  unique() %>% 
  sort() %>% 
  as.Date()


##choose a date when all the monitors have values ##

lab3spdate <- lab3sf %>%
  filter(date == "2020-10-31"
) %>% select(monitor , date , pm25 , geometry)



raster_object <- get_elev_raster(locations= lab3spdate,
                                 z=12,
                                 src="aws",
                                 clip="bbox",
                                 neg_to_na = TRUE,
                                 override_size_check = TRUE)
summary(raster_object)

roj <- raster::extract(x=raster_object,
                       y= lab3spdate,
                       fun=mean,
                       na.rm=TRUE,
                       sf=TRUE)
summary(roj)

## joining the elevation values of the monitors to the data with all ##
## monitors working on a specific date ##

lab3elev <- cbind(lab3spdate, elevation = roj)
subset <- lab3elev[lab3elev$monitor == 1009, ]
 subset$elevation <- 1650
  lab3elev[lab3elev$monitor == 1009, "elevation"] <- subset$elevation




## mapping the locations of the monitors ##

tmap_mode("view")
tm_basemap("Esri.WorldTopoMap") + 
  tm_shape(lab3elev) +
  tm_dots(col="monitor",size=0.1)

## joining the elevation variable to the main sf object ##

lab3sfele <- st_join(lab3sf, lab3elev, join = st_intersects) %>% select(monitor.x ,
                                          date.x , pm25.x , elevation , geometry)


lab3sfele <- rename(lab3sfele , monitor = monitor.x , date = date.x , pm25 = pm25.x)


##mapping the monitors ##

     

pal <- colorNumeric(palette="Spectral",seq(1 , 20,by = 2 ),reverse=TRUE)

  
   leaflet() %>%
   addTiles() %>%
   addCircleMarkers(
        data = lab3spdate,
       weight = 1, 
       color="black",
      fillOpacity=0.75,
      fillColor = ~ pal(pm25)
     ) %>%
   addLegend(pal=pal,  values = lab3spdate$pm25)

## transeferring projection to utm ##

lab31utm <- lab3elev %>%
  st_transform("EPSG:32613") %>%
  cbind(.,st_coordinates(.)) 

lab3eleutm <- lab3sfele %>%
  st_transform("EPSG:32613") %>%
  cbind(.,st_coordinates(.)) 

##grids##

predpts <- st_make_grid(lab3elev %>% 
                             st_buffer(dist=1000),
                           cellsize=c(0.03,0.03),
                           what="centers") %>%
  st_sf() 
rasterobj <- get_elev_raster(locations= predpts,
                             z=12,
                             src="aws",
                             clip="bbox",
                             neg_to_na = TRUE,
                             override_size_check = TRUE)
summary(rasterobj)

roj1 <- raster::extract(x=rasterobj,
                        y= predpts,
                        fun=mean,
                        na.rm=TRUE,
                        sf=TRUE)
summary(roj1)


predpts1 <- cbind(predpts, elevation = roj1)
predpts2 <- na.omit(predpts1)



predraster <- predpts2 %>% 
  st_rasterize() 

pred3utm <- predpts2 %>%
  st_transform(st_crs(lab31utm))%>%
  cbind(.,st_coordinates(.)) 


## kriging with fixed nu ##

krigelab3 <- fitme(pm25 ~ 1 + Matern(1|X+Y),
                      data = lab31utm,
                   )
summary(krigelab3)



#predict at prediction points
preds <- predict(krigelab3,
                 newdata=pred3utm %>%
                   st_drop_geometry(), 
                 variances=list(linPred=TRUE,predVar=TRUE))

dim(preds)

#extraction of mean and variance ##
predsutm <- tibble(pred3utm,
                    predvalue = preds[,1],
                    varvalue = attributes(preds)$predVar) %>%
  st_as_sf(coords=c("X","Y"),
           crs=st_crs(lab31utm)) 

## converting prediction values to a raster ##
predrast <- predsutm %>%
  st_transform(st_crs(lab3elev)) %>%
  st_rasterize(template = predraster)

pal <- colorNumeric(palette="Spectral",seq(1,20,by= 0.2),reverse=TRUE)

leaflet() %>%
  addTiles() %>%
  addStarsImage(x = predrast[2,,], 
                layerId = "predvalue", 
                colors = pal) %>%
  addCircleMarkers(
    data = lab3elev,
    weight = 1, 
    color="black",
    fillOpacity = 0.75,
    fillColor = ~ pal(pm25)
  ) %>%
  addLegend(pal = pal,
            values = lab3elev$pm25,
            title = "pm2.5 values")

leaflet() %>%
  addTiles() %>%
  addStarsImage(x = predrast[3,,], 
                layerId = "varvalue", 
                colors = pal) %>%
  addLegend(pal = pal,
            values = predsutm$varvalue,
            title = "pm2.5 variance")










## code for generating 61maps##


startdate <- as.Date("2020-09-01")
enddate <- as.Date("2020-10-31")

dates <- seq(enddate , startdate , by = -1)

maps <- list()

for (i in seq_along(dates)) {
  
  # subset the lab31utm data for the current date
  currentd <- subset(lab3eleutm, date == as.character(dates[i]))
  print(currentd)
  
  predpt <- st_make_grid(lab3sfele %>% 
                           st_buffer(dist=1000),
                         cellsize=c(0.03,0.03),
                         what="centers") %>%
    st_sf() 
  rasterobj1 <- get_elev_raster(locations= predpt,
                                z=12,
                                src="aws",
                                clip="bbox",
                                neg_to_na = TRUE,
                                override_size_check = TRUE)
  summary(rasterobj1)
  
  roj3 <- raster::extract(x=rasterobj1,
                          y= predpt,
                          fun=mean,
                          na.rm=TRUE,
                          sf=TRUE)
  summary(roj3)
  
  
  predpts1 <- cbind(predpt, elevation = roj3)
  predpt2 <- na.omit(predpts1)
  
  
  
  predraster <- predpt2 %>% 
    st_rasterize() 
  
  pred3utm <- predpt2 %>%
    st_transform(st_crs(currentd))%>%
    cbind(.,st_coordinates(.)) 
  # fit a kriging model to the pm25 values in currentd
  krigelab <- fitme(pm25 ~ 1 + Matern(1|X+Y),
                    data = currentd)
  
  # predict values for the pred3utm points using the kriging model
  predslab <- predict(krigelab,
                      newdata = pred3utm %>% st_drop_geometry(), 
                      variances = list(linPred = TRUE, predVar = TRUE))
  
  # create a new tibble with the predicted values
  predele <- tibble(pred3utm,
                    predvalue = predslab[,1],
                    varvalue = attributes(predslab)$predVar) %>%
    st_as_sf(coords = c("X","Y"),
             crs = st_crs(currentd)) 
  
  # convert the predicted values to a raster format
  predrast3 <- predele %>%
    st_transform(st_crs(currentd)) %>%
    st_rasterize(template = predraster)
  
  # create a map with the predicted values and the lab3eleutm data
  finallab3  <- tm_shape(predrast3) +
    tm_raster() +
    tm_shape(lab3eleutm) +
    tm_dots(col = "pm25") +
    tm_layout(frame = FALSE) +
    tmap_mode("plot")
  
  # store the map in the maps list
  maps[[i]] <- finallab3
  
  # show the current map
  print(finallab3)
  
  # save the current map as an image file
  tmap_save(finallab3, filename = paste0("map_", i, ".png"))
  
  cat("Completed", i, "of", length(dates), "\n")
}







key <- "cae655db94de4eb8814a2f5267333dc600a98dc8"

variable_list <- load_variables(2019, "acs5", cache = TRUE)

acsrace <- c("B01001_001",
                      "B01001I_001",
                      "B17026_001",
                      "B17026_002",
                      "B17026_003",
                      "B17026_004")

acsrace2 <- get_acs(geography = "tract",
                      variables = acsrace, 
                      geometry = TRUE,
                    county = "Larimer",
                      state = "CO", 
                      key = key, 
                      year = 2019)

acsrace2 <- acsrace2 %>% 
  dplyr::select(-moe) %>%
  spread(key=variable,value=estimate) %>%
  mutate( `PercentHispanic` = B01001I_001/B01001_001 * 100,
          `PercentPoverty` = (B17026_002+B17026_003+B17026_004)/B17026_001  * 100) %>% 
  dplyr::select(-starts_with("B01"),-starts_with("B17")) %>%
  rename("ZCTA"="GEOID")

centroidacs <- st_centroid(acsrace2 , of_largest_polygon = TRUE)

centroidacs <- na.omit(centroidacs)
library(sf)

centroidacs <- na.omit(centroidacs)
centroidacs <- st_transform(centroidacs, crs = st_crs(lab3sf))
acsrace2 <- st_transform(acsrace2, crs = st_crs(lab3sf))


tmap_mode("view")
tm_basemap("Esri.WorldTopoMap") + 
  tm_shape(centroidacs) +
  tm_dots(col="PercentHispanic",size=0.1)




tmap_mode("view")
tm_basemap("Esri.WorldTopoMap") + 
  tm_shape(centroidacs) +
  tm_dots(col="PercentPoverty",size=0.1)


interdata1 <- st_intersection(centroidacs , lab3sf)

counties <- st_join(acsrace2 , lab3sf, join = st_intersects)

counties <- na.omit(counties)


counties1 <- st_centroid(counties , of_largest_polygon = TRUE)



countiessf <- counties1 %>%  
  st_as_sf(coords=c("Y","X")) %>%
  st_set_crs("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
  st_transform("+proj=longlat +datum=WGS84") 

days <- countiessf$date %>% 
  unique() %>% 
  sort() %>% 
  as.Date()
countiesspdate <- countiessf %>%
  filter(date == "2020-10-31"
  )



rasterob <- get_elev_raster(locations= countiessf,
                            z=12,
                            src="aws",
                            clip="bbox",
                            neg_to_na = TRUE,
                            override_size_check = TRUE)
summary(rasterob)

roj <- raster::extract(x=rasterob,
                       y= countiessf,
                       fun=mean,
                       na.rm=TRUE,
                       sf=TRUE)
summary(roj)

counties3 <- cbind(countiessf, elevation = roj)
subset <- counties3[counties3$monitor == 1006, ]
subset$elevation <- 1516
counties3[counties3$monitor == 1006, "elevation"] <- subset$elevation



counties3utm <- counties3 %>%
  st_transform("EPSG:32613") %>%
  cbind(.,st_coordinates(.)) 


startdate <- as.Date("2020-09-01")
enddate <- as.Date("2020-10-31")

dates <- seq(enddate , startdate , by = -1)

maps <- list()

for (i in seq_along(dates)) {
  
  # subset the lab31utm data for the current date
  currentd <- subset(counties3utm, date == as.character(dates[i]))
  print(currentd)
  
  predpt <- st_make_grid(counties3 %>% 
                           st_buffer(dist=1000),
                         cellsize=c(0.03,0.03),
                         what="centers") %>%
    st_sf() 
  rasterobj1 <- get_elev_raster(locations= predpt,
                                z=12,
                                src="aws",
                                clip="bbox",
                                neg_to_na = TRUE,
                                override_size_check = TRUE)
  summary(rasterobj1)
  
  roj3 <- raster::extract(x=rasterobj1,
                          y= predpt,
                          fun=mean,
                          na.rm=TRUE,
                          sf=TRUE)
  summary(roj3)
  
  
  predpts1 <- cbind(predpt, elevation = roj3)
  predpt2 <- na.omit(predpts1)
  
  
  
  predraster <- predpt2 %>% 
    st_rasterize() 
  
  pred3utm <- predpt2 %>%
    st_transform(st_crs(currentd))%>%
    cbind(.,st_coordinates(.)) 
  # fit a kriging model to the pm25 values in currentd
  krigelab <- fitme(pm25 ~ 1 + Matern(1|X+Y),
                    data = currentd)
  
  # predict values for the pred3utm points using the kriging model
  predslab <- predict(krigelab,
                      newdata = pred3utm %>% st_drop_geometry(), 
                      variances = list(linPred = TRUE, predVar = TRUE))
  
  # create a new tibble with the predicted values
  predele <- tibble(pred3utm,
                    predvalue = predslab[,1],
                    varvalue = attributes(predslab)$predVar) %>%
    st_as_sf(coords = c("X","Y"),
             crs = st_crs(currentd)) 
  
  # convert the predicted values to a raster format
  predrast3 <- predele %>%
    st_transform(st_crs(currentd)) %>%
    st_rasterize(template = predraster)
  
  # create a map with the predicted values and the lab3eleutm data
  finallabc  <- tm_shape(predrast3) +
    tm_raster() +
    tm_shape(counties3utm) +
    tm_dots(col = "pm25") +
    tm_layout(frame = FALSE) +
    tmap_mode("plot")
  
  # store the map in the maps list
  maps[[i]] <- finallabc
  
  # show the current map
  print(finallabc)
  
  # save the current map as an image file
  tmap_save(finallabc, filename = paste0("map_", i, ".png"))
  
  cat("Completed", i, "of", length(dates), "\n")
}


 meancounty <- counties %>%
    group_by( NAME) %>%
   mutate(meanpm25 = mean(pm25, na.rm = TRUE)
 )

counties4<- meancounty %>% 
   distinct(NAME, .keep_all = TRUE)


summary(counties4)



modelx <- lm(PercentHispanic  ~  pm25  ,  data = meancounty)
summary(modelx)
confint(modelx)

modely <- lm(PercentPoverty ~ pm25 , data = meancounty)
summary(modely)
confint(modely)

