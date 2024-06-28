

acsrace <- c( "B01001I_001" ,"B01001_001" , "B01001B_001", 
              "B17026_001" , "B17026_002" , "B17026_003" , 
              "B17026_004" , "B17026_005" , "B17026_006" , 
              "B17026_007" , "B17026_008" , "B17026_009")
lab_2019 <- get_acs(
  geography = "tract",
  variables = str_replace_all(acsrace,"-","_"),
  geometry = TRUE,
  state = "FL",
  county = "086",
  year = 2019,
  key = key)
lab_2011 <- get_acs(
  geography = "tract",
  variables = str_replace_all(acsrace,"-","_"),
  geometry = TRUE,
  state = "FL",
  county = "086",
  year = 2011,
  key = key)




calculatepn_2019 <- lab_2019 %>% dplyr:: select(-moe) %>% 
  spread(key = variable , value = estimate) %>% 
  mutate("proportionafricanamerican_2019" = B01001B_001/B01001_001 , 
         "proportionhispanic_2019" = B01001I_001/B01001_001 , 
         "proportionbelow100_2019" = (B17026_002+B17026_003+B17026_004)/B17026_001 , 
         "proportionbelow200_2019"= (B17026_002+B17026_003+B17026_004+B17026_005+B17026_006+B17026_007+B17026_008+B17026_009)/B17026_001) %>% 
  dplyr:: select(- starts_with("B01"), - starts_with("B17") )




calculatepn_2011 <- lab_2011 %>% dplyr:: select(-moe) %>% 
  spread(key = variable , value = estimate) %>%
  mutate("proportionafricanamerican_2011" = B01001B_001/B01001_001 , 
         "proportionhispanic_2011" = B01001I_001/B01001_001 , 
         "proportionbelow100_2011" = (B17026_002+B17026_003+B17026_004)/B17026_001  , 
         "proportionbelow200_2011"= (B17026_002+B17026_003+B17026_004+B17026_005+B17026_006+B17026_007+B17026_008+B17026_009)/B17026_001) %>% 
  dplyr:: select(- starts_with("B01"), - starts_with("B17") )
view(calculatepn_2011)

lab_2011subset <- st_drop_geometry(calculatepn_2011)
labcombined <- merge(calculatepn_2019, lab_2011subset, by = "NAME")
mergedlab1 <- st_as_sf(labcombined, coords = c("longitude", "latitude"), crs = st_crs(calculatepn_2019))
view(mergedlab1)
plot(mergedlab1)

mergedlab1 <- na.omit(mergedlab1)
# Plotting the data with the new column

fproj1 <- tmap_mode("view")
tm_basemap("Esri.WorldTopoMap") + 
  tm_shape(mergedlab1) +
  tm_polygons("proportionafricanamerican_2019", palette = "YlGn")

fproj1 <- tmap_mode("view")
tm_basemap("Esri.WorldTopoMap") + 
  tm_shape(mergedlab1) +
  tm_polygons("proportionafricanamerican_2011", palette = "YlGn")

fproj1 <- tmap_mode("view")
tm_basemap("Esri.WorldTopoMap") + 
  tm_shape(mergedlab1) +
  tm_polygons("proportionhispanic_2011", palette = "YlGn")



fproj1 <- tmap_mode("view")
tm_basemap("Esri.WorldTopoMap") + 
  tm_shape(mergedlab1) +
  tm_polygons("proportionhispanic_2019", palette = "YlGn")




raster_object <- get_elev_raster(locations=mergedlab1,
                                 z=12,
                                 src="aws",
                                 clip="bbox",
                                 neg_to_na = TRUE,
                                 override_size_check = TRUE) 
summary(raster_object)

plot(raster_object)


#Calculating the average elevation within each tract using the extract() function
#in the raster package (running this function may time several minutes).  
#The extract() function is like the summarize() function in tidyverse, but is optimized 


roj <- raster::extract(x=raster_object,
                       y= mergedlab1,
                         fun=mean,
                         na.rm=TRUE,
                       sf=TRUE)
roj

# Map the tract polygon boundaries on top of the elevation raster.

fproj1 <- tmap_mode("view")

tm_basemap("Esri.WorldTopoMap") +
  tm_shape(raster_object) +
  tm_raster() +
  tm_shape(mergedlab1) +
  tm_borders() +
  tm_layout() +
  tm_legend(title = "elevation")


#Map the mean elevation of the tracts.


# Convert the matrix into an sf object
library(sf)

# Assuming roj is a matrix with elevation values
roj_sf <- st_as_sf(mergedlab1, agr = "constant", do_union = FALSE)
roj_sf$elevation <- roj

# Map the mean elevation
fproj1 <- tmap_mode("view")

tm_basemap("Esri.WorldTopoMap") +
  tm_shape(roj_sf) +
  tm_fill(col = "elevation") +
  tm_shape(mergedlab1) +
  tm_borders() +
  tm_layout() +
  tm_legend(title = "Elevation")


# Plot the histogram of tract mean elevations.

hist(roj, main = "Histogram of Mean Elevation", xlab = "v1", ylab = "Frequency")



lab1combinediff <- mergedlab1 %>% mutate("differenceafricanamerican" = proportionafricanamerican_2019 - proportionafricanamerican_2011 , 
                                            "differencehispanic" = proportionhispanic_2019 - proportionhispanic_2011 , 
                                         "differencepoverty100" = proportionbelow100_2019 - proportionbelow100_2011,
                                            "differencepoverty200" = proportionbelow200_2019 - proportionbelow200_2011)

elevation_df <- data.frame(ID = 1:nrow(mergedlab1), elevation = roj)

# Join the elevation data with mergedlab1

mergeddatalab1 <- left_join(lab1combinediff, data.frame(elevation = roj, st_drop_geometry(lab1combinediff)), by = NULL)


##Was the proportion of residents in each of the four sociodemographic groups 
##(I.e., Black/African Americans, Hispanic/Latino, <100% poverty line, and <200% poverty line)
##associated with tract elevation in 2011, and if so by how much? Please show and interpret the 
##parameter estimate and the 95% confidence interval.


model1 <- lm(proportionafricanamerican_2011 ~ elevation  , data = mergeddatalab1)
summary(model1)

model1_summary <- tidy(model1)
write.csv(model1_summary, file = "model1_summary.csv", row.names = FALSE)
model1conf <- confint(model1)
write.csv(model1conf, file = "model1_conf.csv", row.names = FALSE)



model2 <- lm(proportionhispanic_2011 ~  elevation ,  data = mergeddatalab1)
summary(model2)


model2_summary <- tidy(model2)
write.csv(model1_summary, file = "model2_summary.csv", row.names = FALSE)
model2conf <- confint(model2)
write.csv(model2conf, file = "model2_conf.csv", row.names = FALSE)

model3 <- lm(proportionbelow100_2011 ~ elevation, data = mergeddatalab1)
summary(model3)


model3_summary <- tidy(model3)
write.csv(model3_summary, file = "model3_summary.csv", row.names = FALSE)
model3conf <- confint(model3)
write.csv(model3conf, file = "model3_conf.csv", row.names = FALSE)


model4 <- lm(proportionbelow200_2011 ~ elevation, data = mergeddatalab1)
summary(model4)

model4_summary <- tidy(model4)
write.csv(model4_summary, file = "model4_summary.csv", row.names = FALSE)
model4conf <- confint(model4)
write.csv(model4conf, file = "model4_conf.csv", row.names = FALSE)



#b.	Was the proportion of residents in each sociodemographic group associated
#with tract elevation in 2019, and if so by how much? Please show and interpret 
#the parameter estimate and the 95% confidence interval.

model5 <- lm(proportionafricanamerican_2019 ~ elevation, data = mergeddatalab1)
summary(model5)
confint(model5)


model5_summary <- tidy(model5)
write.csv(model5_summary, file = "model5_summary.csv", row.names = FALSE)
model5conf <- confint(model5)
write.csv(model5conf, file = "model5_conf.csv", row.names = FALSE)



model6 <- lm(proportionhispanic_2019 ~ elevation,  data = mergeddatalab1)
summary(model6)

model6_summary <- tidy(model6)
write.csv(model6_summary, file = "model6_summary.csv", row.names = FALSE)
model6conf <- confint(model6)
write.csv(model6conf, file = "model6_conf.csv", row.names = FALSE)

model7 <- lm(proportionbelow100_2019 ~ elevation, data = mergeddatalab1)
summary(model7)

model7_summary <- tidy(model7)
write.csv(model7_summary, file = "model7_summary.csv", row.names = FALSE)
model7conf <- confint(model7)
write.csv(model7conf, file = "model7_conf.csv", row.names = FALSE)


model8 <- lm(proportionbelow200_2019 ~ elevation, data = mergeddatalab1)
summary(model8)

model8_summary <- tidy(model8)
write.csv(model8_summary, file = "model8_summary.csv", row.names = FALSE)
model8conf <- confint(model8)
write.csv(model8conf, file = "model8_conf.csv", row.names = FALSE)



#Did the proportion of residents in the selected socio-demographic groups increase
#in low-lying tracts between 2011 and 2019? Which groups demonstrated increases? 
#Please quantify the increases.

summary(mergeddatalab1)
model9_summary <- " "
write.csv(model9_summary, file = "model9_summary.csv", row.names = FALSE)

#c.	Did the proportion of residents in the selected sociodemographic groups increase
#in low-lying tracts between 2011 and 2019? Which groups demonstrated increases? 



mergedsubset1 <- mergeddatalab1[mergeddatalab1$elevation < 1.71, ]
mergedsubset2 <-  mergeddatalab1[mergeddatalab1$elevation > 1.71, ]
summary_subset <- lapply(mergedsubset1[, c("differenceafricanamerican", "differencehispanic", 
                                           "differencepoverty100", "differencepoverty200")],
                                            summary)

print(summary_subset)

#d.	Did the proportion of residents in the selected sociodemographic groups increase 
#in low-lying tracts more than in other tracts in Miami-Dade County between 2011 and 2019?  

summary_subset2 <- lapply(mergedsubset1[, c("differenceafricanamerican", "differencehispanic", 
                                           "differencepoverty100", "differencepoverty200")],
                         summary)


print(summary_subset2)

##How do your answers to questions 10a - d change if you use only tracts below
##3 meters in elevation?

mergedsubset3 <- mergeddatalab1[mergeddatalab1$elevation < 3, ]
summary(mergedsubset3)

summary_subset3 <- lapply(mergedsubset1[, c("differenceafricanamerican", "differencehispanic", 
                                           "differencepoverty100", "differencepoverty200")],
                         summary)


print(summary_subset3)


mergedsubset4 <- mergedsubset3[mergedsubset3$elevation < 1.61, ]


summary_subset4 <- lapply(mergedsubset4[, c("differenceafricanamerican", "differencehispanic", 
                                            "differencepoverty100", "differencepoverty200")],
                          summary)


print(summary_subset4)

 

mergedsubset5 <- mergedsubset3[mergedsubset3$elevation > 1.61, ]


summary_subset5 <- lapply(mergedsubset5[, c("differenceafricanamerican", "differencehispanic", 
                                            "differencepoverty100", "differencepoverty200")],
                          summary)


print(summary_subset5) 
