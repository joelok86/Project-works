##Download the data and change the names##


library(readxl)
lab4 <- read_excel("lab4data.xlsx")
names(lab4)[names(lab4) == "WBAN #"] <- "wban"
names(lab4)[names(lab4) == "STATION ID"] <- "stationid"
names(lab4)[names(lab4) == "STATION NAME"] <- "name"
names(lab4)[names(lab4) == "LAT N/ LONG W (deg,min,sec)"] <- "geo"
names(lab4)[names(lab4) == "ELEV (ft)"] <- "Elevation"
names(lab4)[names(lab4) == "TOWER HEIGHT (m)"] <- "towerht"


install.packages("bio.geo")
library("biogeo")

lab4geo <- lab4 %>%
  mutate(Latitude_dms = str_split_fixed(`geo`,pattern=" / ",n=2)[,1], 
         Longitude_dms = str_split_fixed(`geo`,pattern=" / ",n=2)[,2]) %>%
  mutate(Latitude_dms = ifelse(str_sub(Latitude_dms,1,1)=="0",
                               str_sub(Latitude_dms,2,str_length(Latitude_dms)),
                               Latitude_dms),
         Longitude_dms = ifelse(str_sub(Longitude_dms,1,1)=="0",
                                str_sub(Longitude_dms,2,str_length(Longitude_dms)),
                                Longitude_dms)) %>%
  mutate(Latitude_NS = "N", #all sites are in the northern hemisphere
         Longitude_EW = ifelse(str_ends(Longitude_dms,"E"), #not all sites are in the western hemisphere
                               "E",
                               "W"),
         Longitude_dms = ifelse(str_ends(Longitude_dms,"E"), #removing the trailing "E" from sites in the eastern hemisphere
                                str_sub(Longitude_dms,1,str_length(Longitude_dms)-1),
                                Longitude_dms)) %>%
  mutate(Latitude_d = Latitude_dms %>% str_sub(.,start=-7,end=-5) %>% as.numeric(), #we have to count characters from the end because some locations have 3 digit degrees and others have 2 digit degrees
         Latitude_m = Latitude_dms %>% str_sub(.,start=-4,end=-3) %>% as.numeric(),
         Latitude_s = Latitude_dms %>% str_sub(.,start=-2,end=-1) %>% as.numeric(),
         Longitude_d = Longitude_dms %>% str_sub(.,start=-7,end=-5) %>% as.numeric(),
         Longitude_m = Longitude_dms %>% str_sub(.,start=-4,end=-3) %>% as.numeric(),
         Longitude_s = Longitude_dms %>% str_sub(.,start=-2,end=-1) %>% as.numeric()) %>%
  mutate(Latitude = dms2dd(dd=Latitude_d,
                           mm=Latitude_m,
                           ss=Latitude_s,
                           ns=Latitude_NS),
         Longitude = dms2dd(dd=Longitude_d,
                            mm=Longitude_m,
                            ss=Longitude_s,
                            ns=Longitude_EW))

## Convert to an sf object. Assume the table uses CRS 4269 ##


lab4sf <- st_as_sf(lab4geo, coords = c("Longitude", "Latitude"), crs = 4269)

##Using tmap, create a map of locations of the storm radar sites.##

tmap_mode("view")
tm_basemap("Esri.WorldTopoMap") + 
  tm_shape(lab4sf) +
  tm_dots(col="stationid",size=0.1)

##Transform radar data to UTM zone 15 (CRS 26915)##

lab4utm <- st_transform(lab4sf, 26915)

st_crs(lab4utm)

##Download the 2019 ACS-5 tract-level race data for Arkansas, Mississippi, and Louisiana.##

library("tidycensus")


key <- "cae655db94de4eb8814a2f5267333dc600a98dc8"

variable_list <- load_variables(2019, "acs5", cache = TRUE)

acsrace <-  c("B01001_001",
               "B01001A_001",
               "B01001B_001",
               "B01001C_001",
               "B01001D_001",
               "B01001E_001",
               "B01001F_001",
               "B01001G_001",
               "B01001H_001",
               "B01001I_001",
              "B06011_001",
              "B17026_002","B17026_003", "B17026_004" , "B17026_001")

acsrace2 <- get_acs(geography = "tract",
                    variables = acsrace, 
                    geometry = TRUE,
                    state = c("AR" , "MS" , "LA"), 
                    key = key, 
                    year = 2019)




acsrace3 <- acsrace2 %>% 
  dplyr::select(-moe) %>%
  spread(key=variable,value=estimate) %>%
  mutate(
         `ProportionAfrican-American` = B01001B_001/B01001_001,
         `Proportion\nBelow Poverty\nLevel` = (B17026_002+B17026_003+B17026_004)/B17026_001) %>%
  dplyr::select(-starts_with("B01"),-starts_with("B17")) %>%
  rename("ZCTA"="GEOID")


st_crs(acsrace3)


acsrace4 <- acsrace3 %>% 
  st_transform(crs = 26915) %>%
  mutate(area = st_area(.))


##Using st_buffer(), compute a 50 mile##
##buffer around each radar site##

st_crs(lab4utm)

lab4utmbox <- st_buffer(lab4utm, dist = 50 * 1609.34)



acsrace5 <- na.omit(acsrace4)


pal <- colorRampPalette(c("white", "blue"))(5)

tm_shape(acsrace5) +
  tm_polygons("ProportionAfrican-American", palette = pal, style = "quantile") +
  tm_shape(lab4utmbox) +
  tm_borders(lwd = 0.02) +
  tm_fill(col = NA, alpha = 0.5) +
  tm_shape(lab4utm) +
  tm_dots(size = 0.02, alpha = 0.5)

 
##Determining the number of tracts that overlap with the radar area##

overlaplab4 <- acsrace5 %>% mutate(lengths(st_intersects(acsrace5, lab4utmbox)) > 0)
summary(overlaplab4)

names(overlaplab4)[names(overlaplab4) == "lengths(st_intersects(acsrace5, lab4utmbox)) > 0"] <- "overlap"


names(overlaplab4)[names(overlaplab4) == "ProportionAfrican-American"] <- "proportionAA"


names(overlaplab4)[names(overlaplab4) == "Proportion\nBelow Poverty\nLevel"] <- "poverty"

##Violin plots##


ggplot(overlaplab4, aes(x = overlap, y = proportionAA , fill = overlap)) +
  geom_violin(scale = "count", trim = FALSE) +
  labs(x = "Overlapping storm radar buffer", y = "Proportion African-American") +
  theme_minimal()


ggplot(overlaplab4, aes(x = overlap, y = poverty , fill = overlap)) +
  geom_violin(scale = "count", trim = FALSE) +
  labs(x = "Overlapping storm radar buffer", y = "Proportion below poverty") +
  theme_minimal()

library(units)
overlaplab4$logarea <- log10(overlaplab4$area)
ggplot(overlaplab4, aes(x = overlap, y = logarea , fill = overlap)) +
  geom_violin(scale = "count", trim = FALSE) +
  labs(x = "Overlapping storm radar buffer", y = "Area of the radar") +
  theme_minimal()

#statistical analysis##

model1lab4 <- glm(overlap ~ proportionAA, data = overlaplab4, family = "quasibinomial")
summary(model1lab4)


model2lab4 <- glm(overlap ~ poverty, data = overlaplab4, family = "quasibinomial")
summary(model2lab4)



model3lab4 <- glm(overlap ~ logarea, data = overlaplab4, family = "quasibinomial")
summary(model3lab4)


model4lab4 <- glm(overlap ~ proportionAA + logarea + poverty, data = overlaplab4, family = "quasibinomial")
summary(model4lab4)

