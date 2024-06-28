## Loading the data##
load(" ")
pollendata <- PollenMonitors
library(tidycensus)
library(ggplot2)
library(tidyverse)
library(sf)
library(tmaptools)
view(pollendata)
rm(PollenMonitors)
gc(reset = TRUE)

## Change of names ##
names(pollendata)[names(pollendata) == "Monitor ID"] <- "monitor"
names(pollendata)[names(pollendata) == "Ambrosia / Iva"] <- "ambrosiaiva"
names(pollendata)[names(pollendata) == "Chenopodium / Amaranthus"] <- "chenopodiumama"
pollendata$monitor <- as.numeric(pollendata$monitor)

##Calculating median values for daily concentrations for each ##
##pollen species by changing the date and time into date format ## ##

pollendata$StartingAt <- as.Date(pollendata$StartingAt)
medianpollendata <- pollendata %>%
  group_by(monitor , StartingAt) %>%
  summarize(Pollen = median(Pollen, na.rm = TRUE),
            Ambrosia = median(Ambrosia, na.rm = TRUE),
             ambrosiaiva = median(ambrosiaiva, na.rm = TRUE),
 Artemisia = median(Artemisia, na.rm = TRUE),
 chenopodiumama = median(chenopodiumama, na.rm = TRUE),
Plantago = median(Plantago, na.rm = TRUE),
Rumex = median(Rumex, na.rm = TRUE),
 Acer = median(Acer, na.rm = TRUE),
 Alnus = median(Alnus, na.rm = TRUE),
 Betula = median(Betula, na.rm = TRUE),
 Carya = median(Carya, na.rm = TRUE),
 Cupressaceae = median(Cupressaceae, na.rm = TRUE),
 Fraxinus = median(Fraxinus, na.rm = TRUE),
 Morus = median(Morus, na.rm = TRUE),
Olea = median(Olea, na.rm = TRUE),
Pinus = median(Pinus, na.rm = TRUE),
Platanus = median(Platanus, na.rm = TRUE),
Populus = median(Populus, na.rm = TRUE),
Quercus = median(Quercus, na.rm = TRUE),
  Salix = median( Salix, na.rm = TRUE),
Ulmus = median(Ulmus, na.rm = TRUE),
Poaceae = median(Poaceae, na.rm = TRUE)) %>% select(monitor , geometry , Pollen , Ambrosia , ambrosiaiva
                                                    , Artemisia , chenopodiumama , Plantago , Rumex ,
                                                    Acer , Alnus , Betula , Carya , Cupressaceae ,
                                                    Fraxinus , Morus , Olea , Pinus , Platanus , Populus,
                                                    Quercus , Salix , Ulmus , Poaceae , StartingAt)

## plotting time series##

pollenagg <- aggregate(Pollen ~ monitor + StartingAt, data = pollendata, sum)
summary(pollenagg)
ggplot(pollenagg, aes(x = StartingAt, y = Pollen, group = monitor, color = monitor)) +
  geom_line() +
  labs(x = "StartingAt", y = "Total Pollen Count", title = "Total Pollen Counts by Monitor") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme_bw()

##Need to draw bounding box for each monitor ID, with 5 km buffer and mapping ##


dates <- medianpollendata %>%
  group_by(StartingAt) %>%
  summarize(n_monitors = n_distinct(monitor))
monitordates <- dates %>%
  filter(n_monitors == 4) 

pollendata2 <- medianpollendata %>% st_buffer(monitor, dist = 5000)

library(dplyr)
library(tidyverse)
library(sf)
library(tidycensus)
library(lubridate)
library(tmap)
library(tmaptools)
tmap_mode("view")
tm_basemap("Esri.WorldTopoMap") + 
 tm_shape(pollendata2) +
  tm_dots(col="monitor",size=0.1)
   

          
          
##Changing the Data into long format##         



pollendata3 <- pollendata2 %>% gather(key = "key" , value = "value"
                                      , -geometry , -StartingAt , - monitor)

##Plotting the data using ggplot ##

pollendata3 <- pollendata3 %>% mutate(month = month(StartingAt))
 summary(pollendata3)
 ggplot(pollendata3, aes(x = factor(month), y = log10(value))) + 
   geom_boxplot() + 
    facet_grid(rows = vars(key), cols = vars(monitor)) +
    ylab("Log10 Pollen Values") + 
    xlab("Month")

 ##Plotting the median daily concentration of each species, and creating a facet_grid() ##
 ##of panels (species in rows, Monitor ID in columns), each of which displays a geom_line() ## 
 ##timeseries of the daily median concentration (on the log10 scale)##
 

 ggplot(pollendata3, aes(x = StartingAt, y = log10(value))) +
   geom_line() +
   scale_y_continuous(name = "Log10 median pollen values") +
   facet_grid(rows = vars(key), cols = vars(monitor))
 
 ##Installing RAQSAPI package and signing up ##
 
 install.packages("RAQSAPI")
 library(RAQSAPI)
 aqs_sign_up(" ")
 aqs_credentials(username=" ",
                 key=" ")
 



 dates2020 <- seq(ymd('2020-01-01'),ymd('2020-12-31'), by = 'day')
 utahmonitorPM10 <- bind_rows(
   aqs_dailysummary_by_cbsa(parameter="81102",
                            bdate=min(dates2020),
                            edate=max(dates2020),
                            cbsa_code="41620"),
   aqs_dailysummary_by_cbsa(parameter="81102",
                            bdate=min(dates2020),
                            edate=max(dates2020),
                            cbsa_code="39340"),
   aqs_dailysummary_by_cbsa(parameter="81102",
                            bdate=min(dates2020),
                            edate=max(dates2020),
                            cbsa_code="36260") ) %>% filter(pollutant_standard=="PM10 24-hour 2006") %>% 
   st_as_sf(coords = c("longitude","latitude")) %>%
   st_set_crs("EPSG:4326") %>% 
   mutate(date = date_local,
          pollutant = "PM10") %>%
   rename("concentration"="arithmetic_mean") %>%
   select(geometry,state_code,county_code,site_number,date,pollutant,concentration) 
 
 ##checking he projection for both data sets ##
 
 
 monitorcrs <- st_crs(utahmonitorPM10)
 monitorcrs
 monitorcrs <- st_crs(pollendata3)
 monitorcrs
 
##mapping the pollen monitors and PM10 monitors## 
 
 names(utahmonitorPM10)[names(utahmonitorPM10) == "site_number"] <- "siteid"
 names(utahmonitorPM10)[names(utahmonitorPM10) == "county_code"] <- "county" 
 
 tmap_mode("view")
 tm_basemap("Esri.WorldTopoMap") + 
   tm_shape(pollendata2) +
   tm_dots(col="monitor",size=0.1) +
 tm_shape(utahmonitorPM10) +
   tm_dots(col = "siteid", size = 0.1) 

summary(utahmonitorPM10) 


##calculating distance between the pollen sense monitors and PM10 monitors##
dates1 <- utahmonitorPM10 %>%
  group_by(date) %>%
  summarize(n_monitors = n_distinct(siteid))
monitordates1 <- dates1 %>%
  filter(n_monitors == 6)

pollensingleday <- pollendata3 %>% 
  filter(StartingAt == "2020-01-10")

pm10singleday <- utahmonitorPM10 %>% 
  filter(date == "2020-01-10")

distance <- st_distance(pollensingleday , pm10singleday)

##Identifying the nearest PM10 for each pollen monitor##
 

nearpm10 <- st_nearest_feature(pollensingleday, pm10singleday)

closestdistance <- data.frame(pollenloc = pollensingleday$monitor,
                              +                          closestpm = pm10singleday[closestdist, "siteid"],
                              +                          distances = distance[cbind(1:nrow(distance), closestdist)]) 
print(closestdistance)

## Modifying data for join - Assigning common matching value, changing into date format, 
## dropping geometry of the PM10 file ##

trialdatapollen <- pollendata3 %>% 
  mutate(alpha = case_when(
    monitor == 34017 ~ "a",
    monitor == 52236 ~ "b",
    monitor == 54147 ~ "c",
    monitor == 66123 ~ "d",
    TRUE ~ NA_character_ 
  ))

trialdatapm10 <- utahmonitorPM10 %>% 
  mutate(alpha = case_when(
    siteid == 4001 ~ "a",
    siteid == 4001 ~ "b",
    siteid == 4001 ~ "c",
    siteid == 3006 ~ "d",
    TRUE ~ NA_character_ 
  )) 

trialdatapm10f <- st_drop_geometry(trialdatapm10)

trialdatapm10p <- trialdatapm10f[complete.cases(trialdatapm10f), ]

trialdatapollen <- rename(trialdatapollen, date = StartingAt)

trialdatapm10p$date <- as.Date(trialdatapm10p$date)


## Joining the pollen monitors with PM 10 values of the nearest PM 10 monitor ##

mergedlab2 <- left_join(trialdatapollen, trialdatapm10p, by = c("alpha", "date"))



##Creating a multipanel plot using facet_wrap() with a panel for each species, ##
##and within each panel use geom_point() to display a scatter plot of daily ##
##median pollen concentration vs. daily PM10 concentration.  Include a linear##
##trend line using geom_smooth(method=”lm”)##


scatterplot <- ggplot(mergedlab2, aes(x = concentration, y = value, color = key)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Daily PM10 concentration", y = "Daily median pollen values") +
  theme_bw()
multipanelplot <- scatterplot +
  facet_wrap(~key)
print(multipanelplot)


mergedlab2final <- na.omit(mergedlab2)

##Linear model for each species for association between pollen concentration and PM10 values##

lab2summary <- mergedlab2final %>%
  group_by(key) %>%
  group_modify(~ lm(concentration ~ value + monitor + month, data = .) %>% 
                 broom::tidy(conf.int=TRUE))
  
lab2table <- subset(lab2summary, term != c("monitor" , "month"))
lab2table

## Exporting the table with value and intercept in excel format##

install.packages("openxlsx")
library(openxlsx)
write.xlsx(lab2table, file = "summarylab2.xlsx")

 
 
 
 
 
 
 
 
 
 
 
 
 
 
