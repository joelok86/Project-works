install.packages("parallel")
library("parallel")


detectCores() 

install.packages("foreach")

library("tidyverse")
library("sf")
library("foreach")


##creating smoke Polys##

urlBase <- 'https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile'

destfile <- "C:\\EHOH6621\\Environmental Data Science Course\\Smoke Polygons\\Polygons\\temp.zip"

destfile <- "C:\\Users\\Dell\\Documents\\Polygons\\temp.zip"

dates <- seq.Date(from=as.Date("2020-01-01"),to=as.Date("2020-12-31"),by="day") 



# loop takes 2.5 minutes on my computer, downloads about 10 MB of data
system.time({
  
  smokepolys <- foreach(date = dates, .packages=c("sf","tidyverse"), .combine="rbind")%do%{ #file input/output usually can't be parallelized
    
    yyyy <- str_sub(date,1,4)
    mm <- str_sub(date,6,7)
    #dd <- str_sub(date,9,10)
    dt <- str_replace_all(date,pattern="-",replacement="")
    
   
   fileURL <- str_c(urlBase, yyyy, mm, str_c("hms_smoke",dt,".zip"), sep ="/")
    destfile <- str_c(".\\Polygons\\temp.zip")
    
    try_error <- try(
      download.file(url=fileURL, destfile=destfile)
    )
    
    if( !(class(try_error) == "try-error") ){
      
      unzip(zipfile=destfile, 
            exdir=str_c(".\\Polygons\\hms_smoke",dt))
      file.remove(destfile)
      
      shape <- st_read(str_c(".\\Polygons\\hms_smoke",dt))
      
      if(identical(colnames(shape),c("Satellite","Start","End","Density","geometry"))){
        
        return( shape %>% 
                  mutate(Date = date))
        
      }else{
        
        return(NULL)
        
      }
      
      
      
    }else{
      
      return(NULL)
      
    }
    
  }
  
})#end system.time

save(x=smokepolys,file="US_HMSSmokePolygons_2020.RData")

##RAQSAPI PM25 DATA - MOntana, Idaho , Wyoming##

library(RAQSAPI)
aqs_credentials(username="james.crooks@cuanschutz.edu",
                key="carmelfox91")


library("lubridate")

dates2020 <- seq(ymd('2020-01-01'),ymd('2020-12-31'), by = 'day')
monitorPM2.5 <- bind_rows(
  aqs_dailysummary_by_state(parameter="88101",
                           bdate=min(dates2020),
                           edate=max(dates2020),
                           stateFIPS ="16"),
  aqs_dailysummary_by_state(parameter="88502",
                           bdate=min(dates2020),
                           edate=max(dates2020),
                           stateFIPS ="16"),
  aqs_dailysummary_by_state(parameter="88502",
                           bdate=min(dates2020),
                           edate=max(dates2020),
                           stateFIPS="30"),
 aqs_dailysummary_by_state(parameter="88101",
                           bdate=min(dates2020),
                           edate=max(dates2020),
                           stateFIPS ="30"),
  aqs_dailysummary_by_state(parameter="88502",
                           bdate=min(dates2020),
                           edate=max(dates2020),
                           stateFIPS ="56") ,
 aqs_dailysummary_by_state(parameter="88101",
                           bdate=min(dates2020),
                           edate=max(dates2020),
                           stateFIPS ="56") ) %>% filter(pollutant_standard=="PM25 24-hour 2012") %>% 
  st_as_sf(coords = c("longitude","latitude")) %>%
  st_set_crs("EPSG:4326") %>% 
  mutate(date = date_local,
         pollutant = "PM2.5") %>%
  rename("concentration"="arithmetic_mean") %>%
  select(geometry,state_code,county_code,site_number,date,pollutant,concentration) 


##smokeplumestable##

table(smokepolys$Satellite)
which.max(table(smokepolys$Satellite))

#group-splitting##

library(dplyr)


# Convert date column to date format
monitorpm25 <- monitorPM2.5
monitorpm25$date <- as.Date(monitorpm25$date)

# Split by date
monitorpm25list <- monitorpm25 %>% 
  group_split(date)

smokedf <- smokepolys
smokedf$date <- as.Date(smokepolys$Date)

smokedlist <- smokedf %>% 
  group_split(date)

##leaflet##

pm25day238 <- monitorpm25list[[which(dates == "2020-08-25")]] 

# filter smoke plume sf object for day 238 of 2020
smokeday238 <- smokedflist[[which(dates == "2020-08-25")]]

# create leaflet map
library("leaflet")

map <- leaflet() %>%
  addTiles() %>%
  addPolygons(data = smokeday238, color = "black", fillColor = "red", fillOpacity = 0.5) %>%
  addCircleMarkers(data = pm25day238, radius = 5, color = "black", fillColor = ~colorQuantile("YlOrRd", 
                                    pm25day238$concentration)(pm25day238$concentration), 
                   fillOpacity = 0.7, stroke = FALSE, label = ~paste("PM2.5:", pm25day238$concentration, "µg/m³"))
# print map
map

install.packages("parallel")

library(parallel)

# create a cluster with 4 cores
cl <- makeCluster(4)

# register the cluster with foreach
registerDoParallel(cl)

cores <- detectCores() - 1
cl <- makeCluster(cores)
registerDoParallel(cl)
library(doParallel)
library("sf")
library("dplyr")
library("sp")
library("tidycensus")
install.packages("foreach")
library("foreach")

smokepolys <- st_make_valid(smokepolys)

st_crs(smokepolys)
st_crs(monitorpm25)
smokecrs <- st_crs(smokepolys)

# Set the SRS of monitorpm25 to be the same as smokepolys
monitorpm25 <- st_set_crs(monitorpm25, smokecrs)


st_bbox(monitorpm25)
st_bbox(smokepolys)

smokepolys <- na.omit(smokepolys)

monitorpm25$date <- as.Date(monitorpm25$date)
smokepolys$date <- as.Date(smokepolys$Date)
dates <- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day")
smokepolys <- st_make_valid(smokepolys)

# Define function to compute the number of overlapping plumes and whether there's at least 1 overlapping plume

countoverlaps <- function(dates, monitorpm25list, smokedlist) {
  overlaps <- lapply(seq_along(dates), function(i) {
    monitorpm <- monitorpm25list[[i]]
    smokepoly <- smokedlist[[i]]
    monitorsdate <- monitorpm %>% filter(date == dates[i])
    overlapdate <- monitorsdate %>%
      mutate(overlap = lengths(st_intersects(., smokepoly %>% st_make_valid(.))) > 0)
    return(overlapdate)
  })
  return(overlaps)
}

system.time(results <- foreach(i = seq_along(dates)) %do% {
  countoverlaps(dates[i], monitorpm25list, smokedlist)
})




##this worked##



countoverlaps <- function(monitorpm25, smokepolys) {
  
  overlaplab5 <- monitorpm25%>% mutate(plumecount = lengths(st_intersects(monitorpm25, smokepolys)),
plumeyesno = ifelse(plumecount>0, TRUE, FALSE))
return(tibble(overlaplab5))}
system.time({ results <- foreach(i=1:366, .packages = c("tidyverse", "sf")) %do% {
  countoverlaps(monitorpm25list[[i]], smokedlist[[i]])}
})

## with %dopar%##


countoverlaps <- function(monitorpm25, smokepolys) {
  
overlaplab5 <- monitorpm25%>% mutate(plume_count = lengths(st_intersects(monitorpm25, smokepolys)),
plumeyesno = ifelse(plumecount>0, TRUE, FALSE)) return(tibble(overlaplab5))}

system.time({ resultspar <- foreach(i=1:366, .packages = c("tidyverse", "sf")) %dopar% {
  countoverlaps(monitorpm25list[[i]], smokedlist[[i]])}
})


##confirmation if both are identical ##


identical(results, resultspar)


# Sequential loop with matrix inversion
system.time({
  for (i in 1:366) {
    countoverlaps(monitorpm25list[[i]], smokedlist[[i]])
    n <- 400
    solve(matrix(rnorm(n*n),n,n))
  }
})

# Parallel loop with matrix inversion
system.time({
  cl <- makeCluster(detectCores()-1)
  registerDoParallel(cl)
  foreach(i=1:366, .packages=c("tidyverse", "sf")) %dopar% {
    countoverlaps(monitorpm25list[[i]], smokedlist[[i]])
    n <- 400
    solve(matrix(rnorm(n*n),n,n))
  }
  stopCluster(cl)
})

# Increase n to 1000 and repeat
system.time({
  for (i in 1:366) {
    countoverlaps(monitorpm25list[[i]], smokedlist[[i]])
    n <- 1000
    solve(matrix(rnorm(n*n),n,n))
  }
})

system.time({
  cl <- makeCluster(detectCores()-1)
  registerDoParallel(cl)
  foreach(i=1:366, .packages=c("tidyverse", "sf")) %dopar% {
    countoverlaps(monitorpm25list[[i]], smokedlist[[i]])
    n <- 1000
    solve(matrix(rnorm(n*n),n,n))
  }
  stopCluster(cl)
})



library(foreach)
library(doParallel)
library(sf)
library(tidyverse)

# define the function countoverlaps
countoverlaps <- function(monitorpm25, smokepolys) {
  overlaplab5 <- monitorpm25 %>%
    mutate(plumecount = lengths(st_intersects(monitorpm25, smokepolys)),
           plumeyesno = ifelse(plumecount > 0, TRUE, FALSE))
  return(tibble(overlaplab5))
}

# define the number of cores to use
cores <- min(detectCores() - 1, 4)

# register a parallel backend with different number of cores
times <- vector(mode = "numeric", length = cores)
for (i in 1:cores) {
  cl <- makeCluster(i)
  registerDoParallel(cl)
  times[i] <- system.time({
    resultspar <- foreach(
      i = 1:366, .packages = c("tidyverse", "sf")) %dopar% {
        countoverlaps(monitorpm25list[[i]], smokedlist[[i]])
      }
  })["elapsed"]
  stopCluster(cl)
}

# plot the run time changes with the number of cores
plot(1:cores, times, type = "b", xlab = "Number of cores", ylab = "Time (seconds)")

#Statistical analysis and table form ##

overlaplab5 <- monitorpm25 %>% filter(date == dates) %>% mutate(lengths(st_intersects(monitorpm25, smokepolys)) > 0)


names(overlaplab5)[names(overlaplab5) == "overlap"] <- "overlap"

overlaplab5$month <- factor(format(overlaplab5$date, format="%m"))

overlaplab5$logx <- log(overlaplab5$concentration)

 overlaplab5cl <- overlaplab5[is.finite(overlaplab5$logx),]

model1lab5 <- glm(logx ~ plume_count + month + site_number, data = overlaplab5cl, family = "gaussian") %>% broom::tidy(conf.int=TRUE)

model2lab5 <- glm(logx ~ plume_count, data = overlaplab5cl, family = "gaussian")

summary(model1lab5)
summary(model2lab5)


library(openxlsx)
write.xlsx(model1lab5, file = "summarylab5.xlsx")
