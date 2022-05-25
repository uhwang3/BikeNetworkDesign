library(sf)
library(tidyverse)
library(tmap)
library(lwgeom)
library(httr)
library(jsonlite)
library(dplyr)
library(data.table)
library(tictoc)
library(nngeo)
library(spdep)
library(magrittr)

setwd("#####")


## load drive data
data <- st_read('./data/OSM/data/drive_network_0405/edges/edges.shp')


## get coordinates for each end of edges (3-4 hours)
coord <- st_coordinates(data) %>% as.data.frame()
coord.ends <- data.frame(start.x = numeric(0), start.y = numeric(0), end.x = numeric(0), end.y = numeric(0))
coord.ends.temp <- data.frame(start.x = numeric(0), start.y = numeric(0), end.x = numeric(0), end.y = numeric(0))

pb <- txtProgressBar(min = 0, max = length(unique(coord$L1)), style = 3)

for (i in unique(coord$L1)) {
  coord.ends.temp[,1:2] <- coord[coord$L1 == i,1:2][1,]
  coord.ends.temp[,3:4] <- coord[coord$L1 == i,1:2][length(coord$L1[coord$L1 == i]),]
  
  coord.ends <- rbind(coord.ends, coord.ends.temp)  
  setTxtProgressBar(pb, i)
}
rm(coord, coord.ends.temp)

## create dataset for BING maps api query
edges <- data.frame(id = 1:nrow(data)) %>% 
  cbind(select(data, oneway, length) %>% st_set_geometry(NULL), coord.ends)


## split edges not to exceed the BING API limit (125,000 per key)
edges.1 <- edges[1:60000,]
edges.2 <- edges[60001:120000,]
edges.3 <- edges[120001:177110,]


# save.image(file = '0406_data_for_bing.RData') 
# load('0406_data_for_bing.RData')


## set progress bars
tic()
pb.1 <- txtProgressBar(min = 1, max = nrow(edges.1), style = 3)
pb.2 <- txtProgressBar(min = 1, max = nrow(edges.2), style = 3)
pb.3 <- txtProgressBar(min = 1, max = nrow(edges.3), style = 3)

##### BING API -- time 1 (1/3)###################################################################
api.key.1 <- "#####"
time.8  <- "02/12/2020 08:00:00"

result.time.8.1 <- edges.1 %>% 
  select(id, length) %>% 
  mutate(dist = NA, dur = NA, dur.t = NA, dist.r = NA, dur.r = NA, dur.t.r = NA)
  
for (i in 1:nrow(edges.1)) {
  start  <-  paste0(edges.1[i,5],',',edges.1[i,4])
  end <-  paste0(edges.1[i,7],',',edges.1[i,6])
  prefix <-  'https://dev.virtualearth.net/REST/v1/Routes?'
  url <- paste0(prefix,"wp.1=",start,'&wp.2=',end,'&dateTime=',time.8,'&key=',api.key.1)
  
  json <- tryCatch(expr = {fromJSON(readLines(url, warn = F))}, warning = function(w){NULL})
  #json <- fromJSON(readLines(url, warn = F))
  
  if (edges.1[i,2] == 'False') {
    url.r <- paste0(prefix,"wp.1=",end,'&wp.2=',start,'&dateTime=',time.8,'&key=',api.key.1)
    json.r <- tryCatch(expr = {fromJSON(readLines(url.r, warn = F))}, warning = function(w){NULL})
    #json.r <- fromJSON(readLines(url.r, warn = F))
    
    if (is.null(json) == F & is.null(json.r) == F) {
    result.time.8.1[i,'dist'] <- json$resourceSets[[2]][[1]]$travelDistance
    result.time.8.1[i,'dur'] <- json$resourceSets[[2]][[1]]$travelDuration
    result.time.8.1[i,'dur.t'] <- json$resourceSets[[2]][[1]]$travelDurationTraffic
    result.time.8.1[i,'dist.r'] <- json.r$resourceSets[[2]][[1]]$travelDistance
    result.time.8.1[i,'dur.r'] <- json.r$resourceSets[[2]][[1]]$travelDuration
    result.time.8.1[i,'dur.t.r'] <- json.r$resourceSets[[2]][[1]]$travelDurationTraffic
    }
  
  } else {
    if(is.null(json) == F) {
      result.time.8.1[i,'dist'] <- json$resourceSets[[2]][[1]]$travelDistance
      result.time.8.1[i,'dur'] <- json$resourceSets[[2]][[1]]$travelDuration
      result.time.8.1[i,'dur.t'] <- json$resourceSets[[2]][[1]]$travelDurationTraffic
    }
  }
  setTxtProgressBar(pb.1, i)
}
print("1/9 finished")


##### BING API -- time 1 (2/3)###################################################################
api.key.2 <- "#####"
time.8  <- "02/12/2020 08:00:00"

result.time.8.2 <- edges.2 %>% 
  select(id, length) %>% 
  mutate(dist = NA, dur = NA, dur.t = NA, dist.r = NA, dur.r = NA, dur.t.r = NA)

for (i in 1:nrow(edges.2)) {
  start  <-  paste0(edges.2[i,5],',',edges.2[i,4])
  end <-  paste0(edges.2[i,7],',',edges.2[i,6])
  prefix <-  'https://dev.virtualearth.net/REST/v1/Routes?'
  url <- paste0(prefix,"wp.1=",start,'&wp.2=',end,'&dateTime=',time.8,'&key=',api.key.2)
  
  json <- tryCatch(expr = {fromJSON(readLines(url, warn = F))}, warning = function(w){NULL})
  #json <- fromJSON(readLines(url, warn = F))
  
  if (edges.2[i,2] == 'False') {
    url.r <- paste0(prefix,"wp.1=",end,'&wp.2=',start,'&dateTime=',time.8,'&key=',api.key.2)
    json.r <- tryCatch(expr = {fromJSON(readLines(url.r, warn = F))}, warning = function(w){NULL})
    #json.r <- fromJSON(readLines(url.r, warn = F))
    
    if (is.null(json) == F & is.null(json.r) == F) {
      result.time.8.2[i,'dist'] <- json$resourceSets[[2]][[1]]$travelDistance
      result.time.8.2[i,'dur'] <- json$resourceSets[[2]][[1]]$travelDuration
      result.time.8.2[i,'dur.t'] <- json$resourceSets[[2]][[1]]$travelDurationTraffic
      result.time.8.2[i,'dist.r'] <- json.r$resourceSets[[2]][[1]]$travelDistance
      result.time.8.2[i,'dur.r'] <- json.r$resourceSets[[2]][[1]]$travelDuration
      result.time.8.2[i,'dur.t.r'] <- json.r$resourceSets[[2]][[1]]$travelDurationTraffic
    }
    
  } else {
    if(is.null(json) == F) {
      result.time.8.2[i,'dist'] <- json$resourceSets[[2]][[1]]$travelDistance
      result.time.8.2[i,'dur'] <- json$resourceSets[[2]][[1]]$travelDuration
      result.time.8.2[i,'dur.t'] <- json$resourceSets[[2]][[1]]$travelDurationTraffic
    }
  }
  setTxtProgressBar(pb.2, i)
}
print("2/9 finished")


##### BING API -- time 1 (3/3)###################################################################
api.key.3 <- "#####"
time.8  <- "02/12/2020 08:00:00"

result.time.8.3 <- edges.3 %>% 
  select(id, length) %>% 
  mutate(dist = NA, dur = NA, dur.t = NA, dist.r = NA, dur.r = NA, dur.t.r = NA)

for (i in 1:nrow(edges.3)) {
  start  <-  paste0(edges.3[i,5],',',edges.3[i,4])
  end <-  paste0(edges.3[i,7],',',edges.3[i,6])
  prefix <-  'https://dev.virtualearth.net/REST/v1/Routes?'
  url <- paste0(prefix,"wp.1=",start,'&wp.2=',end,'&dateTime=',time.8,'&key=',api.key.3)
  
  json <- tryCatch(expr = {fromJSON(readLines(url, warn = F))}, warning = function(w){NULL})
  #json <- fromJSON(readLines(url, warn = F))
  
  if (edges.3[i,2] == 'False') {
    url.r <- paste0(prefix,"wp.1=",end,'&wp.2=',start,'&dateTime=',time.8,'&key=',api.key.3)
    json.r <- tryCatch(expr = {fromJSON(readLines(url.r, warn = F))}, warning = function(w){NULL})
    #json.r <- fromJSON(readLines(url.r, warn = F))
    
    if (is.null(json) == F & is.null(json.r) == F) {
      result.time.8.3[i,'dist'] <- json$resourceSets[[2]][[1]]$travelDistance
      result.time.8.3[i,'dur'] <- json$resourceSets[[2]][[1]]$travelDuration
      result.time.8.3[i,'dur.t'] <- json$resourceSets[[2]][[1]]$travelDurationTraffic
      result.time.8.3[i,'dist.r'] <- json.r$resourceSets[[2]][[1]]$travelDistance
      result.time.8.3[i,'dur.r'] <- json.r$resourceSets[[2]][[1]]$travelDuration
      result.time.8.3[i,'dur.t.r'] <- json.r$resourceSets[[2]][[1]]$travelDurationTraffic
    }
    
  } else {
    if(is.null(json) == F) {
      result.time.8.3[i,'dist'] <- json$resourceSets[[2]][[1]]$travelDistance
      result.time.8.3[i,'dur'] <- json$resourceSets[[2]][[1]]$travelDuration
      result.time.8.3[i,'dur.t'] <- json$resourceSets[[2]][[1]]$travelDurationTraffic
    }
  }
  setTxtProgressBar(pb.3, i)
}

result.time.8 <- rbind(result.time.8.1, result.time.8.2, result.time.8.3)
print("3/9 finished")



##### BING API -- time 2 (1/3)###################################################################
api.key.4 <- "#####"
time.13  <- "02/12/2020 13:00:00"

result.time.13.1 <- edges.1 %>% 
  select(id, length) %>% 
  mutate(dist = NA, dur = NA, dur.t = NA, dist.r = NA, dur.r = NA, dur.t.r = NA)

for (i in 1:nrow(edges.1)) {
  start  <-  paste0(edges.1[i,5],',',edges.1[i,4])
  end <-  paste0(edges.1[i,7],',',edges.1[i,6])
  prefix <-  'https://dev.virtualearth.net/REST/v1/Routes?'
  url <- paste0(prefix,"wp.1=",start,'&wp.2=',end,'&dateTime=',time.13,'&key=',api.key.4)
  
  json <- tryCatch(expr = {fromJSON(readLines(url, warn = F))}, warning = function(w){NULL})
  #json <- fromJSON(readLines(url, warn = F))
  
  if (edges.1[i,2] == 'False') {
    url.r <- paste0(prefix,"wp.1=",end,'&wp.2=',start,'&dateTime=',time.13,'&key=',api.key.4)
    json.r <- tryCatch(expr = {fromJSON(readLines(url.r, warn = F))}, warning = function(w){NULL})
    #json.r <- fromJSON(readLines(url.r, warn = F))
    
    if (is.null(json) == F & is.null(json.r) == F) {
      result.time.13.1[i,'dist'] <- json$resourceSets[[2]][[1]]$travelDistance
      result.time.13.1[i,'dur'] <- json$resourceSets[[2]][[1]]$travelDuration
      result.time.13.1[i,'dur.t'] <- json$resourceSets[[2]][[1]]$travelDurationTraffic
      result.time.13.1[i,'dist.r'] <- json.r$resourceSets[[2]][[1]]$travelDistance
      result.time.13.1[i,'dur.r'] <- json.r$resourceSets[[2]][[1]]$travelDuration
      result.time.13.1[i,'dur.t.r'] <- json.r$resourceSets[[2]][[1]]$travelDurationTraffic
    }
    
  } else {
    if(is.null(json) == F) {
      result.time.13.1[i,'dist'] <- json$resourceSets[[2]][[1]]$travelDistance
      result.time.13.1[i,'dur'] <- json$resourceSets[[2]][[1]]$travelDuration
      result.time.13.1[i,'dur.t'] <- json$resourceSets[[2]][[1]]$travelDurationTraffic
    }
  }
  setTxtProgressBar(pb.1, i)
}
print("4/9 finished")


##### BING API -- time 2 (2/3)###################################################################
api.key.5 <- "#####"
time.13  <- "02/12/2020 13:00:00"

result.time.13.2 <- edges.2 %>% 
  select(id, length) %>% 
  mutate(dist = NA, dur = NA, dur.t = NA, dist.r = NA, dur.r = NA, dur.t.r = NA)

for (i in 1:nrow(edges.2)) {
  start  <-  paste0(edges.2[i,5],',',edges.2[i,4])
  end <-  paste0(edges.2[i,7],',',edges.2[i,6])
  prefix <-  'https://dev.virtualearth.net/REST/v1/Routes?'
  url <- paste0(prefix,"wp.1=",start,'&wp.2=',end,'&dateTime=',time.13,'&key=',api.key.5)
  
  json <- tryCatch(expr = {fromJSON(readLines(url, warn = F))}, warning = function(w){NULL})
  #json <- fromJSON(readLines(url, warn = F))
  
  if (edges.2[i,2] == 'False') {
    url.r <- paste0(prefix,"wp.1=",end,'&wp.2=',start,'&dateTime=',time.13,'&key=',api.key.5)
    json.r <- tryCatch(expr = {fromJSON(readLines(url.r, warn = F))}, warning = function(w){NULL})
    #json.r <- fromJSON(readLines(url.r, warn = F))
    
    if (is.null(json) == F & is.null(json.r) == F) {
      result.time.13.2[i,'dist'] <- json$resourceSets[[2]][[1]]$travelDistance
      result.time.13.2[i,'dur'] <- json$resourceSets[[2]][[1]]$travelDuration
      result.time.13.2[i,'dur.t'] <- json$resourceSets[[2]][[1]]$travelDurationTraffic
      result.time.13.2[i,'dist.r'] <- json.r$resourceSets[[2]][[1]]$travelDistance
      result.time.13.2[i,'dur.r'] <- json.r$resourceSets[[2]][[1]]$travelDuration
      result.time.13.2[i,'dur.t.r'] <- json.r$resourceSets[[2]][[1]]$travelDurationTraffic
    }
    
  } else {
    if(is.null(json) == F) {
      result.time.13.2[i,'dist'] <- json$resourceSets[[2]][[1]]$travelDistance
      result.time.13.2[i,'dur'] <- json$resourceSets[[2]][[1]]$travelDuration
      result.time.13.2[i,'dur.t'] <- json$resourceSets[[2]][[1]]$travelDurationTraffic
    }
  }
  setTxtProgressBar(pb.2, i)
}
print("5/9 finished")


##### BING API -- time 2 (3/3)###################################################################
api.key.6 <- "#####"
time.13  <- "02/12/2020 13:00:00"

result.time.13.3 <- edges.3 %>% 
  select(id, length) %>% 
  mutate(dist = NA, dur = NA, dur.t = NA, dist.r = NA, dur.r = NA, dur.t.r = NA)

for (i in 1:nrow(edges.3)) {
  start  <-  paste0(edges.3[i,5],',',edges.3[i,4])
  end <-  paste0(edges.3[i,7],',',edges.3[i,6])
  prefix <-  'https://dev.virtualearth.net/REST/v1/Routes?'
  url <- paste0(prefix,"wp.1=",start,'&wp.2=',end,'&dateTime=',time.13,'&key=',api.key.6)
  
  json <- tryCatch(expr = {fromJSON(readLines(url, warn = F))}, warning = function(w){NULL})
  #json <- fromJSON(readLines(url, warn = F))
  
  if (edges.3[i,2] == 'False') {
    url.r <- paste0(prefix,"wp.1=",end,'&wp.2=',start,'&dateTime=',time.13,'&key=',api.key.6)
    json.r <- tryCatch(expr = {fromJSON(readLines(url.r, warn = F))}, warning = function(w){NULL})
    #json.r <- fromJSON(readLines(url.r, warn = F))
    
    if (is.null(json) == F & is.null(json.r) == F) {
      result.time.13.3[i,'dist'] <- json$resourceSets[[2]][[1]]$travelDistance
      result.time.13.3[i,'dur'] <- json$resourceSets[[2]][[1]]$travelDuration
      result.time.13.3[i,'dur.t'] <- json$resourceSets[[2]][[1]]$travelDurationTraffic
      result.time.13.3[i,'dist.r'] <- json.r$resourceSets[[2]][[1]]$travelDistance
      result.time.13.3[i,'dur.r'] <- json.r$resourceSets[[2]][[1]]$travelDuration
      result.time.13.3[i,'dur.t.r'] <- json.r$resourceSets[[2]][[1]]$travelDurationTraffic
    }
    
  } else {
    if(is.null(json) == F) {
      result.time.13.3[i,'dist'] <- json$resourceSets[[2]][[1]]$travelDistance
      result.time.13.3[i,'dur'] <- json$resourceSets[[2]][[1]]$travelDuration
      result.time.13.3[i,'dur.t'] <- json$resourceSets[[2]][[1]]$travelDurationTraffic
    }
  }
  setTxtProgressBar(pb.3, i)
}

result.time.13 <- rbind(result.time.13.1, result.time.13.2, result.time.13.3)
print("6/9 finished")



##### BING API -- time 3 (1/3)###################################################################
api.key.7 <- "#####"
time.18  <- "02/12/2020 18:00:00"

result.time.18.1 <- edges.1 %>% 
  select(id, length) %>% 
  mutate(dist = NA, dur = NA, dur.t = NA, dist.r = NA, dur.r = NA, dur.t.r = NA)

for (i in 1:nrow(edges.1)) {
  start  <-  paste0(edges.1[i,5],',',edges.1[i,4])
  end <-  paste0(edges.1[i,7],',',edges.1[i,6])
  prefix <-  'https://dev.virtualearth.net/REST/v1/Routes?'
  url <- paste0(prefix,"wp.1=",start,'&wp.2=',end,'&dateTime=',time.18,'&key=',api.key.7)
  
  json <- tryCatch(expr = {fromJSON(readLines(url, warn = F))}, warning = function(w){NULL})
  #json <- fromJSON(readLines(url, warn = F))
  
  if (edges.1[i,2] == 'False') {
    url.r <- paste0(prefix,"wp.1=",end,'&wp.2=',start,'&dateTime=',time.18,'&key=',api.key.7)
    json.r <- tryCatch(expr = {fromJSON(readLines(url.r, warn = F))}, warning = function(w){NULL})
    #json.r <- fromJSON(readLines(url.r, warn = F))
    
    if (is.null(json) == F & is.null(json.r) == F) {
      result.time.18.1[i,'dist'] <- json$resourceSets[[2]][[1]]$travelDistance
      result.time.18.1[i,'dur'] <- json$resourceSets[[2]][[1]]$travelDuration
      result.time.18.1[i,'dur.t'] <- json$resourceSets[[2]][[1]]$travelDurationTraffic
      result.time.18.1[i,'dist.r'] <- json.r$resourceSets[[2]][[1]]$travelDistance
      result.time.18.1[i,'dur.r'] <- json.r$resourceSets[[2]][[1]]$travelDuration
      result.time.18.1[i,'dur.t.r'] <- json.r$resourceSets[[2]][[1]]$travelDurationTraffic
    }
    
  } else {
    if(is.null(json) == F) {
      result.time.18.1[i,'dist'] <- json$resourceSets[[2]][[1]]$travelDistance
      result.time.18.1[i,'dur'] <- json$resourceSets[[2]][[1]]$travelDuration
      result.time.18.1[i,'dur.t'] <- json$resourceSets[[2]][[1]]$travelDurationTraffic
    }
  }
  setTxtProgressBar(pb.1, i)
}
print("7/9 finished")


##### BING API -- time 3 (2/3)###################################################################
api.key.8 <- "#####"
time.18  <- "02/12/2020 18:00:00"

result.time.18.2 <- edges.2 %>% 
  select(id, length) %>% 
  mutate(dist = NA, dur = NA, dur.t = NA, dist.r = NA, dur.r = NA, dur.t.r = NA)

for (i in 1:nrow(edges.2)) {
  start  <-  paste0(edges.2[i,5],',',edges.2[i,4])
  end <-  paste0(edges.2[i,7],',',edges.2[i,6])
  prefix <-  'https://dev.virtualearth.net/REST/v1/Routes?'
  url <- paste0(prefix,"wp.1=",start,'&wp.2=',end,'&dateTime=',time.18,'&key=',api.key.8)
  
  json <- tryCatch(expr = {fromJSON(readLines(url, warn = F))}, warning = function(w){NULL})
  #json <- fromJSON(readLines(url, warn = F))
  
  if (edges.2[i,2] == 'False') {
    url.r <- paste0(prefix,"wp.1=",end,'&wp.2=',start,'&dateTime=',time.18,'&key=',api.key.8)
    json.r <- tryCatch(expr = {fromJSON(readLines(url.r, warn = F))}, warning = function(w){NULL})
    #json.r <- fromJSON(readLines(url.r, warn = F))
    
    if (is.null(json) == F & is.null(json.r) == F) {
      result.time.18.2[i,'dist'] <- json$resourceSets[[2]][[1]]$travelDistance
      result.time.18.2[i,'dur'] <- json$resourceSets[[2]][[1]]$travelDuration
      result.time.18.2[i,'dur.t'] <- json$resourceSets[[2]][[1]]$travelDurationTraffic
      result.time.18.2[i,'dist.r'] <- json.r$resourceSets[[2]][[1]]$travelDistance
      result.time.18.2[i,'dur.r'] <- json.r$resourceSets[[2]][[1]]$travelDuration
      result.time.18.2[i,'dur.t.r'] <- json.r$resourceSets[[2]][[1]]$travelDurationTraffic
    }
    
  } else {
    if(is.null(json) == F) {
      result.time.18.2[i,'dist'] <- json$resourceSets[[2]][[1]]$travelDistance
      result.time.18.2[i,'dur'] <- json$resourceSets[[2]][[1]]$travelDuration
      result.time.18.2[i,'dur.t'] <- json$resourceSets[[2]][[1]]$travelDurationTraffic
    }
  }
  setTxtProgressBar(pb.2, i)
}
print("8/9 finished")


##### BING API -- time 3 (3/3)###################################################################
api.key.9 <- "#####"
time.18  <- "02/12/2020 18:00:00"

result.time.18.3 <- edges.3 %>% 
  select(id, length) %>% 
  mutate(dist = NA, dur = NA, dur.t = NA, dist.r = NA, dur.r = NA, dur.t.r = NA)

for (i in 1:nrow(edges.3)) {
  start  <-  paste0(edges.3[i,5],',',edges.3[i,4])
  end <-  paste0(edges.3[i,7],',',edges.3[i,6])
  prefix <-  'https://dev.virtualearth.net/REST/v1/Routes?'
  url <- paste0(prefix,"wp.1=",start,'&wp.2=',end,'&dateTime=',time.18,'&key=',api.key.9)
  
  json <- tryCatch(expr = {fromJSON(readLines(url, warn = F))}, warning = function(w){NULL})
  #json <- fromJSON(readLines(url, warn = F))
  
  if (edges.3[i,2] == 'False') {
    url.r <- paste0(prefix,"wp.1=",end,'&wp.2=',start,'&dateTime=',time.18,'&key=',api.key.9)
    json.r <- tryCatch(expr = {fromJSON(readLines(url.r, warn = F))}, warning = function(w){NULL})
    #json.r <- fromJSON(readLines(url.r, warn = F))
    
    if (is.null(json) == F & is.null(json.r) == F) {
      result.time.18.3[i,'dist'] <- json$resourceSets[[2]][[1]]$travelDistance
      result.time.18.3[i,'dur'] <- json$resourceSets[[2]][[1]]$travelDuration
      result.time.18.3[i,'dur.t'] <- json$resourceSets[[2]][[1]]$travelDurationTraffic
      result.time.18.3[i,'dist.r'] <- json.r$resourceSets[[2]][[1]]$travelDistance
      result.time.18.3[i,'dur.r'] <- json.r$resourceSets[[2]][[1]]$travelDuration
      result.time.18.3[i,'dur.t.r'] <- json.r$resourceSets[[2]][[1]]$travelDurationTraffic
    }
    
  } else {
    if(is.null(json) == F) {
      result.time.18.3[i,'dist'] <- json$resourceSets[[2]][[1]]$travelDistance
      result.time.18.3[i,'dur'] <- json$resourceSets[[2]][[1]]$travelDuration
      result.time.18.3[i,'dur.t'] <- json$resourceSets[[2]][[1]]$travelDurationTraffic
    }
  }
  setTxtProgressBar(pb.3, i)
}

result.time.18 <- rbind(result.time.18.1, result.time.18.2, result.time.18.3)
print("9/9 finished")
toc()


# save.image(file = '0408_APIresult.RData')  ## stored in  Dropbox (GaTech)/CSPAV_complete_street/Uijeong_temp/200405_EBC 
# load('0408_APIresult.RData')

# calculate speed
result.time.8$speed.t   <- 3600 * 0.621371 * result.time.8$dist   / result.time.8$dur.t
result.time.8$speed.t.r <- 3600 * 0.621371 * result.time.8$dist.r / result.time.8$dur.t.r

result.time.13$speed.t   <- 3600 * 0.621371 * result.time.13$dist   / result.time.13$dur.t
result.time.13$speed.t.r <- 3600 * 0.621371 * result.time.13$dist.r / result.time.13$dur.t.r

result.time.18$speed.t   <- 3600 * 0.621371 * result.time.18$dist   / result.time.18$dur.t
result.time.18$speed.t.r <- 3600 * 0.621371 * result.time.18$dist.r / result.time.18$dur.t.r

# calculate direction average (do this for results for each time)
result.time.8$dist.r[is.na(result.time.8$dist.r)]   <- result.time.8$dist[is.na(result.time.8$dist.r)]
result.time.8$dur.r[is.na(result.time.8$dur.r)]     <- result.time.8$dur[is.na(result.time.8$dur.r)]
result.time.8$dur.t.r[is.na(result.time.8$dur.t.r)] <- result.time.8$dur.t[is.na(result.time.8$dur.t.r)]
result.time.8$speed.t.r[is.na(result.time.8$speed.t.r)] <- result.time.8$speed.t[is.na(result.time.8$speed.t.r)]
result.time.8$dist.avg  <- (result.time.8$dist  + result.time.8$dist.r) /2
result.time.8$dur.avg   <- (result.time.8$dur   + result.time.8$dur.r)  /2
result.time.8$dur.t.avg <- (result.time.8$dur.t + result.time.8$dur.t.r)/2
result.time.8$speed.t.avg <- (result.time.8$speed.t + result.time.8$speed.t.r)/2

result.time.13$dist.r[is.na(result.time.13$dist.r)]   <- result.time.13$dist[is.na(result.time.13$dist.r)]
result.time.13$dur.r[is.na(result.time.13$dur.r)]     <- result.time.13$dur[is.na(result.time.13$dur.r)]
result.time.13$dur.t.r[is.na(result.time.13$dur.t.r)] <- result.time.13$dur.t[is.na(result.time.13$dur.t.r)]
result.time.13$speed.t.r[is.na(result.time.13$speed.t.r)] <- result.time.13$speed.t[is.na(result.time.13$speed.t.r)]
result.time.13$dist.avg  <- (result.time.13$dist  + result.time.13$dist.r) /2
result.time.13$dur.avg   <- (result.time.13$dur   + result.time.13$dur.r)  /2
result.time.13$dur.t.avg <- (result.time.13$dur.t + result.time.13$dur.t.r)/2
result.time.13$speed.t.avg <- (result.time.13$speed.t + result.time.13$speed.t.r)/2

result.time.18$dist.r[is.na(result.time.18$dist.r)]   <- result.time.18$dist[is.na(result.time.18$dist.r)]
result.time.18$dur.r[is.na(result.time.18$dur.r)]     <- result.time.18$dur[is.na(result.time.18$dur.r)]
result.time.18$dur.t.r[is.na(result.time.18$dur.t.r)] <- result.time.18$dur.t[is.na(result.time.18$dur.t.r)]
result.time.18$speed.t.r[is.na(result.time.18$speed.t.r)] <- result.time.18$speed.t[is.na(result.time.18$speed.t.r)]
result.time.18$dist.avg  <- (result.time.18$dist  + result.time.18$dist.r) /2
result.time.18$dur.avg   <- (result.time.18$dur   + result.time.18$dur.r)  /2
result.time.18$dur.t.avg <- (result.time.18$dur.t + result.time.18$dur.t.r)/2
result.time.18$speed.t.avg <- (result.time.18$speed.t + result.time.18$speed.t.r)/2


# calculate time average
result <- data.frame(id = 1:nrow(edges))
result$dist  <- (result.time.8$dist.avg  + result.time.13$dist.avg  + result.time.18$dist.avg) /3
result$dur   <- (result.time.8$dur.avg   + result.time.13$dur.avg   + result.time.18$dur.avg)  /3
result$dur.t <- (result.time.8$dur.t.avg + result.time.13$dur.t.avg + result.time.18$dur.t.avg)/3
result$speed.t <- (result.time.8$speed.t.avg + result.time.13$speed.t.avg + result.time.18$speed.t.avg)/3


# join the result to the drive.network
data$id <- 1:nrow(data)
data <- left_join(data, result, by = 'id') %>% select(-id)

# export the FreeDuration for calculating EBC
dur.f <- data %>% st_set_geometry(NULL) %>% select(from, to, dur)
dur.f$dur <- round(dur.f$dur, digits = 1)

write.table(dur.f, file = 'dur_f.txt', sep = ',', row.names = F, col.names = F)
rm(dur.f)

# # join EBC
# read.table(file = 'EBC.csv')


# add lane information
data$lanes[(str_detect(data$highway, 'motorway')|str_detect(data$highway, 'trunk')) *
             is.na(data$lanes) == 1] <- 2

data$lanes[(str_detect(data$highway, 'residential')|str_detect(data$highway, 'tertiary')|
              str_detect(data$highway, 'secondary')|str_detect(data$highway, 'primary')|
              str_detect(data$highway, 'living_street')|str_detect(data$highway, 'road')) * 
             (data$oneway == 'False') *
             is.na(data$lanes) == 1] <- 2

data$lanes[(str_detect(data$highway, 'residential')|str_detect(data$highway, 'tertiary')|
              str_detect(data$highway, 'secondary')|str_detect(data$highway, 'primary')|
              str_detect(data$highway, 'living_street')|str_detect(data$highway, 'road')) *
             (data$oneway == 'True') *
             is.na(data$lanes) == 1] <- 1

data$lanes[(str_detect(data$highway, 'unclassified')|str_detect(data$highway, 'service')|
             str_detect(data$highway, 'track')|str_detect(data$highway, 'path')) *
             is.na(data$lanes) == 1] <- 1


lane.temp <- data$lanes %>% str_extract_all('\\d', simplify = T) %>% as.data.frame()

for (i in 1:ncol(lane.temp)) {
  lane.temp[,i] <- as.numeric(as.character(lane.temp[,i]))
}

lane.temp[is.na(lane.temp)] <- 0

data$lanes.new <- apply(lane.temp,1,function(x) max(x))

rm(i, lane.temp)

# save.image(file = '0408_APIresult_lane.RData') 
# load('0408_APIresult_lane.RData')

rm(coord.ends, edges, result, result.time.13, result.time.18, result.time.8)
drive <- data
rm(data)
save.image(file = 'drive.RData')


#### correct 'number of lanes' (GDOT road inventory + OpenStreetMap)
# load(file = 'drive.RData')

drive <- drive %>% 
  st_transform(32616) %>% 
  mutate(id = 1:nrow(.))

drive.point <- st_line_sample(drive, sample = c(0.33, 0.66)) %>% 
  st_cast('POINT') %>% 
  st_sf() %>% 
  mutate(temp.id = 1:nrow(.), id = rep(drive$id, each = 2))

road.gdot <- st_read('data/road_inventory_2018/road_inventory_prj_5counties.shp') %>% 
  filter(Functional != 1 & Functional != 2) %>% 
  select(Functional, Lanes_Incr, Lanes_Decr)

# buffer from GDOT road inventory shape: 10 meters
road.gdot.buffer <- st_buffer(road.gdot, dist = 10)

road.edge.join <- st_join(drive.point, road.gdot.buffer, join = st_intersects) %>%
  st_set_geometry(NULL) 

road.edge.join.2 <- road.edge.join %>% 
  group_by(id) %>% 
  summarise(na.count = sum(is.na(Functional))) %>% 
  filter(na.count == 0) %>% 
  left_join(road.edge.join, by = 'id') %>% 
  .[!duplicated(.$id), ] %>% 
  mutate(lanes.gdot = Lanes_Incr + Lanes_Decr)

drive.temp <- drive %>% 
  left_join(road.edge.join.2 %>% select(lanes.gdot, id), by ='id') %>% 
  select(-id)

drive.temp$lanes.gdot <- as.numeric(drive.temp$lanes.gdot)

drive.temp <- drive.temp %>% 
  mutate(lanes.new.2 = case_when(
    is.na(lanes.gdot) ~ lanes.new,
    lanes.gdot == 0 ~ lanes.new,
    TRUE ~ lanes.gdot
  ))

# drive.temp$lanes.new.2 -> drive$lanes.new

drive$lanes.new <- drive.temp$lanes.new.2
drive <- drive %>% select(-id)
rm(drive.temp, drive.point, road.edge.join, road.edge.join.2, road.gdot, road.gdot.buffer)

save.image(file = 'drive.RData')

# sum(drive.temp$lanes.new.2 == drive.temp$lanes.new)/nrow(drive.temp)
# 
# summary(drive.temp$lanes.new[drive.temp$lanes.new.2 != drive.temp$lanes.new])
# summary(drive.temp$lanes.new.2[drive.temp$lanes.new.2 != drive.temp$lanes.new])
# 
# hist(drive.temp$lanes.new[drive.temp$lanes.new.2 != drive.temp$lanes.new])
# hist(drive.temp$lanes.new.2[drive.temp$lanes.new.2 != drive.temp$lanes.new])
# 
# hist(drive.temp$lanes.new.2[drive.temp$lanes.new.2 != drive.temp$lanes.new & drive.temp$lanes.new == 1])
# hist(drive.temp$lanes.new.2[drive.temp$lanes.new.2 != drive.temp$lanes.new & drive.temp$lanes.new == 2])
# 
# sum(drive.temp$lanes.gdot == 0, na.rm = T)/nrow(drive.temp)
# sum(is.na(drive.temp$lanes.gdot))/nrow(drive.temp)






