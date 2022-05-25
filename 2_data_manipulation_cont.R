## Packages and directories -----------------------------------------------------------------------------------
# if(!require("devtools")) install.packages("devtools")
# devtools::install_github("jamgreen/lehdr")

if(!require("sf")) install.packages("sf")
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("tmap")) install.packages("tmap")
if(!require("lwgeom")) install.packages("lwgeom")
if(!require("httr")) install.packages("httr")
if(!require("jsonlite")) install.packages("jsonlite")
if(!require("dplyr")) install.packages("dplyr")
if(!require("data.table")) install.packages("data.table")
if(!require("tictoc")) install.packages("tictoc")
if(!require("nngeo")) install.packages("nngeo")
if(!require("spdep")) install.packages("spdep")
if(!require("magrittr")) install.packages("magrittr")
if(!require("tidycensus")) install.packages("tidycensus")
if(!require("readxl")) install.packages("readxl")
if(!require("tigris")) install.packages("tigris")
if(!require("rjson")) install.packages("rjson")

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
library(tidycensus)
library(lehdr)
library(readxl)
library(tigris)
library(rjson)

setwd("#####") 

## Load drive data and cacluate the level of traffic stress ----------------------------------------------------------------------------

## 1. Load drive data with observed speed from Bing API
load('drive.RData') 

## 2. Edit variables
# Road type
drive <- drive %>% 
  filter(str_detect(highway, 'motor') == F) %>% 
  mutate(highway.new = case_when(
    str_detect(highway, 'trunk') ~ 'trunk',
    str_detect(highway, 'primary') ~ 'primary',
    str_detect(highway, 'secondary') ~ 'secondary',
    str_detect(highway, 'tertiary') ~ 'tertiary',
    TRUE ~ 'residential'
    )
  )

# Number of lanes
drive$lanes.new[drive$lanes.new == 0] <- 1
drive$lanes.new[drive$lanes.new >= 6] <- 6

# One-way
drive$oneway[drive$oneway == '[False, True]'] <- 'False'
drive$oneway <- as.factor(as.character(drive$oneway))

# Average speed (if 'Inf', give the average speed of roads with the same road type)
drive$speed.t.new <- drive$speed.t

drive$speed.t.new[(is.na(drive$speed.t) | drive$speed.t == 'Inf') & drive$highway.new == 'residential'] <- 
  mean(drive$speed.t[!is.na(drive$speed.t) & !is.nan(drive$speed.t) & drive$speed.t != 'Inf' & drive$highway.new == 'residential'])

drive$speed.t.new[(is.na(drive$speed.t) | drive$speed.t == 'Inf') & drive$highway.new == 'tertiary'] <- 
  mean(drive$speed.t[!is.na(drive$speed.t) & !is.nan(drive$speed.t) & drive$speed.t != 'Inf' & drive$highway.new == 'tertiary'])

drive$speed.t.new[(is.na(drive$speed.t) | drive$speed.t == 'Inf') & drive$highway.new == 'secondary'] <- 
  mean(drive$speed.t[!is.na(drive$speed.t) & !is.nan(drive$speed.t) & drive$speed.t != 'Inf' & drive$highway.new == 'secondary'])

drive$speed.t.new[(is.na(drive$speed.t) | drive$speed.t == 'Inf') & drive$highway.new == 'primary'] <- 
  mean(drive$speed.t[!is.na(drive$speed.t) & !is.nan(drive$speed.t) & drive$speed.t != 'Inf' & drive$highway.new == 'primary'])

drive$speed.t.new[(is.na(drive$speed.t) | drive$speed.t == 'Inf') & drive$highway.new == 'trunk'] <- 
  mean(drive$speed.t[!is.na(drive$speed.t) & !is.nan(drive$speed.t) & drive$speed.t != 'Inf' & drive$highway.new == 'trunk'])

drive$speed.t.new[drive$speed.t > 130] <- 130

# Create 'id' field
drive$id <- 1:nrow(drive)
save(drive, file = 'drive_edit.RData')

## 3.Put AADT data to drive 
# Load drive
drive <- drive %>% 
  st_transform(32616)

residential <- drive %>% 
  filter(highway.new == 'residential')
non.residential <- drive %>% 
  filter(highway.new != 'residential')

# Load tigris roads and 5 counties
fulton <- roads(state = 'Georgia', county = 'Fulton', class = 'sf')
dekalb <- roads(state = 'Georgia', county = 'DeKalb', class = 'sf')
clayton <- roads(state = 'Georgia', county = 'Clayton', class = 'sf')
cobb <- roads(state = 'Georgia', county = 'Cobb', class = 'sf')
gwinnett <- roads(state = 'Georgia', county = 'Gwinnett', class = 'sf')

tigris <- rbind(fulton, dekalb, clayton, cobb, gwinnett) %>% 
  st_transform(32616) %>% 
  mutate(tigris.id = 1:nrow(.))
rm(fulton, dekalb, clayton, cobb, gwinnett)

counties <- counties(state = 'Georgia', year = 2017, class = 'sf') %>%
  filter(NAME %in% c('Fulton', 'DeKalb', 'Cobb', 'Gwinnett', 'Clayton')) %>% 
  st_transform(32616)

# Load aadt 
aadt <- read.csv('./data/AADT/aadt_and_truckpct.csv') %>% 
  separate(col = 'Lat.Long', into = c('lat','lon'), sep = ', ') %>% 
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon)) %>% 
  st_as_sf(coords = c('lon', 'lat'), dim = 'XY', crs = 4326) %>% 
  st_transform(32616) %>%
  .[counties, ] %>% 
  mutate(AADT = case_when(
    !is.na(AADT_2018) ~ AADT_2018,
    !is.na(AADT_2017) ~ AADT_2017,
    !is.na(AADT_2016) ~ AADT_2016,
    !is.na(AADT_2015) ~ AADT_2015,
    !is.na(AADT_2014) ~ AADT_2014,
    !is.na(AADT_2013) ~ AADT_2013,
    !is.na(AADT_2012) ~ AADT_2012,
    !is.na(AADT_2011) ~ AADT_2011,
    !is.na(AADT_2010) ~ AADT_2010,
    !is.na(AADT_2009) ~ AADT_2009
    )
  ) %>% 
  filter(!is.na(AADT)) %>% 
  select(Station.ID, Functional.Class, AADT) %>% 
  filter(!(Functional.Class %in% c('1R : Rural Principal Arterial - Interstate', 
                                   '1U : Urban Principal Arterial - Interstate',
                                   '2U : Urban Principal Arterial - Freeways & Expressways'))) %>%
  mutate(aadt.id = 1:nrow(.)) 

rm(counties)

# Join aadt to tigris data and non.residential drive data
tigris.aadt <- st_join(tigris, st_buffer(aadt, dist = 5), join = st_intersects) %>%
  st_set_geometry(NULL) %>%
  filter(!is.na(AADT)) %>% 
  group_by(tigris.id) %>% 
  summarise(aadt = mean(AADT)) %>% 
  left_join(tigris, ., by = 'tigris.id') %>% 
  filter(!is.na(aadt))

non.residential.point.buffer <- st_line_sample(non.residential, sample = c(0.5)) %>% 
  st_cast('POINT') %>% 
  st_sf() %>%
  mutate(id = non.residential$id) %>% 
  st_buffer(5)

non.residential.aadt <- st_join(non.residential.point.buffer, tigris.aadt, join = st_intersects) %>% 
  st_set_geometry(NULL) %>%
  filter(!is.na(aadt)) %>% 
  group_by(id) %>% 
  summarise(aadt = mean(aadt)) %>% 
  left_join(non.residential, ., by = 'id')

# Fill in aadt values to the non.resdiential drive edges not joined with aadt
lane <- unique(non.residential.aadt$lanes.new) %>% sort()
road.type <- unique(non.residential.aadt$highway.new) %>% as.character()

fill.in.aadt <- function(edge, lane, road.type) {
  
  df <- edge %>% 
    filter(id == 0)
  
  for (i in lane) {
    for (j in road.type) {
      tic(paste0('For ', i, '-lane ', j, ' roads: '))
      
      df.not.joined <- edge %>% 
        filter(lanes.new == i & highway.new == j & is.na(aadt))
      df.joined <- edge %>% 
        filter(lanes.new == i & highway.new == j & !is.na(aadt))
      df.not.joined$aadt <- st_nearest_feature(df.not.joined, df.joined) %>% 
        df.joined[., ] %>%
        .$aadt 
      df <- rbind(df, df.joined, df.not.joined)
      
      toc()
    }
  }
  return(df %>% arrange(id))
}

non.residential.aadt.complete <- fill.in.aadt(edge = non.residential.aadt, lane = lane, road.type = road.type)

# Assign median residential AADT value to residential roads 
freeway <- st_read('./data/OSM/data/drive_network_0405/edges/edges.shp') %>% 
  .[grepl('motor', .$highway), ] %>% 
  st_transform(32616)

aadt.residential <- st_join(aadt %>% st_buffer(50), non.residential, join = st_intersects) %>%
  filter(is.na(id)) %>% 
  group_by(aadt.id) %>% 
  summarise(AADT = first(AADT)) %>% 
  st_join(freeway, join = st_intersects) %>% 
  filter(is.na(osmid)) %>% 
  group_by(aadt.id) %>% 
  summarise(AADT = first(AADT)) %>% 
  .$AADT %>% 
  median()

residential <- residential %>% 
  mutate(aadt = aadt.residential)
  
# Row-bind non.residential and residential 
drive.aadt <- rbind(non.residential.aadt.complete, residential) %>% 
  arrange(id)

# Save drive filled with aadt
save(drive.aadt, file = 'drive_aadt.RData')
rm(list = setdiff(ls(), 'drive.aadt'))

## 4. Calculate level of traffic stress (LTS) using oneway, aadt, lanes.new, and speed.t.new
# Load drive and calculate effective adt (adt.e) 
drive <- drive.aadt
rm(drive.aadt)
drive$adt.e <- drive$aadt
drive$adt.e[drive$oneway == 'True'] <- drive$adt.e[drive$oneway == 'True'] * 1.5

# Calculate lts
drive <- drive %>% 
  mutate(lts = case_when(
    highway.new == 'residential' & speed.t.new <= 25 ~ 1.10,
    highway.new == 'residential' & speed.t.new > 25 ~ 1.15,
    lanes.new <= 3 & speed.t.new <= 25 ~ 1.20,
    lanes.new <= 3 & speed.t.new <= 35 ~ 1.40,
    lanes.new <= 3 & speed.t.new > 35 ~ 2.00,
    lanes.new <= 5 & speed.t.new <= 25 ~ 1.35,
    lanes.new <= 5 & speed.t.new <= 35 ~ 1.70,
    lanes.new <= 5 & speed.t.new > 35 ~ 2.20,
    lanes.new > 5 & speed.t.new <= 25 ~ 1.67,
    lanes.new > 5 & speed.t.new <= 35 ~ 1.80,
    lanes.new > 5 & speed.t.new > 35 ~ 2.40
    )
  )

# Save the drive data having lts
save(drive, file = 'drive_aadt_lts.RData')
#load('drive_aadt_lts.RData')

## Load and tidy all the network data (drive, bike, arc.bike) --------------------------------------------------------------------------

drive <- drive %>% 
  select(highway = highway.new, 
         lanes = lanes.new, 
         speed = speed.t.new,
         lts, aadt, oneway) %>% 
  mutate(code.drive = 1,
         drive.ID = 1:nrow(.))

bike <- st_read('./data/OSM/data/bike_network_0405/edges/edges.shp') %>% 
  st_transform(32616) %>% 
  select() %>%
  filter(!(duplicated(geometry))) %>% 
  mutate(bike.ID = 1:nrow(.))

arc.bike <- st_read('./data/Shape/Regional_Bikeway_Inventory_2019_NoGolf_join.shp') %>% 
  st_transform(32616) %>% 
  st_cast('MULTILINESTRING') %>% 
  st_cast('LINESTRING') %>% 
  .[st_read('./data/Shape/5counties_area_dissolve.shp'), ] %>% 
  select(bikeway.name = Name, bike.type = category, bike.type.uj = type) %>% 
  filter(bike.type.uj != 'sharrow') %>% 
  mutate(arc.bike.ID = 1:nrow(.), code.bike = 1) 

save.image('drive_bike_arcbike.RData')
#load('drive_bike_arcbike.RData')

## Join the network datas ------------------------------------------------------------------------------------
## 1. Join drive to bike and give bike code.drive column
tic('1. Join drive to bike and make code.drive column')

# Extract one point from each bike edge
bike.point <- st_line_sample(bike, sample = c(0.5)) %>% 
  st_cast('POINT') %>% 
  st_sf() %>%
  mutate(bike.ID = bike$bike.ID)

# Join drive to bike
bike.point.buffer <- st_buffer(bike.point, 0.001)
bike.point.buffer.join <- st_join(bike.point.buffer, drive, join = st_intersects) %>%
  st_set_geometry(NULL) %>% 
  mutate(code.drive = ifelse(is.na(.$drive.ID), 0, 1)) %>% 
  .[!duplicated(.$bike.ID), ]

bike <- bike %>% 
  left_join(bike.point.buffer.join, by = 'bike.ID')

rm(bike.point, bike.point.buffer, bike.point.buffer.join)

toc()

## 2. Join arc.bike to bike and make code.bike column
tic('2. Join arc.bike to bike and make code.bike column')

# Extract two points from each bike edge
bike.points <- st_line_sample(bike, sample = c(0.33, 0.66)) %>% 
  st_cast('POINT') %>% 
  st_sf() %>% 
  mutate(temp.ID = 1:nrow(.), bike.ID = rep(bike$bike.ID, each = 2))

# Join arc.bike to bike.points and give 'code.bike = 1' to corresponding bike edges (20m spatial join)
arc.bike.buffer <- st_buffer(arc.bike, dist = 20)

bike.arc.bike.join <- st_join(bike.points, arc.bike.buffer, join = st_intersects) %>%
  st_set_geometry(NULL) 

bikable.bike <- bike.arc.bike.join %>% 
  group_by(bike.ID) %>% 
  summarise(na.count = sum(is.na(bikeway.name))) %>% 
  filter(na.count == 0) %>% 
  left_join(bike.arc.bike.join, by = 'bike.ID') %>% 
  .[!duplicated(.$bike.ID), ] %>% 
  select(bike.ID, bikeway.name, bike.type, bike.type.uj, arc.bike.ID, code.bike)

bike <- bike %>% 
  left_join(bikable.bike, by ='bike.ID') %>% 
  mutate(code.bike = ifelse(is.na(code.bike), 0 ,1))

rm(bike.points, bike.arc.bike.join, arc.bike.buffer, bikable.bike)

toc()

## 3. Leave only bike edges that either code.bike == 1  or code.drive == 1
tic('3. Leave only bike edges that either code.bike == 1  or code.drive == 1')

bike <- bike %>% 
  filter(code.drive == 1 | code.bike == 1)

toc()

## 4. Get nodes from edges
tic('4. Get nodes from edges')

# Generate nodes from the merged road network data
edges <- bike %>% 
  mutate(edge_ID = 1:nrow(.), length = unclass(st_length(.)))

get_nodes_from_edges <- function(input){
  
  # Inserting unique ID to edges
  message("Inserting unique ID to edges")
  input$edge_ID = seq(1, nrow(input))
  
  # Creating two nodes per edge
  message("Creating two nodes per edge")
  y = data.frame()
  for (i in input$edge_ID){
    t = sf::st_coordinates(input[input$edge_ID == as.numeric(i), ])
    t2 = t[c(1, nrow(t)), c("X", "Y")]
    y = rbind(y, t2)
    rm(t)
    rm(t2)}
  
  # Deleting duplicated nodes
  message("Deleting duplicated nodes")
  y = dplyr::distinct(y,X,Y)
  z = sf::st_as_sf(y, coords = c('X', 'Y'), dim = "XY", crs = sf::st_crs(input))
  z$node_ID = seq(1, nrow(z))
  
  return(z)
}

nodes <- get_nodes_from_edges(edges)

toc()

save(edges, nodes, file = 'nodes_and_edges.RData')
rm(list = setdiff(ls(), c('edges', 'nodes')))
# load('nodes_and_edges.RData')

## Data merge (edges)--------------------------------------------------------------

## 1. Merge GTFS data with edges
# Load GTFS trips data for each agency and route 
asc.trips <- read.delim('./data/GTFS/trips_ASC.txt', sep =',') %>% 
  mutate(shape_id = as.character(shape_id)) %>% 
  group_by(shape_id, service_id) %>% 
  summarise(trip.count = n()) %>% 
  filter(service_id == 'e3037c28-7b59-4724-9aba-3a760b65a4a3') # Select only typical weekday-trips

cct.trips <- read.delim('./data/GTFS/trips_CCT.txt', sep =',') %>% 
  mutate(shape_id = as.character(shape_id)) %>% 
  group_by(shape_id, service_id) %>% 
  summarise(trip.count = n()) %>% 
  filter(service_id == 116) # Select only typical weekday-trips

gct.trips <- read.delim('./data/GTFS/trips_GCT.txt', sep =',') %>% 
  mutate(shape_id = as.character(shape_id)) %>% 
  group_by(shape_id, service_id) %>% 
  summarise(trip.count = n()) %>% 
  filter(service_id == 'c_15299_b_16105_d_31') # Select only typical weekday-trips

grta.trips <- read.delim('./data/GTFS/trips_GRTA.txt', sep =',') %>% 
  mutate(shape_id = as.character(shape_id)) %>% 
  group_by(shape_id, service_id) %>% 
  summarise(trip.count = n()) %>% 
  filter(service_id == 'A') # Select only typical weekday-trips

marta.trips <- read.delim('./data/GTFS/trips_MARTA.txt', sep =',') %>%
  mutate(shape_id = as.character(shape_id)) %>% 
  group_by(shape_id, service_id) %>% 
  summarise(trip.count = n()) %>% 
  filter(service_id == 5) # Select only typical weekday-trips

# Load GTFS routes shapefile data created by ArcGIS 'Display GTFS Route Shapes tool' and merge with trips data
asc <- st_read('./data/GTFS/routes_ASC.shp') %>% 
  mutate(shape_id = as.character(shape_id)) %>% 
  left_join(asc.trips, by = 'shape_id') %>% 
  select(agency_id, route_id, shape_id, trip.count)

cct <- st_read('./data/GTFS/routes_CCT.shp') %>%
  mutate(shape_id = as.character(shape_id)) %>% 
  left_join(cct.trips, by = 'shape_id') %>% 
  select(agency_id, route_id, shape_id, trip.count) %>% 
  mutate(agency_id = 'CCT')

gct <- st_read('./data/GTFS/routes_GCT.shp') %>% 
  mutate(shape_id = as.character(shape_id)) %>% 
  left_join(gct.trips, by = 'shape_id') %>% 
  select(agency_id, route_id, shape_id, trip.count) %>% 
  mutate(agency_id = 'GCT')

grta <- st_read('./data/GTFS/routes_GRTA.shp') %>% 
  mutate(shape_id = as.character(shape_id)) %>% 
  left_join(grta.trips, by = 'shape_id') %>% 
  select(agency_id, route_id, shape_id, trip.count) %>% 
  mutate(agency_id = 'GRTA')

marta.bus <- st_read('./data/GTFS/routes_MARTA.shp') %>% 
  mutate(shape_id = as.character(shape_id)) %>% 
  left_join(marta.trips, by = 'shape_id') %>%
  filter(rt_typ_txt == 'Bus') %>% 
  select(agency_id, route_id, shape_id, trip.count) %>% 
  mutate(agency_id = 'MARTA')

# Merge all the bus routes data with trips
bus <- rbind(asc, cct, gct, grta, marta.bus) %>% 
  mutate(trip.count = ifelse(is.na(trip.count), 0, trip.count)) %>% 
  st_transform(32616)

rm(asc.trips, cct.trips, gct.trips, grta.trips, marta.trips, asc, cct, gct, grta, marta.bus)

# Join bus frequency data to edges
edges.point.buffer <- st_line_sample(edges, sample = c(0.5)) %>% 
  st_cast('POINT') %>% 
  st_sf() %>%
  mutate(edge_ID = edges$edge_ID) %>% 
  st_buffer(11)

edges <- st_join(edges.point.buffer, bus, join = st_intersects) %>%
  st_set_geometry(NULL) %>% 
  group_by(edge_ID) %>% 
  summarise(bus.freq = sum(trip.count)) %>% 
  left_join(edges, ., by = 'edge_ID')

rm(edges.point.buffer)

#### Join subway proximity data to edges
subway <- read_excel('./data/GTFS/MARTA_Subway_Stations.xlsx') %>%
  as.data.frame() %>%
  st_as_sf(coords = c('Lon', 'Lat'), dim = "XY", crs = 4326) %>%
  st_transform(32616)

st_write(subway, './data/GTFS/stations.shp')

#### ODMTS version
# odmts.hub.json <- fromJSON(file = './data/GTFS/cn_data.json')
# 
# odmts.hub.lonlat <- data.frame(Lon = numeric(0), Lat = numeric(0))
# 
# for (i in odmts.hub.json$hubs) {
#   lonlat.temp <- data.frame(Lon = i$lon, Lat = i$lat)
#   odmts.hub.lonlat <- add_row(odmts.hub.lonlat, lonlat.temp)
# }
# 
# odmts.hub <- data.frame(St_Name = names(odmts.hub.json$hubs)) %>% 
#   cbind(odmts.hub.lonlat) %>% 
#   st_as_sf(coords = c('Lon', 'Lat'), dim = "XY", crs = 4326) %>% 
#   st_transform(32616)
# 
# # 'ODMTS hubs' replace 'subway stations' since ODMTS hubs covers all the stations
# subway <- odmts.hub
# 
# rm(i, lonlat.temp, odmts.hub.json, odmts.hub.lonlat, odmts.hub)



subway.dist <- st_distance(edges, subway) %>% 
  apply(1, FUN = min)
  
edges <- edges %>% 
  cbind(sub.prox = 805 - subway.dist) %>% 
  mutate(sub.prox = case_when(
    sub.prox <= 0 ~ 0,
    TRUE ~ sub.prox
    )
  )

## 2. Merge population and employment density with edges
# Load LODES data (for employment density)
lodes <- grab_lodes("ga", 2017, lodes_type = "wac", 
                    job_type = "JT00",
                    segment = "S000",
                    agg_geo = "bg", 
                    download_dir = getwd()
                    ) %>%
  select(GEOID = w_bg, emp = C000) 

# Load tidycensus data (for population density)
census_api_key("####") # var.list <- load_variables(2017, "acs5")
census <- get_acs(geography = "block group",
                  state = "GA",
                  county = c("Fulton", "DeKalb", "Clayton", "Cobb", "Gwinnett"),
                  variables = c(pop = "B01003_001"),
                  year = 2018,
                  survey = "acs5",
                  geometry = TRUE,
                  output = "wide")

# Join the LODES data and the tidycensus data, which produces pop.emp
pop.emp <- left_join(census, lodes, by = "GEOID") %>% 
  st_transform(crs = 32616) %>%
  mutate(area = unclass(st_area(.))/1000000) %>% # area in km^2
  mutate(popden = popE / area, empden = emp / area)
  
pop.emp[is.na(pop.emp)] <- 0


######## attach pop + emp + poi at block group level (to use in identifying additional high demand spots)
poi_point <- st_read('./data/Shape/poi_point_clip_join_select.shp') %>% 
  st_transform(crs = 32616) %>% 
  filter(use == 1) 
poi_polygon <- st_read('./data/Shape/poi_polygon_clip_join_select.shp') %>% 
  st_transform(crs = 32616) %>% 
  filter(use == 1) 

pop.emp.poi.point <- pop.emp %>% 
  st_join(poi_point, join = st_intersects) %>% 
  st_set_geometry(NULL) %>% 
  group_by(GEOID) %>% 
  summarise(poi.point.cnt = sum(!is.na(fclass)))

pop.emp.poi.polygon <- pop.emp %>% 
  st_join(poi_polygon, join = st_intersects) %>% 
  st_set_geometry(NULL) %>% 
  group_by(GEOID) %>% 
  summarise(poi.polygon.cnt = sum(!is.na(fclass)))

pop.emp.poi <- pop.emp %>% 
  left_join(pop.emp.poi.point, by = 'GEOID') %>% 
  left_join(pop.emp.poi.polygon, by = 'GEOID') %>% 
  mutate(poi.cnt = poi.point.cnt + poi.polygon.cnt) %>% 
  select(c(-poi.point.cnt, -poi.polygon.cnt)) %>% 
  mutate(poiden = poi.cnt/(area/2.59))

rm(poi_point, poi_polygon, pop.emp.poi.point, pop.emp.poi.polygon)
save(pop.emp.poi, file = 'pop_emp_poi_block_group.RData')



# Join pop.emp data to edges
edges <- edges %>% 
  st_join(pop.emp, join = st_intersects) %>%
  st_set_geometry(NULL) %>% 
  group_by(edge_ID) %>% 
  summarise(popden = mean(popden),
            empden = mean(empden)) %>% 
  left_join(edges, ., by = 'edge_ID')

save.image(file = 'nodes_and_edges_2.RData')
# load('nodes_and_edges_2.RData')

# Load tidycensus data (for no car ratio -> income/race/ethnic)
# no.car <- get_acs(geography = "tract",
#                   state = "GA",
#                   county = c("Fulton", "DeKalb", "Clayton", "Cobb", "Gwinnett"),
#                   variables = c(vehicle.total = "B08201_001", no.vehicle = "B08201_002"),
#                   year = 2018,
#                   survey = "acs5",
#                   geometry = TRUE,
#                   output = "wide") %>% 
#   mutate(no.car = no.vehicleE/vehicle.totalE) %>% 
#   st_transform(crs = 32616)
# 
# no.car <- no.car %>% 
#   mutate(no.car = case_when(
#     is.na(no.car) ~ mean(no.car, na.rm = T), 
#     TRUE ~ no.car))
# 
# # Join no.car data to edges
# edges <- edges %>% 
#   st_join(no.car, join = st_intersects) %>%
#   st_set_geometry(NULL) %>% 
#   group_by(edge_ID) %>% 
#   summarise(no.car = mean(no.car)) %>% 
#   left_join(edges, ., by = 'edge_ID')

census_api_key("#####") # var.list <- load_variables(2017, "acs5")
poverty <- get_acs(geography = "block group",
                   state = "GA",
                   county = c("Fulton", "DeKalb", "Clayton", "Cobb", "Gwinnett"),
                   variables = c(poverty.total = "C17002_001", poverty.under05 = "C17002_002", poverty.under1 = "C17002_003"),
                   year = 2018,
                   survey = "acs5",
                   geometry = TRUE,
                   output = "wide") %>% 
  mutate(poverty = (poverty.under05E + poverty.under1E)/poverty.totalE) %>% 
  st_transform(crs = 32616) %>% 
  mutate(poverty = case_when(
    is.na(poverty) ~ mean(poverty, na.rm = T),
    TRUE ~ poverty))

race <- get_acs(geography = "block group",
                state = "GA",
                county = c("Fulton", "DeKalb", "Clayton", "Cobb", "Gwinnett"),
                variables = c(race.total = 'B02001_001', race.white = 'B02001_002', race.asian = 'B02001_005'),
                year = 2018,
                survey = "acs5",
                geometry = TRUE,
                output = "wide") %>% 
  mutate(race = (race.totalE - race.whiteE - race.asianE)/race.totalE) %>% 
  st_transform(crs = 32616) %>% 
  mutate(race = case_when(
    is.na(race) ~ mean(race, na.rm = T),
    TRUE ~ race))

ethnic <- get_acs(geography = "block group",
                  state = "GA",
                  county = c("Fulton", "DeKalb", "Clayton", "Cobb", "Gwinnett"),
                  variables = c(ethnic.total = 'B03002_001', ethnic.hisp = 'B03002_012'),
                  year = 2018,
                  survey = "acs5",
                  geometry = TRUE,
                  output = "wide") %>% 
  mutate(ethnic = ethnic.hispE/ethnic.totalE) %>% 
  st_transform(crs = 32616) %>% 
  mutate(ethnic = case_when(
    is.na(ethnic) ~ mean(ethnic, na.rm = T),
    TRUE ~ ethnic))

minority <- poverty %>% 
  left_join(race %>% st_set_geometry(NULL) %>% select(GEOID, race), by = 'GEOID') %>% 
  left_join(ethnic %>% st_set_geometry(NULL) %>% select(GEOID, ethnic), by = 'GEOID')

save(minority, file = 'minority_block_group.RData')

# Join no.car data to edges
edges <- edges %>%
  st_join(minority, join = st_intersects) %>%
  st_set_geometry(NULL) %>%
  group_by(edge_ID) %>%
  summarise(poverty = mean(poverty),
            race = mean(race),
            ethnic = mean(ethnic)) %>%
  left_join(edges, ., by = 'edge_ID')

# Select columns
edges <- edges %>%
  select(highway, lanes, oneway, lts, bike.type, bike.type.uj,
         Code_Drive = code.drive, Code_Bike = code.bike, 
         bus.freq, sub.prox, popden, empden, poverty, race, ethnic, length, aadt, edge_ID)

rm(list = setdiff(ls(),c('edges', 'nodes', 'subway')))


## 3. Merge POI data with edges
# Data name change
EdgeData <- edges

# Merge POI density data with edge data
poi_point <- st_read('./data/Shape/poi_point_clip_join_select.shp') %>% 
  st_transform(crs = 32616) %>% 
  filter(use == 1) 
poi_polygon <- st_read('./data/Shape/poi_polygon_clip_join_select.shp') %>% 
  st_transform(crs = 32616) %>% 
  filter(use == 1) 

EdgeData_buffer <- st_buffer(EdgeData, 60.96) %>% # 200ft = 60.96m 
  select(edge_ID)

edge_poi_point <- st_join(EdgeData_buffer, poi_point, join = st_intersects) %>%
  st_set_geometry(NULL) %>% 
  group_by(edge_ID) %>% 
  summarise(poi.point.cnt = sum(!is.na(fclass)))

edge_poi_polygon <- st_join(EdgeData_buffer, poi_polygon, join = st_intersects) %>%
  st_set_geometry(NULL) %>% 
  group_by(edge_ID) %>% 
  summarise(poi.polygon.cnt = sum(!is.na(fclass)))

EdgeData <- EdgeData %>% 
  left_join(edge_poi_point, by = 'edge_ID') %>%
  left_join(edge_poi_polygon, by = 'edge_ID') %>% 
  mutate(poi.cnt = poi.point.cnt + poi.polygon.cnt) %>% 
  select(c(-poi.point.cnt, -poi.polygon.cnt))

## 4. Deal with NAs in EdgeData
EdgeData <- EdgeData  %>% 
  mutate(lts = ifelse(is.na(lts), 1, lts),
         bus.freq = ifelse(is.na(bus.freq), 0, bus.freq),
         sub.prox = ifelse(is.na(sub.prox), 0, sub.prox),
         popden = ifelse(is.na(popden), 0, popden),
         empden = ifelse(is.na(empden), 0, empden),
         aadt = ifelse(is.na(aadt), 0 , aadt),
         poverty = ifelse(is.na(poverty), 0 , poverty),
         race = ifelse(is.na(race), 0 , race),
         ethnic = ifelse(is.na(ethnic), 0 , ethnic)
         )


## Data merge (nodes)--------------------------------------------------------------

## 1. Calculate node elevations using google elevation API 
baseURL <- "https://maps.googleapis.com/maps/api/elevation/json?locations="
key <- "#####"

NodeData <- nodes %>% 
  st_transform(crs = 4326)

locations_per_batch <- 400
n_rqst <- ceiling(nrow(NodeData)/locations_per_batch)
request_batch <- NULL
elevation_list <- NULL
id_list <- NULL

pb <- txtProgressBar(min = 0, max = n_rqst, style = 3)
for (i in 1:n_rqst) {
  for (j in (((locations_per_batch*(i-1))+1):(locations_per_batch*i))) {
    
    # creating api request list
    request_batch <- paste(request_batch, as.numeric(str_split(NodeData$geometry[j][[1]], ","))[2], 
                           ",", as.numeric(str_split(NodeData$geometry[j][[1]], ","))[1], "|", sep = "")
    
    # deleting the last "|" in the request list
    if (j == locations_per_batch*i) {
      request_batch <- str_sub(request_batch, 1, -2)
    }
    
    # break when it reaches the total number of nodes
    if (j == nrow(NodeData)) {
      request_batch <- str_sub(request_batch, 1, -2)
      break
    }
  }
  # requesting API
  url <- paste0(baseURL, request_batch, "&key=", key)
  temp_list <- NULL
  while (is.null(temp_list)){
    temp_list <- jsonlite::fromJSON(content(GET(url), as = "text"), simplifyDataFrame = T)$results$elevation
  }
  temp_id <- as.numeric((locations_per_batch*(i-1))+1):as.numeric(locations_per_batch*i)
  if (is.null(temp_list) == T) { temp_id = NULL }
  elevation_list <- c(elevation_list, temp_list)
  id_list <- c(id_list, temp_id)
  
  request_batch <- NULL
  
  # break when it reaches the total number of nodes
  if (j == nrow(NodeData)) {
    break
  }
  setTxtProgressBar(pb, i)
}

## 2. Merge lon, lat, elevation data with NodeData 
NodeData <- NodeData %>% 
  st_transform(crs = 32616) %>% 
  mutate(x = st_coordinates(.)[, "X"], y = st_coordinates(.)[, "Y"], elevation = elevation_list)

save(EdgeData, NodeData, subway, file = 'EdgeData_NodeData.RData')
#load('EdgeData_NodeData.RData')
rm(list = setdiff(ls(), c('EdgeData', 'NodeData', 'subway')))


## Calculate grade for EdgeData + Create dataset (A-star input) + Let edges closest to subway stations be bikable -------
## 1. Create dataset
CreateDataset <- function(EdgeData, NodeData) {
  
  # Spatial join => 2 nodes per edge
  EdgeData.Buffer <- EdgeData %>% 
    st_buffer(dist = 0.001) %>% 
    select(edge_ID)
  
  Edge.Node <- st_join(EdgeData.Buffer, NodeData, join = st_intersects) %>% 
    st_set_geometry(NULL) %>% 
    left_join(EdgeData, ., by = 'edge_ID')
  
  nodes.per.edge <- Edge.Node %>%
    st_set_geometry(NULL) %>% 
    group_by(edge_ID) %>%
    summarise(node_num = sum(!is.na(node_ID)))
  
  Edge.Node <- Edge.Node %>% 
    left_join(nodes.per.edge, by = "edge_ID")
  
  # Create dataset using EdgeData and NodeData
  dataset <- Edge.Node %>% 
    filter(node_num == 2) %>% # delete if the number of nodes attached is not 2
    rename(from_ID = node_ID, from_x = x, from_y = y, from_elev = elevation)
  
  dataset_odd <- dataset[!duplicated(dataset$edge_ID),]
  dataset_even <- dataset[duplicated(dataset$edge_ID),] %>% 
    rename(to_ID = from_ID, to_x = from_x, to_y = from_y, to_elev = from_elev)
  
  dataset_odd_even <- left_join(dataset_odd, 
                                dataset_even[, c('edge_ID','to_ID','to_x','to_y','to_elev')] %>% 
                                  st_set_geometry(NULL), 
                                by = 'edge_ID')
  dataset_even_odd <- dataset_odd_even[, c(1:19, 25:28, 24, 20:23)]
  colnames(dataset_even_odd) <- names(dataset_odd_even)
  
  dataset <- rbind(dataset_odd_even, dataset_even_odd) %>% 
    arrange(edge_ID)
  
  return(dataset)
}

dataset <- CreateDataset(EdgeData, NodeData)

## 2. Calculate grade and grade stress using dataset and put them to EdgeData
dataset <- dataset %>% 
  mutate(grade =  abs((to_elev - from_elev)/length)*100,
         grade.stress = case_when(
           grade <= 2 ~ 1.00,
           grade <= 4 ~ 1.37,
           grade <= 6 ~ 2.20,
           grade > 6 ~ 4.20
           )
         )

EdgeData.new <- dataset %>% 
  st_set_geometry(NULL) %>% 
  group_by(edge_ID) %>% 
  summarise(grade = first(grade),
            grade.stress = first(grade.stress)) %>% 
  left_join(EdgeData, ., by = 'edge_ID') %>% 
  mutate(grade = ifelse(is.na(grade), 0, grade),
         grade.stress = ifelse(is.na(grade.stress), 1, grade.stress)
         )

## 3. Remove dead-ends from NodeData and create dataset again with the new NodeData
NodeData.buffer <- st_buffer(NodeData, 0.001)

NodeData.not.deadends <- st_join(NodeData.buffer, EdgeData, join = st_intersects) %>% 
  st_set_geometry(NULL) %>% 
  group_by(node_ID) %>% 
  summarise(edge.count = sum(!is.na(edge_ID))) %>% 
  filter(edge.count >= 2)

NodeData <- NodeData[NodeData$node_ID %in% NodeData.not.deadends$node_ID, ] %>% 
  mutate(node_ID = 1:nrow(.))

dataset <- CreateDataset(EdgeData, NodeData) %>% 
  mutate(grade =  abs((to_elev - from_elev)/length)*100,
         grade.stress = case_when(
           grade <= 2 ~ 1.00,
           grade <= 4 ~ 1.37,
           grade <= 6 ~ 2.20,
           grade > 6 ~ 4.20
           )
         )
EdgeData <- EdgeData.new

# set the grade limit to 20
EdgeData$grade[EdgeData$grade > 20] <- 20
dataset$grade[dataset$grade > 20] <- 20



## 4. Put 1 to the code.bike of edges that are closest to subway stations 
EdgeData.nearest.to.subway.stations <-EdgeData[st_nearest_feature(subway, EdgeData), ]
save(EdgeData.nearest.to.subway.stations, file = 'current_station_edge_length.RData')

EdgeData[st_nearest_feature(subway, EdgeData), ]$Code_Bike <- 1


save(EdgeData, NodeData, dataset, file = 'NodeData_EdgeData_dataset.RData')
#load('NodeData_EdgeData_dataset.RData')
#load('current_station_edge_length.RData')
rm(list = setdiff(ls(), c('EdgeData', 'NodeData', 'dataset', 'EdgeData.nearest.to.subway.stations')))


###############################################################
###############################################################
######################### 1st Gravity #########################
###############################################################
###############################################################

## Chunk generating process: make touching edges to chunks --------------------------------------------------

## 1. Define functions
source('0_chunk_generating_functions.R')

## 2. Create chunks
# Let touching edges be chunks
chunk <- fun_sweeper(EdgeData %>% filter(Code_Bike == 1))

# Distinguish chunk in use and chunk not in use 
filter.index.chunk.isolated <- apply(st_intersects(st_buffer(chunk, 0.001), EdgeData[EdgeData$Code_Drive == 1, ]), 1, sum) == 0
chunk.non.use <- chunk[filter.index.chunk.isolated, ]
chunk <- chunk[!filter.index.chunk.isolated, ]

filter.index.chunk.debris <- chunk$length < 100 & !(chunk$length %in% EdgeData.nearest.to.subway.stations$length)
chunk.non.use <- rbind(chunk.non.use, chunk[filter.index.chunk.debris, ])
chunk <- chunk[!filter.index.chunk.debris, ]

save(chunk, chunk.non.use, file = 'chunk.RData')

## Calculate gravities between chunks and choose which pair of chunks to connect --------------------------------------------------

## 1. Give nodes membership (The ID of the chunck in which each node fall)
chunk.buffer <- st_buffer(chunk, 0.001)
nodes.with.membership <- st_join(NodeData, chunk.buffer, join = st_intersects) %>% 
  filter(!is.na(var)) %>%
  arrange(var) %>%
  select(node_ID, var, x, y, length, popden, empden)

## 2. Calculate the gravities between nodes
dist.matrix <- st_distance(nodes.with.membership)

for (i in unique(nodes.with.membership$var)) {
  block.box <- row.names(nodes.with.membership[nodes.with.membership$var == i,]) %>% as.numeric()
  dist.matrix[block.box, block.box]<- NA
}

numerator <- rep(nodes.with.membership$length, nrow(nodes.with.membership)) %>% 
  matrix(.,nrow(nodes.with.membership))
numerator <- t(numerator) * numerator

gravity <- numerator / (dist.matrix/1000)^2

popden_matrix <- rep(nodes.with.membership$popden, nrow(nodes.with.membership)) %>%
  matrix(., nrow(nodes.with.membership))
popden_matrix <- t(popden_matrix) + popden_matrix

empden_matrix <- rep(nodes.with.membership$empden, nrow(nodes.with.membership)) %>% 
  matrix(., nrow(nodes.with.membership))
empden_matrix <- t(empden_matrix) + empden_matrix

den_matrix <- popden_matrix + empden_matrix

gravity.new <- gravity * den_matrix

## 3. Pick the pair of chuncks to connect
which(gravity.new == max(gravity.new, na.rm = T), arr.ind = T)

node.chunk1.new <- which(gravity.new == max(gravity.new, na.rm = T), arr.ind = T)[1,1]
node.chunk2.new <- which(gravity.new == max(gravity.new, na.rm = T), arr.ind = T)[2,1] 

chunk.pair.new <- nodes.with.membership[c(node.chunk1.new,node.chunk2.new),]$var

nodes.in.chunk1.new <- nodes.with.membership[nodes.with.membership$var == chunk.pair.new[1], ]
nodes.in.chunk2.new <- nodes.with.membership[nodes.with.membership$var == chunk.pair.new[2], ]

# Load minority data for detecting equity spots
load('minority_block_group.RData')
minority <- minority %>% 
  mutate(id.new = 1:nrow(.)) %>% 
  st_transform(crs = 32616)

# Load pop.emp.poi data for detecting demand spots
load('pop_emp_poi_block_group.RData')
pop.emp.poi <- pop.emp.poi %>% 
  mutate(id.new = 1:nrow(.)) %>% 
  st_transform(crs = 32616)


rm(list = setdiff(ls(), c('chunk', 'chunk.non.use', 'dataset', 'EdgeData', 'NodeData', 
                          'nodes.with.membership', 'nodes.in.chunk1.new', 'nodes.in.chunk2.new',
                          'minority', 'pop.emp.poi')))
save.image('nodes_in_chunk1_and_chunk_2.RData')
#load('nodes_in_chunk1_and_chunk_2.RData')

## (ATLANTA ONLY VERSION) Calculate gravities between chunks and choose which pair of chunks to connect ---------------------------------

## 1. Select dataset within city boundary
# Load altanta shapefile and cut data with it
atlanta <- st_read('./data/Shape/CityofAtlanta.shp') %>% 
  st_transform(32616)

dataset <- dataset[atlanta %>% st_buffer(2000), ]
EdgeData <- EdgeData[atlanta %>% st_buffer(2000), ]
NodeData <- NodeData[atlanta %>% st_buffer(1000), ]

# Select chunks within city boundary (old version)
chunk <- chunk[atlanta, ]
chunk.non.use <- chunk.non.use[atlanta, ]

## 2. Give nodes membership (The ID of the chunck in which each node fall)
chunk.buffer <- st_buffer(chunk, 0.001)
nodes.with.membership <- st_join(NodeData, chunk.buffer, join = st_intersects) %>% 
  filter(!is.na(var)) %>%
  arrange(var) %>%
  select(node_ID, var, x, y, length, popden, empden)

## 3. Calculate the gravities between nodes
dist.matrix <- st_distance(nodes.with.membership)

for (i in unique(nodes.with.membership$var)) {
  block.box <- row.names(nodes.with.membership[nodes.with.membership$var == i,]) %>% as.numeric()
  dist.matrix[block.box, block.box]<- NA
}

numerator <- rep(nodes.with.membership$length, nrow(nodes.with.membership)) %>% 
  matrix(.,nrow(nodes.with.membership))
numerator <- t(numerator) * numerator

gravity <- numerator / (dist.matrix/1000)^2

popden_matrix <- rep(nodes.with.membership$popden, nrow(nodes.with.membership)) %>%
  matrix(., nrow(nodes.with.membership))
popden_matrix <- t(popden_matrix) + popden_matrix

empden_matrix<- rep(nodes.with.membership$empden, nrow(nodes.with.membership)) %>% 
  matrix(., nrow(nodes.with.membership))
empden_matrix <- t(empden_matrix) + empden_matrix

den_matrix <- popden_matrix + empden_matrix

gravity.new <- gravity * den_matrix

## 4. Pick the pair of chuncks to connect
which(gravity.new == max(gravity.new, na.rm = T), arr.ind = T)

node.chunk1.new <- which(gravity.new == max(gravity.new, na.rm = T), arr.ind = T)[1,1]
node.chunk2.new <- which(gravity.new == max(gravity.new, na.rm = T), arr.ind = T)[2,1] 

chunk.pair.new <- nodes.with.membership[c(node.chunk1.new,node.chunk2.new),]$var

nodes.in.chunk1.new <- nodes.with.membership[nodes.with.membership$var == chunk.pair.new[1], ]
nodes.in.chunk2.new <- nodes.with.membership[nodes.with.membership$var == chunk.pair.new[2], ]

# Load minority data for detecting equity spots
load('minority_block_group.RData')
minority <- minority %>% 
  mutate(id.new = 1:nrow(.)) %>% 
  st_transform(crs = 32616)

minority.center <- st_centroid(minority) %>% .[atlanta, ]
minority <- minority[minority$GEOID %in% minority.center$GEOID, ]

# Load pop.emp.poi data for detecting demand spots
load('pop_emp_poi_block_group.RData')
pop.emp.poi <- pop.emp.poi %>% 
  mutate(id.new = 1:nrow(.)) %>% 
  st_transform(crs = 32616)

pop.emp.poi.center <- st_centroid(pop.emp.poi) %>% .[atlanta, ]
pop.emp.poi <- pop.emp.poi[pop.emp.poi$GEOID %in% pop.emp.poi.center$GEOID, ]


rm(list = setdiff(ls(), c('chunk', 'chunk.non.use', 'dataset', 'EdgeData', 'NodeData', 
                          'nodes.with.membership', 'nodes.in.chunk1.new', 'nodes.in.chunk2.new',
                          'minority', 'pop.emp.poi')))
save.image('nodes_in_chunk1_and_chunk_2_city.RData')
#load('nodes_in_chunk1_and_chunk_2_city.RData')


## (3 COUNTIES VERSION) Calculate gravities between chunks and choose which pair of chunks to connect ---------------------------------

load('nodes_in_chunk1_and_chunk_2.RData')

## 1. Select dataset within 3 counties boundary
# Load altanta shapefile and cut data with it
three.counties <- st_read('./data/Shape/5counties_area.shp') %>% 
  st_transform(32616) %>% 
  filter(str_detect(place_name, 'Fulton|DeKalb|Clayton')) %>% 
  summarize()

# ggplot(three.counties) +geom_sf()

dataset <- dataset[three.counties %>% st_buffer(2000), ]
EdgeData <- EdgeData[three.counties %>% st_buffer(2000), ]
NodeData <- NodeData[three.counties %>% st_buffer(1000), ]

# Select chunks within 3county boundary
chunk <- st_intersection(chunk, three.counties)
chunk.non.use <- chunk.non.use[three.counties, ]
#ggplot() + geom_sf(data = three.counties) + geom_sf(data = chunk)

## 2. Give nodes membership (The ID of the chunck in which each node fall)
chunk.buffer <- st_buffer(chunk, 0.001)
nodes.with.membership <- st_join(NodeData, chunk.buffer, join = st_intersects) %>% 
  filter(!is.na(var)) %>%
  arrange(var) %>%
  select(node_ID, var, x, y, length, popden, empden)

## 3. Calculate the gravities between nodes
dist.matrix <- st_distance(nodes.with.membership)

for (i in unique(nodes.with.membership$var)) {
  block.box <- row.names(nodes.with.membership[nodes.with.membership$var == i,]) %>% as.numeric()
  dist.matrix[block.box, block.box]<- NA
}

numerator <- rep(nodes.with.membership$length, nrow(nodes.with.membership)) %>% 
  matrix(.,nrow(nodes.with.membership))
numerator <- t(numerator) * numerator

gravity <- numerator / (dist.matrix/1000)^2

popden_matrix <- rep(nodes.with.membership$popden, nrow(nodes.with.membership)) %>%
  matrix(., nrow(nodes.with.membership))
popden_matrix <- t(popden_matrix) + popden_matrix

empden_matrix<- rep(nodes.with.membership$empden, nrow(nodes.with.membership)) %>% 
  matrix(., nrow(nodes.with.membership))
empden_matrix <- t(empden_matrix) + empden_matrix

den_matrix <- popden_matrix + empden_matrix

gravity.new <- gravity * den_matrix

## 4. Pick the pair of chuncks to connect
which(gravity.new == max(gravity.new, na.rm = T), arr.ind = T)

node.chunk1.new <- which(gravity.new == max(gravity.new, na.rm = T), arr.ind = T)[1,1]
node.chunk2.new <- which(gravity.new == max(gravity.new, na.rm = T), arr.ind = T)[2,1] 

chunk.pair.new <- nodes.with.membership[c(node.chunk1.new,node.chunk2.new),]$var

nodes.in.chunk1.new <- nodes.with.membership[nodes.with.membership$var == chunk.pair.new[1], ]
nodes.in.chunk2.new <- nodes.with.membership[nodes.with.membership$var == chunk.pair.new[2], ]


# Load minority data for detecting equity spots
load('minority_block_group.RData')
minority <- minority %>% 
  mutate(id.new = 1:nrow(.)) %>% 
  st_transform(crs = 32616)

minority.center <- st_centroid(minority) %>% .[three.counties, ]
minority <- minority[minority$GEOID %in% minority.center$GEOID, ]

# Load pop.emp.poi data for detecting demand spots
load('pop_emp_poi_block_group.RData')
pop.emp.poi <- pop.emp.poi %>% 
  mutate(id.new = 1:nrow(.)) %>% 
  st_transform(crs = 32616)

pop.emp.poi.center <- st_centroid(pop.emp.poi) %>% .[three.counties, ]
pop.emp.poi <- pop.emp.poi[pop.emp.poi$GEOID %in% pop.emp.poi.center$GEOID, ]

rm(list = setdiff(ls(), c('chunk', 'chunk.non.use', 'dataset', 'EdgeData', 'NodeData', 
                          'nodes.with.membership', 'nodes.in.chunk1.new', 'nodes.in.chunk2.new', 
                          'minority', 'pop.emp.poi')))
save.image('nodes_in_chunk1_and_chunk_2_3counties.RData')
#load('nodes_in_chunk1_and_chunk_2_3counties.RData')




