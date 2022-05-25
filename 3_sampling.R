install.packages("devtools")
devtools::install_github("jamgreen/lehdr")

library(sf)
library(tidyverse)
library(tidycensus)
library(lehdr)
library(readxl)
library(wrswoR)
library(geojsonio)

setwd("#####") 

################# WEIGHTED RANDOM SAMPLING #################
## Load data
load('nodes_in_chunk1_and_chunk_2_city.RData')
rm(list = setdiff(ls(), c('NodeData')))

## Merge population and employment with NodeData
# Load LODES data 
lodes <- grab_lodes("ga", 2017, lodes_type = "wac", 
                    job_type = "JT00",
                    segment = "S000",
                    agg_geo = "bg", 
                    download_dir = getwd()
) %>%
  select(GEOID = w_bg, emp = C000) 

# Load tidycensus data
census_api_key("#####") # var.list <- load_variables(2017, "acs5")
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
  mutate(pop = popE) %>% 
  select(GEOID, pop, emp) %>% 
  st_transform(32616)

pop.emp[is.na(pop.emp)] <- 0

# # Join pop.emp data to NodeData
# NodeData <- NodeData %>% 
#   st_join(pop.emp, join = st_intersects) %>%  
#   .[!duplicated(.$node_ID), ]
# 
# ## Conduct sampling giving more weigths on places with large popualtion and employment
# # Calculate 'Number of Nodes' of each block group
# NodeData <- NodeData %>%
#   group_by(GEOID) %>% 
#   summarize(count = n()) %>% 
#   st_set_geometry(NULL) %>% 
#   left_join(NodeData, ., by = 'GEOID')
# 
# 
# # Calculate probability for the weighted sampling
# NodeData$pop.emp.sum <- NodeData$pop + NodeData$emp
# NodeData$prob <- NodeData$pop.emp.sum / sum(NodeData$pop.emp.sum[duplicated(NodeData$GEOID) == F]) / NodeData$count
# 
# sum(NodeData$prob) # check if sum == 1
# 
# euclidean <- function(point1_x, point1_y, point2_x, point2_y)
#   (((point1_x-point2_x)^2+(point1_y-point2_y)^2)^(1/2))
# 
# # Within 6-mile trip sample
# sample.index.1 <- sample_int_R(nrow(NodeData), round(nrow(NodeData)*0.05), prob = NodeData$prob)
# sample.1 <- NodeData[sample.index.1, ] %>% select(node.id.1 = node_ID, x1 = x, y1 = y) %>% st_set_geometry(NULL)
# sample.index.2 <- sample_int_R(nrow(NodeData), round(nrow(NodeData)*0.05), prob = NodeData$prob)
# sample.2 <- NodeData[sample.index.2, ] %>% select(node.id.2 = node_ID, x2 = x, y2 = y) %>% st_set_geometry(NULL)
# 
# sample <- cbind(sample.1, sample.2)
# sample$dist <- euclidean(sample$x1, sample$y1, sample$x2, sample$y2)
# sample.6mile <- filter(sample, dist < 6*1609.34) %>% .[1:100, ]
# 
# rm(census, lodes, pop.emp, sample, sample.1, sample.2, sample.index.1, sample.index.2)
# 
# # Within 3-mile subway access trip sample
# sample.index.3 <- sample_int_R(nrow(NodeData), round(nrow(NodeData)*0.03), prob = NodeData$prob)
# sample.3 <- NodeData[sample.index.3,] %>% select(node.id.1 = node_ID, x1 = x, y1 = y)
# 
# station <- read_excel('./data/GTFS/MARTA_Subway_Stations.xlsx') %>%
#   as.data.frame() %>% 
#   st_as_sf(coords = c('Lon', 'Lat'), dim = "XY", crs = 4326) %>% 
#   st_transform(32616)
# 
# station.node <-NodeData[st_nearest_feature(station, NodeData), ]
# sample.3.nearest.station <- station.node[st_nearest_feature(sample.3, station.node), ] %>% 
#   select(node.id.2 = node_ID, x2 = x, y2 = y) %>% st_set_geometry(NULL)
# sample.3 <- sample.3 %>% st_set_geometry(NULL)
# 
# sample <- cbind(sample.3, sample.3.nearest.station)
# sample$dist <- euclidean(sample$x1, sample$y1, sample$x2, sample$y2)
# sample.3mile <- filter(sample, dist < 3*1609.34) %>% .[1:100, ]
# rm(sample, sample.3, sample.index.3, sample.3.nearest.station, station, station.node)
# 
# # save the result
# save(sample.3mile, sample.6mile, file = 'sample.RData')



# calculate TAZ pop.emp
pop.emp$orig_area <- st_area(pop.emp)

taz <- st_read('data/Shape/TAZ_CityofAtlanta.shp') %>% 
  select(taz.id = GEOID10)

taz.inter <- st_intersection(taz, pop.emp) %>% 
  mutate(frag_area = st_area(.)) %>% 
  mutate(pop.emp = as.numeric((pop + emp) * (frag_area/orig_area)))

taz <- taz.inter %>% 
  group_by(taz.id) %>% summarize(pop.emp = sum(pop.emp))


# join TAZ centroids to the nearest node
taz.point <- st_centroid(taz)

taz.node <-NodeData[st_nearest_feature(taz.point, NodeData), ]
taz.node$id <- seq(1:247)
taz.node$pop.emp <- taz.point$pop.emp
taz.node$taz.id <- taz.point$taz.id

# calculate 'trip probability' based on pop.emp & distance for all possible pairs
taz.node.pair <- data.frame(taz1 = rep(1:nrow(taz.node), times = nrow(taz.node)),
                            taz2 = rep(1:nrow(taz.node), each = nrow(taz.node))) %>%
  left_join(taz.node %>% 
              st_set_geometry(NULL) %>% 
              select(id, taz.id, node_ID, pop.emp), by = c('taz1' = 'id')) %>% 
  rename(node.id.1 = node_ID, taz.id.1 = taz.id, pop.emp.1 = pop.emp) %>% 
  left_join(taz.node %>% 
              st_set_geometry(NULL) %>% 
              select(id, taz.id, node_ID, pop.emp), by = c('taz2' = 'id')) %>% 
  rename(node.id.2 = node_ID, taz.id.2 = taz.id, pop.emp.2 = pop.emp)

taz.node.pair$dist <- as.vector(st_distance(taz.node))

taz.node.pair <- taz.node.pair %>% 
  filter(taz1 > taz2) %>% 
  filter(dist != 0) %>% 
  filter(dist < 1609.344 * 6) %>% 
  select(-taz1, -taz2) %>% 
  mutate(gravity = pop.emp.1 * pop.emp.2 / dist) %>% 
  mutate(prob = gravity/sum(gravity))

taz.node.pair <- taz.node.pair %>% 
  group_by(node.id.1, node.id.2) %>% 
  summarize(taz.id.1 = first(taz.id.1),
            taz.id.2 = first(taz.id.2),
            prob = sum(prob))

#### 6mile sampling (based on the trip probability)
# sample 200 pairs (can be changed later)
taz.sample.index.1 <- sample_int_R(nrow(taz.node.pair), 200, prob = taz.node.pair$prob)
taz.sample.6mile <- taz.node.pair[taz.sample.index.1,]

#### 3mile (subway access) sampling
# load station.shp
station <- read_excel('./data/GTFS/MARTA_Subway_Stations.xlsx') %>%
  as.data.frame() %>%
  st_as_sf(coords = c('Lon', 'Lat'), dim = "XY", crs = 4326) %>%
  st_transform(32616) %>% 
  .[st_read('./data/Shape/CityofAtlanta.shp') %>% st_transform(32616), ]

# join the station points to the nearest node
station.node <-NodeData[st_nearest_feature(station, NodeData), ]

# nearest stations from the taz centroids and the distance
nearest.station <- station.node[st_nearest_feature(taz.node, station.node), ] %>% 
  st_set_geometry(NULL) %>% 
  select(node.id.2 = node_ID)

nearest.station.distance <- st_distance(taz.node, station.node)

# choose pairs within 3 mile (which are only 216, thus a sampling is unnecessary)
taz.sample.3mile <- taz.node %>% 
  st_set_geometry(NULL) %>% 
  select(node.id.1 = node_ID, taz.id, pop.emp) %>% 
  cbind(nearest.station) %>% 
  cbind(dist = apply(nearest.station.distance, 1, min)) %>% 
  filter(dist < 1609.344 * 3)

taz.sample.3mile <- taz.sample.3mile %>% 
  group_by(node.id.1, node.id.2) %>% 
  summarize(taz.id = first(taz.id),
            pop.emp = sum(pop.emp),
            dist = first(dist))

rm(taz, taz.inter, taz.point, taz.sample.index.1,
   nearest.station, nearest.station.distance, station, station.node)

save(taz.sample.3mile, taz.sample.6mile, file = 'sample_taz.RData')




############ Export the samples for visualization ############ 
# 
# NodeData.lonlat <- NodeData %>% 
#   mutate(x = NodeData %>% st_transform(4326) %>% st_coordinates() %>% .[, 'X'],
#          y = NodeData %>% st_transform(4326) %>% st_coordinates() %>% .[, 'Y']) %>% 
#   st_transform(4326)
# 
# taz.sample.6mile <- taz.sample.6mile %>% 
#   left_join(NodeData.lonlat %>% 
#               select(node_ID, x, y) %>% 
#               st_set_geometry(NULL), 
#             by = c('node.id.1' = 'node_ID')) %>% 
#   rename(x1 = x, y1 = y) %>% 
#   left_join(NodeData.lonlat %>% 
#               select(node_ID, x, y) %>% 
#               st_set_geometry(NULL), 
#             by = c('node.id.2' = 'node_ID')) %>% 
#   rename(x2 = x, y2 = y)
# 
# taz.sample.3mile <- taz.sample.3mile %>% 
#   left_join(NodeData.lonlat %>% 
#               select(node_ID, x, y) %>% 
#               st_set_geometry(NULL), 
#             by = c('node.id.1' = 'node_ID')) %>% 
#   rename(x1 = x, y1 = y) %>% 
#   left_join(NodeData.lonlat %>% 
#               select(node_ID, x, y) %>% 
#               st_set_geometry(NULL), 
#             by = c('node.id.2' = 'node_ID')) %>% 
#   rename(x2 = x, y2 = y)
# 
# write.csv(taz.sample.6mile, file = 'taz_sample_6mile.csv')
# write.csv(taz.sample.3mile, file = 'taz_sample_3mile.csv')



############ Export pop.emp and taz for visualization ############
# 
# taz %>%
#   st_transform(4326) %>%
#   sf_geojson() %>%
#   geojson_write(file = 'taz.geojson')
# pop.emp %>%
#   .[st_read('./data/Shape/CityofAtlanta.shp') %>% st_transform(32616), ] %>% 
#   st_transform(4326) %>%
#   sf_geojson() %>%
#   geojson_write(file = 'pop_emp.geojson')
