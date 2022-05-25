library(tictoc)
library(astar)
require(dplyr)
library(magrittr)
library(sf)
library(units)
library(R.utils)
library(doSNOW)
library(doParallel)
library(foreach)
library(Rmpi)
library(stringr)

setwd("#####") # cluster computer directory

boundary <- 'CityofAtlanta'
# taz.boundary <- 'TAZ_CityofAtlanta'

# Import 'city of Atlanta' data (need to delete 'city' in the object name)
load('../../nodes_in_chunk1_and_chunk_2_city.RData') 
nodes.in.chunk1 <- nodes.in.chunk1.new
nodes.in.chunk2 <- nodes.in.chunk2.new
rm(nodes.in.chunk1.new, nodes.in.chunk2.new)

# 3 is the biggest chunk outside of city boundary
chunk <- chunk[chunk$var != 3, ]

# exclude Atlanta airport block groups
minority <- minority %>% filter(GEOID != '131210108003' & GEOID != '131219800001')
pop.emp.poi <- pop.emp.poi %>% filter(GEOID != '131210108003' & GEOID != '131219800001')

# set the grade limit to 20
EdgeData$grade[EdgeData$grade > 20] <- 20
dataset$grade[dataset$grade > 20] <- 20

# Score -------------------------------------------------------------------
normalize <- function(data)
  (data - min(data)) / (max(data) - min(data))

z.score <- EdgeData %>%
  st_set_geometry(NULL) %>% 
  mutate(z.aadt = (aadt - mean(aadt))/sd(aadt),
         z.grade = (grade - mean(grade))/sd(grade),
         z.bus.freq = (bus.freq - mean(bus.freq))/sd(bus.freq),
         z.sub.prox = (sub.prox - mean(sub.prox))/sd(sub.prox),
         z.popden = (popden - mean(popden))/sd(popden),
         z.empden = (empden - mean(empden))/sd(empden),
         z.poi.cnt = (poi.cnt - mean(poi.cnt))/sd(poi.cnt),
         z.poverty = (poverty - mean(poverty))/sd(poverty),
         z.race = (race - mean(race))/sd(race),
         z.ethnic = (ethnic - mean(ethnic))/sd(ethnic)
  ) %>% 
  mutate(z.score = - z.aadt*1 - z.grade*1
         + z.bus.freq*1 + z.sub.prox*1
         + z.popden*1 + z.empden*1 + z.poi.cnt*1
         + z.poverty*1 + z.race*1 + z.ethnic*1,
         score = normalize(z.score),
         weighted_length = length * (1 - score)
  ) %>% 
  select(edge_ID, z.score, score, weighted_length)

EdgeData <- EdgeData %>% 
  left_join(z.score, by = 'edge_ID')

dataset <- dataset %>% 
  left_join(z.score, by = 'edge_ID')

rm(z.score, normalize)

########################################################################
########################################################################
###########  Additional spots for the network making process ###########
########################################################################
########################################################################

## 0. load and prepare data
# Load bike facility data
arc.bike <- st_read('../../data/Shape/Regional_Bikeway_Inventory_2019_NoGolf_join.shp') %>% 
  st_transform(32616) %>% 
  st_cast('MULTILINESTRING') %>% 
  st_cast('LINESTRING') %>% 
  .[st_read(paste0('../../data/Shape/',boundary,'.shp')), ] %>% 
  select(bikeway.name = Name, bike.type = category, bike.type.uj = type) %>% 
  filter(bike.type.uj != 'sharrow') %>% 
  mutate(arc.bike.ID = 1:nrow(.), code.bike = 1) 

# Join Edgedata and block groups with minority data
edges.bg <- st_join(EdgeData %>% select(edge_ID, score, length), minority %>% select(id.new), join = st_intersects) %>%
  filter(is.na(id.new) == F)

# delete block groups with no edges
equity <- minority %>% 
  filter(id.new %in% unique(edges.bg$id.new)) %>% 
  mutate(equity.score = (poverty - mean(poverty))/sd(poverty) +
           (race - mean(race))/sd(race) +
           (ethnic - mean(ethnic))/sd(ethnic))
print(nrow(equity))

demand <- pop.emp.poi %>% 
  filter(id.new %in% unique(edges.bg$id.new)) %>% 
  mutate(demand.score = (popden - mean(popden))/sd(popden) +
           (empden - mean(empden))/sd(empden) +
           (poiden - mean(poiden))/sd(poiden))
print(nrow(demand))

equity.demand <- left_join(equity %>% select(GEOID, id.new, poverty, race, ethnic, equity.score),
                           demand %>% select(GEOID, area, popden, empden, poiden, demand.score) %>% st_set_geometry(NULL),
                           by = 'GEOID')

save(equity.demand, file = 'test.RData')
rm(edges.bg, minority, pop.emp.poi, equity, demand)  

# stations only (version 2)
hubs <- st_read('../../data/GTFS/stations.shp')


## 1. identify BGs that are distant from the current bike network or hubs (400m from centroid)
bg.center <- st_centroid(equity.demand)

dist.bg.bike.n.hubs <- cbind(st_distance(bg.center, arc.bike) %>% apply(1, FUN = min),
                             st_distance(bg.center, hubs) %>% apply(1, FUN = min)) %>% 
  apply(1, FUN = min)

bg.center.distant <- bg.center[dist.bg.bike.n.hubs > 643, ] #0.4 mile

bg.distant <- equity.demand[equity.demand$id.new %in% bg.center.distant$id.new, ]

rm(bg.center, dist.bg.bike.n.hubs, bg.center.distant)

# select block groups that have high equity or demand score (>1 standard deviation) among the distant block groups
bg.distant.equity <- bg.distant[bg.distant$equity.score > (mean(bg.distant$equity.score) + sd(bg.distant$equity.score)),]
bg.distant.demand <- bg.distant[bg.distant$demand.score > (mean(bg.distant$demand.score) + sd(bg.distant$demand.score)),]

sum(bg.distant.demand$GEOID %in% bg.distant.equity$GEOID)


## 2. select edge (except residential road) that are fartest from the current bike network or hubs
chosen.bg <- equity.demand[equity.demand$GEOID %in% bg.distant.demand$GEOID | equity.demand$GEOID %in% bg.distant.equity$GEOID,]

edges.bg <- st_join(EdgeData %>% filter(highway != 'residential'), chosen.bg, join = st_intersects) %>% 
  filter(!is.na(GEOID)) %>% 
  arrange(GEOID)

chosen.edge <- numeric(0)

tic()
for (k in unique(edges.bg$GEOID)) {
  subset <- edges.bg[edges.bg$GEOID == k,]
  subset.dist <- cbind(st_distance(subset, arc.bike) %>% apply(1, FUN = min),
                       st_distance(subset, hubs) %>% apply(1, FUN = min)) %>% 
    apply(1, FUN = min)
  
  chosen.edge <- c(chosen.edge, subset$edge_ID[which(subset.dist == max(subset.dist, na.rm = T), arr.ind = T)][1])
}
toc()


## 3. additional hubs (for the scenario that we separate them)
odmts <- st_read('../../data/GTFS/odmts_hub.shp')
hubs.current <- odmts[st_buffer(hubs, 300),]
hubs.future <- odmts %>% 
  filter(not(St_Name %in% hubs.current$St_Name)) %>% 
  .[st_read(paste0('../../data/Shape/',boundary,'.shp')), ]

hubs.future.edge <- EdgeData[st_nearest_feature(hubs.future, EdgeData), ]

# spots.edge.id includes equity spots, demand spots, and future hubs
spots.edge.id <- c(chosen.edge, hubs.future.edge$edge_ID)

save(spots.edge.id, file = './iteration_result/spots_edge_id.RData')

# Subway = 200m
load('../../current_station_edge_length.RData')
chunk$length[chunk$length %in% EdgeData.nearest.to.subway.stations$length] <- 200


rm(k, arc.bike, boundary, bg.distant, bg.distant.demand, bg.distant.equity, chosen.bg, chosen.edge, 
   edges.bg, equity.demand, hubs.current, hubs.future, hubs.future.edge, subset, subset.dist, odmts)


########### Functions ############ ----------------------------------------
# A-star functions
is_goal_reached <- function(src, dst)
  src == dst

neighbors <- function(node)
  as.character(dataset$to_ID[dataset$from_ID == node])

euclidean <- function(point1_x, point1_y, point2_x, point2_y)
  (((point1_x-point2_x)^2+(point1_y-point2_y)^2)^(1/2))

heuristic <- function(node, goal)
  euclidean(unique(dataset$from_x[dataset$from_ID == node]),unique(dataset$from_y[dataset$from_ID == node]),
            unique(dataset$from_x[dataset$to_ID == goal]),unique(dataset$from_y[dataset$to_ID == goal]))[1]*0.2

cost <- function(src, dst)
  dataset$weighted_length[dataset$from_ID == src & dataset$to_ID == dst][1]

# Chunk Generating Functions
source('../../0_chunk_generating_functions.R')

############################## iteration ('Original' version) ###############################
iteration = 1

runtime.iteration <- data.frame(iteration = numeric(0), elapsed = numeric(0))
pair.count <- data.frame(iteration = numeric(0), pair.count = numeric(0), chunk1.node = numeric(0), chunk2.node = numeric(0))
chosen.route.info <- data.frame(Edge_ID = numeric(0), length = numeric(0), iteration = numeric(0))
route.score.info <- data.frame(min = numeric(0), first= numeric(0), median = numeric(0), third = numeric(0), max = numeric(0), std = numeric(0))
result_node <- NULL

node.buffer <- st_buffer(NodeData, 0.001)

cl <- makeMPIcluster(mpi.universe.size()-1)
registerDoSNOW(cl)

while(length(unique(chunk$var)) > 1) {
  tic(paste('######## 1. astar preparation_',iteration,' (',format(Sys.time(), "%X"),')',sep=''))
  # exclude nodes that cannot be an origin or a destination
  nodes.in.chunk1 <- nodes.in.chunk1 %>% 
    .[.$node_ID %in% dataset$from_ID, ] %>% 
    select(-popden, -empden)
  nodes.in.chunk2 <- nodes.in.chunk2 %>% 
    .[.$node_ID %in% dataset$from_ID, ] %>% 
    select(-popden, -empden)
  
  # select nodes that are connected to potential bike edges
  nodes.in.chunk1.buffer <- nodes.in.chunk1 %>% 
    st_buffer(dist = 0.1) %>% 
    select(node_ID)
  nodes.in.chunk1.buffer.join <- st_join(nodes.in.chunk1.buffer, dataset[dataset$Code_Bike == 0,], join = st_intersects) %>%
    st_set_geometry(NULL)
  
  nodes.in.chunk1.filter <- nodes.in.chunk1 %>% 
    left_join(nodes.in.chunk1.buffer.join %>%
                filter(is.na(edge_ID) == F) %>%
                .$node_ID %>%
                unique() %>%
                as.data.frame() %>%
                rename(node_ID = colnames(.)) %>%
                mutate(check = 1),
              by = 'node_ID') %>%
    filter(check == 1) %>% 
    select(-check)
  
  nodes.in.chunk2.buffer <- nodes.in.chunk2 %>% 
    st_buffer(dist = 0.1) %>% 
    select(node_ID)
  nodes.in.chunk2.buffer.join <- st_join(nodes.in.chunk2.buffer, dataset[dataset$Code_Bike == 0,], join = st_intersects) %>%
    st_set_geometry(NULL)
  
  nodes.in.chunk2.filter <- nodes.in.chunk2 %>% 
    left_join(nodes.in.chunk2.buffer.join %>%
                filter(is.na(edge_ID) == F) %>%
                .$node_ID %>%
                unique() %>%
                as.data.frame() %>%
                rename(node_ID = colnames(.)) %>%
                mutate(check = 1),
              by = 'node_ID') %>%
    filter(check == 1) %>% 
    select(-check)
  
  rm(nodes.in.chunk1.buffer, nodes.in.chunk1.buffer.join, nodes.in.chunk2.buffer, nodes.in.chunk2.buffer.join)
  
  dist_matrix <- st_distance(nodes.in.chunk1.filter, nodes.in.chunk2.filter)
  
  index.for.nodes.in.chunk1 <- which(dist_matrix < min(dist_matrix)*2, arr.ind = T)[,1]
  index.for.nodes.in.chunk2 <- which(dist_matrix < min(dist_matrix)*2, arr.ind = T)[,2]
  
  selected.pair.of.nodes <- cbind(nodes.in.chunk1.filter[index.for.nodes.in.chunk1, ] %>% st_set_geometry(NULL), 
                                  nodes.in.chunk2.filter[index.for.nodes.in.chunk2, ] %>% st_set_geometry(NULL))
  colnames(selected.pair.of.nodes) <- c("from_ID", "from_var", "from_x", "from_y", "from_length", "to_ID", "to_var", "to_x", "to_y", "to_length")
  
  pair.count <- pair.count %>% 
    add_row(iteration = iteration, pair.count = nrow(selected.pair.of.nodes), 
            chunk1.node = length(unique(index.for.nodes.in.chunk1)), chunk2.node = length(unique(index.for.nodes.in.chunk2)))
  
  if (nrow(selected.pair.of.nodes) > 500) {
    selected.pair.of.nodes <- selected.pair.of.nodes[sample(1:nrow(selected.pair.of.nodes), 500), ]
  }
  toc()
  
  ############## Astar Algorithm ###########################
  tic(paste('######## 2. A-star_',iteration,' (',format(Sys.time(), "%X"),')',sep=''))
  
  result <- data.frame(chunk1 = numeric(0), chunk2 = numeric(0), edge_ID = numeric(0), score = numeric(0), codebike = numeric(0), length = numeric(0))
  rm(result_node)
  
  result_node <- foreach (from_ID=selected.pair.of.nodes$from_ID, 
                          to_ID=selected.pair.of.nodes$to_ID,
                          .combine = rbind,
                          .packages="astar") %dopar% {
                            tryCatch(expr = {
                              cbind(as.data.frame(as.matrix(astar(from_ID, to_ID, heuristic, cost, neighbors, is_goal_reached))), from_ID, to_ID)
                            },
                            error = function(e){
                              data.frame(V1 = numeric(0), from_ID = numeric(0), to_ID = numeric(0))
                            })
                          }
  
  result_node <- left_join(result_node, group_by(result_node, from_ID, to_ID) %>% summarize() %>% as.data.frame() %>% mutate(id = 1:nrow(.)), by = c('from_ID','to_ID'))
  toc <- toc()
  runtime.iteration <- runtime.iteration %>% add_row(iteration = iteration, elapsed = toc$toc - toc$tic)
  
  save(result_node, file = paste('./iteration_result/result_node/result_node_',iteration,'.RData', sep=''))

  # node result -> edge result
  tic(paste('######## 3. summarizing A-star result_',iteration,' (',format(Sys.time(), "%X"),')',sep=''))
  # for (j in 1:max(result_node$id)) {
  #   for (k in 1:(nrow(result_node[result_node$id == j,])-1)) {
  #     edge_temp <- dataset[dataset$to_ID == as.integer(result_node[result_node$id == j,][k+1,'V1']) &
  #                            dataset$from_ID == as.integer(result_node[result_node$id == j,][k,'V1']),
  #                          c('edge_ID','score','Code_Bike','length')] %>% st_set_geometry(NULL)
  #     result <- result %>% add_row(chunk1 = result_node$from_ID[result_node$id==j][1], chunk2 = result_node$to_ID[result_node$id==j][1],
  #                                  edge_ID = edge_temp$edge_ID, score = edge_temp$score, length = edge_temp$length, codebike = edge_temp$Code_Bike)
  #   }
  # }
  
  # node result -> edge result (version 2.)
  result_node$V1 <- as.numeric(result_node$V1)
  result_node$id.2 <- 1:nrow(result_node)
  
  edge_temp <- result_node[,c('from_ID','to_ID','id','id.2','V1')] %>% 
    mutate(V2 = c(result_node$V1[2:nrow(result_node)],NA))
  
  last <- edge_temp %>% 
    group_by(id) %>% 
    summarize(id.2 = last(id.2))
  
  edge_temp <- edge_temp %>% 
    filter(not(id.2 %in% last$id.2))
  
  NodeToEdge <- function(node) {
    dataset[dataset$from_ID == node[5] & 
              dataset$to_ID == node[6], 
            c('edge_ID','score','length','Code_Bike')] %>% 
      .[1, ] %>%
      st_set_geometry(NULL)
  }
  
  result <- apply(edge_temp, 1, NodeToEdge)
  result <- cbind(edge_temp[,c(1,2)],
                  data.frame(matrix(unlist(result), nrow = length(result), byrow = T)))
  
  colnames(result) <- c('chunk1','chunk2','edge_ID','score','length','codebike')
  rm(last, edge_temp, NodeToEdge)
  
  # calculate avg_score (weighted by edge length) and add an index of each route (id)
  result$score_times_length <- result$score * result$length
  result_summary <- result %>% filter(codebike == 0) %>% group_by(chunk1, chunk2) %>% 
    summarize(sum_score_times_length = sum(score_times_length), length_sum = sum(length))
  result_summary$avg_score <- result_summary$sum_score_times_length / result_summary$length_sum
  result_summary$id <-  1:nrow(result_summary)
  result <- left_join(result, result_summary, by = c('chunk1', 'chunk2'))
  
  # join the result to edge.shp (because the result does not have a geometry)
  edge_result <- inner_join(EdgeData, result, by = "edge_ID")
  
  # route score variation check
  route.score.info <- route.score.info %>% add_row(min = round(as.vector(summary(result_summary$avg_score)[1]),4),
                                                   first = round(as.vector(summary(result_summary$avg_score)[2]),4),
                                                   median = round(as.vector(summary(result_summary$avg_score)[3]),4),
                                                   third = round(as.vector(summary(result_summary$avg_score)[5]),4),
                                                   max = round(as.vector(summary(result_summary$avg_score)[6]),4),
                                                   std = round(as.vector(sd(result_summary$avg_score)),4))
  
  
  # filter routes that area above 
  result_summary <- result_summary %>% 
    filter(avg_score > max(avg_score)/1.1 )
  
  # check whether each routes are close to other bike networks and save the result in 'result_summary'
  result_summary$other_network <- 0
  for (l in 1:nrow(result_summary)){
    edge_result_other_network <- st_buffer(edge_result %>% filter(id == l), dist = 100) %>% 
      st_join(filter(chunk, var != unique(selected.pair.of.nodes$from_var), var != unique(selected.pair.of.nodes$to_var)), join = st_intersects)
    if (sum(is.na(edge_result_other_network$var)) == nrow(edge_result_other_network)){
      result_summary$other_network[l] <- 1
    } else {
      result_summary$other_network[l] <- 1.1
    }
  }
  
  # weight 'avg_score' by 'other network'
  result_summary$avg_score_weighted <- result_summary$avg_score * result_summary$other_network
  
  # select the best route
  if (min(result_summary$length_sum) < 200){
    best_route <- result_summary$id[result_summary$length_sum == min(result_summary$length_sum)]
    if (length(best_route) > 1) {
      best_route <- best_route[1]
    }
  } else {
    best_route <- result_summary$id[result_summary$avg_score_weighted == max(result_summary$avg_score_weighted)]
    if (length(best_route) > 1) {
      best_route <- result_summary[result_summary$id %in% best_route,] %>% .[.$length_sum == min(.$length_sum),] %>% .$id
    }
    if (length(best_route) > 1) {
      best_route <- best_route[1]
    }
  }
  
  best_route_edge <- result$edge_ID[result$id == best_route] # for loop: assign(paste('best_route_edge_',i, sep=""), result$edge_ID[result$id == best_route])
  
  # save the route as shp file and change them to 'bikeable' in the dataset (for the next iteration)
  chosen_route <- dataset[dataset$edge_ID %in% best_route_edge, ] %>% 
    .[.$Code_Bike == 0, ] %>% 
    mutate(iter = iteration + 2000)
  save(chosen_route, file = paste('./iteration_result/chosen_route_',iteration,'.RData',sep=''))
  rm(chosen_route)
  
  chosen.route.info.temp <- dataset[dataset$edge_ID %in% best_route_edge, ] %>% 
    st_set_geometry(NULL) %>% 
    select(edge_ID, length) %>% 
    mutate(iteration = iteration)
  chosen.route.info <- rbind(chosen.route.info, chosen.route.info.temp)
  
  dataset$Code_Bike[dataset$edge_ID %in% best_route_edge] <- 1
  EdgeData$Code_Bike[EdgeData$edge_ID %in% best_route_edge] <- 1
  toc()
  
  
  #################### After recieving the edge_IDs of the new bike route ####################
  tic(paste('######## 4. new bike route_',iteration,' (',format(Sys.time(), "%X"),')',sep=''))
  # First, I receive the vector of edge_IDs, corresponding edges of which need to be changed to bikable ones.
  new.bikable.edges <- EdgeData[EdgeData$edge_ID %in% best_route_edge, ]
  
  # make a chunk of bike network of new bikable edges
  new.bikable.path <- fun_sweeper(new.bikable.edges) %>% 
    mutate(var = 1:nrow(.)) %>% 
    select(length, popden, empden, var)
  
  chunk.before <- chunk
  
  connect.chunk.num <- st_join(chunk, 
                               st_buffer(new.bikable.path %>% select(), 0.1) %>% 
                                 mutate(check = 1),
                               join = st_intersects) %>% filter(check == 1)
  
  # (to fix the mysterious error...) attach the chunks into a multilinestring manually
  `%notin%` <- Negate(`%in%`)
  chunk.connect <- chunk.before[chunk.before$var %in% connect.chunk.num$var, ]
  chunk.other <- chunk.before[chunk.before$var %notin% connect.chunk.num$var, ]
  chunk.new <- rbind(chunk.connect, new.bikable.path) %>% 
    mutate(popden.length = popden * length, empden.length = empden * length)
  
  chunk.new.multi <- chunk.new %>% 
    select() %>% 
    st_cast('LINESTRING') %>% 
    group_by() %>% 
    summarize() %>% 
    st_cast('MULTILINESTRING')
  
  chunk.new.multi$length <- sum(chunk.new$length)
  chunk.new.multi$popden <- sum(chunk.new$popden.length)/sum(chunk.new$length)
  chunk.new.multi$empden <- sum(chunk.new$empden.length)/sum(chunk.new$length)
  chunk.new.multi$var <- 0
  
  chunk <- rbind(chunk.new.multi, chunk.other) %>% 
    mutate(var = 1:nrow(.))
  
  rm(chunk.new, chunk.new.multi, chunk.other)  
  
  # Subway = 200m
  chunk$length[chunk$length %in% EdgeData.nearest.to.subway.stations$length] <- 200
  
  # Then, I can use the "Choosing the pair of chunks to connect" step to choose the next pair of chunks to connect, which produces nodes.in.chunk1 and nodes.in.chunck2 again.
  toc()
  
  # Choose which pair of chunks to connect  --------------------------------------------------
  if (length(unique(chunk$var)) == 1) {
    print("IT'S A COMPLETE NETWORK !!!")
  } else {
    
    print(paste('######## 5. new chunk pair_',iteration,' (',format(Sys.time(), "%X"),')',sep=""))
    
    tic('5-1: nodes.with.membership buffer & join')
    
    nodes.with.membership <- tryCatch(expr = {st_join(node.buffer, chunk, 
                                                      join = st_intersects) %>% 
        filter(!is.na(var)) %>%
        arrange(var) %>%
        select(node_ID, var, x, y, length, popden, empden)},
        
        error = function(e){
          st_join(node.buffer, chunk, 
                  join = st_intersects) %>% 
            filter(!is.na(var)) %>%
            arrange(var) %>%
            select(node_ID, var, x, y, length, popden, empden)}
    )
    
    nodes.with.membership <- left_join(NodeData %>% select(node_ID), 
                                       nodes.with.membership %>% st_set_geometry(NULL), by = 'node_ID') %>% filter(!is.na(var))
    toc()
    
    tic('5-2: gravity calculation')
    # Calculate the gravities between nodes
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
    
    # Pick the pair of chuncks to connect
    node.chunk1.new <- which(gravity.new == max(gravity.new, na.rm = T), arr.ind = T)[1,1]
    node.chunk2.new <- which(gravity.new == max(gravity.new, na.rm = T), arr.ind = T)[1,2] 
    
    chunk.pair.new <- nodes.with.membership[c(node.chunk1.new,node.chunk2.new),]$var
    
    nodes.in.chunk1 <- nodes.with.membership[nodes.with.membership$var == chunk.pair.new[1], ]
    nodes.in.chunk2 <- nodes.with.membership[nodes.with.membership$var == chunk.pair.new[2], ]
    toc()
    
    print(paste('Iteration_',iteration,' finished',sep=''))
    
    iteration = iteration + 1
    
    save(EdgeData, NodeData, node.buffer, dataset,
         chunk, chunk.before, chunk.connect, connect.chunk.num, 
         nodes.in.chunk1, nodes.in.chunk2, chunk.non.use,
         iteration, runtime.iteration, 
         pair.count, chosen.route.info, route.score.info, 
         spots.edge.id, spots.edge, EdgeData.nearest.to.subway.stations,
         file = './iteration_result/iteration_cont.RData')
  }
}

save(chunk, file = './iteration_result/consolidated_chunk.RData')
save.image(file = './iteration_result/iteration_finished.RData')
#load('iteration_finished.RData')

load('./iteration_result/spots_edge_id.RData')

save(spots.edge.id, iteration, runtime.iteration, 
     pair.count, chosen.route.info, route.score.info, 
     chunk.non.use, node.buffer, 
     file = './iteration_result/data_for_expanding.RData')



# #################### Post iteration ####################
# 
# # iteration result -> geojson
# tic('merge')
# 
# load('./iteration_result/chosen_route_1.RData')
# chosen.route.all <- chosen_route
# i = 2
# 
# while (i <= iteration) {
#   load(paste('./iteration_result/chosen_route_',i,'.RData', sep=''))
#   chosen.route.all <- rbind(chosen.route.all, chosen_route)
#   i = i + 1
# }
# 
# toc()
# 
# chosen.route.all  <- chosen.route.all %>% mutate(timestamp = paste(as.character(iter),'-01-01 00:00', sep=''))
# chosen.route.all %>% st_transform(4326) %>% sf_geojson() %>% geojson_write(file = './iteration_result/chosen_route.geojson')
# 
# 
# #################### Intra Network Missing Link - Additional Connecting Process ####################

rm(list = setdiff(ls(), c('chunk', 'dataset', 'EdgeData', 'NodeData')))

# 1. Data Preparation and dead-end check---------------------------------------------------------------------
# 1-1. Data preparation ---------------------------------------------------
# Extract nodes and edges from the network whose iteration process is finished
tic()
network <- chunk

nodes.from.network <- st_join(st_buffer(NodeData, 0.1), network, join = st_intersects)[,c('node_ID','var')] %>% 
  st_set_geometry(NULL) %>% 
  left_join(NodeData, ., by = 'node_ID') %>% 
  .[is.na(.$var) == F,]

centroid.EdgeData <- st_line_sample(EdgeData, sample = c(0.5)) %>% 
  st_cast('POINT') %>% 
  st_sf() %>% 
  mutate(edge_ID = EdgeData$edge_ID) %>% 
  st_buffer(0.001)

edges.from.network <- st_join(centroid.EdgeData, network, join = st_intersects)[, c('edge_ID','var')] %>% 
  st_set_geometry(NULL) %>% 
  left_join(EdgeData, ., by = 'edge_ID') %>% 
  .[is.na(.$var) == F,]

rm(centroid.EdgeData)
toc()

save.image('./missing_link_result/intra_network_missing_link1.RData')


# join the extracted nodes and edges
edges.nodes <- st_join(st_buffer(nodes.from.network, 0.1), edges.from.network, join = st_intersects)[,c('node_ID','edge_ID')] %>% 
  st_set_geometry(NULL) %>% 
  left_join(nodes.from.network, ., by = 'node_ID') %>% 
  .[, c('edge_ID','node_ID','var')]

edge.num.in.nodes <- edges.nodes %>% 
  st_set_geometry(NULL) %>% 
  group_by(node_ID) %>% 
  summarize(edge_num = n())


# 1-2. Dead-end check -----------------------------------------------------
nodes.from.network.1 <- edges.nodes[edges.nodes$node_ID %in% edge.num.in.nodes$node_ID[edge.num.in.nodes$edge_num == 1], ]

nodes.from.network.dead <- st_join(st_buffer(nodes.from.network.1, 0.1), 
                                   EdgeData[EdgeData$Code_Bike == 0,],
                                   join = st_intersects)[, 'node_ID'] %>% 
  st_set_geometry(NULL) %>% 
  unique() %>% 
  mutate(check = 1) %>% 
  left_join(nodes.from.network.1, ., by = 'node_ID') %>% 
  .[is.na(.$check) == F,] %>% 
  select(-var, -check)


# 1-3. Diversion check ------------------------------------------------------
# calculate edges' slope
nodes.from.network.2 <- edges.nodes[edges.nodes$node_ID %in% 
                                      edge.num.in.nodes$node_ID[edge.num.in.nodes$edge_num == 2], ]

coord.temp <- st_coordinates(EdgeData) %>% as.data.frame()
edge.temp <- EdgeData %>% st_set_geometry(NULL) %>% select(edge_ID)
edge.temp$x1 <- NA
edge.temp$x2 <- NA
edge.temp$y1 <- NA
edge.temp$y2 <- NA

pb <- txtProgressBar(min = 0, max = length(unique(coord.temp$L1)), style = 3)
for (i in unique(coord.temp$L1)) {
  setTxtProgressBar(pb, i)
  edge.temp$x1[i] <- coord.temp$X[coord.temp$L1 == i][1]
  edge.temp$y1[i] <- coord.temp$Y[coord.temp$L1 == i][1]
  edge.temp$x2[i] <- coord.temp$X[coord.temp$L1 == i][sum(coord.temp$L1 == i)]
  edge.temp$y2[i] <- coord.temp$Y[coord.temp$L1 == i][sum(coord.temp$L1 == i)]
}

nodes.from.network.2 <- left_join(nodes.from.network.2, edge.temp, by = 'edge_ID')
data <- nodes.from.network.2

nodes.from.network.2$edge.slope <- atan((data$y2-data$y1)/(data$x2-data$x1))*180/pi

for (i in 1:nrow(nodes.from.network.2)) {
  if (nodes.from.network.2$x1[i] < nodes.from.network.2$x2[i] & nodes.from.network.2$y1[i] > nodes.from.network.2$y2[i]) {
    nodes.from.network.2$edge.slope[i] <- nodes.from.network.2$edge.slope[i] + 360
  } else if (nodes.from.network.2$x1[i] > nodes.from.network.2$x2[i]) {
    nodes.from.network.2$edge.slope[i] <- nodes.from.network.2$edge.slope[i] + 180
  }
}

for (i in unique(nodes.from.network.2$node_ID)) {
  if (round(nodes.from.network.2$x1[nodes.from.network.2$node_ID==i][1],2) == round(nodes.from.network.2$x2[nodes.from.network.2$node_ID==i][2],2) &
      round(nodes.from.network.2$y1[nodes.from.network.2$node_ID==i][1],2) == round(nodes.from.network.2$y2[nodes.from.network.2$node_ID==i][2],2)) {
    nodes.from.network.2$edge.slope[nodes.from.network.2$node_ID==i][2] <- nodes.from.network.2$edge.slope[nodes.from.network.2$node_ID==i][2] - 180
  } else if (round(nodes.from.network.2$x2[nodes.from.network.2$node_ID==i][1],2) == round(nodes.from.network.2$x1[nodes.from.network.2$node_ID==i][2],2) &
             round(nodes.from.network.2$y2[nodes.from.network.2$node_ID==i][1],2) == round(nodes.from.network.2$y1[nodes.from.network.2$node_ID==i][2],2)) {
    nodes.from.network.2$edge.slope[nodes.from.network.2$node_ID==i][1] <- nodes.from.network.2$edge.slope[nodes.from.network.2$node_ID==i][1] - 180
  } else if (round(nodes.from.network.2$x2[nodes.from.network.2$node_ID==i][1],2) == round(nodes.from.network.2$x2[nodes.from.network.2$node_ID==i][2],2) &
             round(nodes.from.network.2$y2[nodes.from.network.2$node_ID==i][1],2) == round(nodes.from.network.2$y2[nodes.from.network.2$node_ID==i][2],2)) {
    nodes.from.network.2$edge.slope[nodes.from.network.2$node_ID==i][1] <- nodes.from.network.2$edge.slope[nodes.from.network.2$node_ID==i][1] - 180
    nodes.from.network.2$edge.slope[nodes.from.network.2$node_ID==i][2] <- nodes.from.network.2$edge.slope[nodes.from.network.2$node_ID==i][2] - 180
  }
}

nodes.from.network.2 <- nodes.from.network.2 %>% 
  filter(!is.na(edge.slope))
nodes.from.network.2$edge.slope[nodes.from.network.2$edge.slope < 0] <- nodes.from.network.2$edge.slope[nodes.from.network.2$edge.slope < 0] + 360



# check diversion
divert.check <- data.frame(node_ID = unique(nodes.from.network.2$node_ID))
divert.check$angle.at.node <- NA

for (j in 1:nrow(divert.check)) {
  a <- nodes.from.network.2$edge.slope[nodes.from.network.2$node_ID == divert.check$node_ID[j]][1]
  b <- nodes.from.network.2$edge.slope[nodes.from.network.2$node_ID == divert.check$node_ID[j]][2]
  divert.check$angle.at.node[j] <- abs(a - b)
}

divert.check$divert <- 0
divert.check$divert[divert.check$angle.at.node > 240 | divert.check$angle.at.node < 120] <- 1

nodes.from.network.divert <- nodes.from.network.2 %>% select(edge_ID, node_ID) %>% 
  .[.$node_ID %in% divert.check$node_ID[divert.check$divert == 1], ]

rm(coord.temp, edge.temp, data, pb, a, b, i, j)

nodes.from.network.divert <- st_join(st_buffer(nodes.from.network.divert, 0.1), 
                                     EdgeData[EdgeData$Code_Bike == 0,],
                                     join = st_intersects) %>% 
  filter(is.na(Code_Bike) == F) %>% 
  select(node_ID) %>% 
  st_set_geometry(NULL) %>% 
  unique() %>%
  mutate(check = 1) %>% 
  left_join(nodes.from.network.divert, ., by = 'node_ID') %>% 
  filter(is.na(check) == F)



# 2. Node pairs filtering process-----------------------------------------------------------
# merge dead-end nodes and diversion nodes
nodes.from.network.selected <- rbind(nodes.from.network.dead %>% select(node_ID), 
                                     nodes.from.network.divert %>% select(node_ID)) %>% unique()


# 2-1. Filter pairs that are within 1,000 meters --------------------------
nodes.dist.matrix <- st_distance(nodes.from.network.selected)
for (i in 1:length(nodes.dist.matrix[,1])) {
  for (j in 1:length(nodes.dist.matrix[,1])) {
    if (i > j) {
      nodes.dist.matrix[i,j] = NA
    }
  }
}

save.image('./missing_link_result/intra_network_missing_link2.RData')


# load('intra_network_missing_link2.RData')

index1 <- which(nodes.dist.matrix < set_units(1000, 'm'), arr.ind = T)[,1]
index2 <- which(nodes.dist.matrix < set_units(1000, 'm'), arr.ind = T)[,2]

nodes.from.network.selected.pair <- cbind(nodes.from.network.selected[index1, ] %>% 
                                            select(node_ID.1 = node_ID) %>% st_set_geometry(NULL), 
                                          nodes.from.network.selected[index2, ] %>% 
                                            select(node_ID.2 = node_ID) %>% st_set_geometry(NULL))

nodes.from.network.selected.pair <- nodes.from.network.selected.pair %>% filter(node_ID.1 != node_ID.2)
rm(i, j, index1, index2)



# 2-2.  Delete node pairs whose network distance is shorter than 3 times of the Euclidean distance --------------------
# Create A-star dataset for intra-network path finding
dataset$uniq.id <- 1:nrow(dataset)

centroid.dataset <- st_line_sample(dataset, sample = c(0.5)) %>% 
  st_cast('POINT') %>% 
  st_sf() %>% 
  mutate(uniq.id = dataset$uniq.id) %>% 
  st_buffer(0.001)

tic()
dataset.intra.network <- st_join(centroid.dataset, network, join = st_intersects)[, c('uniq.id','var')] %>% 
  st_set_geometry(NULL) %>% 
  left_join(dataset, ., by = 'uniq.id') %>% .[is.na(.$var) == F,]
toc()

rm(centroid.dataset)


# A-star functions
is_goal_reached <- function(src, dst)
  src == dst

neighbors <- function(node)
  as.character(dataset.intra.network$to_ID[dataset.intra.network$from_ID == node])

euclidean <- function(point1_x, point1_y, point2_x, point2_y)
  (((point1_x-point2_x)^2+(point1_y-point2_y)^2)^(1/2))

heuristic <- function(node, goal)
  euclidean(unique(dataset$from_x[dataset$from_ID == node]),unique(dataset$from_y[dataset$from_ID == node]),
            unique(dataset$from_x[dataset$to_ID == goal]),unique(dataset$from_y[dataset$to_ID == goal]))[1]*0.2

cost <- function(src, dst)
  dataset.intra.network$length[dataset.intra.network$from_ID == src & dataset.intra.network$to_ID == dst][1]


# A-star operation
tic()
result.node <- foreach (from_ID = nodes.from.network.selected.pair$node_ID.1, 
                        to_ID = nodes.from.network.selected.pair$node_ID.2,
                        .combine = rbind,
                        .packages= c("astar","R.utils")) %dopar% {
                          
                          withTimeout({
                            tryCatch(expr = {
                              temp <- astar(from_ID, to_ID, heuristic, cost, neighbors, is_goal_reached)
                              if (is.null(temp) == F) {
                                cbind(as.data.frame(as.matrix(temp)), 'from_ID' = from_ID, 'to_ID' = to_ID)
                              } else {
                                data.frame('V1' = NA, 'from_ID' = from_ID, 'to_ID' = to_ID)
                              }
                            },
                            error = function(e){
                              data.frame('V1' = NA, 'from_ID' = from_ID, 'to_ID' = to_ID)
                            })}, 
                            timeout = 150, onTimeout = 'silent')
                        }
toc()

# summarize the A-star result 1: node to edge
result.edge <- data.frame(from_ID = numeric(0), to_ID = numeric(0), edge_ID = numeric(0))

for (n in 1:(nrow(nodes.from.network.selected.pair))) {
  temp <- result.node[result.node$from_ID == nodes.from.network.selected.pair$node_ID.1[n] &
                        result.node$to_ID == nodes.from.network.selected.pair$node_ID.2[n],]
  if (nrow(temp) > 1) {
    for (j in 1:(nrow(temp)-1)) {
      edge.temp <- dataset.intra.network$edge_ID[dataset.intra.network$to_ID == as.integer(temp$V1[j+1]) &
                                                   dataset.intra.network$from_ID == as.integer(temp$V1[j])]
      result.temp <- data.frame(from_ID = temp$from_ID[1], to_ID = temp$to_ID[1], edge_ID = edge.temp)
      result.edge <- rbind(result.edge, result.temp)
    }
  } else {
    result.temp <- data.frame(from_ID = temp$from_ID[1], to_ID = temp$to_ID[1], edge_ID = NA)
    result.edge <- rbind(result.edge, result.temp)
  }
}

# join the 'edge length' column
result.edge <- left_join(result.edge, dataset.intra.network %>%
                           st_set_geometry(NULL) %>% 
                           select(edge_ID, length) %>% 
                           unique(), by = 'edge_ID')

# summarize the A-star result 2: edge to route
result.route <- group_by(result.edge, from_ID, to_ID) %>% 
  summarize(network.distance = sum(length)) %>% 
  as.data.frame() %>% 
  filter(is.na(from_ID) == F)

# calculate euclidean distance
euclidean_apply <- function(x) {
  node <- x[1]
  goal <- x[2]
  a <- unique(NodeData$x[NodeData$node_ID == node])
  b <- unique(NodeData$y[NodeData$node_ID == node])
  c <- unique(NodeData$x[NodeData$node_ID == goal])
  d <- unique(NodeData$y[NodeData$node_ID == goal])
  (((a-c)^2+(b-d)^2)^(1/2))
}
result.route$euclidean.distance <- apply(result.route, 1, euclidean_apply)

# Delete node pairs whose network distance is shorter than 3 times of the Euclidean distance
result.route$ratio <- result.route$network.distance/result.route$euclidean.distance
result.route.filter <- result.route %>% filter(result.route$ratio > 3 | is.na(result.route$ratio))
result.route.filter <- arrange(result.route.filter, euclidean.distance)

save.image('./missing_link_result/intra_network_missing_link3.RData')

# 2-3. Calculate new route and compare the distance -----------------------

# add 'weighted length' columns in dataset
dataset <- dataset %>% 
  mutate(w.length = case_when(
    Code_Bike == 1 ~ weighted_length * 0.5,
    TRUE ~ weighted_length
  ))


# A-star functions that need changes
neighbors <- function(node)
  as.character(dataset$to_ID[dataset$from_ID == node])

cost <- function(src, dst)
  dataset$w.length[dataset$from_ID == src & dataset$to_ID == dst][1]

# A-star operation
result.edge.new <- data.frame(from_ID = numeric(0), to_ID = numeric(0), edge_ID = numeric(0), length = numeric(0))
result.route.new <- data.frame(from_ID = numeric(0), to_ID = numeric(0), network.distance = numeric(0), euclidean.distance = numeric(0),
                               ratio = numeric(0), network.distance.new = numeric(0), ratio.new = numeric(0))

pb <- txtProgressBar(min = 0, max = nrow(result.route.filter), style = 3)

tic()
for (i in 1:nrow(result.route.filter)) {
  setTxtProgressBar(pb, i)
  from_ID <- result.route.filter$from_ID[i]
  to_ID <- result.route.filter$to_ID[i]
  
  # A-star
  result.node.temp <- withTimeout({
    tryCatch(expr = {
      temp <- astar(from_ID, to_ID, heuristic, cost, neighbors, is_goal_reached)
      if (is.null(temp) == F) {
        cbind(as.data.frame(as.matrix(temp)), 'from_ID' = from_ID, 'to_ID' = to_ID)
      } else {
        data.frame('V1' = NA, 'from_ID' = from_ID, 'to_ID' = to_ID)
      }
    },
    error = function(e){
      data.frame('V1' = NA, 'from_ID' = from_ID, 'to_ID' = to_ID)
    })}, 
    timeout = 150, onTimeout = 'silent')
  
  # node to edge
  if (nrow(result.node.temp) > 1) {
    result.edge.temp <- data.frame(from_ID = numeric(0), to_ID = numeric(0), edge_ID = numeric(0), length = numeric(0))
    for (j in 1:(nrow(result.node.temp)-1)) {
      edge.temp <- dataset$edge_ID[dataset$to_ID == as.integer(result.node.temp$V1[j+1]) &
                                          dataset$from_ID == as.integer(result.node.temp$V1[j])]
      length.temp <- dataset$length[dataset$to_ID == as.integer(result.node.temp$V1[j+1]) &
                                           dataset$from_ID == as.integer(result.node.temp$V1[j])]
      result.temp <- data.frame(from_ID = from_ID, to_ID = to_ID, edge_ID = edge.temp, length = length.temp)
      result.edge.temp <- rbind(result.edge.temp, result.temp)
    }
    
    # edge to route
    result.route.temp <- result.route.filter[i,]
    result.route.temp$network.distance.new <- sum(result.edge.temp$length)
    result.route.temp$ratio.new <- result.route.temp$network.distance.new/result.route.temp$euclidean.distance
    
    if (result.route.temp$ratio.new < 1.4) {
      dataset$Code_Bike[dataset$edge_ID %in% result.edge.temp$edge_ID] <- 1
      dataset$w.length[dataset$Code_Bike == 1] <- dataset$weighted_length[dataset$Code_Bike == 1] * 0.5
      EdgeData$Code_Bike[EdgeData$edge_ID %in% result.edge.temp$edge_ID] <- 1
      result.edge.new <- rbind(result.edge.new, result.edge.temp)
      result.route.new <- rbind(result.route.new, result.route.temp)
    }
  }
}
toc()

save.image(file = './missing_link_result/intra_chunk_finished.RData')

#################################################################################################
#################################################################################################
#################### additional connection to underserved neighborhoods #########################
#################################################################################################
#################################################################################################

load('iteration_result/data_for_expanding.RData')

source('../../0_chunk_generating_functions.R')

########## 0. make the additional spots as chunk
# change spots.edges' code_bike to 1
EdgeData$Code_Bike[EdgeData$edge_ID %in% spots.edge.id] <- 1

spots.edge <- EdgeData[EdgeData$edge_ID %in% spots.edge.id, 
                       c('popden', 'empden', 'length')] %>% 
  mutate(var = 10001:(10000+nrow(.)))

link.edge <- EdgeData[EdgeData$edge_ID %in% result.edge.new$edge_ID, 
                      c('popden', 'empden', 'length')] %>% 
  mutate(var = 20001:(20000+nrow(.)))

# (again) Distinguish chunk in use and chunk not in use (in case the spots are isolated)
filter.index.chunk.isolated <- apply(st_intersects(st_buffer(spots.edge, 0.001), EdgeData[EdgeData$Code_Drive == 1, ]), 1, sum) == 0
chunk.non.use <- rbind(chunk.non.use, spots.edge[filter.index.chunk.isolated, ])
spots.edge <- spots.edge[!filter.index.chunk.isolated, ]

# merge the chunk and missing link connections
chunk.new <- rbind(chunk, link.edge) %>% 
  mutate(popden.length = popden * length, empden.length = empden * length)

chunk.new.multi <- chunk %>% 
  select() %>% 
  st_cast('LINESTRING') %>% 
  rbind(., link.edge %>% select()) %>% 
  group_by() %>% 
  summarize() %>% 
  st_cast('MULTILINESTRING')

chunk.new.multi$length <- sum(chunk.new$length)
chunk.new.multi$popden <- sum(chunk.new$popden.length)/sum(chunk.new$length)
chunk.new.multi$empden <- sum(chunk.new$empden.length)/sum(chunk.new$length)

chunk <- chunk.new.multi %>% 
  mutate(var = 1:nrow(.))

rm(chunk.new, chunk.new.multi)

# (again) Create chunk (in case the spots are already connected to the main chunk)
chunk <- rbind(chunk, spots.edge) %>% 
  fun_sweeper(.) %>% 
  mutate(var = 1:nrow(.))



iteration = iteration + 10001

while (length(unique(chunk$var)) > 1) {
  ############ 1. select the nearest spots from the main chunk
  tic(paste('######## 1. nearest spots selection_',iteration,' (',format(Sys.time(), "%X"),')',sep=""))
  # create nodes with chunk membership
  nodes.with.membership <- tryCatch(expr = {st_join(node.buffer, chunk, 
                                                    join = st_intersects) %>% 
      filter(!is.na(var)) %>%
      arrange(var) %>%
      select(node_ID, var, x, y, length, popden, empden)},
      
      error = function(e){
        st_join(node.buffer, chunk, 
                join = st_intersects) %>% 
          filter(!is.na(var)) %>%
          arrange(var) %>%
          select(node_ID, var, x, y, length, popden, empden)}
  )
  
  nodes.with.membership <- left_join(NodeData %>% select(node_ID), 
                                     nodes.with.membership %>% st_set_geometry(NULL), by = 'node_ID') %>% filter(!is.na(var))
  
  # compare the gravity between chunks (the main chunk and the nearest spot will be chosen)
  dist.matrix <- st_distance(nodes.with.membership)
  
  for (i in unique(nodes.with.membership$var)) {
    block.box <- row.names(nodes.with.membership[nodes.with.membership$var == i,]) %>% as.numeric()
    dist.matrix[block.box, block.box]<- NA
  }
  
  numerator <- rep(nodes.with.membership$length, nrow(nodes.with.membership)) %>% 
    matrix(.,nrow(nodes.with.membership))
  numerator <- t(numerator) * numerator
  
  gravity <- numerator / (dist.matrix/1000)^2
  
  # Pick the pair of chuncks to connect
  node.chunk1.new <- which(gravity == max(gravity, na.rm = T), arr.ind = T)[1,1]
  node.chunk2.new <- which(gravity == max(gravity, na.rm = T), arr.ind = T)[1,2] 
  
  chunk.pair.new <- nodes.with.membership[c(node.chunk1.new,node.chunk2.new),]$var
  
  nodes.in.chunk1 <- nodes.with.membership[nodes.with.membership$var == chunk.pair.new[1], ]
  nodes.in.chunk2 <- nodes.with.membership[nodes.with.membership$var == chunk.pair.new[2], ]
  toc()
  
  
  ######### 2. select node pairs from the chunk pair
  tic(paste('######## 2. select node pairs_',iteration,' (',format(Sys.time(), "%X"),')',sep=""))
  # exclude nodes that cannot be an origin or a destination
  nodes.in.chunk1 <- nodes.in.chunk1 %>% 
    .[.$node_ID %in% dataset$from_ID, ] %>% 
    select(-popden, -empden)
  nodes.in.chunk2 <- nodes.in.chunk2 %>% 
    .[.$node_ID %in% dataset$from_ID, ] %>% 
    select(-popden, -empden)
  
  # select nodes that are connected to potential bike edges
  nodes.in.chunk1.buffer <- nodes.in.chunk1 %>% 
    st_buffer(dist = 0.1) %>% 
    select(node_ID)
  nodes.in.chunk1.buffer.join <- st_join(nodes.in.chunk1.buffer, dataset[dataset$Code_Bike == 0,], join = st_intersects) %>%
    st_set_geometry(NULL)
  
  nodes.in.chunk1.filter <- nodes.in.chunk1 %>% 
    left_join(nodes.in.chunk1.buffer.join %>%
                filter(is.na(edge_ID) == F) %>%
                .$node_ID %>%
                unique() %>%
                as.data.frame() %>%
                rename(node_ID = colnames(.)) %>%
                mutate(check = 1),
              by = 'node_ID') %>%
    filter(check == 1) %>% 
    select(-check)
  
  nodes.in.chunk2.buffer <- nodes.in.chunk2 %>% 
    st_buffer(dist = 0.1) %>% 
    select(node_ID)
  nodes.in.chunk2.buffer.join <- st_join(nodes.in.chunk2.buffer, dataset[dataset$Code_Bike == 0,], join = st_intersects) %>%
    st_set_geometry(NULL)
  
  nodes.in.chunk2.filter <- nodes.in.chunk2 %>% 
    left_join(nodes.in.chunk2.buffer.join %>%
                filter(is.na(edge_ID) == F) %>%
                .$node_ID %>%
                unique() %>%
                as.data.frame() %>%
                rename(node_ID = colnames(.)) %>%
                mutate(check = 1),
              by = 'node_ID') %>%
    filter(check == 1) %>% 
    select(-check)
  
  rm(nodes.in.chunk1.buffer, nodes.in.chunk1.buffer.join, nodes.in.chunk2.buffer, nodes.in.chunk2.buffer.join)
  
  dist_matrix <- st_distance(nodes.in.chunk1.filter, nodes.in.chunk2.filter)
  
  index.for.nodes.in.chunk1 <- which(dist_matrix < min(dist_matrix)*2, arr.ind = T)[,1]
  index.for.nodes.in.chunk2 <- which(dist_matrix < min(dist_matrix)*2, arr.ind = T)[,2]
  
  selected.pair.of.nodes <- cbind(nodes.in.chunk1.filter[index.for.nodes.in.chunk1, ] %>% st_set_geometry(NULL), 
                                  nodes.in.chunk2.filter[index.for.nodes.in.chunk2, ] %>% st_set_geometry(NULL))
  
  colnames(selected.pair.of.nodes) <- c("from_ID", "from_var", "from_x", "from_y", "from_length", "to_ID", "to_var", "to_x", "to_y", "to_length")
  
  pair.count <- pair.count %>% 
    add_row(iteration = iteration, pair.count = nrow(selected.pair.of.nodes), 
            chunk1.node = length(unique(index.for.nodes.in.chunk1)), chunk2.node = length(unique(index.for.nodes.in.chunk2)))
  
  if (nrow(selected.pair.of.nodes) > 500) {
    selected.pair.of.nodes <- selected.pair.of.nodes[sample(1:nrow(selected.pair.of.nodes), 500), ]
  }
  toc()
  
  ########## 3. Astar Algorithm ##############
  tic(paste('######## 3. A-star_',iteration,' (',format(Sys.time(), "%X"),')',sep=''))
  
  result <- data.frame(chunk1 = numeric(0), chunk2 = numeric(0), edge_ID = numeric(0), score = numeric(0), codebike = numeric(0), length = numeric(0))
  rm(result_node)
  
  result_node <- foreach (from_ID=selected.pair.of.nodes$from_ID, 
                          to_ID=selected.pair.of.nodes$to_ID,
                          .combine = rbind,
                          .packages="astar") %dopar% {
                            tryCatch(expr = {
                              cbind(as.data.frame(as.matrix(astar(from_ID, to_ID, heuristic, cost, neighbors, is_goal_reached))), from_ID, to_ID)
                            },
                            error = function(e){
                              data.frame(V1 = numeric(0), from_ID = numeric(0), to_ID = numeric(0))
                            })
                          }
  
  result_node <- left_join(result_node, group_by(result_node, from_ID, to_ID) %>% summarize() %>% as.data.frame() %>% mutate(id = 1:nrow(.)), by = c('from_ID','to_ID'))
  toc <- toc()
  runtime.iteration <- runtime.iteration %>% add_row(iteration = iteration, elapsed = toc$toc - toc$tic)
  
  save(result_node, file = paste('./iteration_result/result_node/result_node_',iteration,'.RData', sep=''))
  toc()
  
  ######### 4. summarizing A-star result
  tic(paste('######## 4. A-star summarizing_',iteration,' (',format(Sys.time(), "%X"),')',sep=''))
  # node result -> edge result (version 2.)
  result_node$V1 <- as.numeric(result_node$V1)
  result_node$id.2 <- 1:nrow(result_node)
  
  edge_temp <- result_node[,c('from_ID','to_ID','id','id.2','V1')] %>% 
    mutate(V2 = c(result_node$V1[2:nrow(result_node)],NA))
  
  last <- edge_temp %>% 
    group_by(id) %>% 
    summarize(id.2 = last(id.2))
  
  edge_temp <- edge_temp %>% 
    filter(not(id.2 %in% last$id.2))
  
  NodeToEdge <- function(node) {
    dataset[dataset$from_ID == node[5] & 
              dataset$to_ID == node[6], 
            c('edge_ID','score','length','Code_Bike')] %>% 
      .[1, ] %>%
      st_set_geometry(NULL)
  }
  
  result <- apply(edge_temp, 1, NodeToEdge)
  result <- cbind(edge_temp[,c(1,2)],
                  data.frame(matrix(unlist(result), nrow = length(result), byrow = T)))
  
  colnames(result) <- c('chunk1','chunk2','edge_ID','score','length','codebike')
  rm(last, edge_temp, NodeToEdge)
  
  
  # calculate avg_score (weighted by edge length) and add an index of each route (id)
  result$score_times_length <- result$score * result$length
  result_summary <- result %>% filter(codebike == 0) %>% group_by(chunk1, chunk2) %>% 
    summarize(sum_score_times_length = sum(score_times_length), length_sum = sum(length))
  result_summary$avg_score <- result_summary$sum_score_times_length / result_summary$length_sum
  result_summary$id <-  1:nrow(result_summary)
  result <- left_join(result, result_summary, by = c('chunk1', 'chunk2'))
  
  # select the best route
  if (min(result_summary$length_sum) < 200){
    best_route <- result_summary$id[result_summary$length_sum == min(result_summary$length_sum)]
    if (length(best_route) > 1) {
      best_route <- best_route[1]
    }
  } else {
    best_route <- result_summary$id[result_summary$avg_score == max(result_summary$avg_score)]
    if (length(best_route) > 1) {
      best_route <- result_summary[result_summary$id %in% best_route,] %>% .[.$length_sum == min(.$length_sum),] %>% .$id
    }
    if (length(best_route) > 1) {
      best_route <- best_route[1]
    }
  }
  
  best_route_edge <- result$edge_ID[result$id == best_route]
  
  # save the route as shp file and change them to 'bikeable' in the dataset (for the next iteration)
  chosen_route <- dataset[dataset$edge_ID %in% best_route_edge, ] %>% 
    .[.$Code_Bike == 0, ] %>% 
    mutate(iter = iteration + 2000)
  save(chosen_route, file = paste('./iteration_result/chosen_route_',iteration,'.RData',sep=''))
  rm(chosen_route)
  
  chosen.route.info.temp <- dataset[dataset$edge_ID %in% best_route_edge, ] %>% 
    st_set_geometry(NULL) %>% 
    select(edge_ID, length) %>% 
    mutate(iteration = iteration)
  chosen.route.info <- rbind(chosen.route.info, chosen.route.info.temp)
  
  dataset$Code_Bike[dataset$edge_ID %in% best_route_edge] <- 1
  EdgeData$Code_Bike[EdgeData$edge_ID %in% best_route_edge] <- 1
  toc()
  
  
  ############## 5. create new chunk
  tic(paste('######## 5. Create new chunk_',iteration,' (',format(Sys.time(), "%X"),')',sep=''))
  new.bikable.edges <- EdgeData[EdgeData$edge_ID %in% best_route_edge, ]
  
  # make a chunk of bike network of new bikable edges
  new.bikable.path <- fun_sweeper(new.bikable.edges) %>% 
    mutate(var = 1:nrow(.)) %>% 
    select(length, popden, empden, var)
  
  chunk.before <- chunk
  
  connect.chunk.num <- st_join(chunk, 
                               st_buffer(new.bikable.path %>% select(), 0.1) %>% 
                                 mutate(check = 1),
                               join = st_intersects) %>% filter(check == 1)
  
  # (to fix the mysterious error...) attach the chunks into a multilinestring manually
  `%notin%` <- Negate(`%in%`)
  chunk.connect <- chunk.before[chunk.before$var %in% connect.chunk.num$var, ]
  chunk.other <- chunk.before[chunk.before$var %notin% connect.chunk.num$var, ]
  chunk.new <- rbind(chunk.connect, new.bikable.path) %>% 
    mutate(popden.length = popden * length, empden.length = empden * length)
  
  chunk.new.multi <- chunk.new %>% 
    select() %>% 
    st_cast('LINESTRING') %>% 
    group_by() %>% 
    summarize() %>% 
    st_cast('MULTILINESTRING')
  
  chunk.new.multi$length <- sum(chunk.new$length)
  chunk.new.multi$popden <- sum(chunk.new$popden.length)/sum(chunk.new$length)
  chunk.new.multi$empden <- sum(chunk.new$empden.length)/sum(chunk.new$length)
  chunk.new.multi$var <- 0
  
  chunk <- rbind(chunk.new.multi, chunk.other) %>% 
    mutate(var = 1:nrow(.))
  
  rm(chunk.new, chunk.new.multi, chunk.other)  
  toc()
  
  
  ######## finish or not?
  if (length(unique(chunk$var)) == 1) {
    print("IT'S A COMPLETE NETWORK WITH ADDITIONAL SPOTS !!!")
  } else {
    iteration = iteration + 1
    
    save(EdgeData, NodeData, node.buffer, dataset, chunk, nodes.in.chunk1, nodes.in.chunk2, iteration, runtime.iteration, pair.count, chosen.route.info, route.score.info, 
         file = './iteration_result/iteration_cont.RData')
  }
}



save(chunk, file = './iteration_result/consolidated_chunk_spots.RData')
save.image(file = './iteration_result/expanding_finished.RData')


write.csv(runtime.iteration, file = './iteration_result/runtime_log.csv', row.names = F)
write.csv(pair.count, file = './iteration_result/pair_count.csv', row.names = F)
write.csv(chosen.route.info, file = './iteration_result/chosen_route_info.csv', row.names = F)
write.csv(route.score.info, file = './iteration_result/route.score.info.csv', row.names = F)

stopCluster(cl)
