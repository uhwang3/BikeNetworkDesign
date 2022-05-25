# Evaluation Function
Evaluation <- function(sample, dataset, output, EdgeData) {

    cost <- function(src, dst)
        dataset$weighted_length[dataset$from_ID == src & dataset$to_ID == dst][1]
    
    euclidean <- function(point1_x, point1_y, point2_x, point2_y)
        (((point1_x-point2_x)^2+(point1_y-point2_y)^2)^(1/2))
    
    heuristic <- function(node, goal)
        euclidean(unique(dataset$from_x[dataset$from_ID == node]),unique(dataset$from_y[dataset$from_ID == node]),
                  unique(dataset$from_x[dataset$to_ID == goal]),unique(dataset$from_y[dataset$to_ID == goal]))[1]*0.2
    
    neighbors <- function(node)
        as.character(dataset$to_ID[dataset$from_ID == node])
    
    is_goal_reached <- function(src, dst)
        src == dst
    
    
    # Parallelize
    cl <- makeCluster(detectCores() - 2)
    registerDoParallel(cl)
    
    # Route Finding
    tic()
    result.node <- foreach(i = 1:nrow(sample),              # sample
                           from_ID = sample$node.id.1,      # sample
                           to_ID = sample$node.id.2,        # sample
                           .combine = rbind,
                           .packages= c("astar", "R.utils")) %dopar% {
                               withTimeout({
                                   tryCatch(expr = {
                                       temp <- astar(from_ID, to_ID, heuristic, cost, neighbors, is_goal_reached)    # cost
                                       if (is.null(temp) == F) {
                                           cbind(as.data.frame(as.matrix(temp)), 'from_ID' = from_ID, 'to_ID' = to_ID)
                                       } else {
                                           data.frame('V1' = NA, 'from_ID' = from_ID, 'to_ID' = to_ID)
                                       }
                                   },
                                   error = function(e){
                                       data.frame('V1' = NA, 'from_ID' = from_ID, 'to_ID' = to_ID)
                                   })}, 
                                   timeout = 7200, onTimeout = 'silent')
                           }
    toc()
    
    # Stop Parallelizing
    stopCluster(cl)
    
    # Node Result Summary
    result.node <- left_join(result.node, 
                             group_by(result.node, from_ID, to_ID) %>% 
                                 summarize() %>% 
                                 as.data.frame() %>% 
                                 mutate(id = 1:nrow(.)), 
                             by = c('from_ID','to_ID'))
    
    save(result.node, file = paste0('evaluation_result/result_node_',output,'.RData'))
    
    # Edge Result Summary
    result.edge <- data.frame(id = numeric(0), edge_ID = numeric(0), brs = numeric(0), 
                              lts.reduced = numeric(0), Code_Bike = numeric(0), length = numeric(0))
    for (j in 1:max(result.node$id)) {
        if (nrow(result.node[result.node$id == j,]) != 1){
            for (k in 1:(nrow(result.node[result.node$id == j,])-1)) {
                edge.temp <- dataset[dataset$to_ID == as.integer(result.node[result.node$id == j,][k+1,'V1']) &       # dataset
                                         dataset$from_ID == as.integer(result.node[result.node$id == j,][k,'V1']),      # dataset
                                     c('edge_ID','bicycle.riding.stress','lts.reduced','Code_Bike','length')] %>% st_set_geometry(NULL)
                result.edge <- result.edge %>% add_row(id = j, edge_ID = edge.temp$edge_ID, brs = edge.temp$bicycle.riding.stress,
                                                       lts.reduced = edge.temp$lts.reduced, 
                                                       Code_Bike = edge.temp$Code_Bike, length = edge.temp$length)
            } 
        }
    }
    
    result.edge$brs.length <- result.edge$brs * result.edge$length
    result.edge$lts.reduced.length <- result.edge$lts.reduced * result.edge$length
    result.edge$bike.length <- result.edge$Code_Bike * result.edge$length
    
    # Route Result Summary
    result.route <- result.edge %>%
        group_by(id) %>% 
        summarize(brs.length = sum(brs.length),
                  lts.reduced.length = sum(lts.reduced.length),
                  bike.length = sum(bike.length),
                  length = sum(length))
    
    # export summary
    summary <- result.route %>% group_by() %>% summarize(avg.pct.bike = mean(bike.length/length),
                                                         avg.brs = mean(brs.length/length),
                                                         avg.lts = mean(lts.reduced.length/length),
                                                         length = sum(length),
                                                         brs.length = sum(brs.length)) %>% 
        mutate(total.bike.length = EdgeData %>% 
                   .[st_read('../../data/Shape/CityofAtlanta.shp') %>% st_transform(32616),] %>% 
                   filter(Code_Bike == 1) %>% 
                   st_length() %>% 
                   sum())
    
    write.csv(summary, file = paste0('evaluation_result/summary_',output,'.csv'), row.names = F)
    
    # Return Result
    result.list = list(result.node = result.node, result.edge = result.edge, result.route = result.route)
    return(result.list)
    
}





