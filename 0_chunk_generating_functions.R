## Chunk Generating Functions

# Inner function: Merging function that constantly loops until all touching lines are merged
fun_merger <- function(x, i){
    while(length(st_intersects(x)[[i]]) > 1){
        index.touching.lines <- st_intersects(x)[[i]]
        x$var[index.touching.lines] <- i
        x <- x %>%
            group_by(var) %>%
            summarise(popden = sum(popden*length)/sum(length),
                      empden = sum(empden*length)/sum(length),
                      length = sum(length))
    } 
    message(paste0("Merger loop for connected segment ", i, " is done: ", Sys.time()))
    return(x)
}

# Outer function: A sweeping function that feeds the next row to the merging function
fun_sweeper <- function(x){
    i <- 1
    x <- x %>% mutate(var = 1:nrow(x))
    while(max(apply(st_intersects(x), 1, sum)) > 1){
        x <- fun_merger(x, i)
        i <- i + 1
        x$var <- seq(1:nrow(x))
    }
    
    x <- st_cast(x, "MULTILINESTRING")
    
    return(x)
}