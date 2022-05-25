install.packages("devtools")
devtools::install_github("jamgreen/lehdr")
install.packages('tidycensus')
install.packages('wrswoR')
install.packages('R.utils')
install.packages('remotes')
remotes::install_github('machow/astar-r')

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
library(wrswoR)
library(astar)
library(doParallel)
library(foreach)
library(geojsonsf)
library(geojsonio)
library(R.utils)

setwd("#####")

# Load data
load('nodes_in_chunk1_and_chunk_2_city.RData')
EdgeData.before <- EdgeData
dataset.before <- dataset
rm(list = setdiff(ls(), c('NodeData', 'EdgeData.before', 'dataset.before')))

edges.plan <- read.csv(file = 'data/edges_planned.csv', header = T, sep = ',') %>% 
  .$edge_ID
EdgeData.plan <- EdgeData.before %>% 
  mutate(Code_Bike = case_when(
    edge_ID %in% edges.plan ~ 1,
    TRUE ~ Code_Bike
    )
  )
dataset.plan <- dataset.before %>% 
  mutate(Code_Bike = case_when(
    edge_ID %in% edges.plan ~ 1,
    TRUE ~ Code_Bike
    )
  )
rm(edges.plan)

load('sample_taz.RData')

###### DATA MANIPULATION FOR SIMULATION ######

# Change code_bike for 'subway stations' from 1 to 0 (in the original network)
dataset.before$Code_Bike[dataset.before$Code_Bike == 1 & is.na(dataset.before$bike.type.uj)] <- 0

# Assume that all new bike facilities are 'Protected bike lanes' 
dataset.plan$bike.type.uj[dataset.plan$Code_Bike == 1 & is.na(dataset.plan$bike.type.uj)] <- 'protected'

# Calculate 'New LTS' reduced from bike accommodations
dataset.before <- dataset.before %>% 
  mutate(lts.reduced = case_when(
    bike.type.uj == 'striped' ~ (lts - 1) * 0.5 + 1,
    bike.type.uj == 'buffered' ~ (lts - 1) * 0.35 + 1,
    bike.type.uj == 'protected' ~ (lts - 1) * 0.25 + 1,
    bike.type.uj == 'multi-use' ~ (lts - 1) * 0.25 + 1,
    TRUE ~ lts
    )
  )

dataset.plan <- dataset.plan %>% 
  mutate(lts.reduced = case_when(
    bike.type.uj == 'striped' ~ (lts - 1) * 0.5 + 1,
    bike.type.uj == 'buffered' ~ (lts - 1) * 0.35 + 1,
    bike.type.uj == 'protected' ~ (lts - 1) * 0.25 + 1,
    bike.type.uj == 'multi-use' ~ (lts - 1) * 0.25 + 1,
    TRUE ~ lts
    )
  )

# Calculate weigthed length for route findings for riders in simulation
dataset.before <- dataset.before %>% 
  mutate(bicycle.riding.stress = 1 + (grade.stress - 1) + (lts.reduced - 1))

dataset.plan <- dataset.plan %>% 
  mutate(bicycle.riding.stress = 1 + (grade.stress - 1) + (lts.reduced - 1))


###### SIMULATION BASED EVALUATION ######
# Adjusting Preference for Bike Infrastructure
dataset.before <- dataset.before %>% 
  mutate(weighted_length = bicycle.riding.stress * length)

dataset.plan <- dataset.plan %>% 
  mutate(weighted_length = bicycle.riding.stress * length)


# Define Evaluation function
source('0_evaluation_function.R')

# Implement evaluations
evaluation.6mile.before <- Evaluation(sample = taz.sample.6mile, dataset = dataset.before, output = 'before_6mile', EdgeData = EdgeData.before)
evaluation.3mile.before <- Evaluation(sample = taz.sample.3mile, dataset = dataset.before, output = 'before_3mile', EdgeData = EdgeData.before)
evaluation.6mile.plan <- Evaluation(sample = taz.sample.6mile, dataset = dataset.plan, output = 'plan_6mile', EdgeData = EdgeData.plan)
evaluation.3mile.plan <- Evaluation(sample = taz.sample.3mile, dataset = dataset.plan, output = 'plan_3mile', EdgeData = EdgeData.plan)

save.image(file = 'evaluation_before_plan_finished.RData')



