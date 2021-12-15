library(terra)
library(tidyverse)

df <- read_lines("data/day11.txt")

df <- matrix(as.numeric(unlist(strsplit(df, ""))),
             byrow = T, nrow = 10)

flashed <- c()
flashing <- c()
adj_cells <- c()
n_flashed <- 0
df_i <- df
for(i in 1:100) {
  df_i <- df_i + 1
  
  flashed <- c()
  flashing <- which(df_i == 10)
  while(length(flashing) != 0) {
    for(j in flashing){
      adj_cells <- adjacent(rast(df_i), j, direction = "queen", pairs = T)[,2]
      
      df_i[adj_cells] <- df_i[adj_cells] + 1
      flashed <- c(flashed, j)
    }

  df_i[flashed] <- 0
  
  flashing <- which(df_i > 9)
  }
  n_flashed <- n_flashed + length(flashed)
}


## Part 2
flashed <- c()
flashing <- c()
adj_cells <- c()
n_flashed <- 0
df_i <- df
for(i in 1:1000) {
  df_i <- df_i + 1
  
  flashed <- c()
  flashing <- which(df_i == 10)
  while(length(flashing) != 0) {
    for(j in flashing){
      adj_cells <- adjacent(rast(df_i), j, direction = "queen", pairs = T)[,2]
      
      df_i[adj_cells] <- df_i[adj_cells] + 1
      flashed <- c(flashed, j)
    }
    
    df_i[flashed] <- 0
    
    flashing <- which(df_i > 9)
  }
  
  if(all(df_i == 0)) {
    print(paste0("Step ", i))
    break
  }
  
}
