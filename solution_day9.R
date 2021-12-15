library(tidyverse)
library(terra)

df_raw <- readLines("data/day9.txt")
df_vec <- as.numeric(unlist(strsplit(df_raw, "")))
df <- matrix(df_vec, byrow = T, nrow = length(df_raw))


## Find adjacent cells
# Shift cells to left
left_nb <- df - cbind(df[, -1], 10)

# To the right
right_nb <-  df - cbind(10, df[, -ncol(df)])

# Up
up_nb <- df - rbind(df[-1,], 10)

# Down
down_nb <- df - rbind(10, df[-nrow(df),])

sum(df[which(left_nb < 0 & right_nb  < 0 & up_nb < 0 & down_nb < 0)] + 1)

#### Part 2 ####
df_rast <- df %>% 
  rast() %>% 
  # Replace 9 with NA
  classify(., cbind(9, NA))

## Find basins
basin_size <- patches(df_rast, zeroAsNA = F) %>% 
  # Convert into a list of basin identifiers
  as.data.frame() %>% 
  # Add number cells per basin
  group_by(lyr.1) %>% 
  count() %>% 
  ungroup() %>% 
  slice_max(n, n = 3)
 
prod(basin_size$n)

  