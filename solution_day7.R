#### Day 7 ####
library(tidyverse)

## Read in data
df <- as.integer(unlist(strsplit(readLines("data/day7.txt"), ",")))


#### Part 1 ####
# Find most common value (uses least fuel)
crab_pos <- median(df)

# Find fuel usage (difference between current position and common value)
sum(abs(crab_pos -  df))


#### Part 2 ####
# Cheapest value under new rules
crab_pos2 <- round(mean(df)-.1)


# Create df with differences from new position
pos_diff <- data.frame(abs(crab_pos2 - df))
names(pos_diff) <- "diff"

# Function to return triangle numbers
triangle_fn <- function(x){ tail(cumsum(seq(x)), 1) }

# Apply to each row
triangle_pos <- pos_diff %>% 
  rowwise() %>% 
  mutate(triangle =  triangle_fn(diff))

# Find total fuel usage
sum(triangle_pos$triangle)
