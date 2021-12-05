#### Advent of code: Day 3 ####
library(tidyverse)

#### Load data ####
# Read one character per variable
df <- read.fwf("data/day3.txt", widths = rep(1, 12))

# Return number of rows for later
n.df <- nrow(df)


#### Part 1 ####
## Return most common value (median)
df_sum <- summarise(df, across(everything(), median)) 
  
# Combine values into a string then convert binary code to decimal
gamma <- strtoi(paste0(select(df_sum, everything()), collapse = ""), base = 2)

# Return least common value (1 - most common)
df_eps <- df_sum %>% 
  mutate(across(everything(), ~ 1 - .x))

# Combine valuues into string then convert binary into decimal
epsilon <-  strtoi(paste0(select(df_eps, everything()), collapse = ""), base = 2)
                    
# Multiply gamma + epsilon for result
gamma  * epsilon



#### Part 2 ####
## Oxygen generator rating
oxygen <- df


for(i in 1:ncol(oxygen)) {
  med_i <- oxygen %>% 
    summarise(across(i, median))
    
  bit <- ifelse(med_i[1,1] >= .5, 1, 0)
  
  oxygen <- oxygen %>% 
    filter(across(i, ~ .x == bit)) 
  
  if(nrow(oxygen) == 1) break
}

# Combine values into string then convert binary into decimal
oxygen_rating <-  strtoi(paste0(select(oxygen, everything()), collapse = ""), base = 2)



## C02 rating
co2 <- df


for(i in 1:ncol(co2)) {
  
  med_i <- co2 %>% 
    summarise(across(i, median))
  
  bit <- ifelse(med_i[1,1] >= .5, 0, 1)
  
  co2 <- co2 %>% 
    filter(across(i, ~ .x == bit)) 
  
  if(nrow(co2) == 1) break
}

# Combine values into string then convert binary into decimal
co2_rating <-  strtoi(paste0(select(co2, everything()), collapse = ""), base = 2)

#### Doesn't work, come back to this
oxygen_rating*co2_rating
