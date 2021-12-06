#### Day 6 ####
library(tidyverse)

# Read in data as vector
df <- scan("data/day6.txt", sep= ",")

# Tabulate return number of values (number of 0,1,...8)
df_tab <- tabulate(df,8)

# Add 0 
df_tab  <- c(0, df_tab)

for(i in 1:80){
  
  # Add number of 0 to 8
  new_fish <- df_tab[1]
  
  df_tab <-  df_tab[-1]
  df_tab[7] <-  df_tab[7] + new_fish
  df_tab  <- c(df_tab, new_fish)
  
}

sum(df_tab)

## Part 2
df_tab <- tabulate(df,8)

# Add 0 
df_tab  <- c(0, df_tab)

for(i in 1:256){
  
  # Add number of 0 to 8
  new_fish <- df_tab[1]
  
  df_tab <-  df_tab[-1]
  df_tab[7] <-  df_tab[7] + new_fish
  df_tab  <- c(df_tab, new_fish)
  
}

format(sum(df_tab), scientific = F)
         