#### Advent of code: Day 1 ####

## Read data
df <- read.table("data/day1.txt")

## Part 1: Count number of increases
inc_count <- 0

for(i in 2:nrow(df)){
  inc_count <- inc_count + ifelse(df[i,1] > df[i - 1,1], 1, 0)
}


## Part 2: Sliding windows
window_inc <- 0

for(i in 2:nrow(df)){
  window1_sum  <- sum(df[i:(i+2), 1], na.rm = T)
  window2_sum  <- sum(df[(i-1):(i+1),1], na.rm = T)
  
  window_inc <- window_inc + ifelse(window1_sum > window2_sum,
                                    1, 0)
}




