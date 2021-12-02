#### Advent of code: Day 2 ####


## Load data
df <- read.table("data/day2.txt")


#### Part 1 ####
## Data contains set of commands to move the submarine
# Set origin location
submarine <- data.frame(horizontal = 0,
                        depth = 0)


# Commands include forward (horizontal +), down (depth +), and up (depth -)
for(i in 1:nrow(df)) {
  if(df$V1[i] == "forward") {submarine$horizontal <- submarine$horizontal + df$V2[i]}
  if(df$V1[i] == "down") {submarine$depth <- submarine$depth + df$V2[i]}
  if(df$V1[i] == "up") {submarine$depth <- submarine$depth - df$V2[i]}
}


## Solution is horizontal * depth
submarine$horizontal * submarine$depth


#### Part 2 ####
## Submarine also has aim, set new origin
submarine <- data.frame(horizontal = 0,
                        depth = 0,
                        aim = 0)


## New commands:
# down X: depth + X AND aim + X
# up X: depth - X AND aim - X
# forward X: horizontal + X AND depth +  (X*aim)

for(i in 1:nrow(df)){
  if(df$V1[i] == "forward") {
    submarine$horizontal <- submarine$horizontal + df$V2[i];
    submarine$depth <- submarine$depth  + (df$V2[i] * submarine$aim) 
  }
  if(df$V1[i] == "down") {
    submarine$aim <- submarine$aim + df$V2[i]}
  if(df$V1[i] == "up") {
    submarine$aim <- submarine$aim - df$V2[i]}
}


## Solution is horizontal*depth
submarine$horizontal * submarine$depth


