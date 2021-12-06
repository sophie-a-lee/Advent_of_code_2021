#### Day 5 ####

## Load data
df <- read.table("data/day5.txt")

# Clean data  
day5_tidy <- df %>% 
  separate(V1, c("start_x", "start_y"), sep = ",") %>% 
  separate(V3, c("end_x", "end_y"), sep = ",") %>% 
  # Add 1 to match col/row numbers
  transmute(start_x = as.integer(start_x) + 1, 
            end_x = as.integer(end_x) + 1,
            start_y = as.integer(start_y) + 1,
            end_y = as.integer(end_y) + 1) 

# Create diagram
diagram <- data.frame(matrix(rep(0, 1000*1000), 
                             nrow = 1000,
                             ncol = 1000))


# Only keep horizontal and vertical lines
day5_horizvert <-  day5_tidy %>%
  filter(start_x == end_x | start_y == end_y)


# Add 1 when a line covers a spot
for(i in 1:nrow(day5_horizvert)) {
    
    x_cover <- seq(day5_horizvert$start_x[i], day5_horizvert$end_x[i])
    y_cover <- seq(day5_horizvert$start_y[i], day5_horizvert$end_y[i])
    
    
    diagram[y_cover, x_cover] <- diagram[y_cover, x_cover] + 1
}
  

## How many cells have >= 2?
sum(diagram >= 2)


#### Part 2: add diagonal lines ####
day5_diag <- day5_tidy %>%
  filter(start_x != end_x & start_y != end_y)


# Add 1  where diagonal lines cross
for(i in 1:nrow(day5_diag)) {
  
  x_cover <- seq(day5_diag$start_x[i], day5_diag$end_x[i])
  y_cover <- seq(day5_diag$start_y[i], day5_diag$end_y[i])
  
  coord <- cbind(y_cover, x_cover)
  
  diagram[coord] <- diagram[coord] + 1
}


# How many spots have >= 2 lines crossing?
sum(diagram >= 2)

