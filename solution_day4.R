#### Advent of code: Day 4 ####
library(tidyverse)

## Load in bingo cards
df <- read.table("data/day4.txt", header = F, skip = 2) %>% 
  # Split into separate cards
  mutate(bingo_card = factor(rep(1:(nrow(.)/5), each = 5)))
  
df_list <- split(df, df$bingo_card)



## Load bingo calls 
calls <- read_lines("data/day4.txt", n_max = 1) 
calls  <- as.integer(unlist(str_split(calls, ",")))

# Create function to replace values called with -1
match_bingo <- function(b, n_call = call_i) {
  ifelse(b == n_call, -1, b)
}

# Play bingo!
for(j in 1:length(df_list)){
  
  for(i in 1:length(calls)) {
    
    call_i <-  calls[i]
    
    # Replace called numbers with -1
    df_list[[j]] <- df_list[[j]] %>% 
      mutate(across(starts_with("V"), ~match_bingo(.x)),
             # final number called
             call = call_i,
             # number calls until bingo
             num_calls = i) 
    
    
    # Stop loop when bingo!
    if(sum(match(rowSums(df_list[[j]][,1:5]), -5), na.rm = T) >= 1 |
       sum(match(colSums(df_list[[j]][,1:5]), -5), na.rm = T) >= 1) break
    
  }

}  


# Return quickest board to bingo
bingo_first <- reduce(df_list, rbind) %>% 
  filter(num_calls == min(num_calls)) 

# Add unmarked values (replace -1 with NA)
unmarked_total <- bingo_first %>% 
  mutate(across(starts_with("V"), ~na_if(.x, -1))) %>% 
  summarise(across(starts_with("V"), ~sum(.x, na.rm = T)))

# Total unmarked * last called number
sum(unmarked_total) * bingo_first$call[1]



#### Part 2 ####
# Same as above but pick last  board  to reach bingo
bingo_last <- reduce(df_list, rbind) %>% 
  filter(num_calls == max(num_calls)) 


unmarked_total <- bingo_last %>% 
  mutate(across(starts_with("V"), ~na_if(.x, -1))) %>% 
  summarise(across(starts_with("V"), ~sum(.x, na.rm = T)))


sum(unmarked_total) * bingo_last$call[1]

