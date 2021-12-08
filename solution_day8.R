#### Day 8 ####
library(tidyverse)
#### Opposite of %in% ####
'%!in%' <- function(x,y)!('%in%'(x,y))


df <- strsplit(readLines("data/day8.txt"), " ")



# Return number of letters and count number with 2 (1), 3 (7), 4 (4) and 7 (8) letters
count_nums <- function(data) {
  
  # Keep values after "|"
  data_part1 <- data[12:15]
  
  # Count number letters
  num_letters <- nchar(data_part1)
  
  
  # Return number that match criteria
  sum(num_letters %in% c(2:4, 7))
}

# Apply over all lines, add total number
sum(unlist(lapply(df, count_nums)))


#### Part 2 ####
# 2 lights = "1", 3 lights = "7", 4 lights = "4", 
# 5 lights = "2", "3" or "5",  
# 6 lights = "0", "6" or "9"

# Set formula to apply to each line
df_result <- NA

pt2_fn <- function(x){
  
  df_result <- data.frame(original  = x,
                          num =  nchar(x), 
                          result = NA) %>% 
    # Create known numbers
    mutate(result = case_when(
      num == 2 ~ 1,
      num == 3 ~ 7,
      num == 4 ~ 4,
      num == 7 ~ 8
    ),
    chr_list = strsplit(original, ""))
  
  
  # Extract known values + their letters
  pos <- df_result %>% 
    filter(!is.na(result)) %>% 
    distinct(result, .keep_all = T) %>% 
    pull(chr_list, name = result)
  
  names(pos) <- paste0("lights_", names(pos))
  
  
  df_result <- df_result %>% 
    rowwise() %>% 
    # Number shared with 7 and 4
    mutate(count_7 = sum(str_count(original, pos$lights_7)),
           count_4 = sum(str_count(original, pos$lights_4)),
           result = case_when(
             # 5 lights
             num == 5 & count_7 == 3 ~ 3,
             num == 5 & count_4 == 2 ~ 2,
             num == 5 & count_7 == 2 & count_4 == 3 ~ 5,
             # 6 lights
             num == 6 & count_4 == 4 ~ 9,
             num == 6 & count_4 == 3 & count_7 == 2 ~ 6,
             num == 6 & count_4 == 3 & count_7 == 3 ~ 0,
             TRUE ~ result
           )) 
  
  output <- as.numeric(paste(df_result$result[12:15], collapse = ""))
  
  return(output)
}


result_list <- lapply(df, pt2_fn)
sum(unlist(result_list))



