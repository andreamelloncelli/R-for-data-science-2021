
library(tidyverse)
library(nycflights13)


# left join default example -----------------------------------------------

carrier_del_tbl <- 
  flights %>% 
  select(carrier, arr_delay) %>%
  group_by(carrier) %>% 
  summarise(average = mean(arr_delay, na.rm = TRUE),
            std_dev = sd(arr_delay, na.rm = TRUE))

result <- 
  carrier_del_tbl %>% 
  left_join(airlines) %>% 
  select(
    carrier_name = name,
    arr_mean_delay = average,
    arr_sd_delay = std_dev
  )

# left join example 2 -----------------------------------------------

airlines_2 <- airlines %>% 
  rename(id = carrier)

flights_2 <- flights %>% 
  mutate(id = as.character(row_number())) %>% 
  select(id, arr_delay, carrier)

result <- 
  flights_2 %>% 
  left_join(airlines_2, by = c('carrier' = 'id')) 

# left_join is the tidyverse function for merge()

# full_join, right_join, inner_join

main_tbl <- tibble(
  id = 1:5,
  lower = c('100', letters[3:6]) # sample(c(NA_character_, letters[:4]), size = 5)
)
main_tbl

letter_tbl <- tibble(
  lower = letters,
  upper = LETTERS
)
letter_tbl

main_tbl %>% 
  left_join(letter_tbl)

result <- 
  main_tbl %>% 
  right_join(letter_tbl)
result

result <- 
  main_tbl %>% 
  full_join(letter_tbl)
result


result <- 
  main_tbl %>% 
  inner_join(letter_tbl)
result
