# install.packages('tidyverse')

library(tidyverse)

ds <- data.frame(
  id = 1:5,
  heigth = c(1.7, 1.8, 1.75, 1.8, 1.9),
  weight = c( 70, 73, 80, 95, 100)
)
ds

ds$bmi <- ds$weight / (ds$heigth)^2



# tibble ------------------------------------------------------------------

library(tidyverse)

ds <- tibble(
  id = 1:5,
  heigth = c(1.7, 1.8, 1.75, 1.8, 1.9),
  weight = c( 70, 73, 80, 95, 100),
  bmi = weight / (heigth)^2
)
ds

# NSE: Non Standard Evaluation


# df to tibble ------------------------------------------------------------

mtcars
mtcars_tbl <- as_tibble(mtcars)
mtcars_tbl

iris_tbl <- as_tibble(iris)

iris_tbl

print(iris_tbl)


# row names to column -----------------------------------------------------

rownames_to_column(mtcars, var = 'car')

mtcars_tbl <- as_tibble(rownames_to_column(mtcars, var = 'car'))
mtcars_tbl



# read into tibbles -------------------------------------------------------

dc <- read_delim('data/dc-wikia-data.csv', delim = ',')
dc <- read_csv('data/dc-wikia-data.csv')

# dc <- read_delim('data/dc-wikia-data.csv', delim = ';')
# dc <- read_csv2('data/dc-wikia-data.csv')


dc <- read_csv('data/dc-wikia-data.csv',
               col_types = cols(
                 page_id = col_integer(),
                 name = col_character(),
                 urlslug = col_character(),
                 ID = col_factor(),
                 ALIGN = col_factor(),
                 EYE = col_factor(),
                 HAIR = col_factor(),
                 SEX = col_factor(),
                 GSM = col_character(),
                 ALIVE = col_character(),
                 APPEARANCES = col_double(),
                 `FIRST APPEARANCE` = col_character(),
                 YEAR = col_double()
               ))
dc



# lowercase columns -------------------------------------------------------

colnames(dc) <- tolower(colnames(dc))

dc


# save as Rds -------------------------------------------------------------

saveRDS(object = dc, file = 'data/dc-wikia-data-2.Rds')

dc <- readRDS('data/dc-wikia-data-2.Rds')
dc


# dplyr -------------------------------------------------------------------

# select, filter, arrange, mutate, summarise

summary(dc)

select(dc, name, align, eye, hair)

# NSE Non standard Evaluation




summary(
  select(dc, name, align, eye, hair)
)

dc %>% 
  select(name, align, eye, hair) %>% 
  summary()

dc %>% 
  # select columns: 'name', and from 'id' to 'year'
  # select(name, id, align, eye, hair, sex, gsm, alive, appearances, `first appearance`, year)
  select(name, id:year)

# Examlpe with vector
v <- c('name', 'id')
dc %>% 
  select(v)

dc %>% 
  select(-c(page_id, urlslug))

dc <- dc %>%
  rename(secret_id = id)

dc %>% 
  select(name, contains('appearance'))

dc %>% 
  select(ends_with('id'))

# helpers: starts_with, 
dc %>% 
  select(starts_with('a'))

dc %>% 
  select(matches('appearance.*'))

dc %>% 
  select(`first appearance`)


dc %>% 
  filter(sex == "Female Characters")

dc %>% 
  filter(sex != "Female Characters")

# a = 2
# a == 3  
# a == 2  
# a == 2 & 1 == 1
# a == 2 | 1 == 2


dc %>% 
  filter(sex == "Female Characters" & hair == "Blond Hair") %>% 
  select(name, sex, hair)

# this is the same operation
dc %>% 
  filter(sex == "Female Characters", hair == "Blond Hair") %>% 
  select(name, sex, hair)

dc %>% 
  filter(sex == "Female Characters" | hair == "Blond Hair") %>% 
  select(name, sex, hair)

dc %>% 
  distinct(sex)

dc %>%
  distinct(hair)

unique(dc$hair)

dc %>% 
  # filter appeareances greater or equal of 1000
  filter(appearances >= 1000)

dc %>% 
  filter(appearances %in% c(900, 2496))  

dc %>% 
  filter(appearances > 900 & appearances < 1000)

dc %>% 
  filter(between(appearances, 900, 1000))

dc %>% 
  filter(appearances < 900 | appearances > 1000)

dc %>% 
  filter(secret_id %in% c("Secret Identity", "Identity Unknown")) %>% 
  select(name, secret_id) 


# filter extras -----------------------------------------------------------

dc %>% 
  slice(5:30)

dc %>% 
  sample_n(3)

dc %>% 
  sample_frac(0.10)

dc %>% 
  sample_frac(2.00, replace = TRUE)


# arrange -----------------------------------------------------------------

dc %>% 
  arrange(appearances)

dc %>% 
  arrange(desc(appearances)) %>% 
  head(3)

dc %>% 
  arrange(sex, desc(appearances)) %>% 
  View()


# mutate ------------------------------------------------------------------

dc %>% 
  mutate(age = 2021 - year) %>% 
  arrange(desc(age)) %>% 
  select(name, age)

# install.packages("nycflights13")

library(nycflights13)

flights %>% 
  select(dep_delay, arr_delay) %>% 
  mutate(time_gain = dep_delay - arr_delay)

flights %>% 
  select(arr_delay) %>% 
  mutate(delay_class = case_when(
    arr_delay > 1000 ~ "big-delay",
    arr_delay < 1000 & arr_delay > 0 ~ "delay",
    arr_delay <= 0 ~ "no-delay",
    TRUE ~ NA_character_
  )) %>% 
  View()


mean(flights$arr_delay, na.rm = TRUE)
# mean: R^n -> R

2021 - dc$year
# func: R^n -> R^n



flights %>% 
  select(arr_delay) %>%
  summarise(average = mean(arr_delay, na.rm = TRUE),
            std_dev = sd(arr_delay, na.rm = TRUE),
            count = n())

# n()

flights %>% 
  count()

flights %>% 
  distinct(carrier)

flights %>% 
  summarise(n_carriers = n_distinct(carrier))





tbl <- flights %>% 
  group_by(carrier, origin, dest) %>%
  summarise(mean_arr_delay = mean(arr_delay, na.rm = TRUE)) %>% 
  ungroup()

tbl

grouped_flights <- flights %>% 
  group_by(carrier) %>% 
  ungroup()

grouped_flights %>% 
  summarise(mean_arr_delay = mean(arr_delay, na.rm = TRUE))


# Exercises ---------------------------------------------------------------


# Exercises: pdf/01_dplyr_exercises.pdf
# - Simple operations
# - Multiple operations on a single dataset


