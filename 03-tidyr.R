
library(tidyverse)

covid_tbl <- read_csv('data/covid-ita-regions.csv')
covid_tbl

covid_status <- covid_tbl %>% 
  select(time, region = region_name, confirmed, recovered, deaths) %>% 
  arrange(time, region)

# Long version 
status_long <- covid_status %>% 
  pivot_longer(c(confirmed, recovered, deaths), names_to = 'status', values_to = 'count')

## Plot
status_long %>% 
  ggplot(aes(x = time, y = count, color = status)) +
  geom_line() + 
  facet_wrap( ~ region, scales = 'free_y') + 
  theme_bw()



# pivot_wider
status_wide <- status_long %>% 
  pivot_wider(names_from = status, values_from = count)

library(lubridate)

covid_tbl %>% 
  mutate(month = month(time, label = T)) %>% 
  group_by(month, region_name) %>% 
  summarise(hosp = sum(hosp)) %>% 
  pivot_wider(names_from = month, values_from = hosp)
  
















