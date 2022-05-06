# https://r4ds.had.co.nz/relational-data.html

library(tidyverse)
library(nycflights13)

airlines

planes %>% 
  count(tailnum) %>% 
  filter(n > 1)

weather %>% 
  count(year, month, day, hour, origin) %>% 
  filter(n > 1)
