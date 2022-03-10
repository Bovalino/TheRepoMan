#load packages
library(tidyverse)

#review file
mtcars

#summarise by mpg for cyl
summary_1 <- mtcars %>% 
  group_by(cyl) %>% 
  summarise(Ave_MPG = mean(mpg), SD_MPG = sd(mpg))
