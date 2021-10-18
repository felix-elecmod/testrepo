library(tidyverse)
library(palmerpenguins)


penguins %>% 
  ggplot(aes(x=bill_depth_mm,y=bill_length_mm,color=species))+
  geom_point()
