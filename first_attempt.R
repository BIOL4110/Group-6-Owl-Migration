library(tidyverse)

#the ebird data that I pre-filtered only Canadian observations (to get file size down)
Ebird_snowy_owl_Canada


#Checking data in breeding areas (to make sure we have enough)
  # lower Lat of breeding
  above_60 <-Ebird_snowy_owl_Canada %>% 
   filter(LATITUDE > 60)
  #Lower lat of exclusive breeding
  above_68 <- Ebird_snowy_owl_Canada %>% 
   filter(LATITUDE > 68)