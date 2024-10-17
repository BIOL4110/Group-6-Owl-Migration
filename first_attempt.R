library(tidyverse)

####### fucking around with data

#the ebird data that I pre-filtered only Canadian observations (to get file size down)
Ebird_snowy_owl_Canada


#Checking data in breeding areas (to make sure we have enough)
  # lower Lat of breeding
  above_60 <-Ebird_snowy_owl_Canada %>% 
   filter(LATITUDE > 60)
  #Lower lat of exclusive breeding
  above_68 <- Ebird_snowy_owl_Canada %>% 
   filter(LATITUDE > 68)
  
  #checking how many observations have Duration (effort data)
    with_time_data<- Ebird_snowy_owl_Canada[!(is.na(Ebird_snowy_owl_Canada$DURATION.MINUTES)), ]
  #checking how many observations have distance data (more effort data)
    with_distance_data<- Ebird_snowy_owl_Canada[!(is.na(Ebird_snowy_owl_Canada$EFFORT.DISTANCE.KM)), ]
  #checking how many observations have both

    
    
    
   
  ######### calculating abundance from ebird data
    
   
    
    
 ########### Aggergating Weather Data
    
    
    
    