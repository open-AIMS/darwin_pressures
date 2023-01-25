library(tidyverse)
library(sf)


x <- read_sf('/home/murray/Work/AIMS/Projects/Darwin Sediment Monitoring/2021/Design/data/primary/GIS/MA_WA_StudyArea.shp') %>%
    dplyr::select(OBJECTID, Zone_Name) 

middle_arm <- x %>% filter(Zone_Name == "Middle Arm") %>%
    mutate(OBJECTID = 4)
west_arm <- x %>% filter(Zone_Name == "West Arm") %>% 
    mutate(OBJECTID = 7)
central_harbour <- x %>% filter(Zone_Name == "Central Harbour") %>% 
    mutate(OBJECTID = 3)

x <- sf::read_sf("/home/murray/Work/AIMS/Projects/Darwin Sediment Monitoring/2021/Design/data/primary/GIS/East_Arm_Sediment_Sampling.shp") %>%
    st_transform(crs = st_crs(west_arm)) %>%
    dplyr::select(OBJECTID, Zone_Name)

east_arm <- x %>% filter(Zone_Name == "East Arm") %>%
    mutate(OBJECTID = 5)
elizabeth <- x %>% filter(Zone_Name == "Elizabeth River") %>% 
    mutate(OBJECTID = 6)


x <- read_sf("/home/murray/Work/AIMS/Projects/Darwin Sediment Monitoring/2021/Design/data/primary/GIS/OuterHarbour_EastArm_ShoalBay.shp") %>% 
    dplyr::select(OBJECTID, Zone_Name) 

outer_harbour <- x %>% filter(Zone_Name == "Outer Harbour") %>%
    mutate(OBJECTID = 1)

shoalbay <- x %>% filter(Zone_Name == "Shoal Bay") %>%
    mutate(OBJECTID = 2)

city <- x %>% filter(is.na(Zone_Name)) %>%
    mutate(OBJECTID = 10)

central_harbour <-
    city %>% 
    st_union(central_harbour, by_feature = TRUE) %>%    
    dplyr::select(OBJECTID, Zone_Name) %>%
    mutate(OBJECTID = 3,
           Zone_Name = 'Central Harbour')

central_harbour %>% 
    ggplot() +
    geom_sf() 

spatial <-
    shoalbay %>%
    rbind(outer_harbour) %>%
    rbind(west_arm) %>%
    rbind(east_arm) %>%
    rbind(elizabeth) %>%
    rbind(central_harbour) %>%
    rbind(middle_arm)

spatial %>%
    st_transform(crs = st_crs(4326)) %>%
    ## left_join(spatial_lookup) %>%    
    ggplot() +
    geom_sf(aes(fill = Zone_Name)) 

sf::write_sf(spatial,
             dsn = paste0(DATA_PATH, 'GIS/darwin_harbour.shp')
             )
