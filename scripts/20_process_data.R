source('../scripts/functions.R')

## ---- load lookups
var_lookup <- readRDS(file = paste0(DATA_PATH, "processed/var_lookup.RData"))
units_lookup <- readRDS(file = paste0(DATA_PATH, "processed/units_lookup.RData"))
map_lookup <- readRDS(file = paste0(DATA_PATH, "processed/map_lookup.RData"))
spatial_lookup <- readRDS(file = paste0(DATA_PATH, "processed/spatial_lookup.RData"))

FOCAL_RESPS <- var_lookup %>%
    filter(Type == 'Response') 
## ----end

## Spatial - harbour
{
    ## ---- load spatials
    spatial <- sf::read_sf(paste0(DATA_PATH, '/GIS/darwin_harbour.shp'))
    saveRDS(spatial, file = paste0(DATA_PATH, '/primary/spatial.RData'))
    ## ----end
    ## ---- process data spatial map
    spatial %>%
        st_transform(crs = st_crs(4326)) %>%
        left_join(map_lookup,
                  by = c("Zone_Name" = "ZoneName")) %>%
        dplyr::rename(ZoneName = Zone_Name) %>%
        ggplot() +
        geom_sf(aes(fill = ZoneName), show.legend = FALSE) +
        lapply(split(map_lookup, 1:nrow(map_lookup)), function(dat) {
            geom_curve(data = dat,
                       aes(x = Longitude, y = Latitude,
                           xend = Lab_long, yend = Lab_lat),
                       curvature = dat$Curvature) 
                }) +
        geom_sf_label(data = map_lookup %>%
                         sf::st_as_sf(coords = c('Lab_long', 'Lab_lat'),
                                      crs = st_crs(4326)),
                      aes(label = ZoneName),
                      fill = "white", label.size = 0,
                      vjust = "outward", hjust = 0.5) +
        scale_fill_discrete('Zone name') +
        ## scale_x_continuous(expand = c(0.05,0.05)) +
        coord_sf(clip = "off") +
        theme_bw() +
        theme(axis.title = element_blank(),
              legend.position = c(0.01, 0.01),
              legend.justification = c(0,0)) ->
        p
    ggsave(filename = paste0(FIGS_PATH, "/map_spatial.png"),
           width = 7,
           height = 8,
           dpi = 300)
    ## ----end
}
## Spatial - sub-catchments
{
    ## ---- load spatials subcatchments
    spatial_subcatchments_lookup <- readRDS(file = paste0(DATA_PATH, "processed/spatial_subcatchments_lookup.RData"))
    spatial_subcatchments <- sf::read_sf(paste0(DATA_PATH, '/GIS/New_Skinner_M52.shp')) %>%
        full_join(spatial_subcatchments_lookup) %>%
        arrange(CatchmentNumber) %>%
        mutate(Catchment = forcats::fct_reorder(Catchment, CatchmentNumber),
               ZoneName = forcats::fct_reorder(ZoneName, CatchmentNumber))
    saveRDS(spatial_subcatchments, file = paste0(DATA_PATH, '/primary/spatial_subcatchments.RData'))
    ## ----end
    ## ---- process data spatial subcatchments map
    spatial %>%
        st_transform(crs = st_crs(4326)) %>%
        left_join(map_lookup,
                  by = c("Zone_Name" = "ZoneName")) %>%
        dplyr::rename(ZoneName = Zone_Name) %>%
        ggplot() +
        geom_sf() +
        geom_sf(data = spatial_subcatchments, aes(fill = interaction(CatchmentNumber, Catchment))) +
        geom_sf_text(data = spatial_subcatchments, aes(label = CatchmentNumber),
                     check_overlap = FALSE,
                     nudge_x = spatial_subcatchments$nudge_x) +
        scale_fill_discrete('Sub-catchment') +
        ## scale_x_continuous(expand = c(0.05,0.05)) +
        coord_sf(clip = "off") +
        theme_bw() +
        theme(axis.title = element_blank()) +
        guides(fill = guide_legend(ncol = 1)) ->
        p2
    p2
    ggsave(filename = paste0(FIGS_PATH, "/map_spatial_subcatchments.png"),
           width = 8,
           height = 8,
           dpi = 300)
    ## ----end
    ## ---- process data spatial subcatchments map combined
    ggsave(filename = paste0(FIGS_PATH, "/map_spatial_subcatchments_combined.png"),
           p + p2 + plot_annotation(tag_levels = 'a', tag_suffix = ")"),
           width = 16,
           height = 8,
           dpi = 300)
    ## ----end
    cat("Spatial objects processed\n\n")
}

## WQ
{
    ## ---- process data 2012_2015
    readRDS(file = paste0(DATA_PATH, "primary/wq_2012.RData")) %>% 
        dplyr::select(
                   Location = `Location ID`,
                   Var = `Observed Property ID`,
                   Date = `Observed DateTime`,
                   Value = `Result Value`,
                   Latitude,
                   Longitude) %>% 
        left_join(FOCAL_RESPS %>%
                  dplyr::select(Var, Measure)) %>%
        mutate(Date = as.Date(Date),
               Year = floor(lubridate::quarter(Date, fiscal_start = 7, with_year = TRUE)),
               Source = "Discrete") %>%
        filter(!is.na(Measure)) %>%
        dplyr::select(-Var) ->
        wq
    ## assign records to a spatial zone
    wq %>%
        filter(!is.na(Longitude), !is.na(Latitude)) %>%
        sf::st_as_sf(coords = c("Longitude", "Latitude"),
                     remove = FALSE,
                     crs = st_crs(4326)) %>%
        st_transform(crs = st_crs(spatial)) %>%
        sf::st_intersection(spatial) %>%
        dplyr::select(ZoneName = Zone_Name, Location, Longitude, Latitude,
                      Date, Year, Source, Measure, Value) ->
        wq
    wq.spatial <- wq
    wq <- wq %>% sf::st_drop_geometry()
    saveRDS(wq, file = paste0(DATA_PATH, 'processed/wq_2012.RData'))
    saveRDS(wq.spatial, file = paste0(DATA_PATH, 'processed/wq.spatial_2012.RData'))
    ## ----end
    ## ---- process data 2012_2015 design
    wq %>%
        filter(Source == "Discrete") %>%
        ## mutate(Site = interaction(Longitude, Latitude)) %>%
        mutate(Site = Location) %>%
        dplyr::select(ZoneName, Site, Year, Measure) %>%
        distinct() ->
        wq.design
    units_lookup %>%
        ## dplyr::select(Measure, FigureLabel) ->
        dplyr::select(Measure, TableLabel) ->
        ## deframe() %>%
        ## list() ->
        lookup
    wq.design %>%
        left_join(lookup) %>%
        ## mutate(Measure = FigureLabel) %>%
        mutate(Measure = TableLabel) %>%
        ggplot() +
        geom_point(aes(y = Site, x = Year, colour = Measure), show.legend = FALSE) +
        facet_grid(paste0(ZoneName) ~ Measure,
                   space = "free", scales = "free_y",
                   labeller = labeller(Measure = label_wrap_gen(20))
                   ) +
        theme_bw(14) +
        theme(
            axis.title = element_blank(),
            axis.text.y = element_text(size = 7),
            ## axis.ticks.y = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1), 
            legend.text.align = 0,
            strip.text.y = element_text(angle = 0),
            panel.spacing.x = unit("0.5", "cm")
        ) ->
        p
    
    ggsave(
        filename = paste0(FIGS_PATH, "/design_wq_2012.png"),
        p,
        width = 12,
        height = 10,
        dpi = 300
    )
    ## ----end
    ## ---- process data 2012_2015 map
    ggplot() +
        geom_sf(data = spatial) +
        geom_sf(data = wq.spatial) +
        theme_bw() ->
        p
    ggsave(filename = paste0(FIGS_PATH, "/map_wq__2012_2015.png"),
           width = 7,
           height = 8,
           dpi = 300)
    ## ----end
    
    ## ---- process data 2016_2022
    readRDS(file = paste0(DATA_PATH, "primary/wq_2016.RData")) %>% 
        mutate(Latitude1 = ifelse(Latitude >100, Longitude, Latitude),
               Longitude = ifelse(Longitude < 0, Latitude, Longitude),
               Latitude = Latitude1) %>%
        dplyr::select(-Latitude1) %>%
        mutate(Source = ifelse(Source == "Discrete" & SAMPTYPE == "CFM", "CFM", Source)) %>%
        dplyr::select(-Region) %>%
        mutate(Date = as.Date(Date, format = "%d/%m/%Y"),
               Year = floor(lubridate::quarter(Date, fiscal_start = 7, with_year = TRUE))) %>%
        pivot_longer(cols = any_of(FOCAL_RESPS %>% pull(Var)),
                     names_to = 'Var',
                     values_to = 'Value') %>%
        left_join(FOCAL_RESPS %>%
                  dplyr::select(Var, Measure)) %>%
        left_join(spatial_lookup) %>%
        filter(!is.na(Measure),
               !is.na(Latitude)) %>%
        dplyr::select(ZoneName, Location = `Location ID`, Longitude, Latitude,
                      Date, Year, Source, Measure, Value) ->
        wq
    saveRDS(wq, file = paste0(DATA_PATH, 'processed/wq_2016.RData'))
    wq.spatial <- wq %>%
        filter(!is.na(Latitude), !is.na(Longitude),
               Latitude != 0, Longitude !=0) %>%
        dplyr::select(Location, Longitude, Latitude, Source, Year, Value, Measure) %>%
        distinct() %>% 
        sf::st_as_sf(coords = c('Longitude', 'Latitude'),
                     crs = st_crs(4326))
    saveRDS(wq.spatial, file = paste0(DATA_PATH, 'processed/wq.spatial_2016.RData'))
    ## ----end
    ## ---- process data 2016_2022 design
    wq %>%
        filter(Source == "Discrete") %>%
        mutate(Site = ifelse(is.na(Location),
                             interaction(Longitude, Latitude),
                             Location)) %>%
        dplyr::select(ZoneName, Site, Year, Measure) %>%
        distinct() ->
        wq.design
    units_lookup %>%
        ## dplyr::select(Measure, FigureLabel) ->
        dplyr::select(Measure, TableLabel) ->
        ## deframe() %>%
        ## list() ->
        lookup
    wq.design %>%
        left_join(lookup) %>%
        ## mutate(Measure = FigureLabel) %>%
        mutate(Measure = TableLabel) %>%
        ggplot() +
        geom_point(aes(y = Site, x = Year, colour = Measure), show.legend = FALSE) +
        facet_grid(paste0(ZoneName) ~ Measure,
                   space = "free", scales = "free_y",
                   labeller = labeller(Measure = label_wrap_gen(20))
                   ) +
        theme_bw(14) +
        theme(
            axis.title = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            legend.text.align = 0,
            strip.text.y = element_text(angle = 0),
            panel.spacing.x = unit("0.5", "cm")
        ) ->
        p
    
    ggsave(
        filename = paste0(FIGS_PATH, "/design_wq_2016.png"),
        p,
        width = 12,
        height = 12,
        dpi = 72
    )
    ggsave(
        filename = paste0(FIGS_PATH, "/design_wq_2016_large.png"),
        p,
        width = 12,
        height = 12,
        dpi = 300
    )
    ## ----end
    ## ---- process data 2016_2022 map
    ggplot() +
        geom_sf(data = spatial) +
        geom_sf(data = wq.spatial %>% filter(Source == "Discrete"),
                    ## filter(Year == 2018, !is.na(Value)),
                    ## filter(Year == 2018) %>%
                    ## group_by(geometry, Source) %>%
                    ## summarise(Value = mean(Value)) %>%
                    ## filter(!is.na(Value)),
                aes(colour = Source)) +
        facet_wrap(~Year) +
        theme_bw() +
        theme(legend.position = c(0.01, 0.01),
              legend.justification = c(0,0)) ->
        p
    p
    ggsave(filename = paste0(FIGS_PATH, "/map_wq__2016_2022.png"),
           width = 7,
           height = 9,
           dpi = 72)
    ggsave(filename = paste0(FIGS_PATH, "/map_wq__2016_2022_large.png"),
           width = 7,
           height = 9,
           dpi = 300)

    
    ggplot() +
        geom_sf(data = spatial) +
        geom_sf(data = wq.spatial,
                aes(colour = Source)) +
        theme_bw() +
        theme(legend.position = c(0.01, 0.01),
              legend.justification = c(0,0)) ->
        p
    p
    ggsave(filename = paste0(FIGS_PATH, "/map_wq__2016_2022_all.png"),
           width = 7,
           height = 8,
           dpi = 72)
    ggsave(filename = paste0(FIGS_PATH, "/map_wq__2016_2022_all_large.png"),
           width = 7,
           height = 8,
           dpi = 300)
    ## ----end

    ## ---- process data 2018 alterations
    readRDS(file = paste0(DATA_PATH, "primary/wq_2018.RData")) %>% 
        mutate(Latitude1 = ifelse(Latitude >100, Longitude, Latitude),
               Longitude = ifelse(Longitude < 0, Latitude, Longitude),
               Latitude = Latitude1) %>%
        dplyr::select(-Latitude1) %>%
        mutate(Source = ifelse(Source == "Discrete" & SAMPTYPE == "CFM", "CFM", Source)) %>%
        dplyr::select(-Region) %>%
        mutate(Date = as.Date(Date, format = "%d/%m/%Y"),
               Year = floor(lubridate::quarter(Date, fiscal_start = 7, with_year = TRUE))) %>%
        pivot_longer(cols = any_of(FOCAL_RESPS %>% pull(Var)),
                     names_to = 'Var',
                     values_to = 'Value') %>%
        left_join(FOCAL_RESPS %>%
                  dplyr::select(Var, Measure)) %>%
        left_join(spatial_lookup) %>%
        filter(!is.na(Measure),
               !is.na(Latitude)) %>%
        dplyr::select(ZoneName, Longitude, Latitude,
                      Date, Year, Source, Measure, Value) ->
        wq
    saveRDS(wq, file = paste0(DATA_PATH, 'processed/wq_2018.RData'))
    wq.spatial <- wq %>%
        filter(!is.na(Latitude), !is.na(Longitude),
               Latitude != 0, Longitude !=0) %>%
        dplyr::select(Longitude, Latitude, Source, Year, Value, Measure) %>%
        distinct() %>% 
        sf::st_as_sf(coords = c('Longitude', 'Latitude'),
                     crs = st_crs(4326))
    saveRDS(wq.spatial, file = paste0(DATA_PATH, 'processed/wq.spatial_2018.RData'))
    ## ----end
    ## ---- process data 2018 alterations design
    wq %>%
        filter(Source == "Discrete") %>%
        mutate(Site = interaction(Longitude, Latitude)) %>%
        dplyr::select(ZoneName, Site, Year, Measure) %>%
        distinct() ->
        wq.design
    units_lookup %>%
        ## dplyr::select(Measure, FigureLabel) ->
        dplyr::select(Measure, TableLabel) ->
        ## deframe() %>%
        ## list() ->
        lookup
    wq.design %>%
        left_join(lookup) %>%
        ## mutate(Measure = FigureLabel) %>%
        mutate(Measure = TableLabel) %>%
        ggplot() +
        geom_point(aes(y = Site, x = Year, colour = Measure), show.legend = FALSE) +
        facet_grid(paste0(ZoneName) ~ Measure,
                   space = "free", scales = "free_y",
                   labeller = labeller(Measure = label_wrap_gen(20))
                   ) +
        theme_bw(14) +
        theme(
            axis.title = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            legend.text.align = 0,
            strip.text.y = element_text(angle = 0),
            panel.spacing.x = unit("0.5", "cm")
        ) ->
        p
    
    ggsave(
        filename = paste0(FIGS_PATH, "/design_wq_2018.png"),
        p,
        width = 12,
        height = 12,
        dpi = 72
    )
    ggsave(
        filename = paste0(FIGS_PATH, "/design_wq_2018_large.png"),
        p,
        width = 12,
        height = 12,
        dpi = 300
    )
    ## ----end
    ## ---- process data 2018 alterations map
    ggplot() +
        geom_sf(data = spatial) +
        geom_sf(data = wq.spatial %>% filter(Source == "Discrete"),
                    ## filter(Year == 2018, !is.na(Value)),
                    ## filter(Year == 2018) %>%
                    ## group_by(geometry, Source) %>%
                    ## summarise(Value = mean(Value)) %>%
                    ## filter(!is.na(Value)),
                aes(colour = Source)) +
        facet_wrap(~Year) +
        theme_bw() +
        theme(legend.position = c(0.01, 0.01),
              legend.justification = c(0,0)) ->
        p
    p
    ggsave(filename = paste0(FIGS_PATH, "/map_wq__2018.png"),
           width = 7,
           height = 5,
           dpi = 72)
    ggsave(filename = paste0(FIGS_PATH, "/map_wq__2018_large.png"),
           width = 7,
           height = 5,
           dpi = 300)

    
    ggplot() +
        geom_sf(data = spatial) +
        geom_sf(data = wq.spatial,
                aes(colour = Source)) +
        theme_bw() +
        theme(legend.position = c(0.01, 0.01),
              legend.justification = c(0,0)) ->
        p
    p
    ggsave(filename = paste0(FIGS_PATH, "/map_wq__2018_all.png"),
           width = 7,
           height = 8,
           dpi = 72)
    ggsave(filename = paste0(FIGS_PATH, "/map_wq__2018_all_large.png"),
           width = 7,
           height = 8,
           dpi = 300)
    ## ----end

    ## ---- process data Routine sites
    routine_sites <- readRDS(file = paste0(DATA_PATH, "primary/wq_routine_sites.RData"))
    routine_sites <- routine_sites %>%
        dplyr::select(Site, Gcode, Latitude, Longitude) %>%
        mutate(Type = "Routine")
    saveRDS(routine_sites, file = paste0(DATA_PATH, "processed/wq_routine_sites.RData"))
    ## ----end
    ## ---- process data Routine sites map
    routine_sites.spatial <- routine_sites %>%
        st_as_sf(coords = c("Longitude", "Latitude"),
                 crs = st_crs(4326))
    ggplot() +
        geom_sf(data = spatial, aes(fill = Zone_Name)) +
        geom_sf(data = routine_sites.spatial) +
        geom_sf_label_repel(data = routine_sites.spatial, aes(label = Site),
                            force = 50) +
        scale_fill_discrete('') + 
        theme_bw() +
        theme(legend.position = c(0.01, 0.01),
              legend.justification = c(0,0),
              axis.title = element_blank()) ->
        p
    p
    ggsave(filename = paste0(FIGS_PATH, "/map_wq__routine_sites.png"),
           width = 7,
           height = 9,
           dpi = 72)
    ggsave(filename = paste0(FIGS_PATH, "/map_wq__routine_sites_large.png"),
           width = 7,
           height = 9,
           dpi = 300)
    ## ----end

    ## ---- process data wq combine
    spatial_lookup <- readRDS(paste0(DATA_PATH, 'processed/spatial_lookup.RData'))
    readRDS(file = paste0(DATA_PATH, 'processed/wq_2012.RData')) %>%
        rbind(readRDS(file = paste0(DATA_PATH, 'processed/wq_2016.RData'))) %>%
        ##round lat/long to three decimal places
        mutate(across(c(Longitude, Latitude), round,3)) %>%
        left_join(spatial_lookup) %>%
        pivot_wider(id_cols = everything(),
                    values_fn = mean,
                    names_from = 'Measure',
                    values_from = 'Value') %>%
        mutate(WQ_ID = 1:n(),
               WQ_SITE = factor(paste(Longitude, Latitude))) %>%
        ## first join on Location/Gcode without lat/long
        ## this is to join on those sites that have a Location, but the lat/longs
        ## may not correspond exactly to the lookup
        left_join(routine_sites %>% dplyr::rename(Location = Gcode) %>%
                  dplyr::select(-Latitude, -Longitude)) %>%
        mutate(WQ_SITE = ifelse(is.na(Location), WQ_SITE, Location),
               WQ_SITE = factor(WQ_SITE)) %>%
        dplyr::select(-Location) %>%
        ## mutate(Site1 = Site) %>%
        ## dplyr::select(-Site) %>%
        ## then join on Latitude/Longitude to apply locations to the more recent records
        ## that do not have location id's
        ## we should also round the lat/longs to 3 places to to help match WQ data 
        left_join(routine_sites %>% dplyr::select(-Gcode) %>%
                  mutate(across(c(Longitude, Latitude), round,3)),
                  by = c('Latitude' = 'Latitude', 'Longitude' = 'Longitude')) %>%
        mutate(Site = ifelse(is.na(Site.y), Site.x, Site.y),
               Type = ifelse(is.na(Type.y), Type.x, Type.y)) %>%
        dplyr::select(-Site.y, -Site.x, -Type.y, -Type.x) %>%
        mutate(WQ_SITE = ifelse(is.na(Site), WQ_SITE, Site),
               WQ_SITE = factor(WQ_SITE)) %>%
        mutate(Type = ifelse(is.na(Type), 'Other', Type),
               Type = ifelse(Type == "Routine" | Year < 2016, 'Routine', 'Other')) %>%
        filter(!is.na(ZoneName)) %>%
        droplevels() ->
        wq

    saveRDS(wq, file = paste0(DATA_PATH, 'processed/wq.RData'))
    ## ----end
    
    ## ---- process data combine design Routine
    wq %>%
        filter(Type == "Routine") %>%
        ## mutate(Site = interaction(Longitude, Latitude)) %>%
        mutate(Site = WQ_SITE) %>%
        pivot_longer(cols = c(Chla, Turbidity, DO, PO4, NH3, NOx),
                     names_to = "Measure",
                     values_to = "Value") %>%
        dplyr::select(ZoneName, Site, Year, Measure) %>%
        distinct() ->
        wq.design
    units_lookup %>%
        ## dplyr::select(Measure, FigureLabel) ->
        dplyr::select(Measure, TableLabel) ->
        ## deframe() %>%
        ## list() ->
        lookup
    wq.design %>%
        left_join(lookup) %>%
        ## mutate(Measure = FigureLabel) %>%
        mutate(Measure = TableLabel) %>%
        ggplot() +
        geom_point(aes(y = Site, x = as.Date(paste0(Year,"-01-01")),
                       colour = Measure), show.legend = FALSE) +
        facet_grid(paste0(ZoneName) ~ Measure,
                   space = "free", scales = "free_y",
                   labeller = labeller(Measure = label_wrap_gen(20))
                   ) +
        theme_bw(14) +
        theme(
            axis.title = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.text.align = 0,
            strip.text.y = element_text(angle = 0),
            panel.spacing.x = unit("0.5", "cm")
        ) ->
        p
    
    ggsave(
        filename = paste0(FIGS_PATH, "/design_wq_routine.png"),
        p,
        width = 15,
        height = 12,
        dpi = 72
    )
    ggsave(
        filename = paste0(FIGS_PATH, "/design_wq_routine_large.png"),
        p,
        width = 15,
        height = 12,
        dpi = 300
    )
    ## ----end
    
    ## ---- process data combine design Discrete
    wq %>%
        filter(Source == "Discrete") %>%
        mutate(Site = interaction(Longitude, Latitude)) %>%
        pivot_longer(cols = c(Chla, Turbidity, DO, PO4, NH3, NOx),
                     names_to = "Measure",
                     values_to = "Value") %>%
        dplyr::select(ZoneName, Site, Year, Measure) %>%
        distinct() ->
        wq.design
    units_lookup %>%
        ## dplyr::select(Measure, FigureLabel) ->
        dplyr::select(Measure, TableLabel) ->
        ## deframe() %>%
        ## list() ->
        lookup
    wq.design %>%
        left_join(lookup) %>%
        ## mutate(Measure = FigureLabel) %>%
        mutate(Measure = TableLabel) %>%
        ggplot() +
        geom_point(aes(y = Site, x = as.Date(paste0(Year,"-01-01")),
                       colour = Measure), show.legend = FALSE) +
        facet_grid(paste0(ZoneName) ~ Measure,
                   space = "free", scales = "free_y",
                   labeller = labeller(Measure = label_wrap_gen(20))
                   ) +
        theme_bw(14) +
        theme(
            axis.title = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.text.align = 0,
            strip.text.y = element_text(angle = 0),
            panel.spacing.x = unit("0.5", "cm")
        ) ->
        p
    
    ggsave(
        filename = paste0(FIGS_PATH, "/design_wq.png"),
        p,
        width = 15,
        height = 12,
        dpi = 72
    )
    ggsave(
        filename = paste0(FIGS_PATH, "/design_wq_large.png"),
        p,
        width = 15,
        height = 12,
        dpi = 300
    )
    ## ----end

    ## ---- process data combine map Routine
    wq.spatial <- wq %>%
        pivot_longer(cols = c(Chla, Turbidity, DO, PO4, NH3, NOx),
                     names_to = "Measure",
                     values_to = "Value") %>%
        filter(!is.na(Latitude), !is.na(Longitude),
               Latitude != 0, Longitude !=0) %>%
        dplyr::select(Longitude, Latitude, Type, Year, Value, Measure) %>%
        distinct() %>% 
        sf::st_as_sf(coords = c('Longitude', 'Latitude'),
                     crs = st_crs(4326))
    saveRDS(wq.spatial, file = paste0(DATA_PATH, 'processed/wq.spatial_combine_routine.RData'))

    ggplot() +
        geom_sf(data = spatial) +
        geom_sf(data = wq.spatial,
                aes(colour = Type)) +
        theme_bw() +
        facet_wrap(~Year) +
        theme(legend.position = c(0.99, 0.01),
              legend.justification = c(1,0)) ->
        p
    p
    ggsave(filename = paste0(FIGS_PATH, "/map_wq_routine.png"),
           width = 7,
           height = 7,
           dpi = 72)
    ggsave(filename = paste0(FIGS_PATH, "/map_wq_routine_large.png"),
           width = 7,
           height = 7,
           dpi = 300)
    ## ----end
    
    ## ---- process data combine map Discrete
    wq.spatial <- wq %>%
        pivot_longer(cols = c(Chla, Turbidity, DO, PO4, NH3, NOx),
                     names_to = "Measure",
                     values_to = "Value") %>%
        filter(!is.na(Latitude), !is.na(Longitude),
               Latitude != 0, Longitude !=0) %>%
        dplyr::select(Longitude, Latitude, Source, Year, Value, Measure) %>%
        distinct() %>% 
        sf::st_as_sf(coords = c('Longitude', 'Latitude'),
                     crs = st_crs(4326))
    saveRDS(wq.spatial, file = paste0(DATA_PATH, 'processed/wq.spatial_combine.RData'))

    ggplot() +
        geom_sf(data = spatial) +
        geom_sf(data = wq.spatial %>% filter(Source == "Discrete"),
                    ## filter(Year == 2018, !is.na(Value)),
                    ## filter(Year == 2018) %>%
                    ## group_by(geometry, Source) %>%
                    ## summarise(Value = mean(Value)) %>%
                    ## filter(!is.na(Value)),
                aes(colour = Source)) +
        facet_wrap(~Year) +
        theme_bw() +
        theme(legend.position = c(0.99, 0.01),
              legend.justification = c(1,0)) ->
        p
    ggsave(filename = paste0(FIGS_PATH, "/map_wq.png"),
           width = 7,
           height = 7,
           dpi = 72)
    ggsave(filename = paste0(FIGS_PATH, "/map_wq_large.png"),
           width = 7,
           height = 7,
           dpi = 300)

    
    ggplot() +
        geom_sf(data = spatial) +
        geom_sf(data = wq.spatial,
                aes(colour = Source)) +
        theme_bw() +
        theme(legend.position = c(0.99, 0.01),
              legend.justification = c(1,0)) ->
        p
    p
    ggsave(filename = paste0(FIGS_PATH, "/map_wq_all.png"),
           width = 7,
           height = 8,
           dpi = 72)
    ggsave(filename = paste0(FIGS_PATH, "/map_wq_all_large.png"),
           width = 7,
           height = 8,
           dpi = 300)
    ## ----end
    
    cat("WQ data processed\n\n")
}

## PRIMARY
{
    ## - Format date of year end
    ## - pivot longer for each parameter
    ## - Abbreviate parameter names
    ## - Address problems with this data set
    ##   - fill in missing Lat/Long values - these should not be missing from a spatial data set - this is data vandalism!
    ##     - it is not accurate, but I am going to fill these missing lat/longs with the average of the other lat/longs from other sites in the same Discharge location 
    ##   - catchment is missing in some instances and this will be problematic - remove (it cant be that important or it would be complete!)
    ##   - there are duplicate values (multiple values collected on same date/site - these are all Waste Discharge Licence) - I will remove this category as it is not really a Parameter
    ##   - there is at least one instance where "Source of discharge" differs for the same Site, Fin Year, Year ref, Lat/Long, Organisation - how is this possible.  I am going to remove the "Source of discharge" field as this inconsistency is problematic. 
    ##   - "Financial year end" and "Year ref." are not consistent with "Financial year end" - see example rows 282:285 of "Primary Point Source Dataset_22.xlsx".  I am going to remove "Financial year end"
    ## ---- load data PRIMARY
    ## The following were provided by Lynda via email (Tue 10/01/2023 11:00)
    missing_lookup <- tribble(
        ~Site,                        ~Latitude,   ~Longitude,
        "Paspaley Pearling Company",  -12.56831,  130.91230,
        "Tasmanian Seafoods",         -12.716360, 130.956849,
        "Beluyen (Woods Inlet WwTP)", -12.53686,  130.70571,
        )
    ## catchment_centroids <- 
    ##         spatial_subcatchments %>%
    ##         group_by(Catchment) %>%
    ##         st_centroid() %>%
    ##         st_transform(crs = st_crs(4326)) %>%
    ##         dplyr::select(Catchment, CatchmentNumber, ZoneName) %>%
    ##         ungroup() %>%
    ##         mutate(Longitude = st_coordinates(.)[,1],
    ##                Latitude = st_coordinates(.)[,2]) %>%
    ##         st_drop_geometry()
    readRDS(paste0(DATA_PATH, '/primary/primary.RData')) %>%
        mutate(Site = ifelse(Site == 'Territory Generation DP1',
                             "Territory Generation ADP1",
                             Site)) %>%
        mutate(`Financial year end` = as.Date(`Financial year end`,
                                              origin = '1900-01-01'),
               Year = `Year ref.`) %>%
        ## remove the "Waste Discharge Licence" Parameter category
        filter(Parameter != "Waste Discharge Licence") %>%
        droplevels() %>% 
        dplyr::select(Site, Year, Load, Parameter, Latitude, Longitude) %>%
        ## fill in missing lat/longs from Lookup provided by Lynda
        rows_update(missing_lookup, by = "Site") %>%
        ## assign spatial context
        st_as_sf(coords = c("Longitude", "Latitude"),
                 remove = FALSE,
                 crs = st_crs(4326)) %>%
        st_join(spatial_subcatchments %>%
                dplyr::select(-Area_km2, -Skinner, -nudge_x) %>%
                st_transform(crs = st_crs(4326)),
               join = st_nearest_feature) %>% 
        ## st_intersection(spatial_subcatchments %>%
        ##                 dplyr::select(-Area_km2, -Skinner, -nudge_x) %>%
        ##                 st_transform(crs = st_crs(4326))) %>%
        ## widen and average duplicates
        pivot_wider(
            id_cols = everything(),
            names_from = "Parameter",
            ## values_fn = mean,
            values_from = "Load") %>%
        dplyr::rename(TotalP = `Total P (t)`,
                      TotalN = `Total N (t)`,
                      TSS = `Total suspended solids (t)`,
                      VSS = `Volatile suspended solids (t)`,
                      Discharge = `Discharge volume (annual kL)`) %>%
        mutate(across(c(Site, ZoneName, Catchment),
                      ~forcats::fct_reorder(.x, CatchmentNumber))) %>%
        mutate(PRIMARY_ID = 1:n()) %>%
        arrange(ZoneName, Site, Year) %>% 
        group_by(ZoneName, Site) %>%
        mutate(across(c(TotalP, TotalN, TSS, VSS, Discharge),
                      list(lag1=~lag(.x,1),
                           lag2=~lag(.x,2)),
                      .names = "{.col}_{.fn}")) %>%
        ungroup() ->
        ## left_join(spatial_lookup_primary %>%
        ##           dplyr::select(-Latitude, -Longitude) %>%
        ##           distinct) ->
        ## filter(Site == 'Paspaley Pearling Company') %>%
        ## as.data.frame
        ## group_by(Site) %>%
        ##     mutate(Latitude = replace_na(ifelse(all(is.na(Latitude)), NA,
        ##                                         unique(na.omit(Latitude))))) ->
        primary
    saveRDS(primary, file = paste0(DATA_PATH, "processed/primary.RData"))
    ## ----end
    ## ---- design PRIMARY
    fl <- primary %>%
        dplyr::select(Catchment, `ZoneName`, Site) %>%
        distinct() %>%
        st_drop_geometry() %>% 
        crossing(`Year` = primary %>%
                     pull(`Year`) %>%
                     unique())
    ## expand(Site, `Year ref.`)#, Parameter = c('TotalN', 'TotalP', 'TSS', 'VSS', 'Discharge'))
    lookup <- units_lookup %>%
        dplyr::select(Measure, FigureLabel) %>%
        deframe %>% list 

    primary %>%
        ##rename(any_of(!!!lookup)) %>%
        ggplot() +
        geom_point(data = fl, aes(y = Site, x = `Year`),
                   position=position_nudge(x= 0.1),
                   shape = 21, colour = 'grey80') +
        geom_point(data = fl, aes(y = Site, x = `Year`),
                   position=position_nudge(x= -0.1),
                   shape = 21, colour = 'grey80') +
        geom_point(data = fl, aes(y = Site, x = `Year`),
                   position=position_nudge(x= -0.1, y = 0.1),
                   shape = 21, colour = 'grey80') +
        geom_point(data = fl, aes(y = Site, x = `Year`),
                   position=position_nudge(x= 0.1, y = 0.1),
                   shape = 21, colour = 'grey80') +
        geom_point(data = fl, aes(y = Site, x = `Year`),
                   position=position_nudge(x= 0.1, y = -0.1),
                   shape = 21, colour = 'grey80') +
        geom_point(data = primary %>% filter(!is.na(TotalN)),
                   aes(y = Site, x = `Year`, colour = lookup[[1]]["TotalN"]),
                   position=position_nudge(x= 0.1)) +
        geom_point(data = primary %>% filter(!is.na(TotalP)),
                   aes(y = Site, x = `Year`, colour = lookup[[1]]["TotalP"]),
                   position = position_nudge(x = -0.1)) +
        geom_point(data = primary %>% filter(!is.na(TSS)),
                   aes(y = Site, x = `Year`, colour = lookup[[1]]["TSS"]),
                   position = position_nudge(x = -0.1, y = 0.1)) +
        geom_point(data = primary %>% filter(!is.na(VSS)),
                   aes(y = Site, x = `Year`, colour = lookup[[1]]["VSS"]),
                   position = position_nudge(x = 0.1, y = 0.1)) +
        geom_point(data = primary %>% filter(!is.na(Discharge)),
                   aes(y = Site, x = `Year`, colour = lookup[[1]]["Discharge"]),
                   position = position_nudge(x = 0.1, y = -0.1)) +
        facet_grid(paste0(Catchment, "\n (", ZoneName,")") ~.,
                   space = 'free', scales = 'free_y',
                   labeller = label_value) +
                   ## labeller = label_wrap_gen(width = 18)) +
                   ## labeller = label_both) +
    scale_colour_discrete('Measure', labels = scales::label_parse()) + 
    theme_bw(14) +
        theme(axis.title = element_blank(),
              legend.text.align = 0,
              strip.text.y = element_text(angle = 0)) ->
        p
    
    ggsave(filename = paste0(FIGS_PATH, "/design_primary.png"),
           width = 12,
           height = 10,
           dpi = 72)
    ggsave(filename = paste0(FIGS_PATH, "/design_primary_large.png"),
           width = 12,
           height = 10,
           dpi = 300)
    ## ----end

    ## ---- process data PRIMARY map
    spatial_lookup_primary <- readRDS(paste0(DATA_PATH, "processed/spatial_lookup_primary.RData"))
    primary_labels <- primary %>%
        dplyr::select(Site, Latitude, Longitude) %>%
        st_drop_geometry() %>%
        distinct() %>%
        left_join(spatial_lookup_primary)
    ggplot() +
        geom_sf(data = spatial_subcatchments %>% st_transform(crs = st_crs(4326)), aes(fill = ZoneName), alpha = 0.2) +
        geom_sf(data = spatial %>% st_transform(crs = st_crs(4326)), aes(fill = Zone_Name), alpha = 0.8 ) +
        geom_sf(data = primary %>% group_by(Site) %>%
                    summarise()) +
        lapply(split(primary_labels, 1:nrow(primary_labels)), function(dat) {
            geom_curve(data = dat,
                       aes(x = Longitude, y = Latitude,
                           xend = Lab_long, yend = Lab_lat),
                       curvature = dat$Curvature) 
                }) +
        geom_label(data = primary_labels,
                   aes(x = Lab_long, y = Lab_lat,
                       label = str_wrap(Site,20)),
                   fill = "white", label.size = 0.1,
                   vjust = "outward", hjust = 0.5) +
        scale_fill_discrete('Zone') +
        ## guides(fill = guide_legend(order = -1)) +
        theme_bw() +
        theme(axis.title = element_blank(),
              legend.position = c(0.01, 0.01),
              legend.justification = c(0,0),
              legend.background = element_rect(fill = NA, colour = NA)) ->
        p
    ggsave(filename = paste0(FIGS_PATH, "/map_primary.png"),
           width = 6,
           height = 8,
           dpi = 72)
    ggsave(filename = paste0(FIGS_PATH, "/map_primary_large.png"),
           width = 6,
           height = 8,
           dpi = 300)
    ## ----end
    cat("Primary point data processed\n\n")
}

## Estimated resident population (ERP)
{
    ## ---- process data ERP
    readRDS(file = paste0(DATA_PATH, "primary/erp.RData")) %>% 
        dplyr::select(Year = `Year ending June`,
                      ERP,
                      ERP_growth = `ERP growth`,
                      GRP = `GRP ($M)`,
                      GRP_change = `Change GRP from previous year (%)`) %>% 
        mutate(ERP_ID = 1:n()) %>%
        mutate(across(c(ERP, ERP_growth, GRP, GRP_change),
                      list(lag1=~lag(.x,1),
                           lag2=~lag(.x,2)),
                      .names = "{.col}_{.fn}")) ->
        erp
    saveRDS(erp, file = paste0(DATA_PATH, "processed/erp.RData"))
    ## ----end
    cat("ERP point data processed\n\n")
}

## Estimated resident population (ERP)
{
    ## Questions:
    ## 1. should MWA be Central Harbour or West Arm, should they be combined or should they be repeated so that the ERP and Pop values effect both West Arm and Central Harbour water quality values
    ## 2. there are numerous apparent '-' in the excel sheet.  Some of these (e.g. H6 are actually a '-'). Others, are exported as zero (0) because they are right justified (e.g, D6-H6) and therefore considered a negative rather than a nulll.  Should they be 0 or -?
    ## 3. this results in 0 values for the population columns
    ## - remove the row sums
    ## - replace `-` characters with NA
    ## - ensure that all variables that begin with a four digit year are considered numeric
    ## - pivot longer so that the main predictors are in a single column
    ## - remove any rows with Value of NA
    ## - separete the column into Year and Predictor
    ## - pivot wider according to Predictor
    ## - rename parameters so that they are easier to work with
    ## ---- process data catchmentERP
    spatial_lookup_catchment_erp <- readRDS(file = paste0(DATA_PATH, "processed/spatial_lookup_catchment_erp.RData"))
    spatial_subcatchments_lookup <- readRDS(file = paste0(DATA_PATH, "processed/spatial_subcatchments_lookup.RData"))
    readRDS(file = paste0(DATA_PATH, "primary/catchment_erp.RData")) %>% 
        filter(`Catchment Name` != "Sum") %>%
        mutate(across(everything(), na_if, '-'),
               across(matches("^[0-9]{4}.*"), as.numeric)) %>%
        pivot_longer(cols = c(-`Catchment Name`, -Region, -Area),
                     names_to = "Variable",
                     values_to = "Value") %>%
        filter(!is.na(Value)) %>% 
        separate(Variable, into = c("Year", "Predictor"), sep = " ") %>%
        pivot_wider(id_cols = everything(),
                    names_from = "Predictor",
                    values_from = "Value") %>%
        dplyr::select(`Catchment Name`,
                      Region,
                      Area,
                      Year,
                      Catch_ERP = ERP,
                      POP = `Population/km2`) %>%
        left_join(spatial_lookup_catchment_erp) %>%
        dplyr::select(-`Catchment Name`, -Area, -Region) %>%
        left_join(spatial_subcatchments_lookup %>%
                  dplyr::select(-nudge_x, -Lab_lat, -Lab_long, -Curvature)) %>%
        mutate(across(c(ZoneName, Catchment),
                      ~forcats::fct_reorder(.x, CatchmentNumber))) %>% 
        mutate(CATCHMENT_ERP_ID = 1:n()) %>%
        arrange(ZoneName, Catchment, Year) %>%
        group_by(ZoneName, Catchment) %>%
        mutate(across(c(Catch_ERP, POP),
                      list(lag1=~lag(.x,1),
                           lag2=~lag(.x,2)),
                      .names = "{.col}_{.fn}")) %>%
        ungroup() ->
        catchment_erp
    saveRDS(catchment_erp, file = paste0(DATA_PATH, "processed/catchment_erp.RData"))
    ## ----end

    ## ---- design catchmentERP
    fl <- catchment_erp %>%
        dplyr::select(Catchment, ZoneName) %>%
        distinct %>%
        st_drop_geometry() %>%
        crossing(`Year` = catchment_erp %>%
                     pull(Year) %>%
                     unique)
    ## expand(Site, `Year ref.`)#, Parameter = c('TotalN', 'TotalP', 'TSS', 'VSS', 'Discharge'))
    units_lookup %>%
        dplyr::select(Measure, FigureLabel) %>%
        deframe %>% list ->
        lookup

    catchment_erp %>%
        ggplot() +
        geom_point(data = fl, aes(y = Catchment, x = Year),
                   position=position_nudge(x= 0.1),
                   shape = 21, colour = 'grey80') +
        geom_point(data = fl, aes(y = Catchment, x = Year),
                   position=position_nudge(x= -0.1),
                   shape = 21, colour = 'grey80') +
        geom_point(data = catchment_erp %>%
                       filter(!is.na(Catch_ERP)),
                   aes(y = Catchment, x = Year,
                       colour = lookup[[1]]["Catch_ERP"]),
                   position=position_nudge(x= 0.1)) +
        geom_point(data = catchment_erp %>%
                       filter(!is.na(POP)),
                   aes(y = Catchment, x = Year,
                       colour = lookup[[1]]["POP"]),
                   position = position_nudge(x = -0.1)) +
        facet_grid(ZoneName ~., space = 'free', scales = 'free_y',
                   labeller = label_value) +
        scale_colour_discrete('Measure', labels = scales::label_parse()) + 
        theme_bw(14) +
        theme(axis.title = element_blank(),
              legend.text.align = 0,
              strip.text.y = element_text(angle = 0)) ->
        p
    
    ggsave(filename = paste0(FIGS_PATH, "/design_catchment_erp.png"),
           width = 12,
           height = 10,
           dpi = 72)
    ggsave(filename = paste0(FIGS_PATH, "/design_catchment_erp_large.png"),
           width = 12,
           height = 10,
           dpi = 300)
    ## ----end
    ## ---- process data catchment_erp map
    ## catchment_erp.spatial <- catchment_erp %>%
    ##     filter(!is.na(Latitude), !is.na(Longitude),
    ##            Latitude != 0, Longitude !=0) %>%
    ##     dplyr::select(Longitude, Latitude, `Catchment Name`,
    ##                   Lab_long, Lab_lat, Curvature) %>%
    ##     distinct() %>% 
    ##     sf::st_as_sf(coords = c('Longitude', 'Latitude'),
    ##                  crs = st_crs(4326))
    ## ggplot() +
    ##     geom_sf(data = spatial %>% st_transform(crs = st_crs(4326)) %>%
    ##                 left_join(spatial_lookup, by = c("Zone_Name" = "ZoneName")),
    ##             aes(fill = Zone_Name), alpha = 0.5) +
    ##     ## geom_sf(data = primary.spatial) +
    ##     ## arrows
    ##     lapply(split(spatial_lookup_catchment_erp, 1:nrow(spatial_lookup_catchment_erp)), function(dat) {
    ##         geom_curve(data = dat,
    ##                    aes(x = Longitude, y = Latitude,
    ##                        xend = Lab_long, yend = Lab_lat),
    ##                    curvature = dat$Curvature) 
    ##             }) +
    ##     ## Observed locations
    ##     geom_sf(data = catchment_erp %>%
    ##                 filter(!is.na(Latitude), !is.na(Longitude),
    ##                        Latitude != 0, Longitude !=0) %>%
    ##                 dplyr::select(Longitude, Latitude, ZoneName) %>%
    ##                 distinct() %>% 
    ##                 sf::st_as_sf(coords = c('Longitude', 'Latitude'),
    ##                              crs = st_crs(4326)),
    ##             aes(fill = ZoneName), shape = 21, color = 'black') +
    ##     ## Labels
    ##     geom_sf_label(data = spatial_lookup_catchment_erp %>%
    ##                      dplyr::select(`Catchment Name`, Lab_long, Lab_lat) %>%
    ##                      distinct() %>%
    ##                      sf::st_as_sf(coords = c('Lab_long', 'Lab_lat'),
    ##                              crs = st_crs(4326)),
    ##                   aes(label = `Catchment Name`),
    ##                   fill = "white", label.size = 0,
    ##                   vjust = "outward", hjust = 0.5) +
    ##     scale_fill_discrete('Recieving area') +
    ##     scale_colour_discrete('Recieving area') +
    ##     theme_bw() +
    ##     theme(axis.title = element_blank(),
    ##           legend.position = c(0.01, 0.01),
    ##           legend.justification = c(0,0)) ->
    ##     p
    ## p
    ## ggsave(filename = paste0(FIGS_PATH, "/map_catchment_erp.png"),
    ##        width = 6,
    ##        height = 8,
    ##        dpi = 72)
    ## ----end
    cat("Catchment ERP point data processed\n\n")
}

## Fire frequency
{
    ## - there are two columns called "11-15 (WA)".  As a result, when imported, numbers are appended.
    ##   Based on the naming patterns of the other columns, it is clear that the second of these "11-15 (WA)" columns should have been "11-15 (SD)". Fix this up
    ## - there is also some inconsistency in the format of the column names.  Most of them are of the format "YR1_YR2 (STAT)", however, the final few columns are of the format "YR1_YR2 STAT" (that is, no brackets) - fix this.
    ## ---- process data fire frequency
    ## The data represent the weighted averages and standard deviations over a 5 year window.
    ## For this to be useful in an analysis, we need to express it as a value for a particular year.
    ## I will take the last year of the range of years as the Year.  The logic here is that the fire frequency has been experienced over that 5 year period.
    spatial_subcatchments_lookup <- readRDS(file = paste0(DATA_PATH, "processed/spatial_subcatchments_lookup.RData"))
    spatial_lookup_fire <- readRDS(file = paste0(DATA_PATH, "processed/spatial_lookup_fire.RData"))

    readRDS(file = paste0(DATA_PATH, "primary/fire_freq.RData")) %>% 
        rename(`Catchment Name` = Catchment) %>%
        left_join(spatial_lookup_fire) %>%
        dplyr::select(Catchment, everything(), -`Catchment Name`) %>%
        rename("11_15 (WA)" = "11_15 (WA)...24",
               "11_15 (STD)" = "11_15 (WA)...25") %>%
        filter(Catchment != "Overall") %>%
        pivot_longer(cols = c(-Catchment),
                     names_to = 'Variable',
                     values_to = 'Fire_TotalKm') %>%
        mutate(Year = as.numeric(paste0("20",
                                        str_replace(Variable,
                                                    "[0-9]{2}_([0-9]{2}).*",
                                                    "\\1"))),
               Subvariable = str_replace(Variable, "[0-9]{2}_[0-9]{2} \\(?([^\\)]*)\\)?", "\\1")
               ) %>%
        dplyr::select(-Variable) %>%
        pivot_wider(id_cols = everything(),
                    names_from = Subvariable,
                    values_from = "Fire_TotalKm",
                    names_prefix = "Fire_TotalKm_") %>% 
        left_join(spatial_subcatchments_lookup %>%
                  dplyr::select(Catchment, ZoneName)) %>% 
        mutate(FIRE_FREQ_ID = 1:n()) %>% 
        arrange(ZoneName, Catchment, Year) %>%
        group_by(ZoneName, Catchment) %>%
        mutate(across(matches("^Fire_TotalKm.*", ignore.case = FALSE),
                      list(lag1=~lag(.x,1),
                           lag2=~lag(.x,2)),
                      .names = "{.col}_{.fn}")) %>%
        ungroup() ->
        fire_freq
    saveRDS(fire_freq, file = paste0(DATA_PATH, "processed/fire_freq.RData"))
    ## ----end

    ## ---- design fire freq
    fl <- fire_freq %>%
        dplyr::select(Catchment, `ZoneName`) %>%
        distinct %>%
        st_drop_geometry() %>% 
        crossing(`Year` = fire_freq %>%
                     pull(`Year`) %>%
                     unique)
    ## expand(Site, `Year ref.`)#, Parameter = c('TotalN', 'TotalP', 'TSS', 'VSS', 'Discharge'))
    units_lookup %>%
        dplyr::select(Measure, TableLabel) %>%
        deframe %>% list ->
        lookup

    fire_freq %>%
        ##rename(any_of(!!!lookup)) %>%
        ggplot() +
        geom_point(data = fl, aes(y = Catchment, x = `Year`),
                   position=position_nudge(x= -0.1),
                   shape = 21, colour = 'grey80') +
        geom_point(data = fl, aes(y = Catchment, x = `Year`),
                   position=position_nudge(x= 0.1),
                   shape = 21, colour = 'grey80') +
        geom_point(data = fire_freq %>% filter(!is.na(Fire_TotalKm_WA)),
                   aes(y = Catchment, x = `Year`, colour = lookup[[1]]["Fire_TotalKm_WA"]),
                   position=position_nudge(x= 0.1)) +
        geom_point(data = fire_freq %>% filter(!is.na(Fire_TotalKm_STD)),
                   aes(y = Catchment, x = `Year`, colour = lookup[[1]]["Fire_TotalKm_STD"]),
                   position = position_nudge(x = -0.1)) +
        facet_grid(ZoneName ~.,
                   space = 'free', scales = 'free_y',
                   labeller = label_value) +
                   ## labeller = label_wrap_gen(width = 18)) +
                   ## labeller = label_both) +
    ## scale_colour_discrete('Measure', labels = scales::label_parse()) + 
        scale_colour_discrete('Measure', labels = scales::label_wrap(width = 20)) + 
        theme_bw(14) +
        theme(axis.title = element_blank(),
              ## legend.spacing = unit(1,'cm'),
              legend.key.height=unit(2, "cm"),
              ## legend.text.align = 0,
              strip.text.y = element_text(angle = 0)) +
        guides(colour = guide_legend(label.vjust = 0)) ->
        p
    
    ggsave(filename = paste0(FIGS_PATH, "/design_fire_freq.png"),
           width = 12,
           height = 10,
           dpi = 72)
    ggsave(filename = paste0(FIGS_PATH, "/design_fire_freq_large.png"),
           width = 12,
           height = 10,
           dpi = 300)
    ## ----end
    cat("Fire frequency data processed\n\n")

}

## Fire areas
{
    ## Areas
    ## ---- process data fire areas
    spatial_lookup_fire_areas <- readRDS(file = paste0(DATA_PATH, "processed/spatial_lookup_fire_areas.RData"))
    spatial_subcatchments_lookup <- readRDS(file = paste0(DATA_PATH, "processed/spatial_subcatchments_lookup.RData")) 
    readRDS(file = paste0(DATA_PATH, "primary/fire_areas.RData")) %>% 
        filter(Catchment != "Overall") %>%
        pivot_longer(cols = -Catchment,
                     names_to = "Year",
                     values_to = "Fire_Areas") %>%
        rename(`Catchment Name` = Catchment) %>%
        left_join(spatial_lookup_fire_areas) %>%
        dplyr::select(Catchment, everything(), -`Catchment Name`) %>%
        left_join(spatial_subcatchments_lookup %>% dplyr::select(Catchment, ZoneName)) %>% 
        mutate(Year = as.numeric(Year)) ->
        fire_areas
    ## ----end
    ## Percentages
    ## ---- process data fire areas percentage
    spatial_lookup_fire_areas <- readRDS(file = paste0(DATA_PATH, "processed/spatial_lookup_fire_areas.RData"))
    spatial_subcatchments_lookup <- readRDS(file = paste0(DATA_PATH, "processed/spatial_subcatchments_lookup.RData")) 
    readRDS(file = paste0(DATA_PATH, "primary/fire_areas_p.RData")) %>% 
        filter(Catchment != "Whole catchment area") %>%
        pivot_longer(cols = -Catchment,
                     names_to = "Year",
                     values_to = "Fire_Areas_p") %>%
        mutate(Catchment = ifelse(Catchment == 'Reichardt Creek', 'Reichhardt Creek', Catchment),
               Catchment = ifelse(Catchment == 'Micket Creek', 'Mickett Creek', Catchment)) %>%
        left_join(spatial_subcatchments_lookup %>% dplyr::select(Catchment, ZoneName)) %>% 
        mutate(Year = as.numeric(Year)) -> fire_areas_p
    ## ----end
    ## Combine
    ## ---- process data fire areas combined
    fire_areas %>%
        full_join(fire_areas_p) %>% 
        mutate(FIRE_AREAS_ID = 1:n()) %>%
        arrange(ZoneName, Catchment, Year) %>%
        group_by(ZoneName, Catchment) %>%
        mutate(across(matches("^Fire_Areas.*", ignore.case = FALSE),
                      list(lag1=~lag(.x,1),
                           lag2=~lag(.x,2)),
                      .names = "{.col}_{.fn}")) %>%
        ungroup() ->
        fire_areas
    saveRDS(fire_areas, file = paste0(DATA_PATH, "processed/fire_areas.RData"))
    ## ----end

    ## ---- design fire areas
    fl <- fire_areas %>%
        dplyr::select(Catchment, `ZoneName`) %>%
        distinct %>%
        st_drop_geometry() %>% 
        crossing(`Year` = fire_freq %>%
                     pull(`Year`) %>%
                     unique)
    ## expand(Site, `Year ref.`)#, Parameter = c('TotalN', 'TotalP', 'TSS', 'VSS', 'Discharge'))
    units_lookup %>%
        dplyr::select(Measure, TableLabel) %>%
        deframe %>% list ->
        lookup

    fire_areas %>%
        ##rename(any_of(!!!lookup)) %>%
        ggplot() +
        geom_point(data = fl, aes(y = Catchment, x = `Year`),
                   position=position_nudge(x= -0.1),
                   shape = 21, colour = 'grey80') +
        geom_point(data = fl, aes(y = Catchment, x = `Year`),
                   position=position_nudge(x= 0.1),
                   shape = 21, colour = 'grey80') +
        geom_point(data = fire_areas %>% filter(!is.na(Fire_Areas)),
                   aes(y = Catchment, x = `Year`, colour = lookup[[1]]["Fire_Areas"]),
                   position=position_nudge(x= 0.1)) +
        geom_point(data = fire_areas %>% filter(!is.na(Fire_Areas_p)),
                   aes(y = Catchment, x = `Year`, colour = lookup[[1]]["Fire_Areas_p"]),
                   position = position_nudge(x = -0.1)) +
        facet_grid(ZoneName ~.,
                   space = 'free', scales = 'free_y',
                   labeller = label_value) +
                   ## labeller = label_wrap_gen(width = 18)) +
                   ## labeller = label_both) +
    ## scale_colour_discrete('Measure', labels = scales::label_parse()) + 
        scale_colour_discrete('Measure', labels = scales::label_wrap(width = 20)) + 
        theme_bw(14) +
        theme(axis.title = element_blank(),
              ## legend.spacing = unit(1,'cm'),
              legend.key.height=unit(1, "cm"),
              ## legend.text.align = 0,
              strip.text.y = element_text(angle = 0)) +
        guides(colour = guide_legend(label.vjust = 0)) ->
        p
    
    ggsave(filename = paste0(FIGS_PATH, "/design_fire_areas.png"),
           width = 12,
           height = 10,
           dpi = 72)
    ggsave(filename = paste0(FIGS_PATH, "/design_fire_areas_large.png"),
           width = 12,
           height = 10,
           dpi = 300)
    ## ----end
    cat("Fire areas data processed\n\n")
}

## Sea level rises (cal year)
{
    ## Financial year
    ## ---- process data sea_level_fin
    readRDS(paste0(DATA_PATH, '/primary/sea_level_fin.RData')) %>%
        rename(any_of(c(!!! var_lookup %>%
                        dplyr::select(Measure, Var) %>%
                        deframe))) %>%
        dplyr::select(-`Financial year`) %>% 
        mutate(SEA_LEVEL_FIN_ID = 1:n()) %>%
        mutate(across("SL",
                      list(lag1=~lag(.x,1),
                           lag2=~lag(.x,2)),
                      .names = "{.col}_{.fn}")) ->
        sea_level_fin
    saveRDS(sea_level_fin, file = paste0(DATA_PATH, "processed/sea_level_fin.RData"))
    ## ----end
    cat("Sea level data processed\n\n")
    
}

## Rainfall (fin year)
{
    ## ---- process data rainfall_fin
    readRDS(paste0(DATA_PATH, '/primary/rainfall_fin.RData')) %>%
        rename(any_of(c(!!! var_lookup %>%
                        dplyr::select(Measure, Var) %>%
                        deframe))) %>%
        dplyr::select(-`Financial year`) %>% 
        mutate(RAINFALL_FIN_ID = 1:n()) %>%
        mutate(across("Rainfall",
                      list(lag1=~lag(.x,1),
                           lag2=~lag(.x,2)),
                      .names = "{.col}_{.fn}")) ->
        rainfall_fin
    saveRDS(rainfall_fin, file = paste0(DATA_PATH, "processed/rainfall_fin.RData"))
    ## ----end
    cat("Rainfall data processed\n\n")
}

## Rainfall anomaly
{
    ## ---- process data rainfall_anom
    readRDS(paste0(DATA_PATH, '/primary/rainfall_anom.RData')) %>%
        rename(any_of(c(!!! var_lookup %>%
                        dplyr::select(Measure, Var) %>%
                        deframe))) %>%
        dplyr::select(Year, Rainfall_anom) %>% 
        mutate(RAINFALL_ANOM_ID = 1:n()) %>%
        mutate(across("Rainfall_anom",
                      list(lag1=~lag(.x,1),
                           lag2=~lag(.x,2)),
                      .names = "{.col}_{.fn}")) ->
        rainfall_anom
    saveRDS(rainfall_anom, file = paste0(DATA_PATH, "processed/rainfall_anom.RData"))
    ## ----end
    cat("Rainfall anomaly data processed\n\n")
}

## Mean air temperature 
{
    ## ---- process data temp
    readRDS(paste0(DATA_PATH, '/primary/temp.RData')) %>%
        rename(Year = Date) %>%
        rename(any_of(c(!!! var_lookup %>%
                        dplyr::select(Measure, Var) %>%
                        deframe))) %>%
        dplyr::select(Year, AirTemp) %>% 
        mutate(TEMP_ID = 1:n()) %>%
        mutate(across("AirTemp",
                      list(lag1=~lag(.x,1),
                           lag2=~lag(.x,2)),
                      .names = "{.col}_{.fn}")) ->
        temp
    saveRDS(temp, file = paste0(DATA_PATH, "processed/temp.RData"))
    ## ----end
    cat("Air temperature data processed\n\n")
}

## Mean air temperature anomaly 
{
    ## ---- process data temp_anom
    readRDS(paste0(DATA_PATH, '/primary/temp_anom.RData')) %>%
        rename(any_of(c(!!! var_lookup %>%
                        dplyr::select(Measure, Var) %>%
                        deframe))) %>%
        dplyr::select(Year, AirTemp_anom) %>% 
        mutate(TEMP_ANOM_ID = 1:n()) %>%
        mutate(across("AirTemp_anom",
                      list(lag1=~lag(.x,1),
                           lag2=~lag(.x,2)),
                      .names = "{.col}_{.fn}")) ->
        temp_anom
    saveRDS(temp_anom, file = paste0(DATA_PATH, "processed/temp_anom.RData"))
    ## ----end
    cat("Air temperature anomaly data processed\n\n")
}

## SOI (fin year) 
{
    ## ---- process data SOI_fin
    readRDS(paste0(DATA_PATH, '/primary/SOI_fin.RData')) %>%
        rename(any_of(c(!!! var_lookup %>%
                        dplyr::select(Measure, Var) %>%
                        deframe))) %>% 
        mutate(SOI_FIN_ID = 1:n(),
               Year = as.numeric(str_extract(Year, "[0-9]{4}"))) %>%
        mutate(across("SOI",
                      list(lag1=~lag(.x,1),
                           lag2=~lag(.x,2)),
                      .names = "{.col}_{.fn}")) ->
        SOI_fin
    saveRDS(SOI_fin, file = paste0(DATA_PATH, "processed/SOI_fin.RData"))
    ## ----end
    cat("SOI data processed\n\n")
}

## SST anomaly 
{
    ## ---- process data SST_anom
    readRDS(paste0(DATA_PATH, '/primary/SST_anom.RData')) %>%
        rename(any_of(c(!!! var_lookup %>%
                        dplyr::select(Measure, Var) %>%
                        deframe))) %>%
        dplyr::select(Year, SST_anom) %>% 
        mutate(SST_ANOM_ID = 1:n()) %>%
        mutate(across("SST_anom",
                      list(lag1=~lag(.x,1),
                           lag2=~lag(.x,2)),
                      .names = "{.col}_{.fn}")) ->
        SST_anom
    saveRDS(SST_anom, file = paste0(DATA_PATH, "processed/SST_anom.RData"))
    ## ----end
    cat("SST anomaly data processed\n\n")
}

## Building activity
{
    ## ---- process data build
    readRDS(paste0(DATA_PATH, '/primary/build.RData')) %>%
        rename(any_of(c(!!! var_lookup %>%
                        dplyr::select(Measure, Var) %>%
                        deframe))) %>%
        dplyr::select(Year, Building_total, Building_eng, Building_non, Building_res) %>% 
        mutate(BUILD_ID = 1:n()) %>%
        mutate(across(matches("^Build.*", ignore.case = FALSE),
                      list(lag1=~lag(.x,1),
                           lag2=~lag(.x,2)),
                      .names = "{.col}_{.fn}")) ->
        build
    saveRDS(build, file = paste0(DATA_PATH, "processed/build.RData"))
    ## ----end
    cat("Building activity data processed\n\n")
}

## Ship counts
{
    ## ---- process data ship
    readRDS(paste0(DATA_PATH, '/primary/ship.RData')) %>%
        dplyr::select(-Year) %>%
        rename(any_of(c(!!! var_lookup %>%
                        dplyr::select(Measure, Var) %>%
                        deframe))) %>%
        dplyr::select(Year, Ship_trade, Ship_cont, Ship_liq, Ship_car,
                      Ship_rig, Ship_cruise, Ship_dry, Ship_live) %>% 
        mutate(SHIP_ID = 1:n()) %>%
        mutate(across(matches("^Ship.*", ignore.case = FALSE),
                      list(lag1=~lag(.x,1),
                           lag2=~lag(.x,2)),
                      .names = "{.col}_{.fn}")) ->
        ship
    saveRDS(ship, file = paste0(DATA_PATH, "processed/ship.RData"))
    ## ----end
    cat("Ship counts data processed\n\n")
}



## Combine all data 
{
    ## Some of the pressures sources have duplicate value as the spatial/temporal modelling scale
    ## For example, the pressures data have two samples collected in 2018 within the Middle Arm Zone (Sites: Paspaley Pearling Company, ConocoPhillips Pipeline Australia Pty Ltd (combined discharges))
    ## I want to allow both.  When joining these data sources to the water quality data set, the number of rows will multiply out and thus the water quality data will be replicated by the number of replicate pressure samples.  We need to be mindful of this lest it weights certain patterns higher
## To speed this up, I will join on the whole or harbour data sets first.    
    ## ---- process data Combine
    readRDS(file = paste0(DATA_PATH, "processed/wq.RData")) -> wq
    readRDS(file = paste0(DATA_PATH, "processed/primary.RData")) -> primary
    readRDS(file = paste0(DATA_PATH, "processed/erp.RData")) -> erp
    readRDS(file = paste0(DATA_PATH, "processed/catchment_erp.RData")) -> catchment_erp
    readRDS(file = paste0(DATA_PATH, "processed/fire_freq.RData")) -> fire_freq
    readRDS(file = paste0(DATA_PATH, "processed/fire_areas.RData")) -> fire_areas
    readRDS(file = paste0(DATA_PATH, "processed/sea_level_fin.RData")) -> sea_level_fin
    readRDS(file = paste0(DATA_PATH, "processed/rainfall_fin.RData")) -> rainfall_fin
    readRDS(file = paste0(DATA_PATH, "processed/rainfall_anom.RData")) -> rainfall_anom
    readRDS(file = paste0(DATA_PATH, "processed/temp.RData")) -> temp
    readRDS(file = paste0(DATA_PATH, "processed/temp_anom.RData")) -> temp_anom
    readRDS(file = paste0(DATA_PATH, "processed/SOI_fin.RData")) -> SOI_fin
    readRDS(file = paste0(DATA_PATH, "processed/SST_anom.RData")) -> SST_anom
    readRDS(file = paste0(DATA_PATH, "processed/build.RData")) -> build
    readRDS(file = paste0(DATA_PATH, "processed/ship.RData")) -> ship

    data <-  
        wq %>%  
        full_join(sea_level_fin) %>% 
        full_join(rainfall_fin) %>% 
        full_join(rainfall_anom) %>% 
        full_join(temp) %>% 
        full_join(temp_anom) %>% 
        full_join(SOI_fin) %>% 
        full_join(SST_anom) %>% 
        full_join(build) %>%
        full_join(ship) %>%
        full_join(primary %>% ##slice(1:32) %>%
                  dplyr::select(Year,
                                TotalN, TotalP, TSS, VSS, Discharge,
                                TotalN_lag1, TotalP_lag1, TSS_lag1, VSS_lag1, Discharge_lag1,
                                TotalN_lag2, TotalP_lag2, TSS_lag2, VSS_lag2, Discharge_lag2,
                                PRIMARY_SITE_ID = Site,
                                ZoneName, Catchment, CatchmentNumber, PRIMARY_ID) %>%
                  st_drop_geometry()
                  ) %>%
        full_join(erp) %>%
        full_join(catchment_erp %>%
                  dplyr::select(Year,
                                Catch_ERP, Catch_ERP_lag1, Catch_ERP_lag2,
                                POP, POP_lag1, POP_lag2,
                                Catchment,
                                CatchmentNumber,
                                ZoneName, CATCHMENT_ERP_ID) %>%
                  mutate(Year = as.numeric(as.character(Year)))) %>%
        full_join(fire_freq) %>% 
        full_join(fire_areas) 
    saveRDS(data, file = paste0(DATA_PATH, "processed/data.RData"))
    ## ----end
    cat("Combine all data processed\n\n")
}

## Tests
## data %>%
##     dplyr::select(ZoneName, Year, Ship_trade, SHIP_ID)
