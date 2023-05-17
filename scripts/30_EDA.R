## module load singularity
## singularity exec -B .:/home/project ../darwin_pressures.sif R
## ess-remote

source('../scripts/functions.R')

## ---- EDA load lookups
units_lookup <- readRDS(file = paste0(DATA_PATH, "processed/units_lookup.RData"))
var_lookup <- readRDS(file = paste0(DATA_PATH, "processed/var_lookup.RData"))
spatial_lookup <- readRDS(file = paste0(DATA_PATH, "processed/spatial_lookup.RData"))

FOCAL_RESPS <- var_lookup %>%
    filter(Type == 'Response') %>%
    pull(Measure) %>%
    unique()
FOCAL_PRESSURES <- var_lookup %>%
    filter(Type == "Pressure")
## ----end
## ---- EDA load data
data <- readRDS(file = paste0(DATA_PATH, "processed/data.RData"))
## ----end

## ---- logTable
logTable <<- expand.grid(Resps = FOCAL_RESPS, Preds = FOCAL_PRESSURES$Measure, Lags = 0:2, Type = c('Routine', 'Discrete'))
write.csv(logTable, file = paste0(DATA_PATH, "modelled/logTable.csv"))
cat(paste0("Resps,Preds,Lag,Type\n"),
    file = paste0(DATA_PATH, "modelled/log.csv"))
## ----end


## Routine Samples ===================

## Temporal Trends
{
    ## Responses
    {
        ## ---- EDA Routine
        data.EDA.routine <- 
            data %>%
            filter(Type == 'Routine') %>%
            dplyr::select(WQ_ID, Zone, ZoneName, Latitude, Longitude, Year, Date, Type,
                          PO4, DO, Turbidity, Chla, NH3, NOx) %>%
            distinct() %>%
            pivot_longer(cols = c(PO4, DO, Turbidity, Chla, NH3, NOx)) %>%
            filter(!is.na(value)) %>%
            mutate(value = ifelse(value==0, 0.001, value)) %>%
            group_by(name) %>%
            nest() %>%
            mutate(EDA = map2(.x = data, .y = name, 
                              ~ EDA(.x, .y)))
        ## ----end
        ## ---- EDA Routine save
        set_cores_furrr()
        future_walk2(.x = data.EDA.routine$name,
              .y = data.EDA.routine$EDA,
              ~ggsave(filename = paste0(FIGS_PATH, '/EDA_routine_',.x, '.png'),
                      .y,
                      width = 10,
                      height = 8,
                      dpi = 72
                     )
              )
        future_walk2(.x = data.EDA.routine$name,
              .y = data.EDA.routine$EDA,
              ~ggsave(filename = paste0(FIGS_PATH, '/EDA_routine_',.x, '_large.png'),
                      .y,
                      width = 10,
                      height = 8,
                      dpi = 300
                     )
              )
        ## ----end
    }
    ## Pressures - these are invariant to response scale
    ## and thus are only calculated once - see below
}


## Discrete samples ==================

## Temporal Trends
{
    ## Responses
    {
        ## ---- EDA
        data.EDA <- 
            data %>%
            filter(Source == 'Discrete') %>%
            dplyr::select(WQ_ID, Zone, ZoneName, Latitude, Longitude, Year, Date, Source,
                          PO4, DO, Turbidity, Chla, NH3, NOx) %>%
            distinct() %>%
            pivot_longer(cols = c(PO4, DO, Turbidity, Chla, NH3, NOx)) %>%
            filter(!is.na(value)) %>%
            mutate(value = ifelse(value==0, 0.001, value)) %>%
            group_by(name) %>%
            nest() %>%
            mutate(EDA = map2(.x = data, .y = name, 
                              ~ EDA(.x, .y)))
        ## ----end
        ## ---- EDA save
        set_cores_furrr()
        future_walk2(.x = data.EDA$name,
              .y = data.EDA$EDA,
              ~ggsave(filename = paste0(FIGS_PATH, '/EDA_',.x, '.png'),
                      .y,
                      width = 10,
                      height = 8,
                      dpi = 72
                     )
              )
        future_walk2(.x = data.EDA$name,
              .y = data.EDA$EDA,
              ~ggsave(filename = paste0(FIGS_PATH, '/EDA_',.x, '_large.png'),
                      .y,
                      width = 10,
                      height = 8,
                      dpi = 300
                     )
              )
        ## ----end
    }
    ## data.EDA[2,"EDA"][[1]]

    ## Pressures
    {
        ## ---- EDA pressures
        set_cores()
        ## for (i in 1:nrow(FOCAL_PRESSURES)) {
        foreach (i = 1:nrow(FOCAL_PRESSURES)) %dopar% {
            MEASURE <- FOCAL_PRESSURES[i, "Measure"][[1]]
            ID <- FOCAL_PRESSURES[i, "ID"][[1]]
            SITE_ID <- FOCAL_PRESSURES[i, "SITE_ID"][[1]]
            if (is.na(SITE_ID)) SITE_ID <- NULL
            FACET <- FOCAL_PRESSURES[i, "Scale"][[1]]
            if (is.na(FACET)) FACET <- "Whole Harbour"
            cat(MEASURE, "\n")
            data.EDA <- 
                data %>%
                {if (FACET == "Whole Harbour") {
                     filter(., is.na(Catchment)) %>%
                     mutate(Catchment = FACET) %>%
                         droplevels()
                 } else {
                     mutate(., Catchment = interaction(CatchmentNumber, Catchment))
                     }
                 } %>%
                dplyr::select(Catchment, Year, !!MEASURE, !!ID, !!SITE_ID) %>%
                distinct() %>%
                rename(Value = {{MEASURE}}) %>%
                rename(SITE_ID = {{SITE_ID}}) %>%
                {if (is.null(SITE_ID)) mutate(., SITE_ID = NA) else .} %>%
                filter(!is.na(Value)) %>%
                droplevels()
                ## mutate(Catchment = forcats::fct_reorder(Catchment, CatchmentNumber))
            gg <- data.EDA %>%
                EDA_pressures(var = {{MEASURE}})
            FACETS <- data.EDA %>% pull(Catchment) %>% unique() %>% length()
            NCOL <- wrap_dims(FACETS, ncol = 3)[2]
            NROW <- wrap_dims(FACETS, ncol = 3)[1]
            cat(paste0(
                "Measures: ", MEASURE,
                "\nNCOL: ", NCOL,
                "\nNROW: ", NROW,
                "\ncalc: ", (6.5 * ((NROW*2.7)/(NCOL*4.5))),
                "\n\n"))
            ggsave(filename = paste0(FIGS_PATH, "/EDA_pressures_",MEASURE,".png"),
                   plot = gg,
                   width = NCOL * 4.5,
                   height = NROW * 2.7,
                   dpi = 72)
            ggsave(filename = paste0(FIGS_PATH, "/EDA_pressures_",MEASURE,"_large.png"),
                   plot = gg,
                   width = NCOL * 4.5,
                   height = NROW * 2.7,
                   dpi = 300)
        }
        ## ----end
    }
}

## All samples
## Temporal Trends
{
    ## Responses
    {
        ## ---- EDA All
        ## calculate discrete and CFM averages separately per ZoneName/Year
        ## then average the sources together per ZoneName/Year
        data.EDA.all <- 
            data %>%
            dplyr::select(WQ_ID, Zone, ZoneName, Latitude, Longitude, Year, Date, Source,
                          PO4, DO, Turbidity, Chla, NH3, NOx) %>%
            distinct() %>%
            pivot_longer(cols = c(PO4, DO, Turbidity, Chla, NH3, NOx)) %>%
            filter(!is.na(value)) %>%
            group_by(ZoneName, Zone, Year, Source, name) %>%
            summarise(value = mean(value),
                      Date = mean(Date),
                      Latitude = mean(Latitude),
                      Longitude = mean(Longitude)) %>%
            ungroup() %>%
            group_by(ZoneName, Zone, Year, name) %>%
            summarise(value = mean(value),
                      Date = mean(Date),
                      Latitude = mean(Latitude),
                      Longitude = mean(Longitude)) %>%
            mutate(WQ_ID = 1, WQ_SITE = 1) %>%
            mutate(value = ifelse(value==0, 0.001, value)) %>%
            group_by(name) %>%
            nest() %>%
            mutate(EDA = map2(.x = data, .y = name, 
                              ~ EDA(.x, .y)))
        ## ----end
        ## ---- EDA All save
        set_cores_furrr()
        future_walk2(.x = data.EDA.all$name,
              .y = data.EDA.all$EDA,
              ~ggsave(filename = paste0(FIGS_PATH, '/EDA_all_',.x, '.png'),
                      .y,
                      width = 10,
                      height = 8,
                      dpi = 72
                     )
              )
        ## ----end
    }
    ## Pressures - these are invariant to response scale
    ## and thus are only calculated once - see below
}



## Associations ---------------------------------------------------
## Routine Samples ==================
{
    ## ---- EDA Routine associations data prep
    assoc_data_pred(data, type = "Routine", FOCAL_RESPS, FOCAL_PRESSURES)
    ## assoc_data_pred(data, type = "Routine", FOCAL_RESPS, FOCAL_PRESSURES, diff = TRUE)
    ## ----end
    ## ---- EDA Routine dual plots
    dual_plots(type = "Routine", FOCAL_RESPS, FOCAL_PRESSURES)
    dual_plots(type = "Routine", FOCAL_RESPS, FOCAL_PRESSURES, lag = 1)
    dual_plots(type = "Routine", FOCAL_RESPS, FOCAL_PRESSURES, lag = 2)
    ## ----end

    ## ---- EDA Routine Associations
    associations(type = "Routine", FOCAL_RESPS, FOCAL_PRESSURES)
    associations(type = "Routine", FOCAL_RESPS, FOCAL_PRESSURES, lag = 1)
    associations(type = "Routine", FOCAL_RESPS, FOCAL_PRESSURES, lag = 2)
    ## ----end
}

## Discrete samples =================
{
    ## ---- EDA associations data prep
    assoc_data_pred(data, type = "Discrete", FOCAL_RESPS, FOCAL_PRESSURES)
    ## ----end

    ## ---- EDA dual plots
    dual_plots(type = "Discrete", FOCAL_RESPS, FOCAL_PRESSURES)
    dual_plots(type = "Discrete", FOCAL_RESPS, FOCAL_PRESSURES, lag = 1)
    dual_plots(type = "Discrete", FOCAL_RESPS, FOCAL_PRESSURES, lag = 2)
    ## ----end

    ## ---- EDA Associations
    associations(type = "Discrete", FOCAL_RESPS, FOCAL_PRESSURES)
    associations(type = "Discrete", FOCAL_RESPS, FOCAL_PRESSURES, lag = 1)
    associations(type = "Discrete", FOCAL_RESPS, FOCAL_PRESSURES, lag = 2)
    
    ## for (j in FOCAL_RESPS) {
    ##     cat(j, "\n")
    ##     ylab <- units_lookup %>%
    ##         filter(Measure == {{j}}) %>%
    ##         pull(TableLabel)
    ##     for (i in 1:nrow(FOCAL_PRESSURES)) {
    ##         MEASURE <- FOCAL_PRESSURES[i, "Measure"][[1]]
    ##         settings <- readRDS(paste0(DATA_PATH, "summarised/settings__", j, "__", MEASURE,".RData"))
    ##         list2env(settings, envir = globalenv())
    ##         cat("\t",MEASURE, "\n")
    ##         xlab <- units_lookup %>%
    ##             filter(Measure == {{MEASURE}}) %>% 
    ##             pull(TableLabel)
            
    ##         data.EDA <- readRDS(file = paste0(DATA_PATH, "summarised/data.EDA__", j, "__", MEASURE, ".RData"))
    ##             ## mutate(Value = ifelse(Value==0, 0.001, Value)) %>%
    ##         g <- data.EDA %>%
    ##             ggplot(aes(y = Value, x = !!sym(MEASURE))) +
    ##             geom_point() +
    ##             ## geom_smooth(method = 'gam', formula = y ~ s(x, bs = 'ps'),
    ##             ##             method.args = list(method = "REML",
    ##             ##                                family = Gamma(link = 'log'))) +
    ##             ## scale_y_continuous(trans = scales::pseudo_log_trans()) 
    ##             geom_smooth(method = 'gam', formula = y ~ s(x, bs = 'ps'),
    ##                         method.args = list(method = "REML",
    ##                                            family = gaussian()),
    ##                         color = "blue",
    ##                         fill = "#0000FF50") +
    ##             ## scale_y_log10(scales::label_parse()(lab)) +
    ##             ## scale_x_continuous(scales::label_parse()(xlab)) +
    ##             scale_y_log10(ylab) +
    ##             scale_x_continuous(xlab) +
    ##             theme_bw() +
    ##             theme(strip.text.y = element_text(angle = 0),
    ##                   strip.background = element_rect(fill = "#446e9b50"),
    ##                   panel.spacing.x = unit("0.5", "cm"))
    ##         FACETS <- data.EDA %>% pull(ZoneName) %>% unique %>% length()
    ##         NCOL <- wrap_dims(FACETS, ncol = 3)[2]
    ##         NROW <- wrap_dims(FACETS, ncol = 3)[1]

    ##         p <- patchwork::wrap_plots( 
    ##                       g + facet_wrap(~ZoneName, scales='free', ncol = 3),
    ##                       g,
    ##                       ncol = 1,
    ##                       heights = c(2*NROW, 2*2)
    ##                       )
    ##         ggsave(filename = paste0(FIGS_PATH, "/gamPlots__",j,"__",MEASURE,".png"),
    ##                p,
    ##                width = NCOL * 4,
    ##                height = NROW * 4,
    ##                dpi = 72)


            
    ##         ## Fit models
    ##         data.EDA.mod <- data.EDA %>%
    ##             mutate(DV = !!sym(MEASURE)) %>%
    ##             { if (max(.$DV) > 1e5) mutate(.,DV = DV/1e5) else . } %>%
    ##             group_by(Zone, ZoneName) %>%
    ##             summarise(data = list(cur_data_all()), .groups = "drop") %>%
    ##             mutate(Mod = map(.x = data,
    ##                              .f = ~fitModels(.x)),
    ##                    ## ModLin = map(.x = data,
    ##                    ##           .f = ~fitModel(.x, type = 'linear')),
    ##                    ## GAM = map(.x = data,
    ##                    ##           .f = ~fitModelGAM(.x)),
    ##                    Sum = map(.x = Mod,
    ##                              .f = ~ {if(!is.null(.x))
    ##                                          summary(.x)
    ##                                      else NULL}),
    ##                    Params = map(.x = Mod,
    ##                                 .f = ~ {if(!is.null(.x))
    ##                                             parameters::model_parameters(.x)
    ##                                         else NULL}),
    ##                    Params2 = map(.x = Mod,
    ##                                 .f = ~ {if(!is.null(.x))
    ##                                             broom.mixed::tidy(.x, conf.int = TRUE)
    ##                                         else NULL}),
    ##                    R2 = map(.x = Mod,
    ##                             .f = ~ {if(!is.null(.x))
    ##                                         MuMIn::r.squaredGLMM(.x)
    ##                                     else NULL}),
    ##                    DHARMa = map(.x = Mod,
    ##                                 .f = ~ {if(!is.null(.x))
    ##                                             DHARMa::simulateResiduals(.x, plot = FALSE)
    ##                                             else NULL}),
    ##                    DHARMa_uniform = map2(.x = DHARMa, .y = paste0(Zone, ". ", ZoneName),
    ##                                          .f = ~patchwork::wrap_elements(~DHARMa::testUniformity(.x)) +
    ##                                              ggtitle(.y)),
    ##                    DHARMa_quantiles = map2(.x = DHARMa, .y = paste0(Zone, ". ", ZoneName),
    ##                                            .f = ~{if(!is.null(.x))
    ##                                                       patchwork::wrap_elements(~DHARMa::testQuantiles(.x)) +
    ##                                                           ggtitle(.y)
    ##                                                       else NULL}
    ##                                           ),
    ##                    Emmeans = map2(.x = Mod,
    ##                                   .y = data,
    ##                                   .f = ~{if(!is.null(.x))
    ##                                             emmeansCalc(.x, .y, MEASURE)
    ##                                             else NULL}),
    ##                    Part = map2(.x = Mod,
    ##                                .y = data,
    ##                                .f = ~ {if(!is.null(.x))
    ##                                            partialPlots(.x, .y, j, MEASURE, ylab, xlab)
    ##                                            else NULL})
    ##                    ## Partial = map(.x = Mod,
    ##                    ##               .f = ~ggeffects::ggemmeans(.x, terms = ~ DV) %>% plot(add.data = TRUE) +
    ##                    ##                   scale_y_log10()) 
    ##                              )
    ##         saveRDS(data.EDA.mod %>% dplyr::select(Zone, ZoneName, Mod),
    ##                 file = paste0(DATA_PATH,"summarised/data.EDA.mod__",j,"__",MEASURE,".RData"))
                
    ##         p <- patchwork::wrap_plots(data.EDA.mod$DHARMa_uniform, ncol = NCOL) &
    ##             theme(plot.title = element_text(hjust = 0.5))
    ##         ggsave(filename = paste0(FIGS_PATH, "/DHARMa_unif__",j,"__",MEASURE,".png"),
    ##                p,
    ##                width = NCOL * 4,
    ##                height = NROW * 4,
    ##                dpi = 72)

    ##         p <- patchwork::wrap_plots(data.EDA.mod$DHARMa_quantiles, ncol = NCOL) &
    ##             theme(plot.title = element_text(hjust = 0.5))
    ##         ggsave(filename = paste0(FIGS_PATH, "/DHARMa_quant__",j,"__",MEASURE,".png"),
    ##                p,
    ##                width = NCOL * 4,
    ##                height = NROW * 4,
    ##                dpi = 72)
    ##         p <- patchwork::wrap_plots(data.EDA.mod$Part, ncol = NCOL) 
    ##         ggsave(filename = paste0(FIGS_PATH, "/partial__",j,"__",MEASURE,".png"),
    ##                p,
    ##                width = NCOL * 4,
    ##                height = NROW * 3,
    ##                dpi = 72)

    ##         data.EDA.mod.sum <- data.EDA.mod %>% dplyr::select(Zone, ZoneName, Params, Params2,R2, Emmeans)
    ##         saveRDS(data.EDA.mod.sum,
    ##                 file = paste0(DATA_PATH,"summarised/data.EDA.mod.sum__",j,"__",MEASURE,".RData"))
            

    ##         ## patchwork::wrap_plots(data.EDA.mod$Part)
    ##         ## patchwork::wrap_plots(data.EDA.mod$DHARMa_uniform) 
    ##         ## patchwork::wrap_plots(data.EDA.mod$DHARMa_uniform) & theme(plot.title = element_text(hjust = 0.5))
    ##         ## ## patchwork::wrap_plots(data.EDA.mod$DHARMa_uniform) &
    ##         ## ##     theme(plot.title = ggtext::element_markdown(hjust = 0.5))
    ##         ## patchwork::wrap_plots(data.EDA.mod$DHARMa_quantiles) & theme(plot.title = element_text(hjust = 0.5))
    ##         ## patchwork::wrap_plots(data.EDA.mod$DHARMa_quantiles) +
    ##         ##     ggtitle('shdf') +
    ##         ##     patchwork::plot_annotation(tag_levels = 'A')
    ##         ## parameters::compare_models(data.EDA.mod$Mod,
    ##         ##                            column_names = data.EDA.mod$ZoneName)
    ##         ## parameters::compare_parameters(data.EDA.mod$Mod,
    ##         ##                            column_names = data.EDA.mod$ZoneName)
    ##         ## parameters::compare_parameters(data.EDA.mod$Mod,
    ##         ##                                column_names = data.EDA.mod$ZoneName,
    ##         ##                                style = "ci")
    ##         ## data.EDA.mod$Mod[[1]] %>% simulate_model(2)
    ##         ## data.EDA.mod[1,'Mod'][[1]][[1]] %>% summary()
    ##         ## data.EDA.mod[1,'Mod'][[1]][[1]] %>% model_parameters()
    ##         ## data.EDA.mod[1,'Mod'][[1]][[1]] %>% ci()
    ##         ## data.EDA.mod[1,'Mod'][[1]][[1]] %>% equivalence_test()
    ##         ## data.EDA.mod[1,'Mod'][[1]][[1]] %>% model_parameters(exponentiate = TRUE)
    ##         ## data.EDA.mod[1,'Sum'][[1]][[1]]
    ##         ## data.EDA.mod[1,'Params'][[1]][[1]]
    ##         ## data.EDA.mod[1,'R2'][[1]][[1]]
    ##         ## data.EDA.mod[1,'DHARMa'][[1]][[1]] %>% DHARMa::testQuantiles()
    ##         ## data.EDA.mod[1,'DHARMa'][[1]][[1]] %>% DHARMa::testUniformity()
    ##         ## data.EDA.mod[1,'DHARMa_uniform'][[1]][[1]]
    ##         ## data.EDA.mod[1,'Partial'][[1]][[1]]
    ##         ## data.EDA.mod[1,'Part'][[1]][[1]]
    ##         ## patchwork::wrap_elements( ~ plot(data.EDA.mod[1,'DHARMa'][[1]][[1]]))
    ##         ## data.EDA.mod[4,'Mod'][[1]][[1]] %>%  summary()
    ##         ## data.EDA.mod[4,'Mod'][[1]][[1]] %>%  ggemmeans(terms = "DV [100]")
            
    ##         ## mod <- glmmTMB::glmmTMB(Value ~ Ship_dry +
    ##         ##                             (1|WQ_SITE/ZoneName),
    ##         ##                         ## family = Gamma(link = 'log'),
    ##         ##                         family = glmmTMB::tweedie(link = 'log'),
    ##         ##                         data = data.EDA)
    ##         ## summary(mod)
    ##         ## mod.resid <- DHARMa::simulateResiduals(mod)
    ##         ## plot(mod.resid)
    ## ----end
}

## All aggregated samples ===========
{
    ## ---- EDA All associations data prep
    assoc_data_pred(data, type = "All", FOCAL_RESPS, FOCAL_PRESSURES)
    ## ----end

    ## ---- EDA All dual plots
    dual_plots(type = "All", FOCAL_RESPS, FOCAL_PRESSURES)
    dual_plots(type = "All", FOCAL_RESPS, FOCAL_PRESSURES, lag = 1)
    dual_plots(type = "All", FOCAL_RESPS, FOCAL_PRESSURES, lag = 2)
    ## ----end
    
    ## ---- EDA All Associations
    associations(type = "All", FOCAL_RESPS, FOCAL_PRESSURES)
    associations(type = "All", FOCAL_RESPS, FOCAL_PRESSURES, lag = 1)
    associations(type = "All", FOCAL_RESPS, FOCAL_PRESSURES, lag = 2)
    ## ----end
}



## library(gbm)

## mod <- gbm(Chla ~
##                TotalN +
##                TotalP +
##                TSS +
##                VSS +
##                Discharge +
##                ERP +
##                ERP_growth +
##                GRP +
##                GRP_change +
##                Catch_ERP +
##                POP +
##           Fire_TotalKm_WA,
##            data = data %>% filter(!is.na(Chla)),
##            n.trees = 1000,
##            interaction.depth = 9,
##            shrinkage = 0.01,
##            bag.fraction = 0.5,
##            cv.folds = 3
##            )

## n.trees <- gbm.perf(mod, method = 'OOB')
## summary(mod, n.trees = n.trees)
## plot(mod, 2)
