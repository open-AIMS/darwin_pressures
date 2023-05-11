## module load singularity
## singularity exec -B .:/home/project ../darwin_pressures.sif R
## ess-remote
source('../scripts/functions.R')

## ---- load lookups
units_lookup <- readRDS(file = paste0(DATA_PATH, "processed/units_lookup.RData"))
var_lookup <- readRDS(file = paste0(DATA_PATH, "processed/var_lookup.RData"))
spatial_lookup <- readRDS(file = paste0(DATA_PATH, "processed/spatial_lookup.RData"))

FOCAL_RESPS <- var_lookup %>%
  filter(Type == "Response") %>%
  pull(Measure) %>%
  unique()
FOCAL_PRESSURES <- var_lookup %>%
  filter(Type == "Pressure")
## ----end

## ---- getTables - trees
load(file = paste0(DATA_PATH, "modelled/formatted.tables.RData"))
## ----end

## ---- simpler_summary_tables
simpler_summary_table <- function(j, MEASURE, k, type = "Routine") {
    if (type == 'Routine') {
        file <- paste0(DATA_PATH,"summarised/data.EDA.routine.mod.sum__",
                       j,"__",MEASURE,k,".RData")
    } else if (type == "" | type == "Discrete") {
        file <- paste0(DATA_PATH,"summarised/data.EDA.mod.sum__",
                       j,"__",MEASURE,k,".RData")
    } else {
        file <- paste0(DATA_PATH,"summarised/data.EDA.all.mod.sum__",
                       j,"__",MEASURE,k,".RData")
    }

    if (!file.exists(file)) {
        return(cat(paste("\n\n", file, "does not exist.\n")))
    }
    data.EDA.mod.sum <- readRDS(file = file)
    data.EDA.mod.sum <- data.EDA.mod.sum %>%
        mutate(ZoneName = paste0(Zone,'.',ZoneName))
    
    if (all(sapply(data.EDA.mod.sum$Params, is.null)))
        return(cat(paste("\n\n no valid models resulted.\n")))

    R2 <- data.EDA.mod.sum %>%
        mutate(R2c = map(.x = R2, .f = ~ .x[nrow(.x),1])) %>%
        mutate(R2m = map(.x = R2, .f = ~ .x[nrow(.x),2])) %>%
        dplyr::select(ZoneName, R2c, R2m) %>%
        unnest(c(R2c, R2m)) %>%
        pivot_longer(cols = c(R2c, R2m),
                     names_to = "term") %>%
        mutate(effect = ifelse(term == 'R2c','fixed','Total')) %>%
        mutate(group = "") %>%
        mutate(Parameter = sprintf("% 0.3f",value)) %>%
        dplyr::select(-value) %>%
        suppressMessages() %>%
        suppressWarnings()

    Emmeans <- data.EDA.mod.sum %>% dplyr::select(ZoneName, Emmeans) %>%
        unnest(Emmeans) %>%
        group_by(ZoneName) %>%
        summarise(Emmeans = list(response), .groups = "drop")
    
    EM <- data.EDA.mod.sum %>% dplyr::select(ZoneName, Emmeans) %>%
        unnest(Emmeans) %>%
        ## ensure there are no Inf values
        mutate(response = ifelse(is.infinite(response), NA, response)) %>%
        {split(.$response, .$ZoneName)}

    EffectSize <- data.EDA.mod.sum %>%
        dplyr::select(ZoneName, R, CohenD) %>%
        unnest(c(R, CohenD)) %>%
        mutate(CohenD = ifelse(R<0, -1 * CohenD, CohenD),
               ES = ifelse(abs(R) < abs(CohenD), R, CohenD)) %>%
        mutate(across(c(R, CohenD, ES), ~round(.x, 3))) %>%
        rename(`Effect Size__R` = R,
               `Effect Size__CohenD` = CohenD,
               `Effect Size__ES` = ES
               ) 
    Trend <- data.EDA.mod.sum %>%
        dplyr::select(ZoneName, R) %>%
        unnest(c(R)) %>%
        mutate(Trend = ifelse(R > 0, "Positive", "Negative"))
    
    ## Remove any that have try-catch Params2 values
    wch <- sapply(data.EDA.mod.sum$Params2, function(x) any(class(x) != "try-error")) 
    data.EDA.mod.sum <- data.EDA.mod.sum[wch,]
    ## param_table <- data.EDA.mod.sum %>%
    ##     dplyr::select(ZoneName, Mod_Number, Params2) %>%
    ##     unnest(c(Mod_Number, Params2)) 
    param_table <- data.EDA.mod.sum %>%
        dplyr::select(ZoneName, Params2) %>%
        unnest(c(Params2)) %>%
        ## add group variable if it is not present (all non-hierarchical models)
        bind_rows(tibble(group = character()))

    mod_num <- data.EDA.mod.sum %>%
        dplyr::select(ZoneName, Mod_Number) %>%
        unnest(c(Mod_Number)) 
    
    param_table2 <-
        as_tibble(param_table) %>%
        dplyr::select(-one_of("edf", "ref.df")) %>%
        mutate(
            effect = ifelse(is.na(effect),
                           ifelse(str_detect(term, 's\\(WQ_SITE\\)'), "ran_pars", 'fixed'),
                            effect),
            group = ifelse(is.na(group),
                           ifelse(str_detect(term, 's\\(WQ_SITE\\)'), "WQ_SITE", NA),
                            group),
            term = str_replace(term, 'sd__', ""),
               term = str_replace(term, 's\\(WQ_SITE\\)', "Site (SD)"),
               estimate = ifelse(!is.na(statistic), statistic, estimate)) %>%
        ## mutate(Parameter = paste(insight::format_value(estimate,),
        ##                           insight::format_ci(conf.low, conf.high, ci = NULL))) %>%
        ## mutate(Parameter = ifelse(is.null(estimate),
        ##                           NA,
        ##                           sprintf("% 0.3f\n[% 2.3f,% 2.3f]", estimate,conf.low, conf.high))) %>%
        mutate(Parameter = sprintf("% 0.3f\n[% 2.3f,% 2.3f]", estimate,conf.low, conf.high)) %>%
        mutate(Parameter = str_replace_all(Parameter, "NA|NaN|Inf","")) %>%
        mutate(Parameter = str_replace_all(Parameter, "\\n\\[ , \\]","")) %>%
        mutate(Parameter = replace_na(Parameter, "")) %>%
        ## mutate(Parameter = str_glue("{format(estimate,justify = 'left', digits = 3, scientific = FALSE)}")) %>%
        dplyr::select(-std.error, -statistic, -p.value, -estimate, -conf.low, -conf.high) %>% 
        full_join(R2 %>% dplyr::select(-group)) %>%
        ## filter(!Stat %in% c('z', 'df_error', 'p', 'SE', 'CI')) %>%
        filter(!(term == "(Intercept)" & effect == "fixed")) %>% #dplyr::select(-Parameter) %>% 
        dplyr::select(-component) %>%
        mutate(term = paste0(group,term)) %>%
        ## arrange(ZoneName, effect, term) %>%
        mutate(term = str_replace(term, "WQ_SITE.*", "Site (SD)")) %>%
        dplyr::select(-group) %>%
        ## pivot_wider(id_cols = everything(),
        ##             names_from = Stat,
        ##             values_from = value)
                                        #unite(col = 'Parameter', Effects, Component, Group, Parameter, Stat,) %>%
        unite(col = 'Name', effect, term, sep = "__",na.rm = FALSE) %>%
                                        #unite(col = 'Name', Name, Parameter, sep = "__") %>%
        ## unite(col = 'Parameter', Name, Stat, sep = "__") %>%
        mutate(Name = str_replace_all(Name, "NA","")) %>% 
        mutate(Name = str_replace(Name, "poly\\(DV, 3\\)1", "DV (Linear)")) %>%
        mutate(Name = str_replace(Name, "poly\\(DV, 3\\)2", "DV (Quadratic)")) %>%
        mutate(Name = str_replace(Name, "poly\\(DV, 3\\)3", "DV (Cubic)")) %>%
        mutate(Name = str_replace(Name, "fixed", "Fixed")) %>%
        mutate(Name = str_replace(Name, "ran_pars", "Random")) %>%
        arrange(Name) %>%
        pivot_wider(id_cols = everything(),
                    names_from = Name,
                    values_from = Parameter) %>%
        arrange(ZoneName) %>%
        full_join(mod_num) %>%
        full_join(EffectSize) %>%
        dplyr::select(ZoneName, Mod_Number, everything()) %>% 
        rename(Zone = ZoneName, Model = Mod_Number) %>%
        suppressMessages() %>%
        suppressWarnings()

    param_table2 %>%
        full_join(Emmeans %>% rename(Zone = ZoneName)) %>%
        dplyr::rename(Total__Trend = Emmeans) %>%
        dplyr::mutate(Total__Trend = "") %>% 
                                        #dplyr::select(-Trend) %>%
        mutate(across(matches("DV|Year|Random"), ~replace_na(.x, " "))) %>%
        dplyr::select(Zone, Model,
                      matches("s\\(DV\\)"),
                      matches("__DV"),
                      matches(".*Linear.*"),
                      matches(".*Quadratic.*"),
                      matches(".*Cubic.*"),
                      matches("R2c"),
                      matches("Site"),
                      matches("R2m"),
                      everything()) %>%
        mutate(across(`Effect Size__R`,
                      ~cell_spec(.x, background = cohenRColours(.x)))
               ) %>%
        mutate(across(c(`Effect Size__CohenD`, `Effect Size__ES`),
                      ~cell_spec(.x, background = cohenDColours(.x)))
               ) %>%
        left_join(Trend %>% dplyr::select(Zone = ZoneName, Trend))
}

    
## ----end

## ---- makeTables - assoc
dfs <- vector('list')
l <- 0
for (type in c('Routine', 'Discrete')) {
    for (zones in c(TRUE, FALSE)) {
        for (j in FOCAL_RESPS) {
            lab <- units_lookup %>%
                filter(Measure == j) %>%
                pull(TableLabel) %>%
                str_replace_all("\\$","\\\\$")
            for (i in 1:nrow(FOCAL_PRESSURES)) {
                MEASURE <- FOCAL_PRESSURES[i, "Measure"][[1]]
                MEASURE_lab <- units_lookup %>%
                    filter(Measure == MEASURE) %>%
                    pull(TableLabel) %>% 
                    str_replace_all("\\$","\\\\$")
                for (k in 0:2) {
                    l <- l + 1
                    cat(paste0("\n##### Type:", type,
                               " zones: ", zones,
                               " FOCAL_RESP: ", j,
                               " i: ", i,
                               " k: ", k, "\n"))
                    if (k == 0) {
                        k <- ""
                        df <- simpler_summary_table(j, MEASURE, k, type = type) %>%
                            mutate(Type = type,
                                   include_lags = FALSE)
                    } else {
                        k <- paste0("_lag",k)
                        df <- simpler_summary_table(j, MEASURE, k, type = type)
                       if (is.null(df)) next 
                        df <- df %>% 
                            mutate(Type = type,
                                   include_lags = TRUE)
                    }
                    df <- df %>% filter(ifelse(zones,
                                               Zone != '.Whole Harbour',
                                               Zone == '.Whole Harbour') 
                                        ) %>%
                        mutate(Zones = zones,
                               Measure = MEASURE,
                               Response = j,
                               lag = str_remove(k, "_lag")) %>%
                        dplyr::select(Type, include_lags, lag, Zones, ZoneName = Zone,
                                      Measure, Response,
                                      R = `Effect Size__R`,
                                      R2 = `Fixed__R2c`,
                                      Trend) %>%
                        mutate(R = str_remove_all(R, "<[^>]*>"),
                               R = format(as.numeric(R), digits = 6),
                               ZoneName = str_remove(ZoneName, '^[0-9]?\\.'))
                    dfs[[l]] <- df
                    ## simpler_summary_table(j, MEASURE, k, type = 'All') %>%
                    
                    ## save_kable(file = paste0(TABS_PATH,"/tab.all__",j,"__", MEASURE,k,".html"),
                    ##            self_contained = F)
                }
            }
        }
    }
}
df <- do.call('rbind', dfs)

df <-
    df %>%
    left_join(units_lookup %>% dplyr::select(Response = Measure, TableLabel) %>% distinct()) %>%
    dplyr::select(-Response) %>% 
    mutate(
        R2 = as.numeric(as.character(R2)),
        R2trend = ifelse(Trend == 'Negative', -1, 1) * R2) %>%
    pivot_wider(id_cols = c(Type, include_lags, lag, Zones, ZoneName, Measure),
                names_from = TableLabel,
                values_from = R2trend) %>%
    left_join(units_lookup %>% dplyr::select(Measure, TableLabel) %>% distinct()) %>%
    dplyr::select(Type, include_lags, lag, Zones, ZoneName, TableLabel, everything(), -Measure) %>%
    group_by(Type, include_lags, Zones) %>%
    nest() %>%
    dplyr::rename(data2 = data)

save(df, file = paste0(DATA_PATH, "modelled/df.RData"))
load(file = paste0(DATA_PATH, "modelled/df.RData"))
## ----end



## ---- combine tables
## formatted.table.Corr are the trees
## df are the regression models
comb.tab <- formatted.table.Corr %>%
    left_join(df)
## comb.tab[2,'data'][[1]][[1]]
## comb.tab[2,'data2'][[1]][[1]]
## ----end

## ---- make comb tables old
CR1 <- colorRampPalette(colors = c("#de425b",
                            "#f48358",
                            "#fcbe6e",
                           "#FFFFFF",
                           "#FFFFFF",
                           "#FFFFFF",
                           "#c5d275", 
                           "#89b050",
                           "#488f31"))
## comb.tab <- formatted.table.Corr %>%
comb.tab <- formatted.table.R2 %>%
    unnest(data) %>%
    ungroup() %>%
    group_by(Type, Zones, include_lags, ZoneName) %>%
    nest() %>%
    left_join(df %>%
              unnest(data2) %>%
              mutate(TableLabel = ifelse(lag >0, paste0(TableLabel, " (lag ", lag,")"), TableLabel)) %>%
              ungroup() %>%
              group_by(Type, Zones, include_lags, ZoneName) %>%
              nest() %>%
              rename(data2 = data)
              )

comb.tab1 <- comb.tab %>%
    mutate(data.t = map(.x = data,
                        .f = ~ .x %>% 
                            pivot_longer(cols = c(-TableLabel)) %>%
                            mutate(name = factor(name),
                                   name = str_replace_all(name, "<br>", " "),
                                   TableLabel = factor(TableLabel))),
           data2.t = map(.x = data2,
                        .f = ~ .x %>% 
                            pivot_longer(cols = c(-TableLabel, -lag)) %>%
                            mutate(name = factor(name),
                                   TableLabel = factor(TableLabel))),
           data3.t = map2(.x = data.t, .y = data2.t,
                         .f = ~ .x %>% left_join(.y %>% dplyr::rename(value2 = value)))
           ) %>%
    mutate(data.p = map(.x = data,
                      .f = ~ .x %>% 
                          pivot_longer(cols = c(-TableLabel)) %>%
                          mutate(
                              name = str_replace_all(name, "<br>", " "),
                              name = factor(name),
                              TableLabel = factor(TableLabel)) %>% 
                          mutate(X = as.numeric(name),
                                 Y = as.numeric(TableLabel)) %>%
                          group_by(name, TableLabel, X, Y) %>% 
                          nest() 
                      ),
           data2.p = map(.x = data2,
                      .f = ~ .x %>% 
                          pivot_longer(cols = c(-TableLabel, -lag)) %>%
                          mutate(name = factor(name),
                                   TableLabel = factor(TableLabel)) %>% 
                            mutate(X = as.numeric(name),
                                   Y = as.numeric(TableLabel)) %>%
                            group_by(name, TableLabel, X, Y) %>% 
                            nest() 
                      ),
           data3.p = map2(.x = data.p, .y = data2.p,
                        .f = ~ .x %>% left_join(.y %>% ungroup() %>% dplyr::select(TableLabel, name, data2 = data)) %>%
                            mutate(name = factor(name),
                                   TableLabel = factor(TableLabel)) %>% 
                            mutate(X = as.numeric(name),
                                   Y = as.numeric(TableLabel)) 
                        )
           )  %>%
    mutate(data4.p = map(.x = data3.p,
                   .f = ~ {
                       .x %>% mutate(A = purrr::pmap(.l = list(X, Y, data),
                                                     .f = ~ data.frame(x = c(-0.5, 0.5, -0.5, -0.5),
                                                                       y = c(-0.5, 0.5, 0.5, -0.5),
                                                                       value = ..3$value[1]
                                                                       )),
                                     B = purrr::pmap(.l = list(X, Y, data2),
                                                     .f = ~ {
                                                         if (is.null(..3)) {
                                                             data.frame(x = NA, y = NA, value = NA)
                                                         } else {
                                                             data.frame(x = c(-0.5, 0.5, 0.5, -0.5),
                                                                        y = c(-0.5, -0.5, 0.5, -0.5),
                                                                        value = ..3$value[1]
                                                                        )
                                                         }
                                                     }
                                                     )
                                     )
                   }
                   )
           )

comb.tab2 <- comb.tab1 %>%
    mutate(
        Data = map2(.x = data3.t, .y = data4.p,
                    .f = ~ .x %>% left_join(.y %>% dplyr::select(-data, -data2, -X, -Y)) %>%
                        mutate(value2 = as.numeric(value2)) %>% 
                        group_by(TableLabel) %>%
                        mutate(Sum = sum(c(abs(value), abs(value2)), na.rm = TRUE)) %>%
                        ungroup() %>%
                        arrange(desc(Sum), name) %>%
                        mutate(TableLabel = factor(TableLabel, levels = rev(unique(TableLabel))),
                               name = factor(name, levels = unique(name)),
                               X = as.numeric(name),
                               Y = as.numeric(TableLabel)) %>%
                        droplevels() %>%
                        group_by(TableLabel, name) %>%
                        mutate(NN = 1:n()) 
                    ),
        Data = map(.x = Data,
                   .f = ~ {
                       if (nrow(.x %>% filter(NN == 2))>0) {
                           .x$value2[str_detect(.x$TableLabel, "^Catchment Estimated resident population") &
                                     .x$NN ==1] =
                               .x$value2[str_detect(.x$TableLabel, '^Estimated resident population') &
                                         .x$NN ==2]
                       }
                       .x %>% filter(NN!=2)
                   }),
        Data = map(.x = Data, .y = ZoneName,
                   .f = ~ .x %>% mutate(ZoneName = .y)),
        Data = map(.x = Data,
                   .f = ~ .x %>% 
                        group_by(TableLabel) %>%
                        mutate(Sum = sum(c(abs(value), abs(value2)), na.rm = TRUE)) %>%
                        ungroup() %>%
                        arrange(desc(Sum), name) %>%
                        mutate(TableLabel = factor(TableLabel, levels = rev(unique(TableLabel))),
                               name = factor(name, levels = unique(name)),
                               X = as.numeric(name),
                               Y = as.numeric(TableLabel)) %>%
                       droplevels() %>%
                       filter(Y > max(Y)-20) %>%
                       mutate(TableLabel = factor(TableLabel, levels = rev(unique(TableLabel))),
                              name = factor(name, levels = unique(name)),
                              X = as.numeric(name),
                              Y = as.numeric(TableLabel)) %>%
                       droplevels()
                   ),
        g = map(.x = Data,
                .f = ~ {
                    maxY <- max(.x$Y)
                    .x %>%
                        mutate(name = str_wrap(name, 16)) %>%
                        mutate(name = str_replace_all(name, '<br>', ' ')) %>%
                        ggplot(aes(y = TableLabel, x = name)) +
                        geom_blank() +
                        geom_polygon(data = .x %>% dplyr::select(-value) %>% unnest(A),
                                     aes(y = Y + y, x = X + x,
                                         fill = as.numeric(sqrt(abs(value))*ifelse(value<0, -1, 1)), group = interaction(X, Y)),
                                     colour = 'grey') +
                        geom_polygon(data = .x %>% dplyr::select(-value) %>% unnest(B),
                                     aes(y = Y + y, x = X + x,
                                         fill = as.numeric(sqrt(abs(value))*ifelse(value<0, -1, 1)), group = interaction(X, Y)),
                                     colour = 'grey') +
                        annotate(geom='text', x = 0.75+0.5, y = -0.7, label = 'GLM/GAM') +
                        annotate(geom='text', x = 0.3+0.5, y = -0.3, label = 'GBM') +
                        annotate(geom='polygon', y = c(-1, -1, 0, -1),
                                                      x = c(0.5, 1.5, 1.5, 0.5), fill = NA, colour = 'grey') +
                        annotate(geom='polygon', y = c(-1, 0, 0, -1),
                                                      x = c(0.5, 1.5, 0.5, 0.5), fill = NA, colour = 'grey') +
                        geom_text(aes(label = abs(value)), nudge_x = -0.3, nudge_y = 0.1) +
                        geom_text(aes(label = abs(value2)), nudge_x = 0.3, nudge_y = -0.1) +
                        geom_segment(data = NULL, aes(y = 0.5, yend = 0.5, x = 6.7, xend = -3)) +
                        geom_segment(data = NULL, aes(y = maxY+0.5, yend = maxY+0.5, x = 6.7, xend = -3)) +
                        geom_segment(data = NULL, aes(y = maxY+0.5+3, yend = maxY+0.5+3, x = 6.7, xend = -3)) +
                        scale_x_discrete(position = 'top') +
                        scale_fill_stepsn("Correlation", breaks = seq(-1,1, by = 0.2),
                                          colours = CR1(10), na.value = "#FFFFFF",
                                          limits = c(-1, 1)) +
                        coord_cartesian(expand = 0, clip = 'off', xlim = c(0.5,6.5),
                                        ylim = c(0.5,maxY+0.5)) +
                        theme_bw() +
                        facet_grid(ZoneName ~ .) +
                        theme(axis.title = element_blank(),
                              axis.ticks.length = unit(0, 'mm'),
                              panel.background = element_blank(),
                              panel.border = element_blank(),
                              panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              axis.line = element_blank(),
                              axis.text = element_text(size = rel(1.1)),
                              axis.text.y = element_text(hjust = 0),
                              legend.position = 'bottom', legend.direction = 'horizontal',
                              legend.key.width = unit(20, 'mm'),
                              strip.text = element_text(size = rel(1.25))
                              ) +
                        guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))
                }
                )
        )


comb.tab2 <- comb.tab2 %>%
    mutate(Label = paste0("summary_table__", Type, "_", ZoneName, "__", ifelse(include_lags, 'lags', 'noLags')))

purrr::walk2(.x = comb.tab2$g, .y = comb.tab2$Label,
             .f = ~ ggsave(filename = paste0(FIGS_PATH, "/", .y, ".png"),
                           .x,
                           width = 15, height = 8))
                           
## ----end
if (1==2) {
## ---- make comb tables old
CR1 <- colorRampPalette(colors = c("#de425b",
                            "#f48358",
                            "#fcbe6e",
                           "#FFFFFF",
                           "#FFFFFF",
                           "#FFFFFF",
                           "#c5d275", 
                           "#89b050",
                           "#488f31"))

comb.tab1 <- comb.tab %>%
    mutate(data.t = map(.x = data,
                        .f = ~ .x %>% 
                            pivot_longer(cols = c(-TableLabel, -ZoneName)) %>%
                            mutate(name = factor(name),
                                   name = str_replace_all(name, "<br>", " "),
                                   TableLabel = factor(TableLabel))),
           data2.t = map(.x = data2,
                        .f = ~ .x %>% 
                            pivot_longer(cols = c(-TableLabel, -lag, -ZoneName)) %>%
                            mutate(name = factor(name),
                                   TableLabel = factor(TableLabel))),
           data3.t = map2(.x = data.t, .y = data2.t,
                         .f = ~ .x %>% left_join(.y %>% dplyr::rename(value2 = value)))
           ) %>%
    mutate(data.p = map(.x = data,
                      .f = ~ .x %>% 
                          pivot_longer(cols = c(-TableLabel, -ZoneName)) %>%
                          mutate(
                              name = str_replace_all(name, "<br>", " "),
                              name = factor(name),
                              TableLabel = factor(TableLabel)) %>% 
                          mutate(X = as.numeric(name),
                                 Y = as.numeric(TableLabel)) %>%
                          group_by(name, TableLabel, X, Y) %>% 
                          nest() 
                      ),
           data2.p = map(.x = data2,
                      .f = ~ .x %>% 
                          pivot_longer(cols = c(-TableLabel, -lag, -ZoneName)) %>%
                          mutate(name = factor(name),
                                   TableLabel = factor(TableLabel)) %>% 
                            mutate(X = as.numeric(name),
                                   Y = as.numeric(TableLabel)) %>%
                            group_by(name, TableLabel, X, Y) %>% 
                            nest() 
                      ),
           data3.p = map2(.x = data.p, .y = data2.p,
                        .f = ~ .x %>% left_join(.y %>% ungroup() %>% dplyr::select(TableLabel, name, data2 = data)) %>%
                            mutate(name = factor(name),
                                   TableLabel = factor(TableLabel)) %>% 
                            mutate(X = as.numeric(name),
                                   Y = as.numeric(TableLabel)) 
                        )
           ) %>%
    mutate(data4.p = map(.x = data3.p,
                   .f = ~ {
                       .x %>% mutate(A = purrr::pmap(.l = list(X, Y, data),
                                                     .f = ~ data.frame(x = c(-0.5, 0.5, -0.5, -0.5),
                                                                       y = c(-0.5, 0.5, 0.5, -0.5),
                                                                       value = ..3$value[1]
                                                                       )),
                                     B = purrr::pmap(.l = list(X, Y, data2),
                                                     .f = ~ {
                                                         if (is.null(..3)) {
                                                             data.frame(x = NA, y = NA, value = NA)
                                                         } else {
                                                             data.frame(x = c(-0.5, 0.5, 0.5, -0.5),
                                                                        y = c(-0.5, -0.5, 0.5, -0.5),
                                                                        value = ..3$value[1]
                                                                        )
                                                         }
                                                     }
                                                     )
                                     )
                   }
                   )
           )

comb.tab1
## Discrete, no zones, no lags
### trees
comb.tab1[2,'data.t'][[1]][[1]]
comb.tab1[2,'data.p'][[1]][[1]]
### gam
comb.tab1[2,'data2.t'][[1]][[1]]

comb.tab1[2,'data3.t'][[1]][[1]] %>% filter(TableLabel == 'Total P (t)')

comb.tab1[2,'data.p'][[1]][[1]]
comb.tab1[2,'data2.p'][[1]][[1]]
comb.tab1[2,'data2.p'][[1]][[1]][1,'data'][[1]][[1]]
comb.tab1[2,'data3.p'][[1]][[1]]
comb.tab1[2,'data3.p'][[1]][[1]] %>% filter(TableLabel == "Total P (t)")


comb.tab1[1,'data2'][[1]][[1]] %>% filter(ZoneName == 'Middle Arm')
comb.tab1[1,'data2'][[1]][[1]] %>% filter(ZoneName == 'Middle Arm') %>% pull(TableLabel) %>% unique()
comb.tab1[1,'data3.t'][[1]][[1]] %>% filter(ZoneName == 'Middle Arm') %>% pull(TableLabel) %>% unique()
comb.tab1[1,'data3.t'][[1]][[1]] %>% filter(ZoneName == 'Middle Arm') %>% pull(TableLabel) %>% levels()


comb.tab1[1,'data4.p'][[1]][[1]] %>% filter(TableLabel == 'Total building activity ($B)') %>% `[`(1,'data') %>% `[[`(1) %>% `[[`(1) 

comb.tab1[1,'data4.p'][[1]][[1]] %>% filter(TableLabel == 'Total building activity ($B)') %>% `[`(1,'data2') %>% `[[`(1) %>% `[[`(1) 

comb.tab1[1,'data4.p'][[1]][[1]] %>% filter(TableLabel == 'Total building activity ($B)') 
comb.tab1[1,'data4.p'][[1]][[1]][1,'data'][[1]][[1]] %>% filter(ZoneName == 'Middle Arm') 


x <- comb.tab1[1,'data3.t'][[1]][[1]] %>% filter(TableLabel == 'Total building activity ($B)')
y <- comb.tab1[1,'data4.p'][[1]][[1]] %>% filter(TableLabel == 'Total building activity ($B)')
x %>% left_join(y %>% dplyr::select(-data, -data2, -X, -Y)) %>%
    as.data.frame()

comb.tab2 <- comb.tab1 %>%
    mutate(
        Data = map2(.x = data3.t, .y = data4.p,
                    .f = ~ .x %>% left_join(.y %>% dplyr::select(-data, -data2, -X, -Y)) %>%
                        mutate(value2 = as.numeric(value2)) %>% 
                        group_by(ZoneName, TableLabel) %>%
                        mutate(Sum = sum(c(abs(value), abs(value2)), na.rm = TRUE)) %>%
                        ungroup() %>%
                        arrange(desc(Sum), name) %>%
                        mutate(TableLabel = factor(TableLabel, levels = rev(unique(TableLabel))),
                               name = factor(name, levels = unique(name)),
                               X = as.numeric(name),
                               Y = as.numeric(TableLabel)) %>%
                        droplevels()
                    ),
        g = map(.x = Data,
                .f = ~ {
                    maxY <- max(.x$Y)
                    .x %>%
                        mutate(name = str_wrap(name, 16)) %>%
                        mutate(name = str_replace_all(name, '<br>', ' ')) %>%
                        ggplot(aes(y = TableLabel, x = name)) +
                        geom_blank() +
                        geom_polygon(data = .x %>% dplyr::select(-value) %>% unnest(A),
                                     aes(y = Y + y, x = X + x,
                                         fill = as.numeric(value), group = interaction(X, Y)),
                                     colour = 'grey') +
                        geom_polygon(data = .x %>% dplyr::select(-value) %>% unnest(B),
                                     aes(y = Y + y, x = X + x,
                                         fill = as.numeric(value), group = interaction(X, Y)),
                                     colour = 'grey') +
                        annotate(geom='text', x = 0.75, y = -0.6, label = 'GLM/GAM') +
                        annotate(geom='text', x = 0.3, y = -0.4, label = 'GBM') +
                        annotate(geom='polygon', y = c(-1, -1, 0, -1),
                                                      x = c(0, 1, 1, 0), fill = NA, colour = 'grey') +
                        annotate(geom='polygon', y = c(-1, 0, 0, -1),
                                                      x = c(0, 1, 0, 0), fill = NA, colour = 'grey') +
                        geom_text(aes(label = value), nudge_x = -0.3, nudge_y = 0.1) +
                        geom_text(aes(label = value2), nudge_x = 0.3, nudge_y = -0.1) +
                        ## geom_segment(data = NULL, aes(y = 18, yend = 18, x = 6.5, xend = -3)) +
                        geom_segment(data = NULL, aes(y = 0.5, yend = 0.5, x = 6.5, xend = -3)) +
                        ## geom_segment(data = NULL, aes(y = 16.5, yend = 16.5, x = 6.5, xend = -3)) +
                        scale_x_discrete(position = 'top') +
                        scale_fill_stepsn("Correlation", breaks = seq(-1,1, by = 0.2),
                                          colours = CR1(10), na.value = "#FFFFFF",
                                          limits = c(-1, 1)) +
                        coord_cartesian(expand = 0, clip = 'off', xlim = c(0.5,6.5),
                                        ylim = c(0.5,maxY+0.5)) +
                        theme_bw() +
                        facet_grid(ZoneName ~ .) +
                        theme(axis.title = element_blank(),
                              axis.ticks.length = unit(0, 'mm'),
                              panel.background = element_blank(),
                              panel.border = element_blank(),
                              panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              axis.line = element_blank(),
                              axis.text = element_text(size = rel(1.1)),
                              axis.text.y = element_text(hjust = 0),
                              legend.position = 'bottom', legend.direction = 'horizontal',
                              legend.key.width = unit(20, 'mm'),
                              strip.text = element_text(size = rel(1.25))
                              ) +
                        guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))
                }
                )
        )
    

        ## g = map2(.x = data3.t, .y = data4.p,
        ##             .f = ~ {
        ##                 maxY <- max(.y$Y)
        ##                 .x %>%
        ##                     mutate(name = str_wrap(name, 16)) %>%
        ##                     mutate(name = str_replace_all(name, '<br>', ' ')) %>%
        ##                     ggplot(aes(y = TableLabel, x = name)) +
        ##                     geom_blank() +
        ##                     geom_polygon(data = .y %>% unnest(A),
        ##                                  aes(y = y, x=x,
        ##                                      fill = as.numeric(value), group = interaction(X, Y)),
        ##                                  colour = 'grey') +
        ##                     ## if (nrow(.y)>0)
        ##                     geom_polygon(data = .y %>% unnest(B),
        ##                                  aes(y = y, x=x,
        ##                                      fill = as.numeric(value), group = interaction(X, Y)),
        ##                                  colour = 'grey') +
        ##                     geom_text(aes(label = value), nudge_x = -0.3, nudge_y = 0.1) +
        ##                     geom_text(aes(label = value2), nudge_x = 0.3, nudge_y = -0.1) +
        ##                     geom_segment(data = NULL, aes(y = 18, yend = 18, x = 6.5, xend = -3)) +
        ##                     geom_segment(data = NULL, aes(y = 0.5, yend = 0.5, x = 6.5, xend = -3)) +
        ##                     geom_segment(data = NULL, aes(y = 16.5, yend = 16.5, x = 6.5, xend = -3)) +
        ##                     scale_x_discrete(position = 'top') +
        ##                     scale_fill_stepsn("Correlation", breaks = seq(-1,1, by = 0.2),
        ##                                       colours = CR1(10), na.value = "#FFFFFF",
        ##                                       limits = c(-1, 1)) +
        ##                     coord_cartesian(expand = 0, clip = 'off', xlim = c(0.5,6.5),
        ##                                     ylim = c(0.5,maxY+0.5)) +
        ##                     theme_bw() +
        ##                     facet_grid(ZoneName ~ .) +
        ##                     theme(axis.title = element_blank(),
        ##                           axis.ticks.length = unit(0, 'mm'),
        ##                           panel.background = element_blank(),
        ##                           panel.border = element_blank(),
        ##                           PANEL.GRID.MAJOR = ELEMENT_BLANK(),
        ##                           PANEL.GRID.MINOR = ELEMENT_BLANK(),
        ##                           AXIS.LINE = ELEMENT_BLANK(),
        ##                           AXIS.TEXT = ELEMENT_TEXT(SIZE = REL(1.1)),
        ##                           AXIS.TEXT.Y = ELEMENT_TEXT(HJUST = 0),
        ##                           LEGEND.POSITION = 'BOTTOM', LEGEND.DIRECTION = 'HORIZONTAL',
        ##                           LEGEND.KEY.WIDTH = UNIT(20, 'MM'),
        ##                           STRIP.TEXT = ELEMENT_TEXT(SIZE = REL(1.25))
        ##                           ) +
        ##                     GUIDES(FILL = GUIDE_COLOURBAR(TITLE.POSITION = "TOP", TITLE.HJUST = 0.5))
        ##             }
        ##             )
        ##    )

## COMB.TAB2[1,'G'][[1]][[1]]
## COMB.TAB2[2,'DATA3.P'][[1]][[1]][1,'DATA'][[1]][[1]]
## COMB.TAB2[2,'DATA3.P'][[1]][[1]][1,'DATA2'][[1]][[1]]

## COMB.TAB2[2,'G'][[1]][[1]]

## Discrete, Zones, No lags
for (r in 1:9) {
    ggsave(filename = paste0(FIGS_PATH, '/summary_table_discrete__Zones_noLags_part',r,'.png'),
           comb.tab2[1,'g'][[1]][[1]] + ggforce::facet_grid_paginate(ZoneName ~ ., ncol = 1, nrow = 1, page = r, space = 'fixed', scales = 'fixed'),
           width = 15, height = 8)
}

ggsave(filename = paste0(FIGS_PATH, '/summary_table_discrete__Zones_noLags_part2.png'),
       comb.tab2[1,'g'][[1]][[1]] + ggforce::facet_grid_paginate(ZoneName ~ ., ncol = 1, nrow = 1, page = 2),
       width = 15, height = 30)

## Discrete, No zones, No lags
ggsave(filename = paste0(FIGS_PATH, '/summary_table_discrete__WholeHarbour_noLags.png'),
       comb.tab2[2,'g'][[1]][[1]],
       width = 15, height = 8)

## Routine, No zones, No lags
ggsave(filename = paste0(FIGS_PATH, '/summary_table_routine__WholeHarbour_noLags.png'),
       comb.tab2[5,'g'][[1]][[1]],
       width = 15, height = 8)


## ----end
}
