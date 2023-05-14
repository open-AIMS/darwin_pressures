## ---- loadPackages
library(knitr)
library(tidyverse)
library(readxl)
library(lubridate)
library(glmmTMB)
library(performance)
library(emmeans)
library(patchwork)
library(sf)
library(ggsflabel)
library(foreach)
library(doParallel)
library(furrr)
library(kableExtra)
library(gbm)
library(skimr)
library(flextable)
library(officedown)
library(skimr)
sf_use_s2(FALSE)
## ----end

## ---- preparePaths
DATA_PATH <<- "../data/"
OUTPUT_PATH <<- "../output/"
FIGS_PATH <<- paste0(OUTPUT_PATH, "figures")
TABS_PATH <<- paste0(OUTPUT_PATH, "tables")

if (!dir.exists(DATA_PATH)) dir.create(DATA_PATH)
if (!dir.exists(paste0(DATA_PATH,"primary"))) dir.create(paste0(DATA_PATH, "primary"))
if (!dir.exists(paste0(DATA_PATH,"processed"))) dir.create(paste0(DATA_PATH, "processed"))
if (!dir.exists(paste0(DATA_PATH,"modelled"))) dir.create(paste0(DATA_PATH, "modelled"))
if (!dir.exists(paste0(DATA_PATH,"summarised"))) dir.create(paste0(DATA_PATH, "summarised"))

if (!dir.exists(OUTPUT_PATH)) dir.create(OUTPUT_PATH)
if (!dir.exists(FIGS_PATH)) dir.create(FIGS_PATH)
if (!dir.exists(TABS_PATH)) dir.create(TABS_PATH)
## ----end

## ---- set up cores function
set_cores <- function() {
    if (detectCores() < 9) {
        registerDoParallel(cores=4)
    } else {
        registerDoParallel(cores=20)
    }
}
## ----end

## ---- set up cores furrr function
set_cores_furrr <- function() {
    if (availableCores() < 9) {
        plan(multicore, workers = 4)
    } else {
        plan(multicore, workers = 20)
    }
}
## ----end

## ---- EDA function
EDA <- function(x, var) {
    units_lookup <- readRDS(file = paste0(DATA_PATH,
                                          "processed/units_lookup.RData"))
    lab <- units_lookup %>%
        filter(Measure == {{var}}) %>%
        pull(FigureLabel)
    year_range <- x %>%
        pull(Year) %>%
        range() %>%
        paste0("-01-01") %>%
        as.Date()
    x %>%
        mutate(ZoneName = forcats::fct_reorder(ZoneName, Zone)) %>% 
        ggplot(aes(y = value, x = as.Date(Date))) +
        ## geom_blank(aes(x = as.Date(paste0(Year,"-01-01")))) +
        geom_line(aes(group = interaction(Latitude, Longitude)),
                  stat = 'summary', fun = "mean", alpha = 0.1) + 
        geom_point(alpha = 0.1) +
        geom_smooth(
            colour = "orange",
            fill = "#FFA50050",
            method = 'gam', formula = y ~ s(x, bs = 'ps'),
            ## method.args = list(family = Gamma(link = "log"),
            method.args = list(family = gaussian(),
                               method = "REML")) +
        facet_wrap(interaction(Zone, ZoneName)~., scales = "free") +
        ## scale_y_continuous(scales::label_parse()(lab),
        ##                    trans = scales::log_trans()) +
        scale_y_log10(scales::label_parse()(lab)) +
        scale_x_date(limits = year_range) +
        theme_bw() +
        theme(strip.text.y = element_text(angle = 0),
              axis.title.x = element_blank(),
              strip.background = element_rect(fill = "#446e9b50"),
              panel.spacing.x = unit("0.5", "cm"))
}
## ----end

## ---- EDA pressures function
EDA_pressures <- function(dat, var) {
    units_lookup <- readRDS(file = paste0(DATA_PATH,
                                          "processed/units_lookup.RData"))
    lab <- units_lookup %>%
        filter(Measure == {{var}}) %>%
        pull(FigureLabel)
    lab <- str_replace_all(lab, "\\$", "'$'*")
    lab <- str_replace_all(lab, "\\%", "'%'")
    year_range <- dat %>%
        pull(Year) %>%
        range() %>%
        paste0("-01-01") %>%
        as.Date()
    preds <- dat %>%
        mutate(x = lubridate::decimal_date(as.Date(paste0(Year, "-01-01")))) %>%
        group_by(Catchment) %>%
        nest() %>%
        mutate(GAM = map(.x = data,
                         .f = function(d) {
                             if ((nrow(d) > 3) & (var(d$Value) > 0)) {
                                 mod <- mgcv::gam(Value ~ s(x, bs = 'ps', k = nrow(d)-1),
                                                  data = d,
                                                  method = "REML",
                                                  family = "gaussian")
                             } else {
                                 NULL
                             }
                         })) %>%
        mutate(Pred = map2(.x = data, .y = GAM,
                           .f = function(d, g) {
                               if ((nrow(d) > 3) & (var(d$Value) > 0)) {
                                   nd <- d %>% expand(x = full_seq(Year, 0.1)) %>%
                                       mutate(Date = lubridate::date_decimal(x))
                                   pred <- predict(g, newdata = nd, se.fit = TRUE)
                                   nd %>% mutate(Pred = pred$fit,
                                                lower = pred$fit - 2*pred$se.fit,
                                                upper = pred$fit + 2*pred$se.fit)
                               } else {
                                  NULL #d %>% mutate(Pred = NA, lower = NA, upper = NA)
                               }
                           })) %>%
        unnest(Pred)
    
    dat %>%
        ggplot(aes(y = Value, x = as.Date(paste0(Year,"-01-01")))) +
        geom_blank(aes(x = as.Date(paste0(Year,"-01-01")))) +
        geom_line(aes(group = SITE_ID),
                   stat = 'summary', fun = "mean", alpha = 0.1) + 
        geom_point(alpha = 0.5) +
        geom_ribbon(data = preds, aes(y = Pred, x = as.Date(Date), ymin = lower, ymax = upper), fill = "#A020F050") +
        geom_line(data = preds, aes(y = Pred, x = as.Date(Date)), colour = "purple", size = 1) +
        facet_wrap(Catchment~., scales = "free", ncol = 3) +
        scale_y_continuous(scales::label_parse()(lab)) +
        scale_x_date(limits = year_range) +
        theme_bw() +
        theme(strip.text.y = element_text(angle = 0),
              axis.title.x = element_blank(),
              strip.background = element_rect(fill = "#446e9b50"),
              panel.spacing.x = unit("0.5", "cm"))
}
## ----end

## ---- EDA associations file append function
file_append <- function(type) {
    case_when(
        type == "Discrete" ~ ".",
        type == "All" ~ ".all.",
        type == "Routine" ~ ".routine."
    )
}
## ----end
## ---- EDA associations file lag function
file_lag <- function(lag) {
    ifelse(is.null(lag), "", paste0(".",lag))
}
## ----end

## ---- EDA associations data prep function
assoc_data_pred <- function(data, type = "Discrete", FOCAL_RESPS, FOCAL_PRESSURES) {
    units_lookup <- readRDS(file = paste0(DATA_PATH,
                                          "processed/units_lookup.RData"))
    spatial_lookup <- readRDS(file = paste0(DATA_PATH,
                                            "processed/spatial_lookup.RData"))
    DATE_RANGE <- data %>% dplyr::select(Date, !!FOCAL_RESPS) %>%
        pivot_longer(cols = -Date) %>%
        filter(!is.na(value)) %>%
        pull(Date) %>% range()

    FILE_APPEND <- file_append(type)

    set_cores()
    
    for (j in FOCAL_RESPS) {
        lab <- units_lookup %>%
            filter(Measure == {{j}}) %>%
            pull(FigureLabel)
        cat(j, "\n")
        ## Save data
        data.EDA.resp <-
            data %>%
            {if (type == 'Discrete')
                 filter(., Source == 'Discrete')
             else .
            } %>%
            {if (type == 'Routine')
                 filter(., Type == 'Routine')
             else .
            } %>%
            droplevels() %>%
            filter(!is.na(!!sym(j))) %>%
            dplyr::select(WQ_ID, WQ_SITE, !!j, ZoneName, Zone, Year, Date) %>%
            distinct() %>%
            mutate(Value = !!sym(j),
                   Measure = j,
                   Value = ifelse(Value == 0, 0.01, Value))
        if (type == "All") {
            ## calculate discrete and CFM averages separately per ZoneName/Year
            ## then average the sources together per ZoneName/Year
            data.EDA.resp <- data %>%
                droplevels() %>%
                filter(!is.na(!!sym(j))) %>%
                group_by(ZoneName, Zone, Year, Source) %>%
                summarise(!!sym(j) := mean(!!sym(j)),
                          Date = mean(Date)) %>%
                ungroup() %>%
                group_by(ZoneName, Zone, Year) %>%
                summarise(!!sym(j) := mean(!!sym(j)),
                          Date = mean(Date)) %>%
                mutate(WQ_ID = 1, WQ_SITE = 1) %>%
                dplyr::select(WQ_ID, WQ_SITE, !!j, ZoneName, Zone, Year, Date) %>%
                distinct() %>%
                mutate(Value = !!sym(j),
                       Measure = j,
                       Value = ifelse(Value == 0, 0.01, Value)) %>%
                suppressMessages() %>%
                suppressWarnings()
        }
        
        saveRDS(data.EDA.resp,
                file = paste0(DATA_PATH, "summarised/data.EDA",
                              FILE_APPEND,"resp__", j, ".RData"))

        ## for (i in 1:nrow(FOCAL_PRESSURES)) {
        foreach(i = 1:nrow(FOCAL_PRESSURES)) %dopar% {
            MEASURE <- FOCAL_PRESSURES[i, "Measure"][[1]]
            ID <- FOCAL_PRESSURES[i, "ID"][[1]]
            SITE_ID <- FOCAL_PRESSURES[i, "SITE_ID"][[1]]
            if (is.na(SITE_ID)) SITE_ID <- NULL
            FACET <- FOCAL_PRESSURES[i, "Scale"][[1]]
            if (is.na(FACET)) FACET <- "Whole Harbour"
            cat("\t",MEASURE, "\n")

            ## save the above settings for future loops
            settings <- list(
                DATE_RANGE = DATE_RANGE,
                RESP = j,
                lab = lab,
                PRES = MEASURE,
                ID = ID,
                SITE_ID = SITE_ID,
                FACET = FACET
            )
            saveRDS(settings,
                    file = paste0(DATA_PATH, "summarised/settings",
                                  FILE_APPEND,"__", j, "__", MEASURE,".RData"))
            ## Save dual plot data
            if (j == FOCAL_RESPS[1]) {
                for (LAGS in  0:2) {
                    if (LAGS == 0) {
                        LAG_MEASURE <- MEASURE
                    } else {
                        LAG_MEASURE <- paste0(MEASURE,"_lag", LAGS)
                    }
                    data.EDA.pres <-
                        data %>%
                        filter(!is.na(!!sym(LAG_MEASURE))) %>%
                        dplyr::select(Catchment, ZoneName, Zone, Year,
                                      !!LAG_MEASURE, !!ID, !!SITE_ID) %>%
                        distinct() %>%
                        mutate(Value = !!sym(LAG_MEASURE)) %>%
                        filter(!is.na(Value)) %>%
                        mutate(Date = as.Date(paste0(Year, "-01-01")),
                               Measure = MEASURE,
                               Lag = LAGS) %>%
                        group_by(ZoneName) %>%
                        mutate(Min = min(Value, na.rm = TRUE),
                               Max = max(Value, na.rm = TRUE),
                               sValue = scales::rescale(Value,
                                                        from = c(unique(Min), unique(Max)),
                                                        to = c(1, 100))) %>%
                        ungroup() %>%
                        dplyr::select(-Zone) %>%
                        left_join(spatial_lookup %>% dplyr::select(Zone, ZoneName)) %>%
                        suppressMessages()

                    saveRDS(data.EDA.pres,
                            file = paste0(DATA_PATH, "summarised/data.EDA",
                                          FILE_APPEND,"pres__", MEASURE, "_lag", LAGS, ".RData"))
                }
            }

            ## Save associations data
            LAG_MEASURE <- paste0(MEASURE,"_lag", 1:2)
            data.EDA <-
                data %>%
                {if (type == 'Discrete')
                     filter(., Source == 'Discrete')
                 else .
                } %>%
                {if (type == 'Routine')
                     filter(., Type == 'Routine')
                 else .
                } %>%
                droplevels() %>%
                filter(!is.na(!!sym(j)), !is.na(!!sym(MEASURE))) %>%
                dplyr::select(WQ_ID, WQ_SITE, !!j, ZoneName, Zone, Year,
                              !!MEASURE, !!LAG_MEASURE,
                              !!ID, !!SITE_ID) %>%
                distinct() %>%
                mutate(Value = !!sym(j)) 
            if (type == "All") {
                data.EDA <- data %>%
                    droplevels() %>%
                    filter(!is.na(!!sym(j)), !is.na(!!sym(MEASURE))) %>%
                    group_by(ZoneName, Zone, Year, Source) %>%
                    summarise(across(c(!!j, !!MEASURE, !!LAG_MEASURE, !!ID, !!SITE_ID, Date),
                                     ~mean(.x, na.rm = TRUE))) %>%
                    ungroup() %>%
                    group_by(ZoneName, Zone, Year) %>%
                    summarise(across(c(!!j, !!MEASURE, !!LAG_MEASURE, !!ID, !!SITE_ID, Date),
                                     ~mean(.x, na.rm = TRUE))) %>%
                    mutate(WQ_ID = 1, WQ_SITE = 1) %>%
                    dplyr::select(WQ_ID, WQ_SITE, !!j, ZoneName, Zone, Year, Date,
                                  !!MEASURE, !!LAG_MEASURE,
                                  !!ID, !!SITE_ID) %>%
                    distinct() %>%
                    mutate(Value = !!sym(j),
                           Measure = j,
                           Value = ifelse(Value == 0, 0.01, Value)) %>%
                    suppressMessages() %>%
                    suppressWarnings()
            }
            saveRDS(data.EDA,
                    file = paste0(DATA_PATH, "summarised/data.EDA",
                                  FILE_APPEND,"__", j, "__", MEASURE, ".RData"))
        }
    }
}
## ----end
## ---- EDA associations dual plots function
dual_plots <- function(type = "Discrete", FOCAL_RESPS, FOCAL_PRESSURES, lag = NULL) {
    set_cores()    

    FILE_APPEND <- file_append(type)
    FILE_LAG <- file_lag(lag)

    if (is.null(lag)) {
        LAGS <- 0
    } else {
        LAGS = lag
    }
    
    for (j in FOCAL_RESPS) {
        cat(j, "\n")
        data.EDA.resp <- readRDS(file = paste0(DATA_PATH,
                                               "summarised/data.EDA",
                                               FILE_APPEND,"resp__", j, ".RData"))
        ## for (i in 1:nrow(FOCAL_PRESSURES)) {
        foreach (i = 1:nrow(FOCAL_PRESSURES)) %dopar% {
            MEASURE <- FOCAL_PRESSURES[i, "Measure"][[1]]
            settings <- readRDS(paste0(DATA_PATH, "summarised/settings",
                                       FILE_APPEND,"__", j, "__", MEASURE,".RData"))
            list2env(settings, envir = globalenv())
            cat("\t",MEASURE, "\n")

            data.EDA.pres <- readRDS(file = paste0(DATA_PATH, "summarised/data.EDA",
                                                   FILE_APPEND,"pres__", MEASURE, "_lag", LAGS, ".RData"))
            ## if (!is.null(lag)) data.EDA.pres <- data.EDA.pres %>%
            ##                        mutate(Year = Year + lag)

            g1 <- data.EDA.resp %>% group_by(ZoneName) %>%
                ## nest() %>%
                ## rename(Resp = data) %>%
                ## nest the data
                summarise(Resp = list(cur_data_all()), .groups = "drop") %>%
                mutate(Resp = map(.x = Resp,
                                  .f = ~.x %>% mutate(x = lubridate::decimal_date(Date),
                                                      ZoneName = paste0(Zone, ". ", ZoneName)))) %>%
                ## fit Gamma GAM
                mutate(GAM = map(.x = Resp,
                                 .f = ~fitSimpleGAM_resp(.x)),
                       ## model predictions
                       Pred = map2(.x = Resp,
                                   .y = GAM,
                                   .f = ~predSimpleGAM_resp(.x, .y)),
                       ## get min and max on raw scale
                       Resp = map2(.x = Resp,
                                   .y = Pred,
                                   .f = ~ .x %>% mutate(Min = min(c(Value,.y$lower), na.rm = TRUE),
                                                        Max = max(c(Value,.y$upper), na.rm = TRUE))),
                       ## log-transform Value, Min and Max
                       Resp = map(.x = Resp,
                                  .f = ~ .x %>% mutate(across(c(Value, Min, Max),
                                                              .f = list(l = log),
                                                              .names = "{.fn}{.col}"))),
                       ## Rescale the log Values
                       Resp = map(.x = Resp,
                                  .f = ~ .x %>%
                                      mutate(slValue = scales::rescale(lValue,
                                                                       from = c(unique(lMin), unique(lMax)),
                                                                       to = c(1, 100)))),
                       ## Put predictions on log scale
                       Pred = map(.x = Pred,
                                  .f = ~ .x %>% mutate(across(c(Pred, lower, upper),
                                                              .f = list(l = log),
                                                              .names = "{.fn}{.col}"))),
                       ## Rescale predictions (Pred, lower, upper)
                       Pred = map2(.x = Pred,
                                   .y = Resp,
                                   .f = ~ .x %>%
                                       mutate(across(c(lPred, llower,lupper),
                                                     .f = list(s = ~ scales::rescale(.x,
                                                                                     from = c(unique(.y$lMin),
                                                                                              unique(.y$lMax)),
                                                                                     to = c(1,100))),
                                                     .names = "{.fn}{.col}")))
                       ) %>% 
                full_join(data.EDA.pres %>%
                          group_by(ZoneName) %>%
                          ## nest() %>%
                          ## rename(Pres = data) %>%
                          summarise(Pres = list(cur_data_all()), .groups = "drop") %>%
                          mutate(Pres = map(.x = Pres,
                                            .f = ~.x %>%
                                                mutate(x = lubridate::decimal_date(as.Date(
                                                                          paste0(Year, "-01-01"))),
                                                       ZoneName = paste0(Zone, ". ", ZoneName)))) %>%
                          mutate(GAM_pres = map(.x = Pres,
                                                .f = ~fitSimpleGAM_pres(.x)),
                                 Pred_pres = map2(.x = Pres,
                                                  .y = GAM_pres,
                                                  .f = ~predSimpleGAM_pres(.x, .y)),
                                 Pred_pres1 = map2(.x = Pred_pres, .y = Pres,
                                                   .f = ~ {if(!is.null(.x))
                                                               .x %>%
                                                                   mutate(across(c(Pred, lower, upper),
                                                                                 ~ scales::rescale(.x,
                                                                                                   from = c(unique(.y$Min), unique(.y$Max)),
                                                                                                   to = c(1, 100))))
                                                   }
                                                   )
                                 )
                          ) %>%
                left_join(spatial_lookup %>% dplyr::select(ZoneName, Zone)) %>%
                mutate(ZoneName = forcats::fct_reorder(ZoneName, Zone)) %>%
                arrange(ZoneName) %>%
                filter(!is.na(ZoneName)) %>%
                mutate(g = pmap(.l = list(Resp, Pres, Pred, Pred_pres1),
                                .f = ~dual_plot(..1, ..2, ..3, ..4, DATE_RANGE))) %>%
                suppressMessages() %>%
                suppressWarnings()

            saveRDS(g1, file = paste0(DATA_PATH, "summarised/dual_plot",
                                      FILE_APPEND,"__",j,"__",MEASURE, FILE_LAG,".RData"))
            FACETS <- g1 %>% pull(ZoneName) %>% unique %>% length()
            NCOL <- wrap_dims(FACETS, ncol = 3)[2]
            NROW <- wrap_dims(FACETS, ncol = 3)[1]
            g1 <- g1 %>% rowwise() %>%
                filter(!is.null(Resp))
            g <-  patchwork::wrap_plots(g1$g, ncol = NCOL) & theme_bw()
            ggsave(filename = paste0(FIGS_PATH, "/EDA_dual_plot",
                                     FILE_APPEND,"__",j,"__",MEASURE, FILE_LAG, ".png"),
                   g,
                   width = 4 * NCOL,
                   height = 3 * NROW,
                   dpi = 72)
        }
    }
    
}

## ----end
## ---- EDA associations modelled_settings_prior function
modelled_settings_prior <- function(MEASURE, lag, units_lookup) {
    if (!is.null(lag)) {
        MEASURE_ <- paste0(MEASURE,"_lag",lag)
        LAB_LAG <- paste0("Lag (",lag,"yr) ")
    } else {
        MEASURE_ <- MEASURE
        LAB_LAG <- ""
    }
    xlab <- units_lookup %>%
        filter(Measure == {{MEASURE}}) %>% 
        pull(TableLabel) %>%
        paste0(LAB_LAG, .)
    list(
        MEASURE = MEASURE,
        MEASURE_ = MEASURE_,
        LAB_LAG = LAB_LAG,
        xlab = xlab,
        lag = lag)
}
## ----end

## ---- EDA associations simple_gam function
## This function is applied to the response vs pressure data
simple_gam <- function(data.EDA, MEASURE_, ylab, xlab, FILE_APPEND, FILE_LAG, j) {
    g <- try({
        gg <- data.EDA %>%
            ggplot(aes(y = Value, x = !!sym(MEASURE_))) +
            ## geom_point() +
            geom_text(aes(label = as.numeric(factor(Year)))) +
            ## geom_smooth(method = 'gam', formula = y ~ s(x, bs = 'ps'),
            ##             method.args = list(method = "REML",
            ##                                family = Gamma(link = 'log'))) +
            ## scale_y_continuous(trans = scales::pseudo_log_trans()) 
            geom_smooth(method = 'gam', formula = y ~ s(x, bs = 'ps'),
                        method.args = list(method = "REML",
                                           family = gaussian()),
                        color = "blue",
                        fill = "#0000FF50") +
            ## scale_y_log10(scales::label_parse()(lab)) +
            ## scale_x_continuous(scales::label_parse()(xlab)) +
            scale_y_log10(ylab) +
            scale_x_continuous(xlab) +
            theme_bw() +
            theme(strip.text.y = element_text(angle = 0),
                  strip.background = element_rect(fill = "#446e9b50"),
                  panel.spacing.x = unit("0.5", "cm"))
        FACETS <- data.EDA %>% pull(ZoneName) %>% unique %>% length()
        NCOL <- wrap_dims(FACETS, ncol = 3)[2]
        NROW <- wrap_dims(FACETS, ncol = 3)[1]
        
        p <- patchwork::wrap_plots( 
                            gg + facet_wrap(~ZoneName, scales='free', ncol = 3),
                            gg,
                            ncol = 1,
                            heights = c(2*NROW, 2*2)
                        )
        ggsave(filename = paste0(FIGS_PATH, "/gamPlots",
                                 FILE_APPEND,"__",j,"__",MEASURE_,FILE_LAG,".png"),
               p,
               width = NCOL * 4,
               height = NROW * 4,
               dpi = 72) %>%
            suppressMessages() %>%
            suppressWarnings()
    }, silent = TRUE)
    if (any(class(g) == 'try-error')) {
        g <- try({
            gg <- data.EDA %>%
                ggplot(aes(y = Value, x = !!sym(MEASURE_))) +
                ## geom_point() +
                geom_text(aes(label = as.numeric(factor(Year)))) +
                ## geom_smooth(method = 'gam', formula = y ~ s(x, bs = 'ps'),
                ##             method.args = list(method = "REML",
                ##                                family = Gamma(link = 'log'))) +
                ## scale_y_continuous(trans = scales::pseudo_log_trans()) 
                geom_smooth(method = 'gam', formula = y ~ s(x, bs = 'ps'),
                            method.args = list(method = "REML",
                                               family = gaussian()),
                            color = "blue",
                            fill = "#0000FF50") +
                ## scale_y_log10(scales::label_parse()(lab)) +
                ## scale_x_continuous(scales::label_parse()(xlab)) +
                scale_y_continuous(ylab) +
                scale_x_continuous(xlab) +
                theme_bw() +
                theme(strip.text.y = element_text(angle = 0),
                      strip.background = element_rect(fill = "#446e9b50"),
                      panel.spacing.x = unit("0.5", "cm"))
            FACETS <- data.EDA %>% pull(ZoneName) %>% unique %>% length()
            NCOL <- wrap_dims(FACETS, ncol = 3)[2]
            NROW <- wrap_dims(FACETS, ncol = 3)[1]
            
            p <- patchwork::wrap_plots( 
                                gg + facet_wrap(~ZoneName, scales='free', ncol = 3),
                                gg,
                                ncol = 1,
                                heights = c(2*NROW, 2*2)
                            )
            ggsave(filename = paste0(FIGS_PATH, "/gamPlots",
                                     FILE_APPEND,"__",j,"__",MEASURE_,FILE_LAG,".png"),
                   p,
                   width = NCOL * 4,
                   height = NROW * 4,
                   dpi = 72) %>%
                suppressMessages() %>%
                suppressWarnings()
        }, silent = TRUE)
    }
}
## ----end

## ---- EDA associations associations function
associations <- function(type = "Discrete", FOCAL_RESPS, FOCAL_PRESSURES, lag = NULL) {
    set_cores()    

    FILE_APPEND <- file_append(type)
    FILE_LAG <- file_lag(lag)

    for (j in FOCAL_RESPS) {
        cat(j, "\n")
        ylab <- units_lookup %>%
            filter(Measure == {{j}}) %>%
            pull(TableLabel)
        for (i in 1:nrow(FOCAL_PRESSURES)) {
        ## foreach (i = 1:nrow(FOCAL_PRESSURES)) %dopar% {
        ## foreach(i = 27:29) %do% {
        ## for (i in 20:29) {
        ## for (i in 29) {
            MEASURE <- FOCAL_PRESSURES[i, "Measure"][[1]]
            settings <- readRDS(paste0(DATA_PATH, "summarised/settings",
                                       FILE_APPEND,"__", j, "__", MEASURE,".RData"))
            list2env(settings, envir = globalenv())
            MODELLED_SETTINGS <- modelled_settings_prior(MEASURE, lag, units_lookup)
            list2env(MODELLED_SETTINGS, envir = globalenv())

            cat("\t",paste(j, ": ", MEASURE_, "(", MEASURE, ")"), "\n")

            data.EDA <- readRDS(file = paste0(DATA_PATH, "summarised/data.EDA",
                                              FILE_APPEND,"__", j, "__", MEASURE, ".RData"))
                ## mutate(Value = ifelse(Value==0, 0.001, Value)) %>%
            ## Simple set of gams relating response and pressure (at give lag)
            ## output to: paste0(FIGS_PATH, "/gamPlots", FILE_APPEND,"__",j,"__",MEASURE_,FILE_LAG,".png")
            simple_gam(data.EDA, MEASURE_, ylab, xlab, FILE_APPEND, FILE_LAG, j) 

            file_suffix <- paste0(FILE_APPEND, "__", j, "__", MEASURE_)
            ## g <- data.EDA %>%
            ##     ggplot(aes(y = Value, x = !!sym(MEASURE_))) +
            ##     geom_point() +
            ##     ## geom_smooth(method = 'gam', formula = y ~ s(x, bs = 'ps'),
            ##     ##             method.args = list(method = "REML",
            ##     ##                                family = Gamma(link = 'log'))) +
            ##     ## scale_y_continuous(trans = scales::pseudo_log_trans()) 
            ##     geom_smooth(method = 'gam', formula = y ~ s(x, bs = 'ps'),
            ##                 method.args = list(method = "REML",
            ##                                    family = gaussian()),
            ##                 color = "blue",
            ##                 fill = "#0000FF50") +
            ##     ## scale_y_log10(scales::label_parse()(lab)) +
            ##     ## scale_x_continuous(scales::label_parse()(xlab)) +
            ##     scale_y_log10(ylab) +
            ##     scale_x_continuous(xlab) +
            ##     theme_bw() +
            ##     theme(strip.text.y = element_text(angle = 0),
            ##           strip.background = element_rect(fill = "#446e9b50"),
            ##           panel.spacing.x = unit("0.5", "cm"))
            ## FACETS <- data.EDA %>% pull(ZoneName) %>% unique %>% length()
            ## NCOL <- wrap_dims(FACETS, ncol = 3)[2]
            ## NROW <- wrap_dims(FACETS, ncol = 3)[1]

            ## p <- patchwork::wrap_plots( 
            ##               g + facet_wrap(~ZoneName, scales='free', ncol = 3),
            ##               g,
            ##               ncol = 1,
            ##               heights = c(2*NROW, 2*2)
            ##               )
            ## ggsave(filename = paste0(FIGS_PATH, "/gamPlots",
            ##                          FILE_APPEND,"__",j,"__",MEASURE_,FILE_LAG,".png"),
            ##        p,
            ##        width = NCOL * 4,
            ##        height = NROW * 4,
            ##        dpi = 72) %>%
            ##     suppressMessages() %>%
            ##     suppressWarnings()

            ## Fit models
            data.EDA.mod <- data.EDA %>%
                mutate(DV = !!sym(MEASURE_),
                       Zone = as.character(Zone)) %>%    ##added when adding type == 'All'
                filter(!is.na(DV)) %>%
                ## Add a Whole Harbour Group
                { if (max(.$DV) > 1e5) mutate(.,DV = DV/1e5) else . } %>%
                {
                    x <- .
                    rbind(x, x %>% mutate(Zone = "", ZoneName = "Whole Harbour"))
                } %>%
                group_by(Zone, ZoneName) %>%
                summarise(data = list(cur_data_all()), .groups = "drop") %>%
                mutate(Mod_info = map(.x = data,
                                      .f = ~fitModels(.x, type, MODELLED_SETTINGS, file_suffix) %>%
                                      suppressWarnings()),
                       Mod_Number = map(.x = Mod_info,
                                        .f = ~ .x$number),
                       Mod = map(.x = Mod_info,
                                 .f = ~.x$mod),
                       ## ModLin = map(.x = data,
                       ##           .f = ~fitModel(.x, type = 'linear')),
                       ## GAM = map(.x = data,
                       ##           .f = ~fitModelGAM(.x)),
                       Sum = map(.x = Mod,
                                 .f = ~ {if(!is.null(.x)) {
                                             tr <- try(summary(.x), silent = TRUE)
                                             if (any(class(tr) == 'try-error')) return(NULL)
                                             tr
                                         } else NULL}),
                       Params = map(.x = Mod,
                                    .f = ~ {if(!is.null(.x)) {
                                                tr <- try(parameters::model_parameters(.x), silent = TRUE)
                                                if (any(class(tr) == 'try-error')) return(NULL)
                                                tr
                                           } else NULL}),
                       Params2 = map(.x = Mod,
                                     .f = ~ {
                                         tdy <- try(broom.mixed::tidy(.x, conf.int = TRUE),
                                                    silent = TRUE)
                                         if (any(class(tdy) == 'try-error'))
                                             tdy <- try(broom.mixed::tidy(.x, conf.int = TRUE,
                                                                      effects='fixed'), silent = TRUE)
                                         if (any(class(tdy) == 'try-error'))
                                             tdy <- try(broom.mixed::tidy(.x, conf.int = FALSE), silent = TRUE)
                                         if (any(class(tdy) == 'try-error'))
                                             tdy <- try(broom.mixed::tidy(.x, conf.int = FALSE,
                                                                      effects='fixed'), silent = TRUE)
                                         tdy
                                     }),
                       
                       ## Params2 = map(.x = Mod,
                       ##              .f = ~ {if(!is.null(.x)) {
                       ##                          tdy <- try(broom.mixed::tidy(.x, conf.int = TRUE),
                       ##                                     silent = TRUE)
                       ##                          if (any(class(tdy) == 'try-error'))
                       ##                              tdy <- broom.mixed::tidy(.x, conf.int = TRUE,
                       ##                                                       effects='fixed')
                       ##                          tdy
                       ##                      } else NULL}),
                       R2 = map(.x = Mod,
                                .f = ~ {if(!is.null(.x))
                                            MuMIn::r.squaredGLMM(.x)
                                        else NULL}),
                       DHARMa = map(.x = Mod,
                                    .f = ~ {if(!is.null(.x))
                                                DHARMa::simulateResiduals(.x, plot = FALSE)
                                                else NULL}),
                       DHARMa_uniform = map2(.x = DHARMa, .y = paste0(Zone, ". ", ZoneName),
                                             .f = ~patchwork::wrap_elements(~DHARMa::testUniformity(.x)) +
                                                 ggtitle(.y)),
                       DHARMa_quantiles = map2(.x = DHARMa, .y = paste0(Zone, ". ", ZoneName),
                                               .f = ~{if(!is.null(.x))
                                                          ## patchwork::wrap_elements(~DHARMa::testQuantiles(.x)) +
                                                          patchwork::wrap_elements(~DHARMa::plotResiduals(.x, quantreg = FALSE)) +
                                                              ggtitle(.y)
                                                          else NULL}
                                              ),
                       Emmeans = map2(.x = Mod,
                                      .y = data,
                                      .f = ~{if(!is.null(.x)) {
                                                 tr <- try(emmeansCalc(.x, .y, MEASURE_), silent = TRUE)
                                                 if (any(class(tr) == 'try-error')) return(NULL)
                                                 tr
                                             } else NULL}),
                       CohenD = map(.x = Emmeans,
                                        .f = ~ {if(!is.null(.x)) {
                                                    tr <- try(cohenD(.x), silent = TRUE)
                                                    if (any(class(tr) == 'try-error')) return(NULL)
                                                    tr
                                                } else NULL}
                                        ),
                       R = map2(.x = R2,
                                .y = Emmeans,
                                .f = ~ {if(!is.null(.x)) {
                                            wch_min <- which.min(.y$response)
                                            wch_max <- which.max(.y$response)
                                            polarity <- ifelse(wch_max > wch_min, 1, -1)
                                            polarity * sqrt(.x[nrow(.x),1])
                                        } else NULL}),
                       Part = map2(.x = Mod,
                                   .y = data,
                                   .f = ~ {if(!is.null(.x)) {
                                               tr <- try(partialPlots(.x, .y, j, MEASURE_, ylab, xlab), silent = TRUE)
                                                 if (any(class(tr) == 'try-error')) return(NULL)
                                                 tr
                                              } else NULL})
                       ## Partial = map(.x = Mod,
                       ##               .f = ~ggeffects::ggemmeans(.x, terms = ~ DV) %>% plot(add.data = TRUE) +
                       ##                   scale_y_log10()) 
                       ) %>%
                suppressMessages() %>%
                suppressWarnings()

            FACETS <- data.EDA.mod %>% pull(ZoneName) %>% unique %>% length()
            NCOL <- wrap_dims(FACETS, ncol = 3)[2]
            NROW <- wrap_dims(FACETS, ncol = 3)[1]

            saveRDS(data.EDA.mod %>% dplyr::select(Zone, ZoneName, Mod),
                    file = paste0(DATA_PATH,"summarised/data.EDA",
                                  FILE_APPEND,"mod__",j,"__",MEASURE_,".RData"))
                
            p <- patchwork::wrap_plots(data.EDA.mod$DHARMa_uniform, ncol = NCOL) &
                theme(plot.title = element_text(hjust = 0.5))
            ggsave(filename = paste0(FIGS_PATH, "/DHARMa_unif",
                                     FILE_APPEND,"__",j,"__",MEASURE_,".png"),
                   p,
                   width = NCOL * 4,
                   height = NROW * 4,
                   dpi = 72)

            p <- data.EDA.mod %>%
                rowwise() %>%
                filter(!is.null(DHARMa_quantiles)) %>%
                pull(DHARMa_quantiles) %>%
                patchwork::wrap_plots(ncol = NCOL) &
                theme(plot.title = element_text(hjust = 0.5))
            if (length(p$patches$plots) != 0)
                ggsave(filename = paste0(FIGS_PATH, "/DHARMa_quant",
                                         FILE_APPEND,"__",j,"__",MEASURE_,".png"),
                       p,
                       width = NCOL * 4,
                       height = NROW * 4,
                       dpi = 72)

            p <- data.EDA.mod %>%
                rowwise() %>%
                filter(!is.null(Part)) %>%
                pull(Part) %>%
                patchwork::wrap_plots(ncol = NCOL) 
            if (length(p$patches$plots) != 0 )
                ggsave(filename = paste0(FIGS_PATH, "/partial",
                                         FILE_APPEND,"__",j,"__",MEASURE_,".png"),
                       p,
                       width = NCOL * 4,
                       height = NROW * 3,
                       dpi = 72)

            data.EDA.mod.sum <- data.EDA.mod %>%
                dplyr::select(Zone, ZoneName,
                              Mod_Number, Params, Params2, R2, CohenD, R, Emmeans)
            saveRDS(data.EDA.mod.sum,
                    file = paste0(DATA_PATH,"summarised/data.EDA",
                                     FILE_APPEND,"mod.sum__",j,"__",MEASURE_,".RData"))

            logTable <- read.csv(file = paste0(DATA_PATH, "modelled/logTable.csv"))
            logTable <- logTable %>%
                mutate(Complete = ifelse(Resps == j &
                                         Preds == MEASURE &
                                         Lags == ifelse(is.null(lag), 0, lag) &
                                         Type == type,
                                         'X',''))
            write.csv(logTable, file = paste0(DATA_PATH, "modelled/logTable.csv"))
            cat(paste0(j,",",MEASURE,',',ifelse(is.null(lag), 0, lag),',',type,'\n'),
                file = paste0(DATA_PATH, "modelled/log.csv"),
                append = TRUE)
        }
    }
}
## ----end

## ---- EDA cohenD function
## https://www.simplypsychology.org/effect-size.html#:~:text=Cohen%20suggested%20that%20d%20%3D%200.2,if%20it%20is%20statistically%20significant.
## https://en.wikipedia.org/wiki/Effect_size
##https://www.socscistatistics.com/effectsize/default3.aspx
cohenD <- function(x) {
    ## Cohen's effect size
    wch_min <- which.min(x$response)
    wch_max <- which.max(x$response)
    SD1 <- x$SE[wch_min] * sqrt(x$df[wch_min])
    SD2 <- x$SE[wch_max] * sqrt(x$df[wch_max])
    SDp <- sqrt((SD1^2 + SD2^2)/2)
    (x$response[wch_max] - x$response[wch_min])/SDp
}
## ----end


## ---- EDA fitSimpleGAM_resp function
fitSimpleGAM_resp <- function(d) {
    if ((nrow(d) > 3) & (var(d$Value) > 0)) {
        k <- min(10, nrow(d)-1)
        mod <- try(mgcv::gam(Value ~ s(x, bs = 'ps', k = k),
                         ## s(WQ_SITE, bs = 're'),
                         data = d,
                         method = "REML",
                         family = Gamma(link = "log")), silent = TRUE) %>% 
            suppressWarnings() %>%
            suppressMessages() 
        if (any(class(mod) == 'try-error')) {
            mod <- glm(Value ~ x,
                       data = d,
                       family = Gamma(link = "log"))
            
        }
        mod
    } else {
        NULL
    }
}
## ----end
## ---- EDA predSimpleGAM_resp function
predSimpleGAM_resp <- function(d, mod) {
    if ((nrow(d) > 3) & (var(d$Value) > 0)) {
        nd <- d %>% expand(x = full_seq(Year, 0.1)) %>%
            mutate(Date = lubridate::date_decimal(x))
        pred <- predict(mod, newdata = nd, se.fit = TRUE,
                        type = 'link')
        nd %>% mutate(Pred = pred$fit,
                      lower = pred$fit - 2*pred$se.fit,
                      upper = pred$fit + 2*pred$se.fit) %>%
            mutate(across(c(Pred, lower, upper), ~exp(.)))
    } else {
        NULL #d %>% mutate(Pred = NA, lower = NA, upper = NA)
    }
}
## ----end
## ---- EDA fitSimpleGAM_pres function
fitSimpleGAM_pres <- function(d) {
    if ((nrow(d) > 3) & (length(unique(d$Value)) > 3) & (var(d$Value) > 0)) {
        k <- min(10, nrow(d)-1)
        mod <- try(mgcv::gam(Value ~ s(x, bs = 'ps', k = k),
                         ## s(WQ_SITE, bs = 're'),
                         data = d,
                         method = "REML",
                         family = gaussian()),
                   silent = TRUE) %>%
            suppressMessages() %>%
            suppressWarnings()
        if (all(class(mod) == 'try-error')) {
            mod <- glm(Value ~ x,
                      data = d,
                      family = gaussian())
            }
        mod
    } else {
        NULL
    }
}
## ----end
## ---- EDA predSimpleGAM_pres function
predSimpleGAM_pres <- function(d, mod) {
    if ((nrow(d) > 3) & (length(unique(d$Value)) > 3) & (var(d$Value) > 0)) {
        nd <- d %>% expand(x = full_seq(Year, 0.1)) %>%
            mutate(Date = lubridate::date_decimal(x))
        pred <- predict(mod, newdata = nd, se.fit = TRUE,
                        type = 'link')
        nd %>% mutate(Pred = pred$fit,
                      lower = pred$fit - 2*pred$se.fit,
                      upper = pred$fit + 2*pred$se.fit)
    } else {
        NULL #d %>% mutate(Pred = NA, lower = NA, upper = NA)
    }
}
## ----end

## ---- EDA dual_plot function
dual_plot <- function(dat.resp = NULL, dat.pres = NULL,
                      dat.resp.smooth = NULL, dat.pres.smooth = NULL,
                      DATE_RANGE) {
    if (is.null(dat.resp)) return(NULL)
    ## br <- pretty(c(dat.resp$lMin, dat.resp$lMax))
    br <- scales::rescale(pretty(c(dat.resp$Min, dat.resp$Max)),
                          to = c(1,100),
                          from = c(unique(dat.resp$Min), unique(dat.resp$Max)))
    brl <- round(scales::rescale(br,
                                 from = c(1,100),
                                 to = c(unique(dat.resp$Min), unique(dat.resp$Max))),2)

    if (!is.null(dat.pres)) {
        br_p <- pretty(c(dat.pres$Min, dat.pres$Max))
        brr_p <- scales::rescale(br_p,
                                 to = c(1,100),
                                 from = c(unique(dat.pres$Min), unique(dat.pres$Max)))
    }
    NAME <- units_lookup$TableLabel[units_lookup$Measure == unique(dat.resp$Measure)]
    NAME_p <- units_lookup$TableLabel[units_lookup$Measure == unique(dat.pres$Measure)]
    ggplot() +
        geom_point(data = dat.resp,
                   aes(y = slValue, x = Date), colour = "orange", alpha = 0.5) +
        {if(!is.null(dat.resp.smooth))
             geom_ribbon(data = dat.resp.smooth,
                         aes(ymin = sllower, ymax=slupper, x = as.Date(Date)),
                         fill = "orange", alpha = 0.3)
        } +
        {if(!is.null(dat.resp.smooth))
             geom_line(data = dat.resp.smooth,
                       aes(y = slPred, x = as.Date(Date)),
                       colour = "orange", alpha = 0.9)
        } +
        {if(!is.null(dat.pres))
             geom_point(data = dat.pres,aes(y = sValue, x = Date), colour = "purple", alpha = 0.5) } +

    {if(!is.null(dat.pres.smooth))
         geom_ribbon(data = dat.pres.smooth,
                     aes(ymin = lower, ymax=upper, x = as.Date(Date)),
                     fill = "purple", alpha = 0.3)
    } +
    {if(!is.null(dat.pres.smooth))
         geom_line(data = dat.pres.smooth,
                   aes(y = Pred, x = as.Date(Date)),
                   colour = "purple", alpha = 0.9)
    } +
    { if(!is.null(dat.pres))
          scale_y_continuous(name = NAME,
                             ## trans = scales::pseudo_log_trans(),
                             breaks = br, labels = brl,
                             sec.axis = sec_axis(trans = ~ .x, name = NAME_p,
                                                 breaks = brr_p, labels = br_p))
    } +
    {if(is.null(dat.pres))
         scale_y_continuous(name = NAME,
                            ## trans = scales::pseudo_log_trans(),
                            breaks = br, labels = brl)
    } +
    ## scale_x_date('', limits = as.Date(paste0(YEAR_RANGE, "-01-01")) + months(c(0,3))) +
    scale_x_date('', limits = DATE_RANGE + months(c(-3,3))) +
    facet_wrap(~ZoneName)
}
## ----end


## ---- EDA associations checkModel function
checkModel <- function(mod) {
    if(any(class(mod) == 'try-error')) {return('Bad')}
    summ <- try(summary(mod))
    if (any(class(summ) == 'try-error')) {return('Bad')}
    if (is.na(AIC(mod))) {return('Bad')}
    return('Good')
}
## ----end
## ---- EDA associations polyModel function
polyModel <- function(dat, type) {
    model <- list()
    model[['engine']] <- 'glmmTMB'
    dat <- dat %>% group_by(WQ_SITE) %>%
        mutate(DV1 = lag(DV))
    if (type == "Routine") {
        model[['re']] <- "WQ_SITE"
        model[['poly']] <- "Yes"
        if (length(unique(dat$Year)) > 4) {
            model[['ar1']] <- 'Yes'
            form <- Value ~ poly(DV,3) + (1|WQ_SITE) + ar1(factor(Year) -1 | WQ_SITE)
            form2 <- Value ~ poly(DV,3) + (1|WQ_SITE)
        }else {
            model[['ar1']] <- 'No'
            form <- Value ~ poly(DV,3) + (1|WQ_SITE)
        }
    } else {
        form <- Value ~ poly(DV,3)
    }
    model[['REML']] <- 'Yes'
    ## Try full autocorrelation model
    mod <- try(glmmTMB::glmmTMB(form,
                                data = dat,
                                ## family = glmmTMB::tweedie(link = "log")
                                family = Gamma(link = "log"),
                                REML = TRUE
                                ) %>%
               suppressMessages() %>%
               suppressWarnings(),
               silent = TRUE)
    ## if that was bad, try dropping the autocorrelation term
    if (checkModel(mod) == 'Bad') {
        model[['ar1']] <- 'No'
        mod <- try(glmmTMB::glmmTMB(form2,
                                    data = dat,
                                    family = Gamma(link = "log"),
                                    REML = TRUE
                                    ) %>%
                   suppressMessages() %>%
                   suppressWarnings(),
                   silent = TRUE)
        if (checkModel(mod) == 'Bad') {
            model[['engine']] <- NULL
            model[['re']] <- NULL
            model[['ar']] <- NULL
            model[['poly']] <- NULL
            model[['REML']] <- NULL
            return(list(mod = NULL, MODEL_SETTINGS = model))
        }
    }
    return(list(mod = mod, MODEL_SETTINGS = model))
}
## ----end
## ---- EDA associations glmmModel function
glmmModel <- function(dat, type, REML = TRUE) {
    model <- list()
    model[['engine']] <- 'glmmTMB'
    model[['poly']] <- "No"
    if (REML) model[['REML']] <- 'Yes'
    else model[['REML']] <- 'No'
    dat <- dat %>% group_by(WQ_SITE) %>%
        mutate(DV1 = lag(DV))
    if (type == "Routine") {
        model[['re']] <- "WQ_SITE"
        if (length(unique(dat$Year)) > 4){
            model[['ar1']] <- 'Yes'
            form <- Value ~ DV + (1|WQ_SITE) + ar1(factor(Year)-1|WQ_SITE)
            form2 <- Value ~ DV + (1|WQ_SITE)
        } else {
            model[['ar1']] <- 'No'
            form <- Value ~ DV + (1|WQ_SITE)
        }
    } else {
        model[['re']] <- "No"
        model[['ar1']] <- 'No'
        form <- Value ~ DV
    }
    ## Try full autocorrelation model
    mod <- try(glmmTMB::glmmTMB(form,
                                data = dat,
                                family = Gamma(link = "log"),
                                REML = REML
                                ) %>%
               suppressMessages() %>%
               suppressWarnings(),
               silent = TRUE)
    ## if that was bad, try dropping the autocorrelation term
    if (checkModel(mod) == 'Bad') {
        model[['re']] <- "No"
        mod <- try(glmmTMB::glmmTMB(form2,
                                    data = dat,
                                    family = Gamma(link = "log"),
                                    REML = REML
                                    ) %>%
                   suppressMessages() %>%
                   suppressWarnings(),
                   silent = TRUE)
        if (checkModel(mod) == 'Bad') {
            model[['engine']] <- NULL
            model[['re']] <- NULL
            model[['ar']] <- NULL
            model[['poly']] <- NULL
            model[['REML']] <- NULL
            return(list(mod = NULL, MODEL_SETTINGS = model))
        }
    }
    return(list(mod = mod, MODEL_SETTINGS = model))
}
## ----end
## ---- EDA associations gamModel function
gamModel <- function(dat, type) {
    model <- list()
    model[['engine']] <- 'gam'
    model[['poly']] <- "No"
    model[['ar1']] <- "No"
    model[['REML']] <- "Yes"
    dat <- dat %>% group_by(WQ_SITE) %>%
        mutate(DV1 = lag(DV))
    k = min(10, length(unique(dat$DV))-2)
    if (type == "Routine") {
        model[['re']] <- "Yes"
        form <- Value ~ s(DV, bs = 'ps', k = k) + Year + s(WQ_SITE, bs = 're')
    } else {
        model[['re']] <- "No"
        form <- Value ~ s(DV, bs = 'ps', k = k) + Year
    }
    mod <- mgcv::gam(form,
                     data = dat,
                     ## family = glmmTMB::tweedie(link = "log")
                     family = Gamma(link = "log"),
                     method = 'REML'
                     ) %>% 
        suppressMessages() %>%
        suppressWarnings()
    ## summary(mod)
    list(mod = mod, MODEL_SETTINGS = model)
}
## ----end
## ---- EDA associations fitModels_old function
fitModels_old <- function(dat, type, MODELLED_SETTINGS) {
    dat %>% droplevels() %>% pull(ZoneName) %>% unique %>% print
    dat <- dat %>% mutate(Value = ifelse(Value == 0, 0.05, Value))
    mods <- list()

    MODEL_NAME <- ""
    MODEL_SETTINGS <- list()
    ## start with poly
    if (length(unique(dat$DV))>4) {
        mod.poly <- polyModel(dat, type)
        ## if (is.null(mod.poly)) mod.poly <- NULL
    } else { 
        mod.poly <- glmmModel(dat, type)
        ## if (class(mod.poly) == "try-error") mod.poly <- NULL
    }
    if(!is.null(mod.poly$mod)) {
        mods[[length(mods) + 1]] <- mod.poly$mod
        MODEL_SETTINGS[[length(mods)]] <- mod.poly$MODEL_SETTINGS
    }
    
    ## linear model
    mod.lin <- glmmModel(dat, type)
    ## if (class(mod.lin) == "try-error")
    if (is.null(mod.lin$mod))
        mod.lin <- glmmModel(dat, type = type, REML = FALSE)
    if (is.null(mod.lin$mod)) {
        mod.lin <- list()
        mod.lin[['mod']] <- try(glmmTMB::glmmTMB(Value ~ DV, data = dat, family = Gamma(link = 'log')), silent = TRUE)
        mod.lin[['MODEL_SETTINGS']] <- list('engine' = 'glmmTMB', 'poly' = 'No',
                                            'REML' = "No", 're' = "No", 'ar1' = 'No') 
    }
    if (class(mod.lin$mod) == "try-error") {
        mod.lin$mod <- NULL
        mod.lin$MODEL_SETTINGS[['engine']] <- NULL
        mod.lin$MODEL_SETTINGS[['re']] <- NULL
        mod.lin$MODEL_SETTINGS[['ar']] <- NULL
        mod.lin$MODEL_SETTINGS[['poly']] <- NULL
        mod.lin$MODEL_SETTINGS[['REML']] <- NULL
    }
    
    if(!is.null(mod.lin$mod)) {
        mods[[length(mods) + 1]] <- mod.lin$mod
        MODEL_SETTINGS[[length(mods)]] <- mod.lin$MODEL_SETTINGS
    }
    
    ## gam model
    if (length(unique(dat$DV))>4) {
        mod.gam <- try(gamModel(dat, type), silent = TRUE)
        if (any(class(mod.gam$mod) == "try-error")) {
            mod.gam$mod <- NULL
            mod.gam$MODEL_SETTINGS[['engine']] <- NULL
            mod.gam$MODEL_SETTINGS[['re']] <- NULL
            mod.gam$MODEL_SETTINGS[['ar']] <- NULL
            mod.gam$MODEL_SETTINGS[['poly']] <- NULL
            mod.gam$MODEL_SETTINGS[['REML']] <- NULL
        }
    } else { 
        mod.gam <- glmmModel(dat, type)
        if (is.null(mod.gam$mod)) {
            k = min(10, length(unique(dat$DV)))
            mod.gam <- try(mgcv::gam(Value ~ s(DV, bs = 'ps', k = k),# + s(WQ_SITE, bs = 're'),
                             data = dat,
                             ## family = glmmTMB::tweedie(link = "log")
                             family = Gamma(link = "log")
                             ), silent = TRUE)
            mod.gam <- list('mod' = mod.gam,
                            'MODEL_SETTINGS' = list('engine' = 'gam', 're' = 'No',
                                                    'ar1' = 'No', 'poly' = 'No',
                                                    'REML' = 'No'))
        }
        if (any(class(mod.gam$mod) == "try-error")) {
            mod.gam$mod <- NULL
            mod.gam$MODEL_SETTINGS[['engine']] <- NULL
            mod.gam$MODEL_SETTINGS[['re']] <- NULL
            mod.gam$MODEL_SETTINGS[['ar']] <- NULL
            mod.gam$MODEL_SETTINGS[['poly']] <- NULL
            mod.gam$MODEL_SETTINGS[['REML']] <- NULL
        }
    }
    if(!is.null(mod.gam)) {
        mods[[length(mods) + 1]] <- mod.gam$mod
        MODEL_SETTINGS[[length(mods)]] <- mod.gam$MODEL_SETTINGS
    }
    
    if (length(mods) == 0) {
        return(list(mod = NULL, MODELL_SETTINGS = NULL))
    } else if (length(mods) ==1) {
        return(list(mod = mods[[1]], MODEL_SETTINGS = MODEL_SETTINGS[[1]]))
    } else {
        wch <- which.min(sapply(mods, AIC))
        if (length(wch) == 0) return(list(mod = mods[[1]], MODEL_SETTINGS[[1]]))
        else return(list(mod = mods[[wch]], MODEL_SETTINGS[[wch]]))
    }
    
}
## ----end
## ---- EDA associations fitModels function
models <- list()
models[[1]] <- list(
    number = 1,
    form = Value ~ poly(DV,3) + (1|WQ_SITE) + ar1(factor(Year) -1 | WQ_SITE),
    engine = 'glmmTMB',
    re = 'yes',
    ar = 'yes',
    poly = 'yes',
    REML = 'yes',
    k = function(dat) min(10, length(unique(dat$DV))-2),
    call = function(form, dat) glmmTMB::glmmTMB(form, data = dat,
                                                family = Gamma(link = "log"),
                                                REML = TRUE
                                                )
)
models[[2]] <- list(
    number = 2,
    form = Value ~ poly(DV,3) + (1|WQ_SITE),
                    engine = 'glmmTMB',
                    re = 'yes',
                    ar = 'no',
                    poly = 'yes',
                    REML = 'yes',
                    k = function(dat) min(10, length(unique(dat$DV))-2),
                    call = function(form, dat) glmmTMB::glmmTMB(form, data = dat,
                                family = Gamma(link = "log"),
                                REML = TRUE
                                )
                    )
models[[3]] <- list(
    number = 3,
    form = Value ~ DV + (1|WQ_SITE) + ar1(factor(Year) -1 | WQ_SITE),
                    engine = 'glmmTMB',
                    re = 'yes',
                    ar = 'yes',
                    poly = 'no',
                    REML = 'yes',
                    k = function(dat) min(10, length(unique(dat$DV))-2),
                    call = function(form, dat) glmmTMB::glmmTMB(form, data = dat,
                                family = Gamma(link = "log"),
                                REML = TRUE
                                )
                    )
models[[4]] <- list(
    number = 4,
    form = Value ~ DV + (1|WQ_SITE),
                    engine = 'glmmTMB',
                    re = 'yes',
                    ar = 'no',
                    poly = 'no',
                    REML = 'yes',
                    k = function(dat) min(10, length(unique(dat$DV))-2),
                    call = function(form, dat) glmmTMB::glmmTMB(form, data = dat,
                                family = Gamma(link = "log"),
                                REML = TRUE
                                )
                    )
models[[5]] <- list(
    number = 5,
    form = Value ~ s(DV, bs = 'ps', k = k) + Year + s(WQ_SITE, bs = 're'),
                    engine = 'gam',
                    re = 'yes',
                    ar = 'no',
                    poly = 'no',
                    REML = 'yes',
                    k = function(dat) min(10, length(unique(dat$DV))-2),
                    call = function(form, dat) {
                        mgcv::gam(form, data = dat,
                                family = Gamma(link = "log"),
                                REML = TRUE
                                )
                        }
                    )
models[[6]] <- list(
    number = 6,
    form = Value ~ s(DV, bs = 'ps', k = k) + Year,
                    engine = 'gam',
                    re = 'no',
                    ar = 'no',
                    poly = 'no',
                    REML = 'yes',
                    k = function(dat) min(10, length(unique(dat$DV))-2),
                    call = function(form, dat) {
                        mgcv::gam(form, data = dat,
                                family = Gamma(link = "log"),
                                REML = TRUE
                                )
                        }
                    )
models[[7]] <- list(
    number = 7,
    form = Value ~ DV,
                    engine = 'glmmTMB',
                    re = 'no',
                    ar = 'no',
                    poly = 'no',
                    REML = 'no',
                    k = function(dat) min(10, length(unique(dat$DV))-2),
                    call = function(form, dat) glmmTMB::glmmTMB(form, data = dat,
                                family = Gamma(link = "log"),
                                REML = FALSE
                                )
                    )
## models[[8]] <- list(
##     number = 8,
##     form = log(Value) ~ poly(DV,3) + (1|WQ_SITE) + ar1(factor(Year) -1 | WQ_SITE),
##                     engine = 'glmmTMB',
##                     re = 'yes',
##                     ar = 'yes',
##                     poly = 'yes',
##                     REML = 'yes',
##                     k = function(dat) min(10, length(unique(dat$DV))-2),
##                     call = function(form, dat) glmmTMB::glmmTMB(form, data = dat,
##                                 family = gaussian(),
##                                 REML = TRUE
##                                 )
##                     )
## models[[9]] <- list(
##     number = 9,
##     form = log(Value) ~ poly(DV,3) + (1|WQ_SITE),
##                     engine = 'glmmTMB',
##                     re = 'yes',
##                     ar = 'no',
##                     poly = 'yes',
##                     REML = 'yes',
##                     k = function(dat) min(10, length(unique(dat$DV))-2),
##                     call = function(form, dat) glmmTMB::glmmTMB(form, data = dat,
##                                 family = gaussian(),
##                                 REML = TRUE
##                                 )
##                     )
## models[[10]] <- list(
##     number = 10,
##     form = log(Value) ~ DV + (1|WQ_SITE) + ar1(factor(Year) -1 | WQ_SITE),
##                     engine = 'glmmTMB',
##                     re = 'yes',
##                     ar = 'yes',
##                     poly = 'no',
##                     REML = 'yes',
##                     k = function(dat) min(10, length(unique(dat$DV))-2),
##                     call = function(form, dat) glmmTMB::glmmTMB(form, data = dat,
##                                 family = gaussian(),
##                                 REML = TRUE
##                                 )
##                     )
## models[[11]] <- list(
##     number = 11,
##     form = log(Value) ~ DV + (1|WQ_SITE),
##                     engine = 'glmmTMB',
##                     re = 'yes',
##                     ar = 'no',
##                     poly = 'no',
##                     REML = 'yes',
##                     k = function(dat) min(10, length(unique(dat$DV))-2),
##                     call = function(form, dat) glmmTMB::glmmTMB(form, data = dat,
##                                 family = gaussian(),
##                                 REML = TRUE
##                                 )
##                     )
## models[[12]] <- list(
##     number = 12,
##     form = log(Value) ~ s(DV, bs = 'ps', k = k) + Year + s(WQ_SITE, bs = 're'),
##                     engine = 'gam',
##                     re = 'yes',
##                     ar = 'no',
##                     poly = 'no',
##                     REML = 'yes',
##                     k = function(dat) min(10, length(unique(dat$DV))-2),
##                     call = function(form, dat) {
##                         mgcv::gam(form, data = dat,
##                                 family = gaussian(),
##                                 REML = TRUE
##                                 )
##                         }
##                     )
## models[[13]] <- list(
##     number = 13,
##     form = log(Value) ~ s(DV, bs = 'ps', k = k) + Year,
##                     engine = 'gam',
##                     re = 'no',
##                     ar = 'no',
##                     poly = 'no',
##                     REML = 'yes',
##                     k = function(dat) min(10, length(unique(dat$DV))-2),
##                     call = function(form, dat) {
##                         mgcv::gam(form, data = dat,
##                                 family = gaussian(),
##                                 REML = TRUE
##                                 )
##                         }
##                     )
## models[[14]] <- list(
##     number = 14,
##     form = log(Value) ~ DV,
##                     engine = 'glmmTMB',
##                     re = 'no',
##                     ar = 'no',
##                     poly = 'no',
##                     REML = 'no',
##                     k = function(dat) min(10, length(unique(dat$DV))-2),
##                     call = function(form, dat) glmmTMB::glmmTMB(form, data = dat,
##                                 family = gaussian(),
##                                 REML = FALSE
##                                 )
##                     )



fitModels <- function(dat, type, MODELLED_SETTINGS, file_suffix) {
    zone <- dat %>% droplevels() %>% pull(ZoneName) %>% unique %>% print
    file_suffix <- paste0(file_suffix, "__", zone)
    dat <- dat %>% mutate(Value = ifelse(Value == 0, 0.05, Value))
    MODELS <- list()
    for (m in 1:length(models)) {
        MODELS[[m]] <- models[[m]]
        form <- models[[m]]$form
        k <- models[[m]]$k(dat)
        mod <- try(models[[m]]$call(form, dat), silent = TRUE)
        MODELS[[m]]$mod <- mod
        if (any(class(mod) == 'try-error')) {
            MODELS[[m]]$mod <- NULL
            next
        }
        ## Calculate AIC
        MODELS[[m]]$AIC <- AIC(mod)
        ## Calculate DHARMa residuals
        MODELS[[m]]$DHARMa <- DHARMa::simulateResiduals(mod, plot = FALSE)
        ## test Residuals
        ## MODELS[[m]]$Residuals <- DHARMa::testResiduals(MODELS[[m]]$DHARMa, plot = FALSE) %>%
        ##     suppressMessages() %>% suppressWarnings()
        MODELS[[m]]$Uniformity <- DHARMa::testUniformity(MODELS[[m]]$DHARMa, plot = FALSE)
        MODELS[[m]]$Uniformity_p <- MODELS[[m]]$Uniformity$p.value
        MODELS[[m]]$Dispersion <- DHARMa::testDispersion(MODELS[[m]]$DHARMa, plot = FALSE)
        MODELS[[m]]$Dispersion_p <- MODELS[[m]]$Dispersion$p.value
        MODELS[[m]]$Outliers <- DHARMa::testOutliers(MODELS[[m]]$DHARMa, plot = FALSE)
        MODELS[[m]]$Outliers_p <- MODELS[[m]]$Outliers$p.value
        ## test for heteroscadacity
        MODELS[[m]]$Residuals <- DHARMa::testQuantiles(MODELS[[m]]$DHARMa, plot = FALSE) %>%
            suppressMessages() %>% suppressWarnings()
        MODELS[[m]]$Quantiles_p <- MODELS[[m]]$Residuals$p.value
    }
    ## save all the models
    save(MODELS, file = paste0(DATA_PATH, "modelled/MODELS_",file_suffix,".RData"))
    ## Check if any models worked
    ## WCH <- 1:length(MODELS)
    wch <- which(sapply(MODELS, function(x) !is.null(x$mod)))
    ## WCH <- WCH[wch]
    if (length(wch)==0) return(list(mod = NULL)) 
    MODELS <- MODELS[wch]
    ## Which have acceptable diagnostics
    unif <- which(sapply(MODELS, function(x) x$Uniformity_p) > 0.05)
    disp <- which(sapply(MODELS, function(x) x$Dispersion_p) > 0.05)
    outl <- which(sapply(MODELS, function(x) x$Outliers_p) > 0.05)
    quan <- which(sapply(MODELS, function(x) x$Quantiles_p) > 0.05)
    ## Put more weight on uniformity than dispersion, outliers and quantiles
    tt <- table(c(rep(unif,3), disp, outl, quan))
    ## select the model(s) that meet the assumptions best
    cand_models <- as.numeric(names(tt))[which(tt == max(tt))] 
    ## WCH <- WCH[cand_models]
    ## cand_models <- as.numeric(names(tt))[which(tt == (tt))] 
    ## Compare AIC
    aics <- sapply(MODELS[cand_models], function(x) x$AIC)
    ## check that AICs are not zero
    which_aic <- which(!is.na(aics))
    if (length(which_aic) ==0) return(list(mod = NULL)) #return(MODELS[[length(aics)]])
    MODELS[[cand_models[which.min(aics)]]]
}
## ----end



## ---- EDA associations fitModelGAM function
fitModelGAM <- function(dat) {
    dat %>% droplevels() %>% pull(ZoneName) %>% unique %>% print
    dat <- dat %>% mutate(Value = ifelse(Value == 0, 0.05, Value))
    if (length(unique(dat$DV))>4) {
        k = min(10, length(unique(dat$DV)))
        mod <- mgcv::gam(Value ~ s(DV, bs = 'ps', k = k),# + s(WQ_SITE, bs = 're'),
                         data = dat,
                         ## family = glmmTMB::tweedie(link = "log")
                         family = Gamma(link = "log"),
                         method = 'REML'
                         )
    } else { 
        mod <- try({
            glmmTMB::glmmTMB(Value ~ DV + (1|WQ_SITE),
                                data = dat,
                                ## family = glmmTMB::tweedie(link = "log")
                                family = Gamma(link = "log")
                             )
        }, silent = TRUE)
        if (class(mod) == "try-error") mod <- NULL
    }
    mod
}
## ----end

## ---- EDA associations partialPlots function
partialPlots <- function(mod,dat, RESP, DV, ylab, xlab) {
    newdata <- dat %>% expand(DV = seq(min(DV), max(DV), length = 100))
    pred <- emmeans::emmeans(mod, ~DV, at = newdata, type = "response") %>%
        as.data.frame()
    ggplot() +
        geom_point(data = dat, aes(y = Value, x = DV), alpha = 0.8) +
        geom_ribbon(data = pred, aes(y = response, x = DV,
                                     ymin = lower.CL, ymax = upper.CL),
                    fill = "#0000FF50", colour = NA) +
        geom_line(data = pred, aes(y = response, x = DV),
                  colour = "blue") +
        theme_bw() +
        scale_y_log10(ylab) +
        scale_x_continuous(xlab) +
        facet_wrap(~paste0(Zone,". ",ZoneName))
}
## ----end

## ---- EDA associations emmeansCalc function
emmeansCalc <- function(mod, dat, DV) {
    newdata <- dat %>% expand(DV = seq(min(DV), max(DV), length = 100))
    pred <- emmeans::emmeans(mod, ~DV, at = newdata, type = "response") %>%
        as.data.frame()
    pred
}
## ----end

## ---- EDA associations summary table
summary_table <- function(j, MEASURE, k, type = "Routine") {
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

    ## get the individual models and extract the ZoneName and model number

    ## rout <- ifelse(routine == TRUE, 'routine', '')
    ## files <- list.files(path = paste0(DATA_PATH, "modelled"),
    ##                     pattern = paste0("MODELS_.",rout,".__",j,'__',MEASURE, k,'__.*'),
    ##                     full.names = TRUE)
    ## load(files[1])
    ## file <- paste0(DATA_PATH,"summarised/data.EDA.routine.mod__",
    ##                j,"__",MEASURE,k,".RData")
    ## a <- readRDS(file)
    
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

    library(kableExtra)
    ##library(webshot) 
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
        kable(escape = FALSE) %>%
        column_spec(ncol(param_table2)+1, image = spec_plot(EM, same_lim = FALSE)) %>%
        header_separate(sep="__") %>%
        kable_classic(full_width = T) %>%
        kable_styling(bootstrap_options = "striped", font_size = 12) %>%
                                        #add_header_above(c(" " = 1, "Fixed" = 6, "Random" = 1, "Total" = 2)) %>%
        ##as_image(file = "../output/figures/test.png")
        ##save_kable("../output/figures/test.png")
        ## knitr::knit_print()
        print() %>%
        suppressMessages() %>%
        suppressWarnings()

}


## ----end


## https://www.learnui.design/tools/data-color-picker.html#divergent
cohenRColours <- function(x) {
    case_when(
        is.na(x) ~ "#FFFFFF",
        x < -0.5 ~ "#de425b",  #Large
              x < -0.3 ~ "#f48358",  #Madium
              x < -0.1 ~ "#fcbe6e",  #Small
              x < 0    ~ "#FFFFFF",  #"No effect"
              x < 0.1  ~ "#FFFFFF",  #"No effect"
              x < 0.3 ~  "#c5d275",  #Small
              x < 0.5 ~  "#89b050",  #Medium
              x >= 0.5 ~ "#488f31")  #Large
    }

cohenDColours <- function(x) {
    case_when(
        is.na(x) ~ "#FFFFFF",
        x < -2   ~ "#de425b",    #Huge
        x < -1.2 ~ "#e4604e",  #Very large
        x < -0.8 ~ "#ef8250",  #Large
        x < -0.5 ~ "#f7a258",  #Medium
        x < -0.2 ~ "#FCC267",  #Small
        x < -0.1 ~ "#FEE17E",  #Very small
        x < 0    ~ "#FFFFFF",  #No effect
        x < 0.1  ~ "#FFFFFF",  #No effect
        x < 0.2  ~ "#d6ec91",  #Very small
        x < 0.5 ~  "#aed987",  #Small
        x < 0.8 ~  "#88c580",  #Medium
        x < 1.2 ~  "#63b179",  #Large
        x < 2 ~    "#3d9c73",  #Very Large
        x >= 2 ~   "#488f31",  #Huge
        )
    }


glimpse_like_table <- function(dat) {
  my_skim <- skim_with(character = sfl(Values = ~ str_trunc(paste(unique(.), collapse = ','),30),
                                       Min = ~ "",#as.character(.[1]),
                                       Max = ~ ""),#as.character(.[1])),
                       numeric = sfl(Values = ~ inline_hist(.),
                                     Min = ~ sprintf("%.3f", min(., na.rm = TRUE)),
                                     Max = ~ sprintf("%.3f", max(., na.rm = TRUE))),
                       POSIXct = sfl(Values = ~ inline_hist(as.numeric(.)),
                                     Min = ~ format(min(., na.rm = TRUE), "%Y-%m-%d"),
                                     Max = ~ format(max(., na.rm = TRUE), "%Y-%m-%d")),
                       logical = sfl(Values = ~ skimr::top_counts(.))
                       )

  dat %>%
    my_skim() %>%
    focus(Missing = n_missing, any_of(c("character.Min", "character.Max", "character.Values",
                                        "numeric.Values", "numeric.Min", "numeric.Max",
                                        "POSIXct.Values", "POSIXct.Min", "POSIXct.Max",
                                        "logical.Values"))) %>%
    ## yank("character") %>%
    as_tibble() %>%
    mutate(Values = coalesce(!!!select(., ends_with(".Values"))),
           Min = coalesce(!!!select(., ends_with(".Min"))),
           Max = coalesce(!!!select(., ends_with(".Max")))
           ) %>%

  dplyr::select(Variable = skim_variable, Type = skim_type, Min, Max, everything(),
                -ends_with(".Min"), -ends_with(".Max"), -ends_with(".Values")) %>%
  flextable::flextable() %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::line_spacing(space = 0.1) %>%
  flextable::set_table_properties(layout = "autofit") %>%
  ## flextable::autofit() %>%
  flextable::align(j = c(3,4), align = "right")
}
