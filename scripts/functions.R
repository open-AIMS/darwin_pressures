## ---- loadPackages
library(knitr)
library(tidyverse)
library(readxl)
library(sf)
sf_use_s2(FALSE)
## ----end

## ---- preparePaths
DATA_PATH <<- "../data/"
OUTPUT_PATH <<- "../output/"
FIGS_PATH <<- paste0(OUTPUT_PATH, "figures")

if (!dir.exists(DATA_PATH)) dir.create(DATA_PATH)
if (!dir.exists(paste0(DATA_PATH,"primary"))) dir.create(paste0(DATA_PATH, "primary"))
if (!dir.exists(paste0(DATA_PATH,"processed"))) dir.create(paste0(DATA_PATH, "processed"))
if (!dir.exists(paste0(DATA_PATH,"modelled"))) dir.create(paste0(DATA_PATH, "modelled"))
if (!dir.exists(paste0(DATA_PATH,"summarised"))) dir.create(paste0(DATA_PATH, "summarised"))

if (!dir.exists(OUTPUT_PATH)) dir.create(OUTPUT_PATH)
if (!dir.exists(FIGS_PATH)) dir.create(FIGS_PATH)
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
