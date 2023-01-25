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

## ---- EDA associations fitModels function
fitModels <- function(dat) {
    dat %>% droplevels() %>% pull(ZoneName) %>% unique %>% print
    dat <- dat %>% mutate(Value = ifelse(Value == 0, 0.05, Value))
    mods <- list()

    polyModel <- function(dat) {
        dat <- dat %>% group_by(WQ_SITE) %>%
            mutate(DV1 = lag(DV))
        mod <- glmmTMB::glmmTMB(Value ~ poly(DV,3) + DV1 + (1|WQ_SITE),
                                data = dat,
                                ## family = glmmTMB::tweedie(link = "log")
                                family = Gamma(link = "log"),
                                REML = TRUE
                                )
        summary(mod)
        mod
    }
    glmmModel <- function(dat, REML = TRUE) {
        dat <- dat %>% group_by(WQ_SITE) %>%
            mutate(DV1 = lag(DV))
        mod <- glmmTMB::glmmTMB(Value ~ DV + Year + (1|WQ_SITE),
                                data = dat,
                                ## family = glmmTMB::tweedie(link = "log")
                                family = Gamma(link = "log"),
                                REML = REML
                                )
        summary(mod)
        mod
    }
    gamModel <- function(dat) {
        dat <- dat %>% group_by(WQ_SITE) %>%
            mutate(DV1 = lag(DV))
        k = min(10, length(unique(dat$DV))-2)
        mod <- mgcv::gam(Value ~ s(DV, bs = 'ps', k = k) + Year,# + s(WQ_SITE, bs = 're'),
                         data = dat,
                         ## family = glmmTMB::tweedie(link = "log")
                         family = Gamma(link = "log"),
                         method = 'REML'
                         )
        summary(mod)
        mod
    }
    ## start with poly
    if (length(unique(dat$DV))>4) {
        mod.poly <- try(polyModel(dat))
        if (class(mod.poly) == "try-error") mod.poly <- NULL
    } else { 
        mod.poly <- try(glmmModel(dat))
        if (class(mod.poly) == "try-error") mod.poly <- NULL
    }
    if(!is.null(mod.poly)) mods[[length(mods) + 1]] <- mod.poly
    ## linear model
    mod.lin <- try(glmmModel(dat))
    if (class(mod.lin) == "try-error") mod.lin <- try(glmmMod(dat, REML = FALSE))
    if (class(mod.lin) == "try-error") mod.lin <- try(glmmTMB::glmmTMB(Value ~ DV, data = dat, family = Gamma(link = 'log')))
    if (class(mod.lin) == "try-error") mod.lin <- NULL
    if(!is.null(mod.lin)) mods[[length(mods) + 1]] <- mod.lin
    ## gam modelr
    if (length(unique(dat$DV))>4) {
        mod.gam <- try(gamModel(dat))
        if (any(class(mod.gam) == "try-error")) mod.gam <- NULL
    } else { 
        mod.gam <- try(glmmModel(dat))
        if (class(mod.gam) == "try-error") {
            k = min(10, length(unique(dat$DV)))
            mod.gam <- try(mgcv::gam(Value ~ s(DV, bs = 'ps', k = k),# + s(WQ_SITE, bs = 're'),
                             data = dat,
                             ## family = glmmTMB::tweedie(link = "log")
                             family = Gamma(link = "log")
                             ))
            mod.gam}
        if (any(class(mod.gam) == "try-error")) mod.gam <- NULL
    }
    if(!is.null(mod.gam)) mods[[length(mods) + 1]] <- mod.gam
    if (length(mods) == 0) {
        return(NULL)
    } else if (length(mods) ==1) {
        return(mods[[1]])
    } else {
        wch <- which.min(sapply(mods, AIC))
        if (length(wch) == 0) return(mods[1])
        else return(mods[[wch]])
    }
    
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
        })
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


