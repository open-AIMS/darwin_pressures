## module load singularity
## singularity exec -B .:/home/project ../darwin_pressures.sif R
## ess-remote
source('../scripts/functions.R')

## ---- Tree functions

bootstrappPredictions <- function(mod, newdata, n.trees) {
    pmap(.l = list(mod, n.trees),
         .f = ~ newdata %>%
             mutate(n = 1:n(),
                    Pred = predict(..1, newdata, n.trees = ..2),
                    Fit = exp(Pred)) %>%
             as.data.frame()) %>%
        enframe(name = "Boot") %>%
        unnest(value) %>%
        group_by(n) %>% 
        summarise(Mean = mean(Fit),
                  Median = median(Fit),
                  Lower = quantile(Fit, p = 0.025),
                  Upper = quantile(Fit, p = 0.975)) %>%
        bind_cols(newdata)
}


bootstrappPreds <- function(mod, newdata, n.trees) {
    pmap(.l = list(mod, n.trees),
         .f = ~ newdata %>%
             mutate(n = 1:n(),
                    Pred = predict(..1, newdata, n.trees = ..2),
                    Fit = exp(Pred)) %>%
             as.data.frame()) %>%
        enframe(name = "Boot") %>%
        unnest(value) ## %>%
        ## group_by(n) %>% 
        ## summarise(Mean = mean(Fit),
        ##           Median = median(Fit),
        ##           Lower = quantile(Fit, p = 0.025),
        ##           Upper = quantile(Fit, p = 0.975)) %>%
        ## bind_cols(newdata)
}


R2 <- function(.x) {
    .x <- .x %>% mutate(Value = Value^2) 
}

summ <- function(.x) {
    .x %>% 
    summarise(Mean = mean(Value),
              Median = median(Value),
              Lower = quantile(Value, p = 0.025),
              Upper = quantile(Value, p = 0.975))
    }

Corr <- function(mod, newdata, Resp, n.trees) {
    Resp <- Resp %>% setNames('Obs')
    pmap(.l = list(mod, n.trees),
         .f = ~ {
             ## print(..3)
             newdata %>%
             mutate(n = 1:n(),
                    Pred = predict(..1, newdata, n.trees = ..2),
                    Fit = exp(Pred)) %>%
                 as.data.frame()
             }) %>%
        enframe(name = "Boot") %>%
        mutate(value = map(.x = value, .f = ~ .x %>% cbind(Resp)),
               Value = map(.x = value, .f = ~ cor(.x$Fit, .x$Obs)),
               Value = ifelse(is.na(Value), 0, Value)) %>% 
        unnest(Value) %>%
        dplyr::select(Value)
    }

Rsqu <- function(mod, newdata, Resp, n.trees) {
    Resp <- Resp %>% setNames('Obs')
    pmap(.l = list(mod, n.trees),
         .f = ~ {
             ## print(..3)
             newdata %>%
             mutate(n = 1:n(),
                    Pred = predict(..1, newdata, n.trees = ..2),
                    Fit = exp(Pred)) %>%
                 as.data.frame()
             }) %>%
        enframe(name = "Boot") %>%
        mutate(value = map(.x = value, .f = ~ .x %>% cbind(Resp)),
               Value = map(.x = value, .f = ~ cor(.x$Fit, .x$Obs)),
               Value = ifelse(is.na(Value), 0, Value)) %>% 
        unnest(Value) %>%
        dplyr::select(Value)
    }

make_plot <- function(dat, focal_resp, focal_pred, Min, Max, R2) {
    units_lookup.wlags <- units_lookup %>% rbind(
                         units_lookup %>%
                         mutate(Measure = paste0(Measure, '_lag1'),
                                TableLabel = paste0(TableLabel, " (lag 1)")),
                         units_lookup %>%
                         mutate(Measure = paste0(Measure, '_lag2'),
                                TableLabel = paste0(TableLabel, " (lag 2)"))
                         )
    xlab <- units_lookup.wlags %>% filter(Measure == {{focal_pred}}) %>% pull(TableLabel)  
    ylab <- units_lookup %>% filter(Measure == {{focal_resp}}) %>% pull(TableLabel)  
    xscale <- units_lookup.wlags %>% filter(Measure == {{focal_pred}}) %>%
       pull(Trans) 
    print(paste0(focal_resp, ": ", focal_pred, ": ", xscale))
    dat %>%
        ggplot() +
        geom_ribbon(aes(ymin = Lower, ymax = Upper, x = Predictor_value),
                    fill = "orange", alpha = 0.5) +
        geom_line(aes(y = Median, x = Predictor_value), colour = "orange") +
        {if (xscale == 'log10') {
             scale_x_log10(str_wrap(xlab, 25))
         } else {
             scale_x_continuous(str_wrap(xlab, 25))
         }
        } +
        scale_y_continuous(ylab, limits = c(Min, Max)) +
        ggtitle(paste0('R2 = ',
                       round(R2$Median,3),
                       " [",
                       round(R2$Lower, 3),
                       ", ",
                       round(R2$Upper, 3),
                       "]")) +
        theme_classic()
    }

make_prediction_grid <- function(Pred, data.resp) {
    pterms <- terms[terms != Pred]
    data.resp %>%
        expand(!!Pred := modelr::seq_range(!! sym(Pred), n = 100)) %>%
        add_column(!!!pterms) %>%
        rename_with(.fn = ~ str_replace_all(.x, "\"", "")) %>%
        mutate(across(all_of(pterms), ~ NA)) %>%
        mutate(Predictor = {{Pred}},
               Predictor_value = .[[Pred]])
}

make_obs_prediction_grid <- function(Pred, data.resp) {
    pterms <- terms[terms != Pred]
    data.resp %>%
        mutate(across(all_of(pterms), ~ NA)) %>% 
        mutate(Predictor = {{Pred}},
               Predictor_value = .[[Pred]])
}


remove_terms <- function(form, term) {
  fterms <- terms(form)
  fac <- attr(fterms, "factors")
  if (all(!term %in% colnames(fac))) return(form)
  idx <- sapply(term, function(x) which(as.logical(fac[x, ])))
  new_fterms <- drop.terms(fterms, dropx = idx, keep.response = TRUE)
  return(formula(new_fterms))
}

LinearES <- function(Fit, Predictor_value) {
    len <- length(Fit)
    rise <- Fit[len] - Fit[1]
    run <- Predictor_value[len] - Predictor_value[1]
    rise/run
}
ES <- function(Fit) {
    len <- length(Fit)
    Fit[len]/Fit[1]
}

fitTreeModel <- function(data, boot = 1, include_lags = FALSE) {
    ## set_cores()
    cl <- makeCluster(min(10, boot))
    registerDoParallel(cl)
    ## mod <- vector('list', boot)
    ## mod <- foreach (b = 1:boot,
    ##                 .packages = c('tidyverse', 'gbm')
    ##                 ) %dopar% {
    ## for (b in 1:boot) {
    if (include_lags) {
        form <- log(Value) ~
            SL+ SL_lag1+ SL_lag2+ 
                Rainfall+ Rainfall_lag1+ Rainfall_lag2+
                Rainfall_anom+ Rainfall_anom_lag1+ Rainfall_anom_lag2+
                AirTemp+ AirTemp_lag1+ AirTemp_lag2+ 
                AirTemp_anom+ AirTemp_anom_lag1+ AirTemp_anom_lag2+
                SOI+ SOI_lag1+ SOI_lag2+
                SST_anom+ SST_anom_lag1+ SST_anom_lag2+ 
                Building_total+ Building_total_lag1+ Building_total_lag2+
                Building_eng+ Building_eng_lag1+ Building_eng_lag2+
                Building_non+ Building_non_lag1+ Building_non_lag2+ 
                Building_res+ Building_res_lag1+ Building_res_lag2+ 
                Ship_trade+ Ship_trade_lag1+ Ship_trade_lag2+
                Ship_cont+ Ship_cont_lag1+ Ship_cont_lag2+ 
                Ship_liq+ Ship_liq_lag1+ Ship_liq_lag2+ 
                Ship_car+ Ship_car_lag1+ Ship_car_lag2+
                Ship_rig+ Ship_rig_lag1+ Ship_rig_lag2+ 
                Ship_cruise+ Ship_cruise_lag1+ Ship_cruise_lag2+ 
                Ship_dry+ Ship_dry_lag1+ Ship_dry_lag2+ 
                Ship_live+ Ship_live_lag1+ Ship_live_lag2+
                TotalN+ TotalN_lag1+ TotalN_lag2+ 
                TotalP+ TotalP_lag1+ TotalP_lag2+ 
                TSS+ TSS_lag1+ TSS_lag2+ 
                VSS+ VSS_lag1+ VSS_lag2+ 
                Discharge+ Discharge_lag1+ Discharge_lag2+ 
                ERP+ ERP_lag1+ ERP_lag2+ 
                ERP_growth+ ERP_growth_lag1+ ERP_growth_lag2+ 
                GRP+ GRP_lag1+ GRP_lag2+ 
                GRP_change+ GRP_change_lag1+ GRP_change_lag2+ 
                Catch_ERP+ Catch_ERP_lag1+ Catch_ERP_lag2+
                POP+ POP_lag1+ POP_lag2+
                Fire_TotalKm_WA+ Fire_TotalKm_WA_lag1+ Fire_TotalKm_WA_lag2+ 
                Fire_TotalKm_STD+ Fire_TotalKm_STD_lag1+ Fire_TotalKm_STD_lag2+ 
                Fire_Areas+ Fire_Areas_lag1+ Fire_Areas_lag2+ 
                Fire_Areas_p + Fire_Areas_p_lag1+ Fire_Areas_p_lag2
    } else {
        form <- log(Value) ~
            SL+ #SL_lag1+ SL_lag2+ 
                Rainfall+ #Rainfall_lag1+ Rainfall_lag2+
                Rainfall_anom+ #Rainfall_anom_lag1+ Rainfall_anom_lag2+
                AirTemp+ #AirTemp_lag1+ AirTemp_lag2+ 
                AirTemp_anom+ #AirTemp_anom_lag1+ AirTemp_anom_lag2+
                SOI+ #SOI_lag1+ SOI_lag2+
                SST_anom+ #SST_anom_lag1+ SST_anom_lag2+ 
                Building_total+ #Building_total_lag1+ Building_total_lag2+
                Building_eng+ #Building_eng_lag1+ Building_eng_lag2+
                Building_non+ #Building_non_lag1+ Building_non_lag2+ 
                Building_res+ #Building_res_lag1+ Building_res_lag2+ 
                Ship_trade+ #Ship_trade_lag1+ Ship_trade_lag2+
                Ship_cont+ #Ship_cont_lag1+ Ship_cont_lag2+ 
                Ship_liq+ #Ship_liq_lag1+ Ship_liq_lag2+ 
                Ship_car+ #Ship_car_lag1+ Ship_car_lag2+
                Ship_rig+ #Ship_rig_lag1+ Ship_rig_lag2+ 
                Ship_cruise+ #Ship_cruise_lag1+ Ship_cruise_lag2+ 
                Ship_dry+ #Ship_dry_lag1+ Ship_dry_lag2+ 
                Ship_live+ #Ship_live_lag1+ Ship_live_lag2+
                TotalN+ #TotalN_lag1+ TotalN_lag2+ 
                TotalP+ #TotalP_lag1+ TotalP_lag2+ 
                TSS+ #TSS_lag1+ TSS_lag2+ 
                VSS+ #VSS_lag1+ VSS_lag2+ 
                Discharge+ #Discharge_lag1+ Discharge_lag2+ 
                ERP+ #ERP_lag1+ ERP_lag2+ 
                ERP_growth+ #ERP_growth_lag1+ ERP_growth_lag2+ 
                GRP+ #GRP_lag1+ GRP_lag2+ 
                GRP_change+ #GRP_change_lag1+ GRP_change_lag2+ 
                Catch_ERP+ #Catch_ERP_lag1+ Catch_ERP_lag2+
                POP+ #POP_lag1+ POP_lag2+
                Fire_TotalKm_WA+ #Fire_TotalKm_WA_lag1+ Fire_TotalKm_WA_lag2+ 
                Fire_TotalKm_STD+ #Fire_TotalKm_STD_lag1+ Fire_TotalKm_STD_lag2+ 
                Fire_Areas+ #Fire_Areas_lag1+ Fire_Areas_lag2+ 
                Fire_Areas_p #Fire_Areas_p_lag1+ Fire_Areas_p_lag2
    }
    mod <- mclapply(1:boot,
             function(i) {
                 data.boot <- data %>%
                     sample_n(size = nrow(data), replace = TRUE)
                 ## Remove any covariates that only have missing values
                 preds <- attr(terms(form), 'term.labels')
                 only.missing <- data.boot %>% summarise(across(preds, sum, na.rm = TRUE)) %>%
                     dplyr::select(preds) %>% 
                     dplyr::select(where(~sum(.) == 0)) %>%
                     colnames()
                 form <- remove_terms(form, only.missing)
                 ## mod[[b]] <-
                 if (nrow(data.boot) > length(preds)) {
                     gbm(form, 
                         data = data.boot,
                         n.trees = 10000,
                         interaction.depth = 9,
                         bag.fraction = 0.5,
                         cv.folds = 3,
                         n.cores = 1,#10,
                         shrinkage = 0.001,
                         n.minobsinnode = 1
                         )
                 } else {
                     return(NULL)
                 }
             }, mc.cores = 10)
    stopCluster(cl)
    mod
}


## ----end

## ---- Trees load lookups
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
## ---- Trees load data
data <- readRDS(file = paste0(DATA_PATH, "processed/data.RData"))
## ----end

## ---- Trees logTable
logTable <<- expand.grid(Resps = FOCAL_RESPS, Preds = FOCAL_PRESSURES$Measure, Lags = 0:2, Type = c('Routine', 'Discrete'))
write.csv(logTable, file = paste0(DATA_PATH, "modelled/logTable_trees.csv"))
cat(paste0("Resps,Preds,Lag,Type\n"),
    file = paste0(DATA_PATH, "modelled/log_trees.csv"))
## ----end

refit <- FALSE

if (1==2) {
    ## DONE
## ---- Trees fit model Routine no lags whole harbour
for (j in FOCAL_RESPS) {
    data.resp <- data %>%
        filter(Type == 'Routine') %>%
        droplevels() %>%
        filter(!is.na(!!sym(j))) %>%
        mutate(Value = !!sym(j),
               Measure = j,
               Value = ifelse(Value == 0, 0.01, Value))
    ## fit the model
    if (refit) {
        mod <- fitTreeModel(data.resp, boot = 100)
        save(mod, file = paste0(DATA_PATH, 'modelled/Trees_mod_routine_', j, '.RData'))
    }
    load(file = paste0(DATA_PATH, 'modelled/Trees_mod_routine_', j, '.RData'))
    ## determine the best number of trees
    n.trees <- lapply(mod, gbm.perf, method = 'cv')

    ## n.trees <- gbm.perf(mod, method = 'OOB')
    ## get relative influences
    terms <- attr(mod[[1]]$Terms, 'term.labels')
    preds <- data.frame(terms) %>% mutate(TermNumber = 1:n())
    sum.tab <- pmap(.l = list(mod, n.trees),
         .f = ~ summary(..1, n.trees = ..2) %>%
             as.data.frame()) %>%
        enframe(name = "Boot") %>%
        unnest(value) %>%
        group_by(var) %>%
        summarise(Mean = mean(rel.inf),
                  Median = median(rel.inf),
                  Lower = quantile(rel.inf, p = 0.025),
                  Upper = quantile(rel.inf, p = 0.975)) %>%
        mutate(Influential = ifelse(Median > 100/length(terms), TRUE, FALSE)) %>%
        dplyr::rename(Predictor = var) %>%
        left_join(preds, by = c('Predictor' = 'terms'))
    
    g1 <- sum.tab %>%
        left_join(units_lookup, by = c("Predictor" = "Measure")) %>%
        mutate(TableLabel = ifelse(duplicated(TableLabel), paste(TableLabel, 1), TableLabel)) %>% 
        mutate(TableLabel = forcats::fct_reorder(TableLabel, Median)) %>%
        ggplot() +
        geom_vline(xintercept = 100/nrow(preds), linetype = 'dashed') +
        geom_pointrange(aes(xmin = Lower, xmax = Upper,
                             x = Median, y = TableLabel, colour = Influential), show.legend = FALSE) +
        scale_x_continuous('Relative influence') +
        scale_colour_manual(breaks = c(FALSE, TRUE), values = c('black','orange')) +
        theme_classic() +
        theme(axis.title.y = element_blank())

    ## influential terms
    iterms <- sum.tab %>%
        ## mutate(Predictor = forcats::fct_reorder(Predictor, Median, .desc = TRUE)) %>%
        arrange(desc(Median)) %>%
        filter(Influential) %>% pull(Predictor)
    iterms <- iterms %>% setNames(iterms)
    a <- imap(.x = iterms,
              .f = ~ make_prediction_grid(.x, data.resp = data.resp)) %>%
        enframe() %>%
        mutate(preds = map(.x = value,
                          .f = ~ bootstrappPreds(mod, .x, n.trees))
               ) %>% 
        mutate(ES = map(.x = preds,
                               .f = ~ {
                                   .x %>% group_by(Boot) %>%
                                       mutate(LinearES = LinearES(Fit, Predictor_value)) %>%
                                       mutate(ES = ES(Fit)) %>%
                                       ungroup() %>%
                                       summarise(
                                           across(c(LinearES, ES),
                                                  list(Mean = mean,
                                                       Median = median,
                                                       Lower = ~ quantile(.x, p = 0.025),
                                                       Upper = ~ quantile(.x, p = 0.975)))) %>%
                                       mutate(Trend = ifelse(LinearES_Median>0, 'Positive', 'Negative'))
                               })) %>%
        mutate(pred = map(.x = value,
                          .f = ~ bootstrappPredictions(mod, .x, n.trees))
               ) %>% 
        mutate(Obs = pmap(.l = list(name),
                          .f = ~ make_obs_prediction_grid(..1,
                                                          data.resp = data.resp %>% filter(!is.na(!!sym(j))))),
               ## Correlation between obs and predicted - not all that useful
               ## (except as a stepping stone for one way to calculate R2)
               Corr = pmap(.l = list(Obs),
                           .f = ~ Corr(mod, ..1, na.omit(data.resp[,j]), n.trees)),
               Corr.sum = map(.x = Corr, .f = ~ summ(.x)), 
               R2 = map(.x = Corr, .f = ~ R2(.x)),
               R2.sum = map(.x = R2, .f = ~ summ(.x)),
               ## R2 calculated as 1 - (var(Pred - Obs)/var(Obs))
               R2.2 = pmap(.l = list(Obs),
                           .f = ~ Rsqu(mod, ..1, na.omit(data.resp[,j]), n.trees)),
               R2.2.sum = map(.x = R2.2, .f = ~ summ(.x))
               ) %>% 
        ## mutate(pred = map(.x = value,
        ##                   .f = ~ .x %>%
        ##                       mutate(Pred = predict(mod, .x, n.trees = n.trees),
        ##                              Fit = exp(Pred)))) %>%
        mutate(Range = map(.x = pred,
                           .f = ~ .x %>% summarise(Min = min(c(Median,Lower)),
                                                   Max = max(c(Median,Upper))))) %>%
        unnest(Range) %>%
        mutate(Min = min(Min),
               Max = max(Max)) %>%
        mutate(g = pmap(.l = list(pred, name, Min, Max, R2.2.sum),
                        .f = ~ make_plot(..1, focal_resp = j, focal_pred = ..2, Min = ..3, Max = ..4, R2 = ..5))) 
    metadata <- list(FOCAL_RESP = j,
                     Type = 'Routine',
                     Source = NULL,
                     include_lags = FALSE,
                     ZoneName = NULL,
                     Assoc = a)
    saveRDS(metadata, file = paste0(DATA_PATH, "modelled/assoc_stats_routine_",j,".RData"))
   ggsave(filename = paste0(FIGS_PATH, '/Trees_routine_',j,'.png'),
           g1 + wrap_plots(a$g) + plot_layout(widths = c(1, 4)),
           width = 15, height = 9)
    
}
## ----end
}


if (1==2) {
    ## Not DONE 22076
## ---- Trees fit model Routine with lags whole harbour
for (j in FOCAL_RESPS) {
    data.resp <- data %>%
        filter(Type == 'Routine') %>%
        droplevels() %>%
        filter(!is.na(!!sym(j))) %>%
        mutate(Value = !!sym(j),
               Measure = j,
               Value = ifelse(Value == 0, 0.01, Value))
    ## fit the model
    if (refit) {
        mod <- fitTreeModel(data.resp, boot = 100, include_lags = TRUE)
        save(mod, file = paste0(DATA_PATH, 'modelled/Trees_mod_routine_lags_', j, '.RData'))
    }
    load(file = paste0(DATA_PATH, 'modelled/Trees_mod_routine_lags_', j, '.RData'))
    ## determine the best number of trees
    n.trees <- lapply(mod, gbm.perf, method = 'cv')

    ## n.trees <- gbm.perf(mod, method = 'OOB')
    ## get relative influences
    terms <- attr(mod[[1]]$Terms, 'term.labels')
    preds <- data.frame(terms) %>% mutate(TermNumber = 1:n())
    sum.tab <- pmap(.l = list(mod, n.trees),
         .f = ~ summary(..1, n.trees = ..2) %>%
             as.data.frame()) %>%
        enframe(name = "Boot") %>%
        unnest(value) %>%
        group_by(var) %>%
        summarise(Mean = mean(rel.inf),
                  Median = median(rel.inf),
                  Lower = quantile(rel.inf, p = 0.025),
                  Upper = quantile(rel.inf, p = 0.975)) %>%
        mutate(Influential = ifelse(Median > 100/length(terms), TRUE, FALSE)) %>%
        dplyr::rename(Predictor = var) %>%
        left_join(preds, by = c('Predictor' = 'terms'))
    units_lookup.wlags <- units_lookup %>% rbind(
                         units_lookup %>%
                         mutate(Measure = paste0(Measure, '_lag1'),
                                TableLabel = paste0(TableLabel, " (lag 1)")),
                         units_lookup %>%
                         mutate(Measure = paste0(Measure, '_lag2'),
                                TableLabel = paste0(TableLabel, " (lag 2)"))
                         )
    g1 <- sum.tab %>%
        left_join(units_lookup.wlags, by = c("Predictor" = "Measure")) %>%
        mutate(TableLabel = ifelse(duplicated(TableLabel), paste(TableLabel, 1), TableLabel)) %>% 
        mutate(TableLabel = forcats::fct_reorder(TableLabel, Median)) %>%
        arrange(desc(Mean)) %>%
        slice(1:(n()/2)) %>%
        ggplot() +
        geom_vline(xintercept = 100/nrow(preds), linetype = 'dashed') +
        geom_pointrange(aes(xmin = Lower, xmax = Upper,
                             x = Median, y = TableLabel, colour = Influential), show.legend = FALSE) +
        scale_x_continuous('Relative influence') +
        scale_colour_manual(breaks = c(FALSE, TRUE), values = c('black','orange')) +
        theme_classic() +
        theme(axis.title.y = element_blank())

    ## influential terms
    iterms <- sum.tab %>%
        ## mutate(Predictor = forcats::fct_reorder(Predictor, Median, .desc = TRUE)) %>%
        arrange(desc(Median)) %>%
        filter(Influential) %>% pull(Predictor)
    iterms <- iterms %>% setNames(iterms)
    a <- imap(.x = iterms,
             .f = ~ make_prediction_grid(.x, data.resp = data.resp)) %>%
        enframe() %>%
        mutate(preds = map(.x = value,
                          .f = ~ bootstrappPreds(mod, .x, n.trees))
               ) %>% 
        mutate(ES = map(.x = preds,
                               .f = ~ {
                                   .x %>% group_by(Boot) %>%
                                       mutate(LinearES = LinearES(Fit, Predictor_value)) %>%
                                       mutate(ES = ES(Fit)) %>%
                                       ungroup() %>%
                                       summarise(
                                           across(c(LinearES, ES),
                                                  list(Mean = mean,
                                                       Median = median,
                                                       Lower = ~ quantile(.x, p = 0.025),
                                                       Upper = ~ quantile(.x, p = 0.975)))) %>%
                                       mutate(Trend = ifelse(LinearES_Median>0, 'Positive', 'Negative'))
                               })) %>%
        mutate(pred = map(.x = value,
                          .f = ~ bootstrappPredictions(mod, .x, n.trees))
                          ) %>% 
        mutate(Obs = pmap(.l = list(name),
                          .f = ~ make_obs_prediction_grid(..1,
                                                          data.resp = data.resp %>% filter(!is.na(!!sym(j))))),
               Corr = pmap(.l = list(Obs),
                           .f = ~ Corr(mod, ..1, na.omit(data.resp[,j]), n.trees)),
               Corr.sum = map(.x = Corr, .f = ~ summ(.x)), 
               R2 = map(.x = Corr, .f = ~ R2(.x)),
               R2.sum = map(.x = R2, .f = ~ summ(.x)),
               R2.2 = pmap(.l = list(Obs),
                           .f = ~ Rsqu(mod, ..1, na.omit(data.resp[,j]), n.trees)),
               R2.2.sum = map(.x = R2.2, .f = ~ summ(.x))
               )%>% 
        ## mutate(pred = map(.x = value,
        ##                   .f = ~ .x %>%
        ##                       mutate(Pred = predict(mod, .x, n.trees = n.trees),
        ##                              Fit = exp(Pred)))) %>%
        mutate(Range = map(.x = pred,
                           .f = ~ .x %>% summarise(Min = min(c(Median,Lower)),
                                                   Max = max(c(Median,Upper))))) %>%
        unnest(Range) %>%
        mutate(Min = min(Min),
               Max = max(Max)) %>%
        mutate(g = pmap(.l = list(pred, name, Min, Max, R2.2.sum),
                        .f = ~ make_plot(..1, focal_resp = j, focal_pred = ..2, Min = ..3, Max = ..4, R2 = ..5))) 

    metadata <- list(FOCAL_RESP = j,
                     Type = 'Routine',
                     Source = NULL,
                     include_lags = TRUE,
                     ZoneName = NULL,
                     Assoc = a)
    saveRDS(metadata, file = paste0(DATA_PATH, "modelled/assoc_stats_routine_logs_",j,".RData"))
   ggsave(filename = paste0(FIGS_PATH, '/Trees_routine_lags_',j,'.png'),
           g1 + wrap_plots(a$g) + plot_layout(widths = c(1, 4)),
           width = 15, height = 9)
    
}
## ----end
}


if (1==2) {
    ## Not DONE 22075
## ---- Trees fit model Discrete no lags whole harbour
for (j in FOCAL_RESPS) {
    data.resp <- data %>%
        filter(Source == 'Discrete') %>%
        droplevels() %>%
        filter(!is.na(!!sym(j))) %>%
        mutate(Value = !!sym(j),
               Measure = j,
               Value = ifelse(Value == 0, 0.01, Value))
    ## fit the model
    if (refit) {
        mod <- fitTreeModel(data.resp, boot = 100)
        save(mod, file = paste0(DATA_PATH, 'modelled/Trees_mod_discrete_', j, '.RData'))
    }
    load(file = paste0(DATA_PATH, 'modelled/Trees_mod_discrete_', j, '.RData'))
    ## determine the best number of trees
    n.trees <- lapply(mod, gbm.perf, method = 'cv')

    ## n.trees <- gbm.perf(mod, method = 'OOB')
    ## get relative influences
    terms <- attr(mod[[1]]$Terms, 'term.labels')
    preds <- data.frame(terms) %>% mutate(TermNumber = 1:n())
    sum.tab <- pmap(.l = list(mod, n.trees),
         .f = ~ summary(..1, n.trees = ..2) %>%
             as.data.frame()) %>%
        enframe(name = "Boot") %>%
        unnest(value) %>%
        group_by(var) %>%
        summarise(Mean = mean(rel.inf),
                  Median = median(rel.inf),
                  Lower = quantile(rel.inf, p = 0.025),
                  Upper = quantile(rel.inf, p = 0.975)) %>%
        mutate(Influential = ifelse(Median > 100/length(terms), TRUE, FALSE)) %>%
        dplyr::rename(Predictor = var) %>%
        left_join(preds, by = c('Predictor' = 'terms'))
    
    g1 <- sum.tab %>%
        left_join(units_lookup, by = c("Predictor" = "Measure")) %>%
        mutate(TableLabel = ifelse(duplicated(TableLabel), paste(TableLabel, 1), TableLabel)) %>% 
        mutate(TableLabel = forcats::fct_reorder(TableLabel, Median)) %>%
        ggplot() +
        geom_vline(xintercept = 100/nrow(preds), linetype = 'dashed') +
        geom_pointrange(aes(xmin = Lower, xmax = Upper,
                             x = Median, y = TableLabel, colour = Influential), show.legend = FALSE) +
        scale_x_continuous('Relative influence') +
        scale_colour_manual(breaks = c(FALSE, TRUE), values = c('black','orange')) +
        theme_classic() +
        theme(axis.title.y = element_blank())

    ## influential terms
    iterms <- sum.tab %>%
        ## mutate(Predictor = forcats::fct_reorder(Predictor, Median, .desc = TRUE)) %>%
        arrange(desc(Median)) %>%
        filter(Influential) %>% pull(Predictor)
    iterms <- iterms %>% setNames(iterms)

    a <- imap(.x = iterms,
             .f = ~ make_prediction_grid(.x, data.resp = data.resp)) %>%
        enframe() %>%
        mutate(preds = map(.x = value,
                          .f = ~ bootstrappPreds(mod, .x, n.trees))
               ) %>% 
        mutate(ES = map(.x = preds,
                               .f = ~ {
                                   .x %>% group_by(Boot) %>%
                                       mutate(LinearES = LinearES(Fit, Predictor_value)) %>%
                                       mutate(ES = ES(Fit)) %>%
                                       ungroup() %>%
                                       summarise(
                                           across(c(LinearES, ES),
                                                  list(Mean = mean,
                                                       Median = median,
                                                       Lower = ~ quantile(.x, p = 0.025),
                                                       Upper = ~ quantile(.x, p = 0.975)))) %>%
                                       mutate(Trend = ifelse(LinearES_Median>0, 'Positive', 'Negative'))
                               })) %>%
        mutate(pred = map(.x = value,
                          .f = ~ bootstrappPredictions(mod, .x, n.trees))
                          ) %>% 
        mutate(Obs = pmap(.l = list(name),
                          .f = ~ make_obs_prediction_grid(..1,
                                                          data.resp = data.resp %>% filter(!is.na(!!sym(j))))),
               Corr = pmap(.l = list(Obs),
                           .f = ~ Corr(mod, ..1, na.omit(data.resp[,j]), n.trees)),
               Corr.sum = map(.x = Corr, .f = ~ summ(.x)), 
               R2 = map(.x = Corr, .f = ~ R2(.x)),
               R2.sum = map(.x = R2, .f = ~ summ(.x)),
               R2.2 = pmap(.l = list(Obs),
                           .f = ~ Rsqu(mod, ..1, na.omit(data.resp[,j]), n.trees)),
               R2.2.sum = map(.x = R2.2, .f = ~ summ(.x))
               )%>% 
        ## mutate(pred = map(.x = value,
        ##                   .f = ~ .x %>%
        ##                       mutate(Pred = predict(mod, .x, n.trees = n.trees),
        ##                              Fit = exp(Pred)))) %>%
        mutate(Range = map(.x = pred,
                           .f = ~ .x %>% summarise(Min = min(c(Median,Lower)),
                                                   Max = max(c(Median,Upper))))) %>%
        unnest(Range) %>%
        mutate(Min = min(Min),
               Max = max(Max)) %>%
        mutate(g = pmap(.l = list(pred, name, Min, Max, R2.2.sum),
                        .f = ~ make_plot(..1, focal_resp = j, focal_pred = ..2, Min = ..3, Max = ..4, R2 = ..5))) 

    metadata <- list(FOCAL_RESP = j,
                     Type = NULL,
                     Source = 'Discrete',
                     include_lags = FALSE,
                     ZoneName = NULL,
                     Assoc = a)
    saveRDS(metadata, file = paste0(DATA_PATH, "modelled/assoc_stats_discrete_",j,".RData"))

   ggsave(filename = paste0(FIGS_PATH, '/Trees_discrete_',j,'.png'),
           g1 + wrap_plots(a$g) + plot_layout(widths = c(1, 4)),
           width = 15, height = 9)
    
}

## ----end
}


if (1==2) {
    ## Not DONE 22077
## ---- Trees fit model Discrete with lags whole harbour
for (j in FOCAL_RESPS) {
    data.resp <- data %>%
        filter(Source == 'Discrete') %>%
        droplevels() %>%
        filter(!is.na(!!sym(j))) %>%
        mutate(Value = !!sym(j),
               Measure = j,
               Value = ifelse(Value == 0, 0.01, Value))
    ## fit the model
    if (refit) {
        mod <- fitTreeModel(data.resp, boot = 100, include_lags = TRUE)
        save(mod, file = paste0(DATA_PATH, 'modelled/Trees_mod_discrete_lags_', j, '.RData'))
    }
    load(file = paste0(DATA_PATH, 'modelled/Trees_mod_discrete_lags_', j, '.RData'))
    ## determine the best number of trees
    n.trees <- lapply(mod, gbm.perf, method = 'cv')

    ## n.trees <- gbm.perf(mod, method = 'OOB')
    ## get relative influences
    terms <- attr(mod[[1]]$Terms, 'term.labels')
    preds <- data.frame(terms) %>% mutate(TermNumber = 1:n())
    sum.tab <- pmap(.l = list(mod, n.trees),
         .f = ~ summary(..1, n.trees = ..2) %>%
             as.data.frame()) %>%
        enframe(name = "Boot") %>%
        unnest(value) %>%
        group_by(var) %>%
        summarise(Mean = mean(rel.inf),
                  Median = median(rel.inf),
                  Lower = quantile(rel.inf, p = 0.025),
                  Upper = quantile(rel.inf, p = 0.975)) %>%
        mutate(Influential = ifelse(Median > 100/length(terms), TRUE, FALSE)) %>%
        dplyr::rename(Predictor = var) %>%
        left_join(preds, by = c('Predictor' = 'terms'))
    units_lookup.wlags <- units_lookup %>% rbind(
                         units_lookup %>%
                         mutate(Measure = paste0(Measure, '_lag1'),
                                TableLabel = paste0(TableLabel, " (lag 1)")),
                         units_lookup %>%
                         mutate(Measure = paste0(Measure, '_lag2'),
                                TableLabel = paste0(TableLabel, " (lag 2)"))
                         )
    
    g1 <- sum.tab %>%
        left_join(units_lookup.wlags, by = c("Predictor" = "Measure")) %>%
        mutate(TableLabel = ifelse(duplicated(TableLabel), paste(TableLabel, 1), TableLabel)) %>% 
        mutate(TableLabel = forcats::fct_reorder(TableLabel, Median)) %>%
        ggplot() +
        geom_vline(xintercept = 100/nrow(preds), linetype = 'dashed') +
        geom_pointrange(aes(xmin = Lower, xmax = Upper,
                             x = Median, y = TableLabel, colour = Influential), show.legend = FALSE) +
        scale_x_continuous('Relative influence') +
        scale_colour_manual(breaks = c(FALSE, TRUE), values = c('black','orange')) +
        theme_classic() +
        theme(axis.title.y = element_blank())

    ## influential terms
    iterms <- sum.tab %>%
        ## mutate(Predictor = forcats::fct_reorder(Predictor, Median, .desc = TRUE)) %>%
        arrange(desc(Median)) %>%
        filter(Influential) %>% pull(Predictor)
    iterms <- iterms %>% setNames(iterms)
    a <- imap(.x = iterms,
             .f = ~ make_prediction_grid(.x, data.resp = data.resp)) %>%
        enframe() %>%
        mutate(preds = map(.x = value,
                          .f = ~ bootstrappPreds(mod, .x, n.trees))
               ) %>% 
        mutate(ES = map(.x = preds,
                               .f = ~ {
                                   .x %>% group_by(Boot) %>%
                                       mutate(LinearES = LinearES(Fit, Predictor_value)) %>%
                                       mutate(ES = ES(Fit)) %>%
                                       ungroup() %>%
                                       summarise(
                                           across(c(LinearES, ES),
                                                  list(Mean = mean,
                                                       Median = median,
                                                       Lower = ~ quantile(.x, p = 0.025),
                                                       Upper = ~ quantile(.x, p = 0.975)))) %>%
                                       mutate(Trend = ifelse(LinearES_Median>0, 'Positive', 'Negative'))
                               })) %>%
        mutate(pred = map(.x = value,
                          .f = ~ bootstrappPredictions(mod, .x, n.trees))
                          ) %>% 
        mutate(Obs = pmap(.l = list(name),
                          .f = ~ make_obs_prediction_grid(..1,
                                                          data.resp = data.resp %>% filter(!is.na(!!sym(j))))),
               Corr = pmap(.l = list(Obs),
                           .f = ~ Corr(mod, ..1, na.omit(data.resp[,j]), n.trees)),
               Corr.sum = map(.x = Corr, .f = ~ summ(.x)), 
               R2 = map(.x = Corr, .f = ~ R2(.x)),
               R2.sum = map(.x = R2, .f = ~ summ(.x)),
               R2.2 = pmap(.l = list(Obs),
                           .f = ~ Rsqu(mod, ..1, na.omit(data.resp[,j]), n.trees)),
               R2.2.sum = map(.x = R2.2, .f = ~ summ(.x))
               )%>% 
        ## mutate(pred = map(.x = value,
        ##                   .f = ~ .x %>%
        ##                       mutate(Pred = predict(mod, .x, n.trees = n.trees),
        ##                              Fit = exp(Pred)))) %>%
        mutate(Range = map(.x = pred,
                           .f = ~ .x %>% summarise(Min = min(c(Median,Lower)),
                                                   Max = max(c(Median,Upper))))) %>%
        unnest(Range) %>%
        mutate(Min = min(Min),
               Max = max(Max)) %>%
        mutate(g = pmap(.l = list(pred, name, Min, Max, R2.2.sum),
                        .f = ~ make_plot(..1, focal_resp = j, focal_pred = ..2, Min = ..3, Max = ..4, R2 = ..5))) 

    metadata <- list(FOCAL_RESP = j,
                     Type = NULL,
                     Source = 'Discrete',
                     include_lags = TRUE,
                     ZoneName = NULL,
                     Assoc = a)
    saveRDS(metadata, file = paste0(DATA_PATH, "modelled/assoc_stats_discrete_lags_",j,".RData"))
   ggsave(filename = paste0(FIGS_PATH, '/Trees_discrete_lags_',j,'.png'),
           g1 + wrap_plots(a$g) + plot_layout(widths = c(1, 4)),
           width = 15, height = 9)
    
}
## ----end
}


if (1==2) {
    ## Not DONE 22168
## ---- Trees fit model Routine no lags by Zones
for (j in FOCAL_RESPS) {
    for (k in unique(data$ZoneName)) {
        data.resp <- data %>%
            filter(Type == 'Routine',
                   ZoneName == k) %>%
            droplevels() %>%
            filter(!is.na(!!sym(j))) %>%
            mutate(Value = !!sym(j),
                   Measure = j,
                   Value = ifelse(Value == 0, 0.01, Value))
        if (nrow(data.resp) == 0) next
        ## fit the model
        if (refit) {
            mod <- fitTreeModel(data.resp, boot = 100)
            save(mod, file = paste0(DATA_PATH, 'modelled/Trees_mod_routine_', j,'__',k, '.RData'))
        }
        load(file = paste0(DATA_PATH, 'modelled/Trees_mod_routine_', j,'__',k, '.RData'))
        ## remove NULLS
        mod[sapply(mod, is.null)] <- NULL
        if (length(mod) == 0) next
        ## determine the best number of trees
        n.trees <- lapply(mod, gbm.perf, method = 'cv')

        ## n.trees <- gbm.perf(mod, method = 'OOB')
        ## get relative influences
        terms <- NULL
        for (t in 1:length(mod)) {
            terms <- unique(c(terms, attr(mod[[t]]$Terms, 'term.labels')))
        }
        preds <- data.frame(terms) %>% mutate(TermNumber = 1:n())
        sum.tab <- pmap(.l = list(mod, n.trees),
                        .f = ~ summary(..1, n.trees = ..2) %>%
                            as.data.frame()) %>%
            enframe(name = "Boot") %>%
            unnest(value) %>%
            group_by(var) %>%
            summarise(Mean = mean(rel.inf),
                      Median = median(rel.inf),
                      Lower = quantile(rel.inf, p = 0.025),
                      Upper = quantile(rel.inf, p = 0.975)) %>%
            mutate(Influential = ifelse(Median > 100/length(terms), TRUE, FALSE)) %>%
            dplyr::rename(Predictor = var) %>%
            left_join(preds, by = c('Predictor' = 'terms'))
        
        g1 <- sum.tab %>%
            left_join(units_lookup, by = c("Predictor" = "Measure")) %>%
            mutate(TableLabel = ifelse(duplicated(TableLabel), paste(TableLabel, 1), TableLabel)) %>% 
            mutate(TableLabel = forcats::fct_reorder(TableLabel, Median)) %>%
            ggplot() +
            geom_vline(xintercept = 100/nrow(preds), linetype = 'dashed') +
            geom_pointrange(aes(xmin = Lower, xmax = Upper,
                                x = Median, y = TableLabel, colour = Influential), show.legend = FALSE) +
            scale_x_continuous('Relative influence') +
            scale_colour_manual(breaks = c(FALSE, TRUE), values = c('black','orange')) +
            theme_classic() +
            theme(axis.title.y = element_blank())

        ## influential terms
        iterms <- sum.tab %>%
            ## mutate(Predictor = forcats::fct_reorder(Predictor, Median, .desc = TRUE)) %>%
            arrange(desc(Median)) %>%
            filter(Influential) %>% pull(Predictor)
        iterms <- iterms %>% setNames(iterms)

        a <- imap(.x = iterms,
                  .f = ~ make_prediction_grid(.x, data.resp = data.resp)) %>%
            enframe() %>%
            mutate(preds = map(.x = value,
                               .f = ~ bootstrappPreds(mod, .x, n.trees))
                   ) %>% 
            mutate(ES = map(.x = preds,
                            .f = ~ {
                                .x %>% group_by(Boot) %>%
                                    mutate(LinearES = LinearES(Fit, Predictor_value)) %>%
                                    mutate(ES = ES(Fit)) %>%
                                    ungroup() %>%
                                    summarise(
                                        across(c(LinearES, ES),
                                               list(Mean = mean,
                                                    Median = median,
                                                    Lower = ~ quantile(.x, p = 0.025),
                                                    Upper = ~ quantile(.x, p = 0.975)))) %>%
                                    mutate(Trend = ifelse(LinearES_Median>0, 'Positive', 'Negative'))
                            })) %>%
            mutate(pred = map(.x = value,
                              .f = ~ bootstrappPredictions(mod, .x, n.trees))
                   ) %>% 
            mutate(Obs = pmap(.l = list(name),
                              .f = ~ make_obs_prediction_grid(..1,
                                                              data.resp = data.resp %>% filter(!is.na(!!sym(j))))),
                   Corr = pmap(.l = list(Obs),
                               .f = ~ Corr(mod, ..1, na.omit(data.resp[,j]), n.trees)),
                   Corr.sum = map(.x = Corr, .f = ~ summ(.x)), 
                   R2 = map(.x = Corr, .f = ~ R2(.x)),
                   R2.sum = map(.x = R2, .f = ~ summ(.x)),
                   R2.2 = pmap(.l = list(Obs),
                               .f = ~ Rsqu(mod, ..1, na.omit(data.resp[,j]), n.trees)),
                   R2.2.sum = map(.x = R2.2, .f = ~ summ(.x))
                   )%>% 
            ## mutate(pred = map(.x = value,
            ##                   .f = ~ .x %>%
            ##                       mutate(Pred = predict(mod, .x, n.trees = n.trees),
            ##                              Fit = exp(Pred)))) %>%
            mutate(Range = map(.x = pred,
                               .f = ~ .x %>% summarise(Min = min(c(Median,Lower)),
                                                       Max = max(c(Median,Upper))))) %>%
            unnest(Range) %>%
            mutate(Min = min(Min),
                   Max = max(Max)) %>%
            mutate(g = pmap(.l = list(pred, name, Min, Max, R2.2.sum),
                            .f = ~ make_plot(..1, focal_resp = j, focal_pred = ..2, Min = ..3, Max = ..4, R2 = ..5))) 



        
        ## a <- imap(.x = iterms,
        ##           .f = ~ make_prediction_grid(.x, data.resp = data.resp)) %>%
        ##     enframe() %>%
        ##     mutate(pred = map(.x = value,
        ##                       .f = ~ bootstrappPredictions(mod, .x, n.trees))
        ##            ) %>% 
        ##     ## mutate(pred = map(.x = value,
        ##     ##                   .f = ~ .x %>%
        ##     ##                       mutate(Pred = predict(mod, .x, n.trees = n.trees),
        ##     ##                              Fit = exp(Pred)))) %>%
        ##     mutate(Range = map(.x = pred,
        ##                        .f = ~ .x %>% summarise(Min = min(c(Median,Lower)),
        ##                                                Max = max(c(Median,Upper))))) %>%
        ##     unnest(Range) %>%
        ##     mutate(Min = min(Min),
        ##            Max = max(Max)) %>%
        ##     mutate(g = pmap(.l = list(pred, name, Min, Max),
        ##                     .f = ~ make_plot(..1, focal_resp = j, focal_pred = ..2, Min = ..3, Max = ..4)))

        metadata <- list(FOCAL_RESP = j,
                         Type = 'Routine',
                         Source = NULL,
                         include_lags = FALSE,
                         ZoneName = k,
                         Assoc = a)
        saveRDS(metadata, file = paste0(DATA_PATH, "modelled/assoc_stats_routine_",j,"__",k,".RData"))
        ggsave(filename = paste0(FIGS_PATH, '/Trees_routine_',j,'__',k,'.png'),
               g1 + wrap_plots(a$g) + plot_layout(widths = c(1, 4)),
               width = 15, height = 9)
    }
}
## ----end
}


if (1==2) {
    ## Not DONE 22079
## ---- Trees fit model Discrete no lags by Zones
for (j in FOCAL_RESPS) {
    for (k in unique(data$ZoneName)) {
        data.resp <- data %>%
            filter(Source == 'Discrete',
                   ZoneName == k) %>%
            droplevels() %>%
            filter(!is.na(!!sym(j))) %>%
            mutate(Value = !!sym(j),
                   Measure = j,
                   Value = ifelse(Value == 0, 0.01, Value))
        if (nrow(data.resp) == 0) next
        ## fit the model
        if (refit) {
            mod <- fitTreeModel(data.resp, boot = 100)
            save(mod, file = paste0(DATA_PATH, 'modelled/Trees_mod_discrete_', j,'__',k, '.RData'))
        }
        load(file = paste0(DATA_PATH, 'modelled/Trees_mod_discrete_', j,'__',k, '.RData'))
        ## remove NULLS
        mod[sapply(mod, is.null)] <- NULL
        if (length(mod) == 0) next
        ## determine the best number of trees
        n.trees <- lapply(mod, gbm.perf, method = 'cv')

        ## n.trees <- gbm.perf(mod, method = 'OOB')
        ## get relative influences
        terms <- NULL
        for (t in 1:length(mod)) {
            terms <- unique(c(terms, attr(mod[[t]]$Terms, 'term.labels')))
        }
        preds <- data.frame(terms) %>% mutate(TermNumber = 1:n())
        sum.tab <- pmap(.l = list(mod, n.trees),
                        .f = ~ summary(..1, n.trees = ..2) %>%
                            as.data.frame()) %>%
            enframe(name = "Boot") %>%
            unnest(value) %>%
            group_by(var) %>%
            summarise(Mean = mean(rel.inf),
                      Median = median(rel.inf),
                      Lower = quantile(rel.inf, p = 0.025),
                      Upper = quantile(rel.inf, p = 0.975)) %>%
            mutate(Influential = ifelse(Median > 100/length(terms), TRUE, FALSE)) %>%
            dplyr::rename(Predictor = var) %>%
            left_join(preds, by = c('Predictor' = 'terms'))
        
        g1 <- sum.tab %>%
            left_join(units_lookup, by = c("Predictor" = "Measure")) %>%
            mutate(TableLabel = ifelse(duplicated(TableLabel), paste(TableLabel, 1), TableLabel)) %>% 
            mutate(TableLabel = forcats::fct_reorder(TableLabel, Median)) %>%
            ggplot() +
            geom_vline(xintercept = 100/nrow(preds), linetype = 'dashed') +
            geom_pointrange(aes(xmin = Lower, xmax = Upper,
                                x = Median, y = TableLabel, colour = Influential), show.legend = FALSE) +
            scale_x_continuous('Relative influence') +
            scale_colour_manual(breaks = c(FALSE, TRUE), values = c('black','orange')) +
            theme_classic() +
            theme(axis.title.y = element_blank())

        ## influential terms
        iterms <- sum.tab %>%
            ## mutate(Predictor = forcats::fct_reorder(Predictor, Median, .desc = TRUE)) %>%
            arrange(desc(Median)) %>%
            filter(Influential) %>% pull(Predictor)
        iterms <- iterms %>% setNames(iterms)
        ## a <- imap(.x = iterms,
        ##           .f = ~ make_prediction_grid(.x, data.resp = data.resp)) %>%
        ##     enframe() %>%
        ##     mutate(pred = map(.x = value,
        ##                       .f = ~ bootstrappPredictions(mod, .x, n.trees))
        ##            ) %>% 
        ##     ## mutate(pred = map(.x = value,
        ##     ##                   .f = ~ .x %>%
        ##     ##                       mutate(Pred = predict(mod, .x, n.trees = n.trees),
        ##     ##                              Fit = exp(Pred)))) %>%
        ##     mutate(Range = map(.x = pred,
        ##                        .f = ~ .x %>% summarise(Min = min(c(Median,Lower)),
        ##                                                Max = max(c(Median,Upper))))) %>%
        ##     unnest(Range) %>%
        ##     mutate(Min = min(Min),
        ##            Max = max(Max)) %>%
        ##     mutate(g = pmap(.l = list(pred, name, Min, Max),
        ##                     .f = ~ make_plot(..1, focal_resp = j, focal_pred = ..2, Min = ..3, Max = ..4)))
        a <- imap(.x = iterms,
                  .f = ~ make_prediction_grid(.x, data.resp = data.resp)) %>%
            enframe() %>%
            mutate(preds = map(.x = value,
                               .f = ~ bootstrappPreds(mod, .x, n.trees))
                   ) %>% 
            mutate(ES = map(.x = preds,
                            .f = ~ {
                                .x %>% group_by(Boot) %>%
                                    mutate(LinearES = LinearES(Fit, Predictor_value)) %>%
                                    mutate(ES = ES(Fit)) %>%
                                    ungroup() %>%
                                    summarise(
                                        across(c(LinearES, ES),
                                               list(Mean = mean,
                                                    Median = median,
                                                    Lower = ~ quantile(.x, p = 0.025),
                                                    Upper = ~ quantile(.x, p = 0.975)))) %>%
                                    mutate(Trend = ifelse(LinearES_Median>0, 'Positive', 'Negative'))
                            })) %>%
            mutate(pred = map(.x = value,
                              .f = ~ bootstrappPredictions(mod, .x, n.trees))
                   ) %>% 
            mutate(Obs = pmap(.l = list(name),
                              .f = ~ make_obs_prediction_grid(..1,
                                                              data.resp = data.resp %>% filter(!is.na(!!sym(j))))),
                   Corr = pmap(.l = list(Obs),
                               .f = ~ Corr(mod, ..1, na.omit(data.resp[,j]), n.trees)),
                   Corr.sum = map(.x = Corr, .f = ~ summ(.x)), 
                   R2 = map(.x = Corr, .f = ~ R2(.x)),
                   R2.sum = map(.x = R2, .f = ~ summ(.x)),
                   R2.2 = pmap(.l = list(Obs),
                               .f = ~ Rsqu(mod, ..1, na.omit(data.resp[,j]), n.trees)),
                   R2.2.sum = map(.x = R2.2, .f = ~ summ(.x))
                   )%>% 
            ## mutate(pred = map(.x = value,
            ##                   .f = ~ .x %>%
            ##                       mutate(Pred = predict(mod, .x, n.trees = n.trees),
            ##                              Fit = exp(Pred)))) %>%
            mutate(Range = map(.x = pred,
                               .f = ~ .x %>% summarise(Min = min(c(Median,Lower)),
                                                       Max = max(c(Median,Upper))))) %>%
            unnest(Range) %>%
            mutate(Min = min(Min),
                   Max = max(Max)) %>%
            mutate(g = pmap(.l = list(pred, name, Min, Max, R2.2.sum),
                            .f = ~ make_plot(..1, focal_resp = j, focal_pred = ..2, Min = ..3, Max = ..4, R2 = ..5))) 


        metadata <- list(FOCAL_RESP = j,
                         Type = NULL,
                         Source = 'Discrete',
                         include_lags = FALSE,
                         ZoneName = k,
                         Assoc = a)
        saveRDS(metadata, file = paste0(DATA_PATH, "modelled/assoc_stats_discrete_",j,"__",k,".RData"))

        ggsave(filename = paste0(FIGS_PATH, '/Trees_discrete_',j,'__',k,'.png'),
               g1 + wrap_plots(a$g) + plot_layout(widths = c(1, 4)),
               width = 15, height = 9)
    }
}


## ----end
}

if (1==2) {
    ## Not DONE 22080
## ---- Trees fit model Discrete with lags by Zones
for (j in FOCAL_RESPS) {
    for (k in unique(data$ZoneName)) {
        data.resp <- data %>%
            filter(Source == 'Discrete',
                   ZoneName == k) %>%
            droplevels() %>%
            filter(!is.na(!!sym(j))) %>%
            mutate(Value = !!sym(j),
                   Measure = j,
                   Value = ifelse(Value == 0, 0.01, Value))
        if (nrow(data.resp) == 0) next
        ## fit the model
        if (refit) {
            mod <- fitTreeModel(data.resp, boot = 100, include_lags = TRUE)
            save(mod, file = paste0(DATA_PATH, 'modelled/Trees_mod_discrete_lags_', j,'__',k, '.RData'))
        }
        load(file = paste0(DATA_PATH, 'modelled/Trees_mod_discrete_lags_', j,'__',k, '.RData'))
        ## remove NULLS
        mod[sapply(mod, is.null)] <- NULL
        if (length(mod) == 0) next
        ## determine the best number of trees
        n.trees <- lapply(mod, gbm.perf, method = 'cv')

        ## n.trees <- gbm.perf(mod, method = 'OOB')
        ## get relative influences
        terms <- NULL
        for (t in 1:length(mod)) {
            terms <- unique(c(terms, attr(mod[[t]]$Terms, 'term.labels')))
        }
        preds <- data.frame(terms) %>% mutate(TermNumber = 1:n())
        sum.tab <- pmap(.l = list(mod, n.trees),
                        .f = ~ summary(..1, n.trees = ..2) %>%
                            as.data.frame()) %>%
            enframe(name = "Boot") %>%
            unnest(value) %>%
            group_by(var) %>%
            summarise(Mean = mean(rel.inf),
                      Median = median(rel.inf),
                      Lower = quantile(rel.inf, p = 0.025),
                      Upper = quantile(rel.inf, p = 0.975)) %>%
            mutate(Influential = ifelse(Median > 100/length(terms), TRUE, FALSE)) %>%
            dplyr::rename(Predictor = var) %>%
            left_join(preds, by = c('Predictor' = 'terms'))
        
        g1 <- sum.tab %>%
            left_join(units_lookup, by = c("Predictor" = "Measure")) %>%
            mutate(TableLabel = ifelse(duplicated(TableLabel), paste(TableLabel, 1), TableLabel)) %>% 
            mutate(TableLabel = forcats::fct_reorder(TableLabel, Median)) %>%
            ggplot() +
            geom_vline(xintercept = 100/nrow(preds), linetype = 'dashed') +
            geom_pointrange(aes(xmin = Lower, xmax = Upper,
                                x = Median, y = TableLabel, colour = Influential), show.legend = FALSE) +
            scale_x_continuous('Relative influence') +
            scale_colour_manual(breaks = c(FALSE, TRUE), values = c('black','orange')) +
            theme_classic() +
            theme(axis.title.y = element_blank())

        ## influential terms
        iterms <- sum.tab %>%
            ## mutate(Predictor = forcats::fct_reorder(Predictor, Median, .desc = TRUE)) %>%
            arrange(desc(Median)) %>%
            filter(Influential) %>% pull(Predictor)
        iterms <- iterms %>% setNames(iterms)
        ## a <- imap(.x = iterms,
        ##           .f = ~ make_prediction_grid(.x, data.resp = data.resp)) %>%
        ##     enframe() %>%
        ##     mutate(pred = map(.x = value,
        ##                       .f = ~ bootstrappPredictions(mod, .x, n.trees))
        ##            ) %>% 
        ##     ## mutate(pred = map(.x = value,
        ##     ##                   .f = ~ .x %>%
        ##     ##                       mutate(Pred = predict(mod, .x, n.trees = n.trees),
        ##     ##                              Fit = exp(Pred)))) %>%
        ##     mutate(Range = map(.x = pred,
        ##                        .f = ~ .x %>% summarise(Min = min(c(Median,Lower)),
        ##                                                Max = max(c(Median,Upper))))) %>%
        ##     unnest(Range) %>%
        ##     mutate(Min = min(Min),
        ##            Max = max(Max)) %>%
        ##     mutate(g = pmap(.l = list(pred, name, Min, Max),
        ##                     .f = ~ make_plot(..1, focal_resp = j, focal_pred = ..2, Min = ..3, Max = ..4)))
        a <- imap(.x = iterms,
                  .f = ~ make_prediction_grid(.x, data.resp = data.resp)) %>%
            enframe() %>%
            mutate(preds = map(.x = value,
                               .f = ~ bootstrappPreds(mod, .x, n.trees))
                   ) %>% 
            mutate(ES = map(.x = preds,
                            .f = ~ {
                                .x %>% group_by(Boot) %>%
                                    mutate(LinearES = LinearES(Fit, Predictor_value)) %>%
                                    mutate(ES = ES(Fit)) %>%
                                    ungroup() %>%
                                    summarise(
                                        across(c(LinearES, ES),
                                               list(Mean = mean,
                                                    Median = median,
                                                    Lower = ~ quantile(.x, p = 0.025),
                                                    Upper = ~ quantile(.x, p = 0.975)))) %>%
                                    mutate(Trend = ifelse(LinearES_Median>0, 'Positive', 'Negative'))
                            })) %>%
            mutate(pred = map(.x = value,
                              .f = ~ bootstrappPredictions(mod, .x, n.trees))
                   ) %>% 
            mutate(Obs = pmap(.l = list(name),
                              .f = ~ make_obs_prediction_grid(..1,
                                                              data.resp = data.resp %>% filter(!is.na(!!sym(j))))),
                   Corr = pmap(.l = list(Obs),
                               .f = ~ Corr(mod, ..1, na.omit(data.resp[,j]), n.trees)),
                   Corr.sum = map(.x = Corr, .f = ~ summ(.x)), 
                   R2 = map(.x = Corr, .f = ~ R2(.x)),
                   R2.sum = map(.x = R2, .f = ~ summ(.x)),
                   R2.2 = pmap(.l = list(Obs),
                               .f = ~ Rsqu(mod, ..1, na.omit(data.resp[,j]), n.trees)),
                   R2.2.sum = map(.x = R2.2, .f = ~ summ(.x))
                   )%>% 
            ## mutate(pred = map(.x = value,
            ##                   .f = ~ .x %>%
            ##                       mutate(Pred = predict(mod, .x, n.trees = n.trees),
            ##                              Fit = exp(Pred)))) %>%
            mutate(Range = map(.x = pred,
                               .f = ~ .x %>% summarise(Min = min(c(Median,Lower)),
                                                       Max = max(c(Median,Upper))))) %>%
            unnest(Range) %>%
            mutate(Min = min(Min),
                   Max = max(Max)) %>%
            mutate(g = pmap(.l = list(pred, name, Min, Max, R2.2.sum),
                            .f = ~ make_plot(..1, focal_resp = j, focal_pred = ..2, Min = ..3, Max = ..4, R2 = ..5))) 


        metadata <- list(FOCAL_RESP = j,
                         Type = NULL,
                         Source = 'Discrete',
                         include_lags = TRUE,
                         ZoneName = k,
                         Assoc = a)
        saveRDS(metadata, file = paste0(DATA_PATH, "modelled/assoc_stats_discrete_lags_",j,"__",k,".RData"))

        ggsave(filename = paste0(FIGS_PATH, '/Trees_discrete_lags_',j,'__',k,'.png'),
               g1 + wrap_plots(a$g) + plot_layout(widths = c(1, 4)),
               width = 15, height = 9)
    }
}


## ----end
}

if (1==2) {
    ## Not DONE 22167
## ---- Trees fit model Routine with lags by Zones
for (j in FOCAL_RESPS) {
    for (k in unique(data$ZoneName)) {
        data.resp <- data %>%
            filter(Type == 'Routine',
                   ZoneName == k) %>%
            droplevels() %>%
            filter(!is.na(!!sym(j))) %>%
            mutate(Value = !!sym(j),
                   Measure = j,
                   Value = ifelse(Value == 0, 0.01, Value))
        if (nrow(data.resp) == 0) next
        ## fit the model
        if (refit) {
            mod <- fitTreeModel(data.resp, boot = 100, include_lags = TRUE)
            save(mod, file = paste0(DATA_PATH, 'modelled/Trees_mod_routine_lags_', j,'__',k, '.RData'))
        }
        load(file = paste0(DATA_PATH, 'modelled/Trees_mod_routine_lags_', j,'__',k, '.RData'))
        ## remove NULLS
        mod[sapply(mod, is.null)] <- NULL
        if (length(mod) == 0) next
        ## determine the best number of trees
        n.trees <- lapply(mod, gbm.perf, method = 'cv')

        ## n.trees <- gbm.perf(mod, method = 'OOB')
        ## get relative influences
        terms <- NULL
        for (t in 1:length(mod)) {
            terms <- unique(c(terms, attr(mod[[t]]$Terms, 'term.labels')))
        }
        preds <- data.frame(terms) %>% mutate(TermNumber = 1:n())
        sum.tab <- pmap(.l = list(mod, n.trees),
                        .f = ~ summary(..1, n.trees = ..2) %>%
                            as.data.frame()) %>%
            enframe(name = "Boot") %>%
            unnest(value) %>%
            group_by(var) %>%
            summarise(Mean = mean(rel.inf),
                      Median = median(rel.inf),
                      Lower = quantile(rel.inf, p = 0.025),
                      Upper = quantile(rel.inf, p = 0.975)) %>%
            mutate(Influential = ifelse(Median > 100/length(terms), TRUE, FALSE)) %>%
            dplyr::rename(Predictor = var) %>%
            left_join(preds, by = c('Predictor' = 'terms'))
        
        g1 <- sum.tab %>%
            left_join(units_lookup, by = c("Predictor" = "Measure")) %>%
            mutate(TableLabel = ifelse(duplicated(TableLabel), paste(TableLabel, 1), TableLabel)) %>% 
            mutate(TableLabel = forcats::fct_reorder(TableLabel, Median)) %>%
            ggplot() +
            geom_vline(xintercept = 100/nrow(preds), linetype = 'dashed') +
            geom_pointrange(aes(xmin = Lower, xmax = Upper,
                                x = Median, y = TableLabel, colour = Influential), show.legend = FALSE) +
            scale_x_continuous('Relative influence') +
            scale_colour_manual(breaks = c(FALSE, TRUE), values = c('black','orange')) +
            theme_classic() +
            theme(axis.title.y = element_blank())

        ## influential terms
        iterms <- sum.tab %>%
            ## mutate(Predictor = forcats::fct_reorder(Predictor, Median, .desc = TRUE)) %>%
            arrange(desc(Median)) %>%
            filter(Influential) %>% pull(Predictor)
        iterms <- iterms %>% setNames(iterms)
        ## a <- imap(.x = iterms,
        ##           .f = ~ make_prediction_grid(.x, data.resp = data.resp)) %>%
        ##     enframe() %>%
        ##     mutate(pred = map(.x = value,
        ##                       .f = ~ bootstrappPredictions(mod, .x, n.trees))
        ##            ) %>% 
        ##     ## mutate(pred = map(.x = value,
        ##     ##                   .f = ~ .x %>%
        ##     ##                       mutate(Pred = predict(mod, .x, n.trees = n.trees),
        ##     ##                              Fit = exp(Pred)))) %>%
        ##     mutate(Range = map(.x = pred,
        ##                        .f = ~ .x %>% summarise(Min = min(c(Median,Lower)),
        ##                                                Max = max(c(Median,Upper))))) %>%
        ##     unnest(Range) %>%
        ##     mutate(Min = min(Min),
        ##            Max = max(Max)) %>%
        ##     mutate(g = pmap(.l = list(pred, name, Min, Max),
        ##                     .f = ~ make_plot(..1, focal_resp = j, focal_pred = ..2, Min = ..3, Max = ..4)))
        a <- imap(.x = iterms,
                  .f = ~ make_prediction_grid(.x, data.resp = data.resp)) %>%
            enframe() %>%
            mutate(preds = map(.x = value,
                               .f = ~ bootstrappPreds(mod, .x, n.trees))
                   ) %>% 
            mutate(ES = map(.x = preds,
                            .f = ~ {
                                .x %>% group_by(Boot) %>%
                                    mutate(LinearES = LinearES(Fit, Predictor_value)) %>%
                                    mutate(ES = ES(Fit)) %>%
                                    ungroup() %>%
                                    summarise(
                                        across(c(LinearES, ES),
                                               list(Mean = mean,
                                                    Median = median,
                                                    Lower = ~ quantile(.x, p = 0.025),
                                                    Upper = ~ quantile(.x, p = 0.975)))) %>%
                                    mutate(Trend = ifelse(LinearES_Median>0, 'Positive', 'Negative'))
                            })) %>%
            mutate(pred = map(.x = value,
                              .f = ~ bootstrappPredictions(mod, .x, n.trees))
                   ) %>% 
            mutate(Obs = pmap(.l = list(name),
                              .f = ~ make_obs_prediction_grid(..1,
                                                              data.resp = data.resp %>% filter(!is.na(!!sym(j))))),
                   Corr = pmap(.l = list(Obs),
                               .f = ~ Corr(mod, ..1, na.omit(data.resp[,j]), n.trees)),
                   Corr.sum = map(.x = Corr, .f = ~ summ(.x)), 
                   R2 = map(.x = Corr, .f = ~ R2(.x)),
                   R2.sum = map(.x = R2, .f = ~ summ(.x)),
                   R2.2 = pmap(.l = list(Obs),
                               .f = ~ Rsqu(mod, ..1, na.omit(data.resp[,j]), n.trees)),
                   R2.2.sum = map(.x = R2.2, .f = ~ summ(.x))
                   )%>% 
            ## mutate(pred = map(.x = value,
            ##                   .f = ~ .x %>%
            ##                       mutate(Pred = predict(mod, .x, n.trees = n.trees),
            ##                              Fit = exp(Pred)))) %>%
            mutate(Range = map(.x = pred,
                               .f = ~ .x %>% summarise(Min = min(c(Median,Lower)),
                                                       Max = max(c(Median,Upper))))) %>%
            unnest(Range) %>%
            mutate(Min = min(Min),
                   Max = max(Max)) %>%
            mutate(g = pmap(.l = list(pred, name, Min, Max, R2.2.sum),
                            .f = ~ make_plot(..1, focal_resp = j, focal_pred = ..2, Min = ..3, Max = ..4, R2 = ..5))) 


        metadata <- list(FOCAL_RESP = j,
                         Type = 'Routine',
                         Source = NULL,
                         include_lags = TRUE,
                         ZoneName = k,
                         Assoc = a)
        saveRDS(metadata, file = paste0(DATA_PATH, "modelled/assoc_stats_routine_lags_",j,"__",k,".RData"))

        ggsave(filename = paste0(FIGS_PATH, '/Trees_routine_lags_',j,'__',k,'.png'),
               g1 + wrap_plots(a$g) + plot_layout(widths = c(1, 4)),
               width = 15, height = 9)
    }
}


## ----end
}

##
