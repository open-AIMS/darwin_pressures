## module load singularity
## singularity exec -B .:/home/project ../darwin_pressures.sif R
## ess-remote
source('../scripts/functions.R')

## ---- 50_summaries getFiles
files <- list.files(path = paste0(DATA_PATH, "modelled"),
                    pattern = "assoc.*",
                    full.names = TRUE)
## ----end

## ---- 50_summaries compile tree stats 
tree_stats <- files %>%
    map(.f = ~ readRDS(.x)) %>%
    map(.f = function(x) {
        x[["Assoc"]] = x[["Assoc"]] %>%
            dplyr::select(-value, -pred, -Obs, -Corr, -R2, -R2.sum, -R2.2, -Min, -Max, -g)
        x
        }) %>%
    ## map(.x, .f = ~ .x[["Assoc"]] <- .x[["Assoc"]][,c(-value, -pred, -Obs, -Corr, -R2)])
    ## map(.f = ~ replace(.x, is.null(.x$ZoneName), NA))
    map(.x,
       .f = ~ .x %>%
            ## Replace NULL ZoneName with 'Whole Harbour'
            modify_at(which(names(.x) == "ZoneName"), \(.y) 
                ifelse (is.null(.y), 'Whole Harbour', .y)
                ) %>%
            ## Replace NULL Type with the values from Source
            modify_at(which(names(.x) == "Type"), \(.y) 
                ifelse (is.null(.y), .x$Source, .y)
                ) 
        ) %>% 
    ## If there are still NULL elements, remove them and convert to tibble
    map(.f = ~ Filter(Negate(is.null), .x) %>%
            as_tibble()) %>% 
    map(.f = ~ .x %>%
            mutate(FOCAL_PRED = Assoc['name'][[1]]) %>%
            mutate(R2 = map(.x = Assoc['R2.2.sum'][[1]], 
                            .f = ~ round(.x$Median, 3))
                   ) %>%
            mutate(Trend = map(.x = Assoc['ES'][[1]],
                               .f = ~ .x)) %>% 
            mutate(R2trend = map2(.x = R2, .y = Trend,
                                  .f = ~ ifelse(.y$Trend == 'Negative', -1, 1) * .x)) %>% 
            mutate(Corr = map(.x = Assoc['Corr.sum'][[1]],
                              .f = ~ round(.x$Median, 3))
                   ) %>%
            unnest(c(FOCAL_PRED, Corr,R2, Trend, R2trend)) %>%
            dplyr::select(FOCAL_RESP, Type, ZoneName, include_lags, FOCAL_PRED,
                          Corr, R2, Trend, R2trend)
        )
## ----end
## ---- 50_summaries corr and R2 tables
corr.tab <- tree_stats %>% bind_rows() %>%
    pivot_wider(id_cols = c(-R2, -Trend, -R2trend),
                names_from = FOCAL_RESP,
                values_from = Corr
                )

R2.tab <- tree_stats %>% bind_rows() %>%
    pivot_wider(id_cols = c(-Corr, -R2, -Trend),
                names_from = FOCAL_RESP,
                values_from = R2trend
                )
save(corr.tab, R2.tab,
     file = paste0(DATA_PATH, 'modelled/Tables.RData'))
## ----end

## ---- 50_summaries functions
CorrColours <- function(x) {
    case_when(
        is.na(x) ~ "#FFFFFF",
        x < -0.8 ~ "#000000",  #Large
              x < -0.6 ~ "#000000",  #Madium
              x < -0.4 ~ "#000000",  #Small
              x < 0    ~ "#000000",  #"No effect"
              x < 0.4  ~ "#000000",  #"No effect"
              x < 0.6 ~  "#000000",  #Small
              x < 0.8 ~  "#000000",  #Medium
              x >= 0.8 ~ "#000000")  #Large
    }
## CorrFills <- function(x) {
##     case_when(
##         is.na(x) ~ "#FFFFFF",
##         x < -0.8 ~ "#de425b",  #Large
##         x < -0.6 ~ "#f48358",  #Madium
##         x < -0.4 ~ "#fcbe6e",  #Small
##         x < 0    ~ "#FFFFFF",  #"No effect"
##         x < 0.4  ~ "#FFFFFF",  #"No effect"
##         x < 0.6 ~  "#c5d275",  #Small
##         x < 0.8 ~  "#89b050",  #Medium
##         x >= 0.8 ~ "#488f31")  #Large
## }

CorrFills <- function(x) {
    case_when(
        is.na(x) ~ "#FFFFFF",
        x < -1*sqrt(0.3) ~ "#de425b",  #Large (R2: 0.4)
        x < -1*sqrt(0.2) ~ "#f48358",  #Madium
        x < -1*sqrt(0.1) ~ "#fcbe6e",  #Small
        x < 0    ~ "#FFFFFF",  #"No effect"
        x < sqrt(0.1)  ~ "#FFFFFF",  #"No effect"
        x < sqrt(0.2) ~  "#c5d275",  #Small
        x < sqrt(0.3) ~  "#89b050",  #Medium
        x >= sqrt(0.3) ~ "#488f31")  #Large
}

wrapper <- function(x, ...)
{
  sapply(x, function(y)
    paste(strwrap(y, ...), collapse = "<br>"))
}
## ----end

## ---- 50_summaries format tables
load(file = paste0(DATA_PATH, 'modelled/Tables.RData'))
units_lookup <- readRDS(paste0(DATA_PATH, 'processed/units_lookup.RData'))
units_lookup <- units_lookup %>%
    mutate(TableLabel = ifelse(Measure == 'Catch_ERP', paste0('Catchment ', TableLabel), TableLabel))
units_lookup.wlags <- units_lookup %>% rbind(
                         units_lookup %>%
                         mutate(Measure = paste0(Measure, '_lag1'),
                                TableLabel = paste0(TableLabel, " (lag 1)")),
                         units_lookup %>%
                         mutate(Measure = paste0(Measure, '_lag2'),
                                TableLabel = paste0(TableLabel, " (lag 2)"))
                         )

formatted.table.R2 <- R2.tab %>%
    mutate(Zones = ifelse(ZoneName == 'Whole Harbour', FALSE, TRUE)) %>%
    group_by(Type, Zones, include_lags) %>%
    nest() %>%
    mutate(data = map(.x = data,
                      .f = ~ .x %>%
                          left_join(units_lookup.wlags %>%
                                    dplyr::select(FOCAL_PRED = Measure, TableLabel)) %>%
                          dplyr::select(TableLabel, everything(),
                                        -FOCAL_PRED) %>% 
                          dplyr::rename(any_of(c(!!!deframe(units_lookup %>%
                                                             dplyr::select(TableLabel, Measure) %>%
                                                             mutate(TableLabel = str_wrap(TableLabel, 20)))))) %>%
                          rename_with(.fn = wrapper, width = 20)
                      )
           )
formatted.table.Corr <- corr.tab %>%
    mutate(Zones = ifelse(ZoneName == 'Whole Harbour', FALSE, TRUE)) %>%
    group_by(Type, Zones, include_lags) %>%
    nest() %>%
    mutate(data = map(.x = data,
                      .f = ~ .x %>%
                          left_join(units_lookup.wlags %>%
                                    dplyr::select(FOCAL_PRED = Measure, TableLabel)) %>%
                          dplyr::select(TableLabel, everything(),
                                        -FOCAL_PRED) %>% 
                          dplyr::rename(any_of(c(!!!deframe(units_lookup %>%
                                                             dplyr::select(TableLabel, Measure) %>%
                                                             mutate(TableLabel = str_wrap(TableLabel, 20)))))) %>%
                          rename_with(.fn = wrapper, width = 20)
                      )
           )



make_kable_table <- function(data, data2) {
    cols <- colnames(data2)[-1:-2]
    out <- data %>%
        kableExtra::kbl(align = c(rep('l', 2), rep('c', 6)),
                        escape = FALSE, format = 'html') %>%
        purrr::reduce(
                   which(names(data2) %in% cols),
                   function(x, y) {
                       col <- data2[,y]
                       print(col)
                       kableExtra::column_spec(x, y, background = CorrFills(col),
                                               color = CorrColours(col),
                                               border_left = "1px solid black",
                                               border_right = "1px solid black")
                   },
                   .init = .
               ) %>%
        row_spec(seq(0, nrow(data), 1), extra_css = "border: 1px solid black;") %>%
        kableExtra::kable_styling(font_size = 12) %>%
        suppressMessages() %>% suppressWarnings()
    if (length(unique(data$ZoneName))>1) {
        out <- out %>% pack_rows('',index = table(data$ZoneName)) %>%
            suppressMessages() %>% suppressWarnings()
    } else {
        out <- out %>%
            suppressMessages() %>% suppressWarnings()
    }
    return(out)
}

formatted.table <- formatted.table.R2 %>%
    full_join(formatted.table.Corr %>% dplyr::rename(data2 = data)) %>%
    mutate(across(c(data, data2),
                  ~ map(.x = .x, .f = ~.x %>%
                                     arrange(ZoneName) %>%
                                     rename(Pressure = TableLabel)
                        ))) %>%
    mutate(Kable = map(.x = data, .y = data2,
                       .f = ~ make_kable_table(.x, .y)
                       )
           )

save(formatted.table.R2, formatted.table.Corr, formatted.table,
     file = paste0(DATA_PATH, "modelled/formatted.tables.RData"))
## ----end


## load(file = paste0(DATA_PATH, "modelled/formatted.tables.RData"))
## cols <- colnames(formatted.table.Corr)[-1]

## tab <- formatted.table.R2 %>%         
##     mutate(TableLabel = str_replace(TableLabel, '\\$', 'AAAA')) %>%
##     rename(Pressure = TableLabel) %>%
##     kableExtra::kbl(align = c('l','c','c','c','c','c','c'),
##                     escape = FALSE,
##                     caption = 'Cross tabulation of partial R2 values for each Pressure and Stressor pair.  Cell colours represent correlative polarity (positive: green, negative: red) and absolute magnitude (higher values equate to darker shades).') %>%
##     purrr::reduce(
##                which(names(formatted.table.R2) %in% cols),
##                function(x, y) {
##                    col <- formatted.table.Corr[,y]
##                    kableExtra::column_spec(x, y, background = CorrFills(col),
##                                            color = CorrColours(col),
##                                            border_left = "1px solid black",
##                                            border_right = "1px solid black")
##                    },
##                .init = .
##                ) %>%
##     row_spec(seq(0, nrow(formatted.table.Corr), 1), extra_css = "border: 1px solid black;") %>%
##     ## row_spec(0, align = 'center') %>%
##     kableExtra::kable_styling(font_size = 12) 

