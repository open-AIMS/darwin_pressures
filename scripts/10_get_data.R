source('../scripts/functions.R')

## WQ data
{
    ## ---- get data WQ
    files <- list.files(path = paste0(DATA_PATH, "primary/WQ Data"),
                        pattern = '.*csv',
                        recursive = TRUE, full.names = TRUE) %>%
        str_subset(paste0(DATA_PATH, "primary/WQ Data/WQ.*"))
    ## ----end

    ## ---- get data WQ 2012_2015
    files %>% str_subset("2012") %>%
        map_df(read_csv) %>%
        suppressWarnings() %>%
        suppressMessages() ->
        wq
    saveRDS(wq, file = paste0(DATA_PATH, "primary/wq_2012.RData"))
    ## ----end
    ## ---- glimpse WQ 2012_2015
    glimpse(wq)
    ## ----end
    ## ---- glimpse-like WQ 2012_2015
    glimpse_like_table(dat = wq) %>%
      padding(padding.top = 0.1, padding.bottom = 0.1, part = "all") %>%
      set_caption("Overview of the supplied 2012-2015 water quality data.",
                 word_stylename = "Table Caption",
                 autonum = run_autonum(seq_id = "tab",
                                       bkm = "dataSource-2015")
                 )
    ## ----end

    ## ---- get data WQ 2016_2022
    files %>% str_subset("2016") %>%
        map_df(read_csv) %>%
        suppressWarnings() %>%
        suppressMessages() ->
        wq
    saveRDS(wq, file = paste0(DATA_PATH, "primary/wq_2016.RData"))
    ## ----end
    ## ---- glimpse WQ 2016_2022
    glimpse(wq)
    ## ----end
    ## ---- glimpse-like WQ 2016_2022
    glimpse_like_table(dat = wq) %>%
      padding(padding.top = 0.1, padding.bottom = 0.1, part = "all") %>%
      set_caption(
        as_paragraph(
          as_chunk("Overview of the supplied 2016-2022 water quality data.")
         ),
        word_stylename = "Table Caption",
        autonum = run_autonum(seq_id = "tab",
                              bkm = "dataSource-2022")
                 )
    ## ----end

    ## ---- get data WQ 2018 alterations
    wq <- read_csv(file = paste0(DATA_PATH, "primary/wq_2018_editV2.csv"),
                   trim_ws = TRUE) 
    saveRDS(wq, file = paste0(DATA_PATH, "primary/wq_2018.RData"))
    ## ----end
    ## ---- glimpse WQ 2018 alterations
    glimpse(wq)
    ## ----end
    ## ---- glimpse-like WQ 2018 alterations
    glimpse_like_table(dat = wq) %>%
      padding(padding.top = 0.1, padding.bottom = 0.1, part = "all") %>%
      set_caption(
        as_paragraph(
          as_chunk("Overview of the supplied modified 2018 water quality data.")
         ),
        word_stylename = "Table Caption",
        autonum = run_autonum(seq_id = "tab",
                              bkm = "dataSource-2018")
                 )
    ## ----end

    ## ---- get data Routine sites
    routine_sites <- read_csv(file = paste0(DATA_PATH, "primary/Routine Sites and analysis_final.csv"),
                   trim_ws = TRUE) 
    saveRDS(routine_sites, file = paste0(DATA_PATH, "primary/wq_routine_sites.RData"))
    ## ----end
    ## ---- glimpse WQ Routine sites 
    glimpse(routine_sites)
    ## ----end
    ## ---- glimpse-like WQ Routine sites
    glimpse_like_table(dat = routine_sites) %>%
      padding(padding.top = 0.1, padding.bottom = 0.1, part = "all") %>%
      set_caption(
        as_paragraph(
          as_chunk("Overview of the lookup of list of routine sites from the water quality data.")
         ),
        word_stylename = "Table Caption",
        autonum = run_autonum(seq_id = "tab",
                              bkm = "dataSource-routine")
                 )
    ## ----end

}

## Issues
## - 2016-2022 are very consistent, but these do not correspond to prior
## - for a given Region/Zone, there are multiple Lat/Long

## PRIMARY data
{
    ## ---- get data PRIMARY
    primary <- read_excel(paste0(DATA_PATH, "primary/Primary Point Source Dataset_22.xlsx"),
                     sheet = "Point sources-Primary DB",
                     trim_ws = TRUE)
    ## ----end

    ## ---- glimpse PRIMARY
    glimpse(primary)
    ## ----end
    ## ---- glimpse-like PRIMARY
    glimpse_like_table(dat = primary) %>%
      padding(padding.top = 0.1, padding.bottom = 0.1, part = "all") %>%
      set_caption(
        as_paragraph(
          as_chunk("Overview of the supplied point source (total nitrogen, total phosphorus, total suspended solids and volitile suspended solids) data.")
         ),
        word_stylename = "Table Caption",
        autonum = run_autonum(seq_id = "tab",
                              bkm = "dataSource-primary")
                 )
    ## ----end

    ## ---- save PRIMARY
    saveRDS(primary, paste0(DATA_PATH, '/primary/primary.RData'))
    ## ----end
}

## Estimated resident population (ERP)
{
    ## ---- get data ERP
    erp <- read_excel(paste0(DATA_PATH, "/primary/2021_ERPGRP.xlsx"),
                      sheet = "Summary data",
                      trim_ws = TRUE)
    ## ----end

    ## ---- glimpse ERP
    glimpse(erp)
    ## ----end
    ## ---- glimpse-like ERP
    glimpse_like_table(dat = erp) %>%
      padding(padding.top = 0.1, padding.bottom = 0.1, part = "all") %>%
      set_caption(
        as_paragraph(
          as_chunk("Overview of the supplied estimated resident population (ERP) and gross regional product (GRP) data.")
         ),
        word_stylename = "Table Caption",
        autonum = run_autonum(seq_id = "tab",
                              bkm = "dataSource-erp")
                 )
    ## ----end

    ## ---- save ERP
    saveRDS(erp, paste0(DATA_PATH, '/primary/erp.RData'))
    ## ----end
}

## Catchment ERP
{
    ## ---- get data catchmentERP
    catchment_erp <- read_excel(paste0(DATA_PATH, "/primary/CatchmentsERP2020_WORK.xlsx"),
                                sheet = "Density_Calcs",
                                trim_ws = TRUE)
    ## ----end

    ## ---- glimpse catchmentERP
    glimpse(catchment_erp)
    ## ----end
    ## ---- glimpse-like catchmentERP
    glimpse_like_table(dat = catchment_erp) %>%
      padding(padding.top = 0.1, padding.bottom = 0.1, part = "all") %>%
      set_caption(
        as_paragraph(
          as_chunk("Overview of the supplied catchment estimated resident population (ERP) data population density.")
         ),
        word_stylename = "Table Caption",
        autonum = run_autonum(seq_id = "tab",
                              bkm = "dataSource-catchmentERP")
                 )
    ## ----end

    ## ---- save catchmentERP
    saveRDS(catchment_erp, paste0(DATA_PATH, '/primary/catchment_erp.RData'))
    ## ----end
}

## Fire frequency
{
    ## ---- get data fire_freq
    fire_freq <- read_excel(paste0(DATA_PATH, "/primary/2022_FireFreq .xlsx"),
                                sheet = "All",
                                trim_ws = TRUE)
    ## ----end

    ## ---- glimpse fire_freq
    glimpse(fire_freq)
    ## ----end
    ## ---- glimpse-like fire_freq
    glimpse_like_table(dat = fire_freq) %>%
      padding(padding.top = 0.1, padding.bottom = 0.1, part = "all") %>%
      set_caption(
        as_paragraph(
          as_chunk("Overview of the supplied fire frequency data.")
         ),
        word_stylename = "Table Caption",
        autonum = run_autonum(seq_id = "tab",
                              bkm = "dataSource-firefreq")
                 )
    ## ----end

    ## ---- save fire_freq
    saveRDS(fire_freq, paste0(DATA_PATH, '/primary/fire_freq.RData'))
    ## ----end
}

## Fire areas total
{
    ## ---- get data fire_areas
    fire_areas <- read_excel(paste0(DATA_PATH, "/primary/2022_FireAreas_Longterm.xlsx"),
                                sheet = "Total (km2)",
                                trim_ws = TRUE)
    ## ----end

    ## ---- glimpse fire_areas
    glimpse(fire_areas)
    ## ----end
    ## ---- glimpse-like fire_areas
    glimpse_like_table(dat = fire_areas) %>%
      padding(padding.top = 0.1, padding.bottom = 0.1, part = "all") %>%
      set_caption(
        as_paragraph(
          as_chunk("Overview of the supplied fire frequency data in area.")
         ),
        word_stylename = "Table Caption",
        autonum = run_autonum(seq_id = "tab",
                              bkm = "dataSource-fireareas")
                 )
    ## ----end

    ## ---- save fire_areas
    saveRDS(fire_areas, paste0(DATA_PATH, '/primary/fire_areas.RData'))
    ## ----end
}

## Fire areas percentage
{
    ## ---- get data fire_areas_p
    fire_areas_p <- read_excel(paste0(DATA_PATH, "/primary/2022_FireAreas_Longterm.xlsx"),
                                sheet = "Total (%)",
                                trim_ws = TRUE)
    ## ----end

    ## ---- glimpse fire_areas_p
    glimpse(fire_areas_p)
    ## ----end
    ## ---- glimpse-like fire_areas_p
    glimpse_like_table(dat = fire_areas_p) %>%
      padding(padding.top = 0.1, padding.bottom = 0.1, part = "all") %>%
      set_caption(
        as_paragraph(
          as_chunk("Overview of the supplied fire frequency percentage data in percentages.")
         ),
        word_stylename = "Table Caption",
        autonum = run_autonum(seq_id = "tab",
                              bkm = "dataSource-fireareasp")
                 )
    ## ----end

    ## ---- save fire_areas_p
    saveRDS(fire_areas_p, paste0(DATA_PATH, '/primary/fire_areas_p.RData'))
    ## ----end
}

## Sea level rises (cal year)
{
    ## ---- get data sea_level_cal
    sea_level_cal <- read_excel(paste0(DATA_PATH, "/primary/SeaLevelRise_1991_2022.xlsx"),
                                sheet = "AvgAnnualSL_cal_year",
                                trim_ws = TRUE)
    ## ----end

    ## ---- glimpse sea_level_cal
    glimpse(sea_level_cal)
    ## ----end
    ## ---- glimpse-like sea_level_cal
    glimpse_like_table(dat = sea_level_cal) %>%
      padding(padding.top = 0.1, padding.bottom = 0.1, part = "all") %>%
      set_caption(
        as_paragraph(
          as_chunk("Overview of the supplied annual sea levels from BOM (calendar years).")
         ),
        word_stylename = "Table Caption",
        autonum = run_autonum(seq_id = "tab",
                              bkm = "dataSource-sealevelcal")
                 )
    ## ----end

    ## ---- save sea_level_cal
    saveRDS(sea_level_cal, paste0(DATA_PATH, '/primary/sea_level_cal.RData'))
    ## ----end
}

## Sea level rises (fin year)
{
    ## ---- get data sea_level_fin
    sea_level_fin <- read_excel(paste0(DATA_PATH, "/primary/SeaLevelRise_1991_2022.xlsx"),
                                sheet = "AvgAnnualSL_Financial_Year",
                                trim_ws = TRUE)
    ## ----end

    ## ---- glimpse sea_level_fin
    glimpse(sea_level_fin)
    ## ----end
    ## ---- glimpse-like sea_level_fin
    glimpse_like_table(dat = sea_level_fin) %>%
      padding(padding.top = 0.1, padding.bottom = 0.1, part = "all") %>%
      set_caption(
        as_paragraph(
          as_chunk("Overview of the supplied annual sea levels from BOM (financial years).")
         ),
        word_stylename = "Table Caption",
        autonum = run_autonum(seq_id = "tab",
                              bkm = "dataSource-sealevelfin")
                 )
    ## ----end

    ## ---- save sea_level_fin
    saveRDS(sea_level_fin, paste0(DATA_PATH, '/primary/sea_level_fin.RData'))
    ## ----end
}

## Annual Rainfall (cal year) 
{
    ## ---- get data rainfall_cal
    rainfall_cal <- read_excel(paste0(DATA_PATH, "/primary/TempRainfallAnomaly_13.12.2021.xlsx"),
                                sheet = "Annual_Rainfall_Calendar",
                                trim_ws = TRUE)
    ## ----end

    ## ---- glimpse rainfall_cal
    glimpse(rainfall_cal)
    ## ----end
    ## ---- glimpse-like rainfall_cal
    glimpse_like_table(dat = sea_level_cal) %>%
      padding(padding.top = 0.1, padding.bottom = 0.1, part = "all") %>%
      set_caption(
        as_paragraph(
          as_chunk("Overview of the supplied annual total rainfall data (per calendar year).")
         ),
        word_stylename = "Table Caption",
        autonum = run_autonum(seq_id = "tab",
                              bkm = "dataSource-rainfallcal")
                 )
    ## ----end

    ## ---- save rainfall_cal
    saveRDS(rainfall_cal, paste0(DATA_PATH, '/primary/rainfall_cal.RData'))
    ## ----end
}

## Annual Rainfall (fin year) 
{
    ## ---- get data rainfall_fin
    rainfall_fin <- read_excel(paste0(DATA_PATH, "/primary/TempRainfallAnomaly_13.12.2021.xlsx"),
                                sheet = "FinancialYear_Rainfall",
                                trim_ws = TRUE)
    ## ----end

    ## ---- glimpse rainfall_fin
    glimpse(rainfall_fin)
    ## ----end
    ## ---- glimpse-like rainfall_fin
    glimpse_like_table(dat = sea_level_fin) %>%
      padding(padding.top = 0.1, padding.bottom = 0.1, part = "all") %>%
      set_caption(
        as_paragraph(
          as_chunk("Overview of the supplied annual total rainfall data (per financial year).")
         ),
        word_stylename = "Table Caption",
        autonum = run_autonum(seq_id = "tab",
                              bkm = "dataSource-rainfallfin")
                 )
    ## ----end

    ## ---- save rainfall_fin
    saveRDS(rainfall_fin, paste0(DATA_PATH, '/primary/rainfallfin.RData'))
    ## ----end
}

## Rainfall anomaly 
{
    ## ---- get data rainfall_anom
    rainfall_anom <- read_excel(paste0(DATA_PATH, "/primary/TempRainfallAnomaly_13.12.2021.xlsx"),
                                sheet = "Rainfall_Anomaly",
                                trim_ws = TRUE)
    ## ----end

    ## ---- glimpse rainfall_anom
    glimpse(rainfall_anom)
    ## ----end
    ## ---- glimpse-like rainfall_anom
    glimpse_like_table(dat = rainfall_anom) %>%
      padding(padding.top = 0.1, padding.bottom = 0.1, part = "all") %>%
      set_caption(
        as_paragraph(
          as_chunk("Overview of the supplied annual total rainfall anomaly data.")
         ),
        word_stylename = "Table Caption",
        autonum = run_autonum(seq_id = "tab",
                              bkm = "dataSource-rainfallanom")
                 )
    ## ----end

    ## ---- save rainfall_anom
    saveRDS(rainfall_anom, paste0(DATA_PATH, '/primary/rainfall_anom.RData'))
    ## ----end
}

## Mean air temperature 
{
    ## ---- get data temp
    temp <- read_excel(paste0(DATA_PATH, "/primary/TempRainfallAnomaly_13.12.2021.xlsx"),
                                sheet = "Mean_Air Temperate",
                                trim_ws = TRUE)
    ## ----end

    ## ---- glimpse temp
    glimpse(temp)
    ## ----end
    ## ---- glimpse-like temp
    glimpse_like_table(dat = temp) %>%
      padding(padding.top = 0.1, padding.bottom = 0.1, part = "all") %>%
      set_caption(
        as_paragraph(
          as_chunk("Overview of the supplied mean annual air temperature data.")
         ),
        word_stylename = "Table Caption",
        autonum = run_autonum(seq_id = "tab",
                              bkm = "dataSource-temp")
                 )
    ## ----end

    ## ---- save temp
    saveRDS(temp, paste0(DATA_PATH, '/primary/temp.RData'))
    ## ----end
}

## Max air temperature anomaly 
{
    ## ---- get data temp_anom
    temp_anom <- read_excel(paste0(DATA_PATH, "/primary/TempRainfallAnomaly_13.12.2021.xlsx"),
                                sheet = "Max_Temp_Anomaly",
                                trim_ws = TRUE)
    ## ----end

    ## ---- glimpse temp_anom
    glimpse(temp_anom)
    ## ----end
    ## ---- glimpse-like temp_anom
    glimpse_like_table(dat = temp_anom) %>%
      padding(padding.top = 0.1, padding.bottom = 0.1, part = "all") %>%
      set_caption(
        as_paragraph(
          as_chunk("Overview of the supplied max annual air temperature anomaly data.")
         ),
        word_stylename = "Table Caption",
        autonum = run_autonum(seq_id = "tab",
                              bkm = "dataSource-tempanom")
                 )
    ## ----end

    ## ---- save temp_anom
    saveRDS(temp_anom, paste0(DATA_PATH, '/primary/temp_anom.RData'))
    ## ----end
}

## SOI (cal year) 
{
    ## ---- get data SOI_cal
    SOI_cal <- read_excel(paste0(DATA_PATH, "/primary/TempRainfallAnomaly_13.12.2021.xlsx"),
                                sheet = "An_SOI_Cal",
                                trim_ws = TRUE)
    ## ----end

    ## ---- glimpse SOI_cal
    glimpse(SOI_cal)
    ## ----end
    ## ---- glimpse-like SOI_cal
    glimpse_like_table(dat = SOI_cal) %>%
      padding(padding.top = 0.1, padding.bottom = 0.1, part = "all") %>%
      set_caption(
        as_paragraph(
          as_chunk("Overview of the supplied annual southern oscillation index (SOI) data (calendar year).")
         ),
        word_stylename = "Table Caption",
        autonum = run_autonum(seq_id = "tab",
                              bkm = "dataSource-SOIcal")
                 )
    ## ----end

    ## ---- save SOI_cal
    saveRDS(SOI_cal, paste0(DATA_PATH, '/primary/SOI_cal.RData'))
    ## ----end
}

## SOI (fin year) 
{
    ## ---- get data SOI_fin
    SOI_fin <- read_excel(paste0(DATA_PATH, "/primary/TempRainfallAnomaly_13.12.2021.xlsx"),
                                sheet = "SOI_fy",
                                trim_ws = TRUE)
    ## ----end

    ## ---- glimpse SOI_fin
    glimpse(SOI_fin)
    ## ----end
    ## ---- glimpse-like SOI_fin
    glimpse_like_table(dat = SOI_fin) %>%
      padding(padding.top = 0.1, padding.bottom = 0.1, part = "all") %>%
      set_caption(
        as_paragraph(
          as_chunk("Overview of the supplied annual southern oscillation index (SOI) data (financial year).")
         ),
        word_stylename = "Table Caption",
        autonum = run_autonum(seq_id = "tab",
                              bkm = "dataSource-SOIfin")
                 )
    ## ----end

    ## ---- save SOI_fin
    saveRDS(SOI_fin, paste0(DATA_PATH, '/primary/SOI_fin.RData'))
    ## ----end
}

## SST anomaly 
{
    ## ---- get data SST_anom
    SST_anom <- read_excel(paste0(DATA_PATH, "/primary/TempRainfallAnomaly_13.12.2021.xlsx"),
                                sheet = "SST_Anomaly",
                                trim_ws = TRUE)
    ## ----end

    ## ---- glimpse SST_anom
    glimpse(SST_anom)
    ## ----end
    ## ---- glimpse-like SST_anom
    glimpse_like_table(dat = SST_anom) %>%
      padding(padding.top = 0.1, padding.bottom = 0.1, part = "all") %>%
      set_caption(
        as_paragraph(
          as_chunk("Overview of the supplied annual sea surface temperature (SST) anomaly data.")
         ),
        word_stylename = "Table Caption",
        autonum = run_autonum(seq_id = "tab",
                              bkm = "dataSource-SSTanom")
                 )
    ## ----end

    ## ---- save SST_anom
    saveRDS(SST_anom, paste0(DATA_PATH, '/primary/SST_anom.RData'))
    ## ----end
}

## Building activity
{
    ## ---- get data build
    build <- read_excel(paste0(DATA_PATH, "/primary/BuildingActivity_29.10.2022.xlsx"),
                                sheet = "Financial Year",
                                trim_ws = TRUE)
    ## ----end

    ## ---- glimpse build
    glimpse(build)
    ## ----end
    ## ---- glimpse-like build
    glimpse_like_table(dat = build) %>%
      padding(padding.top = 0.1, padding.bottom = 0.1, part = "all") %>%
      set_caption(
        as_paragraph(
          as_chunk("Overview of the supplied building activity data.")
         ),
        word_stylename = "Table Caption",
        autonum = run_autonum(seq_id = "tab",
                              bkm = "dataSource-build")
                 )
    ## ----end

    ## ---- save build
    saveRDS(build, paste0(DATA_PATH, '/primary/build.RData'))
    ## ----end
}

## Ship counts
{
    ## ---- get data ship
    ship <- read_excel(paste0(DATA_PATH, "/primary/ShipCounts_2011_2022.xlsx"),
                                ## sheet = "Trade vessels",
                                sheet = "breakdown",
                                trim_ws = TRUE)
    ## ----end

    ## ---- glimpse ship
    glimpse(ship)
    ## ----end
    ## ---- glimpse-like ship
    glimpse_like_table(dat = ship) %>%
      padding(padding.top = 0.1, padding.bottom = 0.1, part = "all") %>%
      set_caption(
        as_paragraph(
          as_chunk("Overview of the supplied ship counts data.")
         ),
        word_stylename = "Table Caption",
        autonum = run_autonum(seq_id = "tab",
                              bkm = "dataSource-ship")
                 )
    ## ----end

    ## ---- save ship
    saveRDS(ship, paste0(DATA_PATH, '/primary/ship.RData'))
    ## ----end
}


## lookups

## - Var: the variable listed in the input file
## - Units: the units listed in the input file
## - Measure: the constent name to apply
## only Measures in this table will be explored
## ---- get data var_lookup
var_lookup <- tribble(
    ~Dataset,       ~Var,                          ~Type,         ~Measure,       ~ID,       ~SITE_ID, ~Scale,
    "WQ 2016-2022", "Chla_mug_PER_L",                    "Response",    "Chla",   "WQ_ID",   NA,       "ZoneName",
    "WQ 2016-2022", "Turbidity_NTU",               "Response",    "Turbidity",    "WQ_ID",   NA,       "ZoneName",
    "WQ 2016-2022", "DO_PERCENT_saturation",       "Response",    "DO",           "WQ_ID",   NA,       "ZoneName",
    "WQ 2016-2022", "NH3_mug_PER_L",               "Response",    "NH3",          "WQ_ID",   NA,       "ZoneName",
    "WQ 2016-2022", "PO4_mug_PER_L",               "Response",    "PO4",          "WQ_ID",   NA,       "ZoneName",
    "WQ 2016-2022", "Nox_mug_PER_L",               "Response",    "NOx",          "WQ_ID",   NA,       "ZoneName",
    "WQ 2012-2015", "Chlorophyll a Lab",           "Response",    "Chla",         "WQ_ID",   NA,       "ZoneName",
    "WQ 2012-2015", "Turbidity Field",             "Response",    "Turbidity",    "WQ_ID",   NA,       "ZoneName",
    "WQ 2012-2015", "Dissolved oxygen %",          "Response",    "DO",           "WQ_ID",   NA,       "ZoneName",
    "WQ 2012-2015", "Ammonia as N",                "Response",    "NH3",          "WQ_ID",   NA,       "ZoneName",
    "WQ 2012-2015", "Phosphorus Total Filterable", "Response",    "PO4",          "WQ_ID",   NA,       "ZoneName",
    "Primary point", "Total N (t)",                "Pressure",    "TotalN",       "PRIMARY_ID", "PRIMARY_SITE_ID", "Catchment",
    "Primary point", "Total P (t)",                "Pressure",    "TotalP",       "PRIMARY_ID", "PRIMARY_SITE_ID", "Catchment",
    "Primary point", "Discharge volume (annual kL)","Pressure",   "Discharge",    "PRIMARY_ID", "PRIMARY_SITE_ID", "Catchment",
    "Primary point", "Total suspended solids (t)", "Pressure",    "TSS",          "PRIMARY_ID", "PRIMARY_SITE_ID", "Catchment",
    "Primary point", "Volatile suspended solids (t)", "Pressure",    "VSS",          "PRIMARY_ID", "PRIMARY_SITE_ID", "Catchment",
    "ERPGDP",        "ERP",                        "Pressure",    "ERP",          "ERP_ID",     NA,     NA,
    "ERPGDP",        "ERP growth",                 "Pressure",    "ERP_growth",   "ERP_ID",     NA,     NA,
    "ERPGDP",        "GRP ($M)",                   "Pressure",    "GRP",          "ERP_ID",     NA,     NA,
    "ERPGDP",        "Change GRP from previous year (%)", "Pressure", "GRP_change","ERP_ID",     NA,    NA,
    "CatchmentERP",  "ERP",                        "Pressure",    "Catch_ERP",          "CATCHMENT_ERP_ID",     NA,  "Catchment",
    "CatchmentERP",  "POP",                        "Pressure",    "POP",          "CATCHMENT_ERP_ID",     NA,  "Catchment",
    "SeaLevels",     "Year (ending)",              "Time",        "Year",         NA,        NA,        NA,
    "SeaLevels",     "Avg SL (m) (fy)",            "Pressure",    "SL",           "SEA_LEVEL_FIN_ID",  NA,  NA,
    "TempRainfallAnom","Precipitaton anomaly (mm)","Pressure",    "Rainfall_anom","RAINFALL_ANOM_ID", NA,  NA,
    "TempRainfallAnom","Mean air temperature (oC) (Darwin Airport)","Pressure","AirTemp","TEMP_ID", NA,  NA,
    "TempRainfallAnom","Max Air_Temperature anomaly (oC) (Darwin Airport)","Pressure","AirTemp_anom","TEMP_ANOM_ID", NA,  NA,
    "TempRainfallAnom","Year ending",              "Time",        "Year",          NA, NA,  NA,
    "TempRainfallAnom","Avg SOI (FY)",             "Pressure",    "SOI",           "SOI_FIN_ID", NA,  NA,
    "TempRainfallAnom","SST Anomaly (Northern tropics) (oC)", "Pressure",    "SST_anom", "SST_ANOM_ID", NA,  NA,
    "Building",        "Year (ending June)",       "Time",        "Year",          NA,       NA,  NA,
    "Building",        "Total  ($B)",              "Pressure",    "Building_total","BUILD_ID", NA,  NA,
    "Building",        "Engineering  ($B)",        "Pressure",    "Building_eng",  "BUILD_ID", NA,  NA,
    "Building",        "Non-residential building  ($B)",  "Pressure",    "Building_non","BUILD_ID", NA,  NA,
    "Building",        "Residential  ($B)",        "Pressure",    "Building_res",  "BUILD_ID", NA,  NA,
    "Shipping",        "Trade ships",              "Pressure",    "Ship_trade",    "SHIP_ID", NA,  NA,
    "Shipping",        "Container/General cargo/Ro-Ro","Pressure","Ship_cont",     "SHIP_ID", NA,  NA,
    "Shipping",        "Liquid bulk",              "Pressure",    "Ship_liq",      "SHIP_ID", NA,  NA,
    "Shipping",        "Car carriers",             "Pressure",    "Ship_car",      "SHIP_ID", NA,  NA,
    "Shipping",        "Rig tender",               "Pressure",    "Ship_rig",      "SHIP_ID", NA,  NA,
    "Shipping",        "Livestock",                "Pressure",    "Ship_live",     "SHIP_ID", NA,  NA,
    "Shipping",        "Cruise ships",             "Pressure",    "Ship_cruise",   "SHIP_ID", NA,  NA,
    "Shipping",        "Dry bulk",                 "Pressure",    "Ship_dry",      "SHIP_ID", NA,  NA,
    "Fire Freq",       "Fire_TotalKm_WA",             "Pressure",    "Fire_TotalKm_WA",  "FIRE_FREQ_ID", NA,  "Catchment",
    "Fire Freq",       "Fire_TotalKm_STD",             "Pressure",    "Fire_TotalKm_STD",  "FIRE_FREQ_ID", NA,  "Catchment",
    "Fire Areas",      "Fire_Areas",               "Pressure",    "Fire_Areas",    "FIRE_AREAS_ID", NA,  "Catchment",
    "Fire Areas",      "Fire_Areas_p",               "Pressure",    "Fire_Areas_p",    "FIRE_AREAS_ID", NA,  "Catchment",
   ) 
saveRDS(var_lookup, file = paste0(DATA_PATH, "processed/var_lookup.RData"))
## ----end

## ---- get data units_lookup
units_lookup <- tribble(
    ~Measure,     ~Units,         ~TableLabel,                            ~FigureLabel,                                                          ~Trans,
    "Chla",       "mu.g/L",       "Chlorophyll-a (µg/L)",                 "Chlorophyll-a~(mu*g/L)",                                              "I",
    "Turbidity",  "NTU",          "Turbidity (NTU)",                      "Turbidity~(NTU)",                                                     "I",
    "DO",         "% saturation", "Dissolved Oxygen (% Saturation)",      "Dissolved~Oxygen~('%'~Saturation)",                                   "I",
    "NH3",        "mu.g/L",       "Ammonia (µg/L)",                       "Ammonia~(mu*g/L)",                                                    "I",
    "PO4",        "mu.g/L",       "Filterable Reactive Phosphate (µg/L)", "Filterable~Reactive~Phosphate~(mu*g/L)",                              "I",
    "NOx",        "mu.g/L",       "NOx as N (µg/L)",                      "Nox~as~N~(mu*g/L)",                                                   "I",
    "TotalN",     "t",            "Total N (t)",                          "Total~N~(t)",                                                         "log10",
    "TotalP",     "t",            "Total P (t)",                          "Total~P~(t)",                                                         "log10",
    "Discharge",  "kL",           "Discharge volume (annual kL)",         "Discharge~volume~(kL)",                                               "log10",
    "TSS",        "t",            "Total suspended solids (t)",           "Total~suspended~solids~(t)",                                          "I",
    "VSS",        "t",            "Volatile suspended solids (t)",           "Volatile~suspended~solids~(t)",                                    "I",
    "ERP",        "",             "Estimated resident population",        "Estimated~resident~population",                                       "log10",
    "ERP_growth", "",             "Estimated resident population growth", "Estimated~resident~population~growth",                                "log10",
    "GRP",        "($M)",         "Gross regional product ($M)",          "Gross~regional~product~($M)",                                         "I",
    "GRP_change", "(%)",          "Gross regional product change (%)",          "Gross~regional~product~change~(%)",                             "I",
    "Catch_ERP",  "",             "Estimated resident population",        "Estimated~resident~population",                                       "I",
    "Fire_TotalKm_WA", "km",      "Five year moving average fire frequency (Km)", "Five~year~moving~average~fire~frequency~(Km)",                "I",
    "Fire_TotalKm_STD", "km",      "Five year standard deviation of fire frequency (Km)", "Five~year~standard~deviation~of~fire~frequency~(Km)", "I",
    "Fire_Areas", "(Km²)",        "Total fire area (Km²)",               "Total~fire~area~(km^2)",                                               "I",
    "Fire_Areas_p", "(%)",        "Percentage fire area (%)",            "Percentage~fire~area~(km^2)",                                          "I",
    "POP",        "(#/km2)",      "Population density (pop/km2)",        "Population~density~(pop/km^2)",                                        "I",
    "SL",         "(m)",          "Mean sea level (m)",             "Mean~sea~level~(m)",                                              "I",
    "Rainfall",   "(mm)",         "Annual Rainfall (mm)",                "Annual~rainfall~(mm)",                                                 "I",
    "Rainfall_anom","(mm)",       "Rainfall anomaly (mm)",               "Rainfall~anomaly~(mm)",                                                "I",
    "AirTemp",    "(oC)",         "Mean air temperature (oC)",           "Mean~air~temperature~(degree*C)",                                      "I",
    "AirTemp_anom","(oC)",        "Mean air temperature anomaly (oC)",   "Mean~air~temperature~anomaly~(degree*C)",                              "I",
    "SOI",        "",             "Southern Oscillation Index",          "Southern~Oscillation~Index",                                           "I",
    "SST_anom",   "",             "Sea Surface Temperature anomaly",     "Sea~Surface~Temperature~anomaly",                                      "I",
    "Building_total","($B)",      "Total building activity ($B)",        "Total~building~activity~($B)",                                         "I",
    "Building_eng","($B)",        "Engineering building activity ($B)",  "Engineering~building~activity~($B)",                                   "I",
    "Building_non","($B)",        "Non-residential building activity ($B)",  "Non-residential~building~activity~($B)",                           "I",
    "Building_res","($B)",        "Residential building activity ($B)",  "Residential~building~activity~($B)",                                   "I",
    "Ship_trade","",          "Number of trade ships",               "Number~of~trade~ships",                                                    "I",
    "Ship_cont", "",          "Number of container ships",           "Number~of~container~ships",                                                "I",
    "Ship_liq", "",          "Number of liquid bulk ships",           "Number~of~liquid~bulk~ships",                                             "I",
    "Ship_car", "",          "Number of car carrier ships",           "Number~of~car~carrier~ships",                                             "I",
    "Ship_live", "",          "Number of livestock ships",           "Number~of~livestock~ships",                                                "I",
    "Ship_rig", "",          "Number of rig tenders",                "Number~of~rig~tenders",                                                    "I",
    "Ship_cruise", "",          "Number of cruise ships",           "Number~of~cruise~ships",                                                    "I",
    "Ship_dry", "",          "Number of dry bulk ships",           "Number~of~dry~bulk~ships",                                                   "I",
    )
saveRDS(units_lookup, file = paste0(DATA_PATH, "processed/units_lookup.RData"))
## ----end

## ---- get data spatial_lookup
spatial_lookup <- tribble(
    ~Region, ~RegionName, ~Zone, ~ZoneName,           ~Area,
     1,       "Upper",     4,     "Middle Arm",       "Inner",
     1,       "Upper",     5,     "East Arm",         "Inner",
     1,       "Upper",     6,     "Elizabeth River",  "Inner",
     1,       "Upper",     7,     "West Arm",         "Inner",
     1,       "Upper",     8,     "Buffalo Creek",    "Outer",
     1,       "Upper",     9,     "Myrmidon Creek",   "Inner",
     2,       "Middle",    3,     "Central Harbour",  "Inner",
     3,       "Outer",     1,     "Outer Harbour",    "Outer",
     3,       "Outer",     2,     "Shoal Bay",        "Outer",
    )
saveRDS(spatial_lookup, file = paste0(DATA_PATH, "processed/spatial_lookup.RData"))
## ----end

## ---- get data spatial_subcatchments_lookup
## Check the spelling of (spellings based on shapefiles):
## - Reichhardt Creek
## - Mickett Creek
spatial_subcatchments_lookup <- tribble(
    ~Catchment,         ~CatchmentNumber, ~ZoneName,        ~nudge_x, ~Lab_lat,  ~Lab_long, ~Curvature,
    "Charles Point",   1,                "Outer Harbour",   0,        -12.4,     130.95,    0.1,
    "Woods Inlet",     2,                "West Arm",        0,        -12.4,     130.95,    0.1,
    "West Arm",        3,                "West Arm",        0,        -12.4,     130.95,    0.1,
    "Creek A",         4,                "West Arm",        0,        -12.53,    131.02,    -0.1,
    "Pioneer Creek",   5,                "Middle Arm",      0,        -12.53,    131.02,    -0.1,
    "Blackmore River", 6,                "Middle Arm",      0,        -12.53,    131.02,    -0.1,
    "Elizabeth River", 7,                "Elizabeth River",   0,      -12.44,    131.0,     0.1,
    "Mitchell Creek",  8,                "Elizabeth River",   0,      -12.44,    131.0,     0.1,
    "Palmerston South",9,                "Elizabeth River",   0,      -12.47,    131.0,   0.1,
    "Myrmidon Creek",  10,               "Myrmidon Creek",    0,      -12.47,    131.0,   0.1,
    "Hudson Creek",    11,               "East Arm",         0,       -12.44,    131.0,     0.1,
    "Bleesers Creek",  12,               "East Arm",         0,       -12.44,    131.0,     0.1,
    "Reichhardt Creek",13,               "East Arm",         0,       -12.47,    131.0,   0.1,
    "Sadgroves Creek", 14,               "East Arm",         0.01,    -12.47, 131.0,   0.1,
    "Darwin CBD",      15,               "Central Harbour",   -0.01,  -12.44,    131.0,     0.1,
    "Ludmilla Creek",  16,               "Outer Harbour",    0,       -12.4,  130.95,  0.1,
    "Rapid Creek",     17,               "Outer Harbour",   0,        -12.4,  130.95,  0.1,
    "Sandy Creek",     18,               "Outer Harbour",   0,        -12.47, 131.0,   0.1,
    "Buffalo Creek",   19,               "Buffalo Creek",   0,        -12.4,     130.95,    0.1,
    "Mickett Creek",   20,               "Shoal Bay",       0,        -12.4,  130.95,  0.1,
    "Kings Creek",     21,               "Shoal Bay",       0,        -12.4,  130.95,  0.1,
    "Howard River",    22,               "Shoal Bay",       0,        -12.4,  130.95,  0.1,
    )
saveRDS(spatial_subcatchments_lookup, file = paste0(DATA_PATH, "processed/spatial_subcatchments_lookup.RData"))
## ----end

## ---- get data spatial_lookup_primary
spatial_lookup_primary <- tribble(
    ~Site,                                    ~Lab_lat, ~Lab_long, ~Curvature,
    "Beluyen (Woods Inlet WwTP)",             -12.45,   130.7,     0.1,
    "Bleesers Creek (Berrimah WwTP)",         -12.3,   131.05,     0.1,
    "Buffalo Creek (Leanyer-Sanderson WwTP)", -12.2,    131.00,    0.1,
    "ConocoPhillips Pipeline Australia Pty Ltd (combined discharges)", -12.6,   130.71,  0.1,
    "East Point (Ludmilla WwTP)",             -12.3,   130.78,  0.1,
    "INPEX Operations Australia Pty Ltd (combined discharges)", -12.56,   131.1,  -0.1,
    "Myrmidon Creek (Palmerston WwTP)",       -12.41,   131.1,  0.1,
    "Paspaley Pearling Company",              -12.7,    131.10,  -0.1,
    "Tasmanian Seafoods",                     -12.8,    131.05,  -0.1,
    "Territory Generation ADP1",              -12.71,    130.83,  -0.1,
    )

    ## ~`Discharge location`,    ~ZoneName,          ~RecievingArea, ~Latitude, ~Longitude, ~Lab_lat,  ~Lab_long, ~Curvature,
    ## "Bladin Point",           "East Arm",         "Inner",        -12.51534, 130.9175,   -12.53, 131.02,  -0.1,
    ## "Bleesers Creek",         "East Arm",         "Inner",        -12.45392, 130.91006,  -12.44, 131.0,  0.1,
    ## "Buffalo Creek",          "Buffalo Creek",    "Outer",        -12.3589,  130.9106,   -12.4,  130.95,  0.1,
    ## "East Point",             "Outer Harbour",    "Outer",        -12.4015,  130.8223,   -12.43, 130.88,   0.1,
    ## "Middle Arm",             "Middle Arm",       "Inner",        -12.56830, 130.9123,   -12.6, 131.0,  0.1,
    ## "Middle Arm",             "Middle Arm",       "Inner",        -12.556639,130.864559,  -12.6,   131.0,  0.1,
    ## "Middle Arm",             "Middle Arm",       "Inner",        -12.56300,130.8906,  -12.6,   131.0,  0.1,
    ## "Middle Arm/East Arm",    "Middle Arm",       "Inner",        -12.51896, 130.86539,   -12.65,   130.8,  0.1,
    ## "Middle Arm/East Arm",    "East Arm",         "Inner",        -12.51896, 130.86539,   -12.65, 130.8,  0.1,
    ## "Myrmidon Creek",         "Myrmidon Creek",   "Inner",        -12.5037,  130.94861,   -12.47, 131.0,   0.1,
    ## "Woods Inlet",            "West Arm",         "Outer",        -12.5,     130.75,     -12.45,     130.7,    0.1,
    ## )
saveRDS(spatial_lookup_primary, file = paste0(DATA_PATH, "processed/spatial_lookup_primary.RData"))
## ----end

## ---- get data map_lookup
map_lookup <- tribble(
    ~Region, ~RegionName, ~Zone, ~ZoneName,          ~Longitude, ~Latitude, ~Lab_lat, ~Lab_long, ~HexColour, ~Curvature,
    1,       "Upper",     4,     "Middle Arm",       130.9001,  -12.60235,  -12.67,   130.8,     "#9F7A96",  0.1,
    1,       "Upper",     5,     "East Arm",         130.8858,  -12.48998,  -12.45,   131.0,    "#828F96",   0.1,
    1,       "Upper",     6,     "Elizabeth River",  130.9495,  -12.52712,  -12.61,   131.04,    "#DC8381",  0.1,
    1,       "Upper",     7,     "West Arm",         130.7794,  -12.55428,  -12.57,   130.65,    "#89B8A0",  0.1,
    1,       "Upper",     8,     "Buffalo Creek",    131.02,    -12.36,     -12.40,   130.93,    "#B89171",  -0.1,
    1,       "Upper",     9,     "Myrmidon Creek",   130.94,    -12.50,     -12.51,   131.03,    "#DC8381",  -0.1,
    2,       "Middle",    3,     "Central Harbour",   130.8169,  -12.49000,  -12.45,   130.65,    "#00A89D", -0.3,
    3,       "Outer",     1,     "Outer Harbour",    130.7603,  -12.35711,  -12.28,   130.70,    "#1A7BBF",  -0.1,
    3,       "Outer",     2,     "Shoal Bay",        130.9730,  -12.28223,  -12.18,   131.05,    "#B89171",  0.2,
    )
saveRDS(map_lookup, file = paste0(DATA_PATH, "processed/map_lookup.RData"))
## ----end

## ---- get data spatial_lookup_catchmentERP
## Check the spelling of (spellings based on shapefiles):
## - Reichhardt Creek
## - Mickett Creek
spatial_lookup_catchment_erp <- tribble(
    ~`Catchment Name`,               ~Catchment,
    "Blackmore River (Middle Arm)",  "Blackmore River",
    "Bleesers Creek",                "Bleesers Creek",
    "Buffalo Creek",                 "Buffalo Creek",     
    "Charles Point",                 "Charles Point",
    "Creek A (Middle Arm)",          "Creek A",
    "Darwin CBD",                    "Darwin CBD",  
    "Elizabeth River (East Arm)",    "Elizabeth River",
    "Howard River",                  "Howard River",
    "Hudson Creek",                  "Hudson Creek",
    "Kings Creek",                   "Kings Creek",
    "Ludmilla Creek",                "Ludmilla Creek",
    "Micket Creek",                  "Mickett Creek",
    "Mitchell Creek",                "Mitchell Creek",
    "Myrmidon Creek",                "Myrmidon Creek",   
    "Palmerston South",              "Palmerston South",
    "Pioneer Creek",                 "Pioneer Creek",
    "Rapid Creek",                   "Rapid Creek",
    "Reichardt Creek",               "Reichhardt Creek",   
    "Sadgroves Creek",               "Sadgroves Creek",   
    "Sandy Creek",                   "Sandy Creek",   
    "West Arm",                      "West Arm",   
    "Woods Inlet",                   "Woods Inlet",   
    )
saveRDS(spatial_lookup_catchment_erp, file = paste0(DATA_PATH, "processed/spatial_lookup_catchment_erp.RData"))
## ----end

## ---- get data spatial_lookup_fire
## Check the spelling of (spellings based on shapefiles):
## - Reichhardt Creek
## - Mickett Creek
spatial_lookup_fire <- tribble(
    ~`Catchment Name`,               ~Catchment,
    "Blackmore River",               "Blackmore River",
    "Bleesers Creek",                "Bleesers Creek",
    "Buffalo Creek",                 "Buffalo Creek",     
    "Charles Point",                 "Charles Point",
    "Creek A (Middle Arm)",          "Creek A",
    "Darwin CBD",                    "Darwin CBD",  
    "Elizabeth River",               "Elizabeth River",
    "Howard River",                  "Howard River",
    "Hudson Creek",                  "Hudson Creek",
    "Kings Creek",                   "Kings Creek",
    "Ludmilla Creek",                "Ludmilla Creek",
    "Micket Creek",                  "Mickett Creek",
    "Mitchell Creek",                "Mitchell Creek",
    "Myrmidon Creek",                "Myrmidon Creek",   
    "Palmerston South",              "Palmerston South",
    "Pioneer Creek",                 "Pioneer Creek",
    "Rapid Creek",                   "Rapid Creek",
    "Reichardt Creek",               "Reichhardt Creek",   
    "Sadgroves Creek",               "Sadgroves Creek",   
    "Sandy Creek",                   "Sandy Creek",   
    "West Arm",                      "West Arm",   
    "Woods Inlet",                   "Woods Inlet",   
    )
saveRDS(spatial_lookup_fire, file = paste0(DATA_PATH, "processed/spatial_lookup_fire.RData"))
## ----end

## ---- get data spatial_lookup_fire areas
## Check the spelling of (spellings based on shapefiles):
## - Reichhardt Creek
## - Mickett Creek
spatial_lookup_fire_areas <- tribble(
    ~`Catchment Name`,               ~Catchment,
    "Blackmore River (Middle Arm)",  "Blackmore River",
    "Bleesers Creek",                "Bleesers Creek",
    "Buffalo Creek",                 "Buffalo Creek",     
    "Charles Point",                 "Charles Point",
    "Creek A (Middle Arm)",          "Creek A",
    "Darwin CBD",                    "Darwin CBD",  
    "Elizabeth River (East Arm)",    "Elizabeth River",
    "Howard River",                  "Howard River",
    "Hudson Creek",                  "Hudson Creek",
    "Kings Creek",                   "Kings Creek",
    "Ludmilla Creek",                "Ludmilla Creek",
    "Micket Creek",                  "Mickett Creek",
    "Mitchell Creek",                "Mitchell Creek",
    "Myrmidon Creek",                "Myrmidon Creek",   
    "Palmerston South",              "Palmerston South",
    "Pioneer Creek (Middle Arm)",    "Pioneer Creek",
    "Rapid Creek",                   "Rapid Creek",
    "Reichardt Creek",               "Reichhardt Creek",   
    "Sadgroves Creek",               "Sadgroves Creek",   
    "Sandy Creek",                   "Sandy Creek",   
    "West Arm",                      "West Arm",   
    "Woods Inlet",                   "Woods Inlet",   
    )
saveRDS(spatial_lookup_fire_areas, file = paste0(DATA_PATH, "processed/spatial_lookup_fire_areas.RData"))
## ----end
