#----------------------------------------
# Conducts QC checks on CUVA wetland data. Output is reported in QC_report_comile.Rmd
#----------------------------------------
#
# Params to turn on when running within script. Otherwise set params in Rmd.
#
# library(wetlandHTLN)
# library(tidyverse)
# library(knitr) # for kable functions
# library(kableExtra) # for additional kable features
# #library(htmltools) # check what this is for before turning on
#
# importData()
#
# year_curr = 2023
# year_range = 2008:2023
# all_years = TRUE

#---- Functions ----
# Summarize results of QC check
QC_check <- function(df, tab, check){
  result <- data.frame("Data" = tab, "Description" = check, "Num_Records" = nrow(df))
}

# function to make tables via kable
make_kable <- function(df, cap){
  QC_table <- if(nrow(df) > 0){
    if(nrow(df) > 1){
    kable(df, format = 'html', align = 'c', caption = cap)  |>
      kable_styling(fixed_thead = TRUE, bootstrap_options = c('condensed'),
                    full_width = TRUE, position = 'left', font_size = 12) |>
      row_spec(0, extra_css = "border-top: 1px solid #000000; border-bottom: 1px solid #000000;") |>
      collapse_rows(1, valign = 'top') |>
      row_spec(nrow(df), extra_css = 'border-bottom: 1px solid #000000;')
    } else if(nrow(df) == 1){
      kable(df, format = 'html', align = 'c', caption = cap)  |>
        kable_styling(fixed_thead = TRUE, bootstrap_options = c('condensed'),
                      full_width = TRUE, position = 'left', font_size = 12) |>
        row_spec(0, extra_css = "border-top: 1px solid #000000; border-bottom: 1px solid #000000;") |>
        row_spec(nrow(df), extra_css = 'border-bottom: 1px solid #000000;')
      }
    } else NULL
}

# Determine whether to include/drop tab in rmd output
tab_include <- function(df){ifelse(nrow(df) > 0, TRUE, FALSE)}

# Determine if table exists or is null used in eval for rmd
check_null <- function(table){
  if(!is.null(table)){table}
}

check_null_print <- function(table, tab_level = 4, tab_title){
  if(!is.null(table)){cat(paste0(rep("#", tab_level), collapse = ""), " ", tab_title, " {.tabset} ", "\n\n")}
  check_null(table)
}

#---- Individual View checking ----
#----- Locations -----
loc <- get("locations", env = HTLNwetlands)
locv <- loc |> filter(FeatureTypes %in% c("VIBIPlotID", "VIBIplotID"))

# Check for LocationIDs that don't match convention of PARKWetlnd
locid_typos <- loc |> filter(!substr(LocationID, 1, 10) %in% c("CUVAWetlnd", "TAPRWetlnd")) |> select(LocationID, FeatureTypes, FeatureID)
QC_table <- QC_check(locid_typos, "Locations", "LocationIDs that don't follow PARKWetlnd naming convention.")

tbl_locid_typos <- make_kable(locid_typos, "LocationIDs that don't follow PARKWetlnd naming convention (note the capitalization).")

# Check for the solo FeatureTypes (ie likely typo b/c only one of them)
feat_typos1 <- data.frame(table(loc$FeatureTypes)) |> filter(Freq == 1)
feat_typos <- loc |> filter(FeatureTypes %in% feat_typos1$Var1) |> select(LocationID, FeatureTypes, FeatureID) |> mutate(Num_sites = 1)

QC_table <- rbind(QC_table,
            QC_check(feat_typos, "Locations", "FeatureTypes only recorded once (likely typo)."))
tbl_feat_typos <- make_kable(feat_typos, "FeatureTypes only recorded once (likely typo)- check capitalization.")

# Check for non-alphanumeric symbols in the FeatureID
feat_symb <- data.frame(loc[grepl("[^[:alnum:]]", loc$FeatureID), c("LocationID", "FeatureTypes", "FeatureID")])

QC_table <- rbind(QC_table,
                  QC_check(feat_symb, "Locations", "Special symbols in FeatureID. Consider revising."))

tbl_feat_symb <- make_kable(feat_symb, "Special symbols in FeatureID. Consider revising.")

# Check where DomVeg_Lev1_orig missing
miss_domveg <- locv |>
  filter(is.na(DomVeg_Lev1_orig)) |>
  select(LocationID, FeatureTypes, FeatureID, X1oPlants, DomVeg_Lev1 = DomVeg_Lev1_orig)

QC_table <- rbind(QC_table,
                  QC_check(miss_domveg, "Locations", "DomVeg_Lev1 missing a value needed for VIBI thresholds."))

tbl_miss_domveg <- make_kable(miss_domveg, "DomVeg_Lev1 missing a value needed for VIBI thresholds. Note that the DomVegID is also missing for those records.")

# Check where DomVeg_Lev1 doesn't mathe X1oPlants, the column used by importData to make the DomVeg_Lev1 column
conflict_domveg <- locv |>
  select(LocationID, FeatureTypes, FeatureID, X1oPlants, DomVeg_Lev1 = DomVeg_Lev1_orig) |>
  filter(!is.na(DomVeg_Lev1)) |>
  filter(!((DomVeg_Lev1 == "Emergent" & X1oPlants == "PEM") |
            (DomVeg_Lev1 == "Shrub" & X1oPlants == "PSS") |
              (DomVeg_Lev1 == "Forest" & X1oPlants == "PFO")))

QC_table <- rbind(QC_table,
                  QC_check(conflict_domveg, "Locations", "DomVeg_Lev1 doesn't match expected value for X1oPlants."))

tbl_conflict_domveg <- make_kable(conflict_domveg, "DomVeg_Lev1 doesn't match expected value for X1oPlants.")

# Check that dividing # modules and AreaHA result in 0.01ha
area_error <- locv |> mutate(mod_area = as.numeric(AreaHA)/as.numeric(TotalMods)) |> filter(mod_area != 0.01) |>
  select(LocationID, FeatureID, TotalMods, AreaHA, PlotConfig, mod_area)

QC_table <- rbind(QC_table,
                  QC_check(area_error, "Locations", "AreaHA and TotMods combination results in modules != 0.01ha."))

tbl_area_error <- make_kable(area_error, "AreaHA and TotalMods combination results in modules != 0.01ha")

# Check that InternMods <= TotalMOds
intern_check <- locv |> filter(InternMods > TotalMods) |>
  select(LocationID, FeatureID, TotalMods, InternMods, PlotConfig)


QC_table <- rbind(QC_table,
                  QC_check(intern_check, "Locations", "InternMods is greater than TotalMods."))

tbl_intern_check <- make_kable(intern_check, "InternMods is greater than TotalMods")
# Check SamplingPeriods table for StartDate format- catch any that don't follow %m/%d/%Y

# check if locations checks returned at least 1 record to determine whether to include that tab in report
loc_check <- QC_table |> filter(Data %in% "Locations" & Num_Records > 0)
loc_include <- tab_include(loc_check)

#----- SamplingEvents/Periods/Locations checks -----
# Find EventIDs in Herbs, Woody, BigTree, Biomass that are missing from the SamplingEvents table

# import database tables to check raw IDs
tryCatch(
  db <- DBI::dbConnect(drv = odbc::odbc(), dsn = "HTLNWetlands"),
  error = function(e){stop(paste0("Unable to connect to DSN to check tbl_SamplingEvents vs tbl_SamplingPeriods."))})
  tbls <- c("tbl_SamplingEvents", "tbl_SamplingPeriods", "tbl_VIBI_Herb", "tbl_VIBI_Herb_Biomass", "tbl_VIBI_Woody", "tbl_BigTrees")
  tbl_import <- lapply(seq_along(tbls), function(x){
                         tab1 <- tbls[x]
                         tab <- DBI::dbReadTable(db, tab1)
                         return(tab)})
DBI::dbDisconnect(db)
tbl_import <- setNames(tbl_import, tbls)
list2env(tbl_import, envir = .GlobalEnv)

# Check that tbl_SamplingEvents$PeriodIDs are included in tbl_SamplingPeriods$PeriodID
miss_periods <- tbl_SamplingEvents[!unique(tbl_SamplingEvents$PeriodID) %in% unique(tbl_SamplingPeriods$PeriodID),]

QC_table <- rbind(QC_table,
                  QC_check(miss_periods, "Periods/Events", "tbl_SamplingEvents$PeriodID missing from tbl_SamplingPeriods$PeriodID."))

tbl_miss_periods <- make_kable(miss_periods, "tbl_SamplingEvents$PeriodID missing from tbl_SamplingPeriods$PeriodID.")

# Find EventIDs in data tables missing from tbl_SamplingEvents
sample_evs <- rbind(tbl_VIBI_Herb |> select(LocationID, EventID) |> unique() |> mutate(table = "tbl_VIBI_Herb", pres = "X"),
                    tbl_VIBI_Woody |> select(LocationID, EventID) |> unique() |> mutate(table = "tbl_VIBI_Woody", pres = "X"),
                    tbl_VIBI_Herb_Biomass |> select(LocationID, EventID) |> unique() |> mutate(table = "tbl_VIBI_Herb_Biomass", pres = "X"),
                    tbl_BigTrees |> select(LocationID, EventID) |> unique() |> mutate(table = "tbl_BigTrees", pres = "X")) |>
pivot_wider(names_from = table, values_from = pres)

miss_samp_evs <- sample_evs[!sample_evs$EventID %in% tbl_SamplingEvents$EventID,]

QC_table <- rbind(QC_table,
                  QC_check(miss_samp_evs, "Periods/Events", "EventIDs in VIBI tables that are missing from tbl_SamplingEvents."))


tbl_miss_samp_evs <- make_kable(miss_samp_evs, "EventIDs in VIBI tables that are missing from tbl_SamplingEvents.")

# Check SamplingPeriods table for StartDate format- catch any that don't follow %m/%d/%Y




  loc_pd_ev <- full_join(loc, tbl_S)
  tbl_sampevs <- tbl_SamplingEvents |> select(Location, PeriodID)

  head(tbl_SamplingPeriods)

  # Find LocationIDs in data tables not in tbl_Locations

  head(sample_evs)


  # check if locations checks returned at least 1 record to determine whether to include that tab in report
  loc_check <- QC_table |> filter(Data %in% "Locations" & Num_Records > 0)
  loc_include <- tab_include(loc_check)

#----- Herbs -----
# Find FeatureIDs in Herbs that don't match Locations via LocationID

# Check for blanks in ScientificName and CovCode, include Comment_Herb

# Check for duplicate species within a given module

# Find species recorded that are not on tlu_WetlndSpecies_List

#----- Woody -----
# Find FeatureIDs in Herbs that don't match Locations via LocationID

# Check for blanks in ScientificName, DiamID, and Count

# Check for duplicate species within a given module

# Look for -9999 Counts

# Check for DiamIDs that aren't capitalized.

# Find counts > 99% of ever recorded per DiamID

# Find counts > 99% overall

# Find species recorded that are not on tlu_WetlndSpecies_List

#----- Big Trees -----
# Find FeatureIDs in Herbs that don't match Locations via LocationID

# Check for duplicate species within a given module

# Check for negative DBH values

# Find DBH > 99% of ever recorded

# Find species recorded that are not on tlu_WetlndSpecies_List


#----- Biomass -----
# Find FeatureIDs in Herbs that don't match Locations via LocationID

# Find biomass eventIDs that don't have corresponding herb eventIDs for a given year

#----- Species list -----
# check that OH_STATUS == "adventive" and SHADE = "advent" match

# Find species recorded in Herb and Woody tables that aren't on FQAI list- will continue to use this
# to find species added during sampling.

# Find species on tlu_WetlndSpecies_list that aren't on FQAI list
