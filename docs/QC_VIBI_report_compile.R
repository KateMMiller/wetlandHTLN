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

#----- Compile Data -----
# import database tables to check raw data (ie, not views)
tryCatch(
  db <- DBI::dbConnect(drv = odbc::odbc(), dsn = "HTLNWetlands"),
  error = function(e){stop(paste0("Unable to connect to DSN to check tbl_SamplingEvents vs tbl_SamplingPeriods."))})
tbls <- c("tbl_SamplingEvents", "tbl_SamplingPeriods", "tbl_VIBI_Herb",
          "tbl_VIBI_Herb_Biomass", "tbl_VIBI_Woody", "tbl_BigTrees",
          "tlu_WetlndSpeciesList")
tbl_import <- lapply(seq_along(tbls), function(x){
  tab1 <- tbls[x]
  tab <- DBI::dbReadTable(db, tab1)
  return(tab)})
DBI::dbDisconnect(db)
tbl_import <- setNames(tbl_import, tbls)
list2env(tbl_import, envir = .GlobalEnv)

tluspp <- if(spp_list == "FQAI"){
  read.csv("https://raw.githubusercontent.com/KateMMiller/wetlandHTLN/refs/heads/main/data/FQAI_species_list.csv")
} else {tlu_WetlndSpeciesList}

head(tluspp) # SCIENTIFIC_NAME for FQAI

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
tbl_SamplingPeriods$StartDate_check <- format(as.Date(tbl_SamplingPeriods$StartDate, format = "%Y-%m-%d"), format = "%Y-%m-%d")
tbl_SamplingEvents$StartDate_check <- format(as.Date(tbl_SamplingEvents$StartDate, tryFormats = c("%m/%d/%Y", "%m/%d/%y")), format = "%Y-%m-%d")

unparsed_dates <- tbl_SamplingEvents |> filter(StartDate_check < as.Date("2008-01-01", format = "%Y-%m-%d") |
                                          StartDate_check > as.Date(Sys.Date(), format = "%Y-%m-%d") |
                                            is.na(StartDate_check)) |>
  select(EventID, StartDate, StartDate_check)


odd_dates1 <- unparsed_dates |> filter(is.na(StartDate_check)) |> select(EventID, StartDate)

QC_table <- rbind(QC_table,
                  QC_check(odd_dates1, "Periods/Events", "tbl_SamplingEvents with StartDates that can't parse."))

tbl_odd_dates1 <- make_kable(odd_dates1, "tbl_SamplingEvents with StartDates that can't parse.")


odd_dates2 <- unparsed_dates |> filter(!is.na(StartDate_check)) |> select(EventID, StartDate, StartDate_check)

QC_table <- rbind(QC_table,
                  QC_check(odd_dates2, "Periods/Events", "tbl_SamplingEvents with StartDates that don't parse properly."))

tbl_odd_dates2 <- make_kable(odd_dates2, "tbl_SamplingEvents with StartDates that don't parse properly or don't make sense.")

# check if periods/events checks returned at least 1 record to determine whether to include that tab in report
pev_check <- QC_table |> filter(Data %in% "Periods/Events" & Num_Records > 0)
pev_include <- tab_include(pev_check)

#----- Herbs -----
# Check for blanks in ScientificName and CovCode, include Comment_Herb
herb_blanks <- tbl_VIBI_Herb |> filter(is.na(Species) | is.na(CovCode) | is.na(ModNo)) |>
  select(VIBI_Herb_ID, EventID, LocationID, Species, ModNo, CovCov, Comments)

QC_table <- rbind(QC_table,
                  QC_check(herb_blanks, "VIBI Herbs", "tbl_SamplingEvents with StartDates that don't parse properly."))

tbl_herb_blanks <- make_kable(herb_blanks, "tbl_SamplingEvents with StartDates that don't parse properly or don't make sense.")

# Check for duplicate species within a given module
herb_dups <- tbl_VIBI_Herb |> group_by(LocationID, EventID, ModNo, Species) |>
  summarize(num_records = sum(!is.na(CovCode)), .groups = "drop") |>
  filter(num_records != 1)

QC_table <- rbind(QC_table,
                  QC_check(herb_dups, "VIBI Herbs", "Duplicate species records within the same LocationID, EventID, and Module."))

tbl_herb_dups <- make_kable(herb_dups, "Duplicate species records within the same LocationID, EventID, and Module.")

# Find species recorded that are not on FQAI list
herb_miss <- anti_join(tbl_VIBI_Herb, tluspp, by = c("Species" = "SCIENTIFIC_NAME")) |>
  arrange(Species, LocationID, EventID, ModNo) |> select(VIBI_Herb_ID, LocationID, EventID, Species, ModNo, Comments)

if(nrow(herb_miss) > 0){
  assign("unmatched_herb_spp", herb_miss, envir = .GlobalEnv)
}

QC_table <- rbind(QC_table,
                  QC_check(herb_miss, "VIBI Herbs", paste0("Species in tbl_VIBI_Herb that are not in the ", spp_list, " species list.")))

tbl_herb_miss <- make_kable(herb_miss, paste0("Species in tbl_VIBI_Herb that are not in the ", spp_list, " species list.",
                                              "Note that output data frame is saved as 'unmatched_herb_spp' in global environment for easier review."))

# Find species that have only been recorded once
herb_once1 <- tbl_VIBI_Herb |> group_by(Species) |>
  summarize(num_recs = sum(!is.na(CovCode))) |> filter(num_recs == 1) |> select(Species)

herb_once <- tbl_VIBI_Herb |> filter(Species %in% herb_once1$Species) |> arrange(Species, LocationID, EventID)

QC_table <- rbind(QC_table,
                  QC_check(herb_once, "VIBI Herbs", "Species that have only been recorded once in tbl_VIBI_Herb. Not necessarily an error, but good to check."))

tbl_herb_once <- make_kable(herb_once, "Species that have only been recorded once in tbl_VIBI_Herb. Not necessarily an error, but good to check.")

# check if Herb checks returned at least 1 record to determine whether to include that tab in report
herb_check <- QC_table |> filter(Data %in% "VIBI Herbs" & Num_Records > 0)
herb_include <- tab_include(herb_check)

#----- Woody -----
# Find FeatureIDs in Woody table that don't match Locations via LocationID
woody_fid_check <- full_join(locv |> select(LocationID, FeatureID),
                             tbl_VIBI_Woody |> select(LocationID, FeatureID),
                             by = "LocationID",
                             suffix = c("_loc", "_wdy")) |>
  filter(FeatureID_loc != FeatureID_wdy)

QC_table <- rbind(QC_table,
                  QC_check(woody_fid_check, "Woody", "FeatureIDs in tbl_VIBI_Woody that don't match FeatureIDs in tbl_Locations."))

tbl_woody_fid_check <- make_kable(woody_fid_check, "FeatureIDs in tbl_VIBI_Woody that don't match FeatureIDs in tbl_Locations, based on join of LocationID field. If two FeatureIDs look identical, there's likely a space in one of them, which R reads as different.")

# Check for blanks in ScientificName, DiamID, and Count
woody_blanks <- tbl_VIBI_Woody |> filter(is.na(Scientific_Name) | is.na(Count) | is.na(DiamID))


QC_table <- rbind(QC_table,
                  QC_check(woody_blanks, "Woody", "Blanks in Scientific_Name, DiamID, or Count fields in tbl_VIBI_Woody."))

tbl_woody_blanks <- make_kable(woody_blanks, "Blanks in Scientific_Name, DiamID, or Count fields in tbl_VIBI_Woody.")

# Check for -9999 Counts
woody9s <- tbl_VIBI_Woody |> filter(Count < 0)

QC_table <- rbind(QC_table,
                  QC_check(woody9s, "Woody", "Negative 9999 counts, indicating flag."))

tbl_woody9s <- make_kable(woody9s, "Negative 9999 counts, indicating a flag..")

# Check for DiamIDs that don't match tlu b/c of capitalization


# Check for duplicate species within a given module
QC_table <- rbind(QC_table,
                  QC_check(df, "", ""))

tbl_df <- make_kable(df, "")

# Look for -9999 Counts
QC_table <- rbind(QC_table,
                  QC_check(df, "", ""))

tbl_df <- make_kable(df, "")

# Check for DiamIDs that aren't capitalized.
QC_table <- rbind(QC_table,
                  QC_check(df, "", ""))

tbl_df <- make_kable(df, "")

# Find counts > 99% of ever recorded per DiamID
QC_table <- rbind(QC_table,
                  QC_check(df, "", ""))

tbl_df <- make_kable(df, "")

# Find counts > 99% overall
QC_table <- rbind(QC_table,
                  QC_check(df, "", ""))

tbl_df <- make_kable(df, "")

# Find species recorded that are not on tlu_WetlndSpecies_List
QC_table <- rbind(QC_table,
                  QC_check(df, "", ""))

tbl_df <- make_kable(df, "")

# check if Woody checks returned at least 1 record to determine whether to include that tab in report
woody_check <- QC_table |> filter(Data %in% "Woody" & Num_Records > 0)
woody_include <- tab_include(woody_check)

#----- Big Trees -----
# Find FeatureIDs in Big Trees that don't match Locations via LocationID


QC_table <- rbind(QC_table,
                  QC_check(df, "", ""))

tbl_df <- make_kable(df, "")

# Check on BigTree counts that don't match between Woody and BigTree tables.

# Check for duplicate species within a given module
QC_table <- rbind(QC_table,
                  QC_check(df, "", ""))

tbl_df <- make_kable(df, "")

# Check for negative DBH values
QC_table <- rbind(QC_table,
                  QC_check(df, "", ""))

tbl_df <- make_kable(df, "")

# Find DBH > 99% of ever recorded
QC_table <- rbind(QC_table,
                  QC_check(df, "", ""))

tbl_df <- make_kable(df, "")

# Find species recorded that are not on tlu_WetlndSpecies_List
QC_table <- rbind(QC_table,
                  QC_check(df, "", ""))

tbl_df <- make_kable(df, "")

# check if Big Tree checks returned at least 1 record to determine whether to include that tab in report
bigt_check <- QC_table |> filter(Data %in% "Big Trees" & Num_Records > 0)
bigt_include <- tab_include(bigt_check)
#----- Biomass -----
# Find FeatureIDs in Herbs that don't match Locations via LocationID
QC_table <- rbind(QC_table,
                  QC_check(df, "", ""))

tbl_df <- make_kable(df, "")

# Find biomass eventIDs that don't have corresponding herb eventIDs for a given year
QC_table <- rbind(QC_table,
                  QC_check(df, "", ""))

tbl_df <- make_kable(df, "")

# check if Biomass checks returned at least 1 record to determine whether to include that tab in report
biomass_check <- QC_table |> filter(Data %in% "Biomass" & Num_Records > 0)
biomass_include <- tab_include(biomass_check)

#----- Species list -----
# check that OH_STATUS == "adventive" and SHADE = "advent" match
QC_table <- rbind(QC_table,
                  QC_check(df, "", ""))

tbl_df <- make_kable(df, "")

# Find species recorded in Herb and Woody tables that aren't on FQAI list- will continue to use this
# to find species added during sampling.
QC_table <- rbind(QC_table,
                  QC_check(df, "", ""))

tbl_df <- make_kable(df, "")

# Find species on tlu_WetlndSpecies_list that aren't on FQAI list
QC_table <- rbind(QC_table,
                  QC_check(df, "", ""))

tbl_df <- make_kable(df, "")

# check if species list checks returned at least 1 record to determine whether to include that tab in report
spp_check <- QC_table |> filter(Data %in% "Species List" & Num_Records > 0)
spp_include <- tab_include(spp_check)
