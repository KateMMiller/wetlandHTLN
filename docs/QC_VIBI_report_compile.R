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
# years = 2008:2023

#---- Functions ----
# Summarize results of QC check
QC_check <- function(df, tab, check){
  result <- data.frame("Data" = tab, "Description" = check, "Num_Records" = nrow(df))
}

# function to make tables via kable
make_kable <- function(df, cap){
  QC_table <- if(nrow(df) > 0){
    kable(df, format = 'html', align = 'c', caption = cap)  |>
      kable_styling(fixed_thead = TRUE, bootstrap_options = c('condensed'),
                    full_width = TRUE, position = 'left', font_size = 12) |>
      row_spec(0, extra_css = "border-top: 1px solid #000000; border-bottom: 1px solid #000000;") |>
      collapse_rows(1, valign = 'top') |>
      row_spec(nrow(df), extra_css = 'border-bottom: 1px solid #000000;')
  } else NULL
}

# Determine whether to include/drop tab in rmd output
tab_include <- function(df){ifelse(nrow(df) > 0, TRUE, FALSE)}

# Determine if table exists or is null used in eval for rmd
check_null <- function(table){
  if(!is.null(table)){table}
}

# View checking
# Function to check for potential issues in the raw views
check_view <- function(view_name, nums = NA){
  cat(paste0("Checks for: ", view_name, "\n"))
  view <- get(view_name, envir = ROCKY)
  cat("Column Names: ", names(view), "\n")
  if(any(names(view) %in% "StartDate")){cat("Summary of year range: ", sort(unique(as.numeric(format(view$StartDate, "%Y")))), "\n")}
  if(any(names(view) %in% "SpeciesCode")){cat("Species codes: ", sort(unique(view$SpeciesCode)), "\n")}
  if(any(names(view) %in% "CoverCode")){cat("Cover codes: ", sort(unique(view$CoverCode)), "\n")}
  cat("List of sites: ", sort(unique(view$SiteCode)) ,"\n")

  if(!any(is.na(nums))){
    if(length(nums) == 1){
      cat("Checks on ", nums, ": ", sep = "")
      cat("Range: ", paste(range(view[, nums], na.rm = T)[1], range(view[, nums], na.rm = T)[2], sep = ", "), "; ", sep = "")
      cat("NA Count: ", length(view[is.na(view[,nums]), nums]))
    } else {
      cat("Checks on numeric fields: ", "\n")
      invisible(lapply(seq_along(nums), function(x){
        num = nums[x]
        cat(num, ": ", sep = "")
        cat("Range: ", paste(range(view[, num], na.rm = T)[1], range(view[, num], na.rm = T)[2], sep = ", "), "; ", sep = "")
        cat("NA Count: ", length(view[is.na(view[,num]), num]))
        cat("\n")
      }))
    }
  }
}

#---- Individual View checking ----
#----- Locations -----
loc <- get("locations", env = HTLNwetlands)
table(loc$LocationID)

# Check for LocationIDs that don't match convention of PARKWetlnd
locid_typos <- loc |> filter(!substr(LocationID, 1, 10) %in% c("CUVAWetlnd", "TAPRWetlnd")) |> select(LocationID, FeatureTypes, FeatureID)
QC_table <- QC_check(locid_typos, "Locations", "LocationIDs that don't follow PARKWetlnd naming convention.")

tbl_locid_typos <- make_kable(locid_typos, "LocationIDs that don't follow PARKWetlnd naming convention (note the capitalization).")

# Check for the solo FeatureTypes (ie likely typo b/c only one of them)
feat_typos1 <- data.frame(table(loc$FeatureTypes)) |> filter(Freq == 1)
feat_typos <- loc |> filter(FeatureTypes %in% feat_typos1$Var1) |> select(LocationID, FeatureTypes, FeatureID) |> mutate(Num_sites = 1)

QC_table <- rbind(QC_table,
            QC_check(feat_typos, "Locations", "FeatureTypes only recorded once (likely typo)."))
tbl_feat_typos <- make_kable(feat_typos, "FeatureTypes only recorded once (likely typo).")

# Check for non-alphanumeric symbols in the FeatureID
feat_symb <- loc[grepl("[^[:alnum:]]", loc$FeatureID), c("LocationID", "FeatureTypes", "FeatureID")]
QC_table <- rbind(QC_table,
                  QC_check(feat_symb, "Locations", "Special symbols in FeatureID - consider revising."))

tbl_feat_symb <- make_kable(feat_symb, "Special symbols in FeatureID - consider revising.")

# Check that dividing # modules and AreaHA result in 0.01ha

# Check that InternMods <= TotalMOds

# Check SamplingPeriods table for StartDate format- catch any that don't follow %m/%d/%Y

#----- SamplingEvents/Periods -----
# Find EventIDs in Herbs, Woody, BigTree, Biomass that are missing from the SamplingPeriods table

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

QC_table <- rbind(QC_table,
                  QC_check())
