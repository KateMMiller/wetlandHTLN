#' @title joinVIBI_module: join module-level VIBI metrics
#'
#' @importFrom dplyr arrange between case_when filter first full_join group_by left_join mutate select summarize
#' @importFrom tidyr pivot_wider
#' @importFrom tidyselect starts_with
#' @importFrom purrr reduce
#'
#' @description This function summarizes module-level VIBI and filters by plot, year, and plot types. This
#' function will return a VIBI score for each module within a site, and allows for more equal comparisons
#' among sites with fewer than 4 modules. Averaging the module scores within a site is also possible via sumVIBI.
#'
#' NOTE 1: Because the results from sumVIBI are averages rather than sums (ie the VIBI spreadsheet), their
#' scores will be lower than other datasets that use the VIBI spreadsheet, which are straight sums.
#'
#' NOTE 2: The Biomass metric assumes the area sampled is always 0.01m2. Currently, if there's only a biomass
#' record for a given year and no herb or woody data, those records are dropped.
#'
#' NOTE 3: Metric calculations and thresholds follow INTEGRATED WETLAND ASSESSMENT PROGRAM Part 9: Field Manual
#' for the Vegetation Index of Biotic Integrity for Wetlands v. 1.5. Pages 17 - 20 and Table 2 were most useful.
#'
#' @param years Numeric. Filter on years of survey. Default is all.
#'
#' @param survey_type Filter on survey type of plots. Options are "all" (default), "reference",
#' "survey", "survey, womc", "womc", and "womc, reference". Can choose multiple options.
#'
#' @param hgm_class Filter on HGM class. Options are "all" (default), "Depression",
#' "Impoundment", "Riverine", "Slope". Can choose multiple options.
#'
#' @param dom_veg1 Filter on level 1 dominant vegetation type. Options are "all" (default), "emergent",
#' "forest", "shrub". Can choose multiple options. Note that emergent-coastal was not included as an option
#' in this VIBI, but wouldn't take much to add as an option.
#'
#' @param plotID Quoted string. Default is 'all'. If specified will return data for only plots specified.
#' Can choose multiple plots. Based on FeatureID in database.
#'
#' @param region Quoted string. Default is "NCNE". Specifies the Army Corps Region for OH. Options are "EMP", "MW", and "NCNE".
#'
#' @param intens_mods Filter on total number of intensive modules. Ranges from 1 to 4. Can select multiple.
#' Default is 1:4 (all).
#'
#' @examples
#' \dontrun{
#' # run first
#' importData()
#'
#' # calculate module-level VIBI for all sites (default).
#' vibi <- joinVIBI_module()
#'
#' write.csv(vibi, "./testing_scripts/vibimodule.csv", row.names=F)
#'
#' # calculate module-level VIBI for plot 124 and year 2023
#' vibi124 <- joinVIBI_module(plotID = "124", years = 2023)
#'
#' # calculate module-level VIBI for all sites sampled in 2023
#' vibi23 <- joinVIBI_module(years = 2023)
#'
#' # calculate module-level VIBI for all emergent wetlands
#' vibi_emerg <- joinVIBI_module(dom_veg1 = "emergent")
#'
#' # calculate module-level VIBI for depressional wetlands
#' vibi_dep <- joinVIBI_module(hgm_class = "Depression")
#'
#' # calculate
#'
#'
#' }
#'
#' @return Returns a data frame of VIBI calculations for each module of each plot
#' @export
#'

joinVIBI_module <- function(years = 2008:as.numeric(format(Sys.Date(), format = "%Y")),
                    survey_type = 'all', hgm_class = 'all', dom_veg1 = 'all',
                    plotID = 'all', region = "NCNE", intens_mods = 1:4){

  #---- Bug handling ----
  survey_type <- match.arg(survey_type, several.ok = T,
                           choices = c("all", "reference", "survey", "survey, womc", "womc", "womc, reference"))
  hgm_class <- match.arg(hgm_class, choices = c("all", "Depression", "Impoundment", "Riverine", "Slope"),
                         several.ok = T)
  dom_veg1 <- match.arg(dom_veg1, choices = c("all", "emergent", "forest", "shrub"), several.ok = T)
  region <- match.arg(region, choices = c("NCNE", "EMP", "MW"))
  stopifnot(class(years) == "numeric" | class(years) == "integer", years >= 2008)
  stopifnot(class(intens_mods) == "numeric" | class(intens_mods) == "integer")

  #---- Compile Plot and visit-level data ----
  env <- if(exists("HTLNwetlands")){HTLNwetlands} else {.GlobalEnv}

  plots <- getPlots(plot_type = "VIBIplotID", survey_type = survey_type, hgm_class = hgm_class,
                    dom_veg1 = dom_veg1, plotID = plotID, intens_mods = intens_mods)

  tryCatch(tluSpp <- get("tluSpp", envir = env),
           error = function(e){stop("tlu_WetlndSpeciesList not found. Please run importData() first.")})

  # Compile the loc/visit table to left-join with final results
  plots_abbr <- plots[,c("LocationID", "FeatureTypes", "FeatureID", "Park", "County", "TotalMods", "PlotConfig", "AreaHA",
                         "X1oPlants", "X2oVegID", "X1oHGM", "DomVegID", "HGM_ID", "HGMClass",
                         "DomVeg_Lev1", "DomVeg_Lev2", "DomVeg_Lev3")]

  #---- Compile Herb Metrics ----
  herbs1 <- getHerbs(years = years, survey_type = survey_type, hgm_class = hgm_class,
                     dom_veg1 = dom_veg1, plotID = plotID, nativity = 'all', intens_mods = intens_mods)

  # Set wet status based on the column chosen, like in the macros code
  herbs1$WETreg <- ifelse(is.na(herbs1[,region]), herbs1$WET, herbs1[,region])
  herbs1$WETreg <- ifelse(is.na(herbs1$WETreg), "ND", herbs1$WETreg)

  # Extract genus from scientific name
  herbs1$genus <- gsub("([A-Za-z]+).*", "\\1", herbs1$ScientificName)

  # Calc Relative Cover
  herbs_rc <- herbs1 |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1) |>
    summarize(tot_cov = sum(MidPoint, na.rm = T), .groups = 'drop')

  # join back with larger dataset
  herbs <- left_join(herbs1, herbs_rc,
                     by = c("LocationID", "FeatureID", "EventID", "SampleDate",
                            "SampleYear", "ModuleNo", "DomVeg_Lev1")) |>
    filter(!is.na(MidPoint)) |>  # dropping FeatureID 1007 from 2023 for Mod NA with blank Species and cover.
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, DomVeg_Lev1, DomVeg_Lev2, ModuleNo,
             genus, FAMILY, OH_STATUS, TYPE, SHADE, FORM, WET, WETreg, COFC, HABIT,
             ScientificName) |>
    summarize(spp_cov = sum(MidPoint),
              rel_cov = spp_cov/first(tot_cov),
              tot_cov = first(tot_cov),
              cov_wt_c = rel_cov*first(COFC),
              .groups = "drop")

  # join back with larger dataset
  # herbs <- left_join(herbs1, herbs_rc,
  #                    by = c("LocationID", "FeatureID", "EventID", "SampleDate",
  #                           "SampleYear", "ModuleNo", "DomVeg_Lev1")) |>
  #   filter(!is.na(MidPoint)) |>  # dropping FeatureID 1007 from 2023 for Mod NA with blank Species and cover.
  #   mutate(rel_cov = MidPoint/tot_cov)

  # Create table to left_join with herb vibi metrics; There are no sample qualifiers for sampled, but non present
  # So assuming if there's a record in this df below, and the community matches, the VIBI should be 0 for herb vibis
  herbs_lj <- herbs |> select(LocationID, FeatureID, DomVeg_Lev1, SampleYear, ModuleNo) |> unique()

  # herbs_check <- herbs |> group_by(LocationID, FeatureID, EventID, SampleYear, ModuleNo) |>
  #   summarize(sum_rel = sum(rel_cov))
  # table(herbs_check$sum_rel, useNA = 'always') # all sum to 1

  # Create list of wet indicators from tluSpp
  wet <- unique(tluSpp$WET[grepl("OBL|FACW", tluSpp$WET)])

  # Using macros cases to determine the exact ranges of VIBI scores (ie whether <= or < vs >= or >)
  # Carex Community E, SH
  carex1 <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, DomVeg_Lev1, ModuleNo, genus, ScientificName) |>
    #filter(DomVeg_Lev1 %in% c("emergent", "shrub")) |>
    filter(genus == "Carex") |> unique() |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1, genus) |>
    summarize(Num_Carex = sum(!is.na(genus)), .groups = 'drop') |>
    mutate(Carex_Score = case_when(is.na(Num_Carex) ~ NA_real_,
                                   Num_Carex <= 1 ~ 0,
                                   between(Num_Carex, 2, 3) ~ 3,
                                   Num_Carex == 4 ~ 7,
                                   Num_Carex >= 5 ~ 10,
                                   TRUE ~ NA_real_))

  # Add 0s where no Carex were found in emergent or shrub wetlands
  carex <- left_join(herbs_lj, carex1, by = c("LocationID", "FeatureID", "SampleYear", "ModuleNo", "DomVeg_Lev1"))
  carex$Num_Carex[is.na(carex$Num_Carex)] <- 0
  carex$Carex_Score[carex$Num_Carex == 0] <- 0
  carex$Carex_Score[carex$DomVeg_Lev1 == "forest"] <- NA

  # cyperaceae Community Ecoastal only
  cyper1 <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear,
           DomVeg_Lev1, ModuleNo, FAMILY, ScientificName) |>
    #filter(DomVeg_Lev1 == "emergent-coastal") |>
    filter(FAMILY == "Cyperaceae") |> unique() |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1, FAMILY) |>
    summarize(Num_Cyper = sum(!is.na(FAMILY)), .groups = 'drop') |>
    mutate(Cyper_Score = case_when(is.na(Num_Cyper) ~ NA_real_,
                                   Num_Cyper <= 1 ~ 0,
                                   between(Num_Cyper, 2, 3) ~ 3,
                                   between(Num_Cyper, 4, 6) ~ 7,
                                   Num_Cyper >= 7 ~ 10,
                                   TRUE ~ NA_real_))

  # Add 0s where no Cyperaceae were found in emergent-coastal
  cyper <- left_join(herbs_lj, cyper1, by = c("LocationID", "FeatureID", "SampleYear", "ModuleNo", "DomVeg_Lev1"))
  cyper$Num_Cyper[is.na(cyper$Num_Cyper)] <- 0
  cyper$Cyper_Score[cyper$Num_Cyper == 0] <- 0
  cyper$Cyper_Score[!cyper$DomVeg_Lev1 %in% "emergent-coastal"] <- NA

  # dicot Community E SH
  dicot1 <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1,
           OH_STATUS, TYPE, ScientificName) |>
    #filter(DomVeg_Lev1 %in% c("emergent", "shrub")) |>
    filter(OH_STATUS == "native" & TYPE == "DI") |> unique() |> # native dicots
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1) |>
    summarize(Num_Dicot = sum(!is.na(ScientificName)), .groups = 'drop') |>
    mutate(Dicot_Score = case_when(is.na(Num_Dicot) ~ NA_real_,
                                   DomVeg_Lev1 == "emergent" & Num_Dicot <= 10 ~ 0,
                                   DomVeg_Lev1 == "emergent" & between(Num_Dicot, 11, 17) ~ 3,
                                   DomVeg_Lev1 == "emergent" & between(Num_Dicot, 18, 24) ~ 7,
                                   DomVeg_Lev1 == "emergent" & Num_Dicot >= 25 ~ 10,

                                   DomVeg_Lev1 == "shrub" & Num_Dicot <= 9 ~ 0,
                                   DomVeg_Lev1 == "shrub" & between(Num_Dicot, 10, 14) ~ 3,
                                   DomVeg_Lev1 == "shrub" & between(Num_Dicot, 15, 23) ~ 7,
                                   DomVeg_Lev1 == "shrub" & Num_Dicot >= 24 ~ 10,
                                   TRUE ~ NA_real_))

  # Add 0s where no dicots were found
  dicot <- left_join(herbs_lj, dicot1, by = c("LocationID", "FeatureID", "SampleYear", "ModuleNo", "DomVeg_Lev1"))
  dicot$Num_Dicot[is.na(dicot$Num_Dicot)] <- 0
  dicot$Dicot_Score[dicot$Num_Dicot == 0] <- 0
  dicot$Dicot_Score[dicot$DomVeg_Lev1 == "forest"] <- NA

  # Shade community F
  shade1 <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1,
           OH_STATUS, SHADE, ScientificName) |>
    #filter(DomVeg_Lev1 == "forest") |>
    filter(OH_STATUS == "native" & SHADE %in% c("shade", "partial")) |> unique() |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1) |>
    summarize(Num_Shade = sum(!is.na(ScientificName)), .groups = 'drop') |>
    mutate(Shade_Score = case_when(is.na(Num_Shade) ~ NA_real_,
                                   Num_Shade <= 7 ~ 0,
                                   between(Num_Shade, 8, 13) ~ 3,
                                   between(Num_Shade, 14, 20) ~ 7,
                                   Num_Shade >= 21 ~ 10,
                                   TRUE ~ NA_real_))


  # Add 0s where no shade spp were found in forest
  shade <- left_join(herbs_lj, shade1, by = c("LocationID", "FeatureID", "SampleYear", "ModuleNo", "DomVeg_Lev1"))
  shade$Num_Shade[is.na(shade$Num_Shade)] <- 0
  shade$Shade_Score[shade$Num_Shade == 0] <- 0
  shade$Shade_Score[!shade$DomVeg_Lev1 %in% "forest"] <- NA


  # Shrub- region Community E, SH
  shrub_reg1 <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1,
           OH_STATUS, FORM, WETreg, ScientificName) |>
    #filter(DomVeg_Lev1 %in% c("emergent", "shrub")) |>
    filter(OH_STATUS == "native" & FORM == "shrub" & WETreg %in% c("FACW", "OBL")) |> unique() |> #, "(FACW)", "FACW+", "(FACW+)")) |> unique() |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1) |>
    summarize(Num_Shrub_reg = sum(!is.na(ScientificName)), .groups= "drop") |>
    mutate(Shrub_Score_reg = case_when(is.na(Num_Shrub_reg) ~ NA_real_,
                                       Num_Shrub_reg <= 1 ~ 0,
                                       Num_Shrub_reg == 2 ~ 3,
                                       between(Num_Shrub_reg, 3, 4) ~ 7,
                                       Num_Shrub_reg >= 5 ~ 10,
                                       TRUE ~ NA_real_))

  # Add 0s where no Shrub were found in emergent or shrub wetlands
  shrub_reg <- left_join(herbs_lj, shrub_reg1, by = c("LocationID", "FeatureID", "SampleYear", "ModuleNo", "DomVeg_Lev1"))
  shrub_reg$Num_Shrub_reg[is.na(shrub_reg$Num_Shrub_reg)] <- 0
  shrub_reg$Shrub_Score_reg[shrub_reg$Num_Shrub_reg == 0] <- 0
  shrub_reg$Shrub_Score_reg[shrub_reg$DomVeg_Lev1 == "forest"] <- NA

  # Shrub- statewide
  shrub1 <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1,
           OH_STATUS, FORM, WET, ScientificName) |>
    #filter(DomVeg_Lev1 %in% c("emergent", "shrub")) |>
    filter(OH_STATUS == "native" & FORM == "shrub" & WET %in% c("FACW", "OBL", "(FACW)")) |> unique() |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1) |>
    summarize(Num_Shrub = sum(!is.na(ScientificName)), .groups= "drop") |>
    mutate(Shrub_Score = case_when(is.na(Num_Shrub) ~ NA_real_,
                                   Num_Shrub <= 1 ~ 0,
                                   Num_Shrub == 2 ~ 3,
                                   between(Num_Shrub, 3, 4) ~ 7,
                                   Num_Shrub >= 5 ~ 10,
                                   TRUE ~ NA_real_))

  # Add 0s where no Shrub were found in emergent or shrub wetlands
  shrub <- left_join(herbs_lj, shrub1, by = c("LocationID", "FeatureID", "SampleYear", "ModuleNo", "DomVeg_Lev1"))
  shrub$Num_Shrub[is.na(shrub$Num_Shrub) & shrub$DomVeg_Lev1 %in% c("emergent", "shrub")] <- 0
  shrub$Shrub_Score[shrub$Num_Shrub == 0 & shrub$DomVeg_Lev1 %in% c("emergent", "shrub")] <- 0

  # Hydrophyte richness- region # native FACW and OBL
  hydrop_reg1 <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1,
           OH_STATUS, WETreg, ScientificName) |>
    #filter(DomVeg_Lev1 %in% c("emergent", "shrub")) |>
    filter(OH_STATUS == "native" & WETreg %in% c("FACW", "OBL")) |> unique() |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1) |>
    summarize(Num_Hydro_reg = sum(!is.na(ScientificName)), .groups = 'drop') |>
    mutate(Hydro_Score_reg = case_when(is.na(Num_Hydro_reg) ~ NA_real_,
                                       DomVeg_Lev1 == "emergent" & Num_Hydro_reg <= 10 ~ 0,
                                       DomVeg_Lev1 == "emergent" & between(Num_Hydro_reg, 11, 20) ~ 3,
                                       DomVeg_Lev1 == "emergent" & between(Num_Hydro_reg, 21, 30) ~ 7,
                                       DomVeg_Lev1 == "emergent" & Num_Hydro_reg >= 31 ~ 10,

                                       DomVeg_Lev1 == "shrub" & Num_Hydro_reg <= 9 ~ 0,
                                       DomVeg_Lev1 == "shrub" & between(Num_Hydro_reg, 10, 14) ~ 3,
                                       DomVeg_Lev1 == "shrub" & between(Num_Hydro_reg, 15, 20) ~ 7,
                                       DomVeg_Lev1 == "shrub" & Num_Hydro_reg >= 21 ~ 10,

                                       TRUE ~ NA_real_))

  # Add 0s where no hydrophytes were found in emergent or shrub wetlands
  hydrop_reg <- left_join(herbs_lj, hydrop_reg1, by = c("LocationID", "FeatureID", "SampleYear", "ModuleNo", "DomVeg_Lev1"))
  hydrop_reg$Num_Hydro_reg[is.na(hydrop_reg$Num_Hydro_reg)] <- 0
  hydrop_reg$Hydro_Score_reg[hydrop_reg$Num_Hydro_reg == 0] <- 0
  hydrop_reg$Hydro_Score_reg[hydrop_reg$DomVeg_Lev1 == "forest"] <- NA

  # hydro - statewide
  hydrop1 <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1,
           OH_STATUS, WET, ScientificName) |>
    #filter(DomVeg_Lev1 %in% c("emergent", "shrub")) |>
    filter(OH_STATUS == "native" & WET %in% wet) |> unique() |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1) |>
    summarize(Num_Hydro = sum(!is.na(ScientificName)), .groups = 'drop') |>
    mutate(Hydro_Score = case_when(is.na(Num_Hydro) ~ NA_real_,
                                         DomVeg_Lev1 == "emergent" & Num_Hydro <= 10 ~ 0,
                                         DomVeg_Lev1 == "emergent" & between(Num_Hydro, 11, 20) ~ 3,
                                         DomVeg_Lev1 == "emergent" & between(Num_Hydro, 21, 30) ~ 7,
                                         DomVeg_Lev1 == "emergent" & Num_Hydro >= 31 ~ 10,

                                         DomVeg_Lev1 == "shrub" & Num_Hydro <= 9 ~ 0,
                                         DomVeg_Lev1 == "shrub" & between(Num_Hydro, 10, 14) ~ 3,
                                         DomVeg_Lev1 == "shrub" & between(Num_Hydro, 15, 20) ~ 7,
                                         DomVeg_Lev1 == "shrub" & Num_Hydro >= 21 ~ 10,

                                         TRUE ~ NA_real_))

  # Add 0s where no hydrophytes were found in emergent or shrub wetlands
  hydrop <- left_join(herbs_lj, hydrop1, by = c("LocationID", "FeatureID", "SampleYear", "ModuleNo", "DomVeg_Lev1"))
  hydrop$Num_Hydro[is.na(hydrop$Num_Hydro)] <- 0
  hydrop$Hydro_Score[hydrop$Num_Hydro == 0] <- 0
  hydrop$Hydro_Score[hydrop$DomVeg_Lev1 == "forest"] <- NA

  # annual : perennial ratio - did not explicitly state native, so not including OH_STATUS in summary
  ap_ratio1 <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1,
           HABIT, ScientificName) |>
    #filter(DomVeg_Lev1 %in% c("emergent")) |>
    filter(HABIT %in% c("AN", "PE")) |> unique() |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, HABIT, DomVeg_Lev1) |>
    summarize(Num_Spp = sum(!is.na(ScientificName)), .groups = 'drop') |>
    pivot_wider(names_from = HABIT, values_from = Num_Spp, values_fill = 0) |>
    mutate(AP_Ratio = ifelse(PE > 0, AN/PE, 1),
           AP_Score = case_when(is.na(AP_Ratio) ~ NA_real_,
                                AP_Ratio > 0.48 ~ 0,
                                between(AP_Ratio, 0.32, 0.48) ~ 3,
                                between(AP_Ratio, 0.20, 0.32) ~ 7,
                                AP_Ratio <= 0.20 ~ 10,
                                TRUE ~ NA_real_
                                ))

  # Add 0s where no ann/per were found in emergent
  ap_ratio <- left_join(herbs_lj, ap_ratio1, by = c("LocationID", "FeatureID", "SampleYear", "ModuleNo", "DomVeg_Lev1"))
  ap_ratio$AP_Ratio[is.na(ap_ratio$AP_Ratio)] <- 0
  ap_ratio$AP_Score[ap_ratio$AP_Ratio == 0] <- 0
  ap_ratio$AP_Score[!ap_ratio$DomVeg_Lev1 %in% "emergent"] <- NA

  # Seedless Vascular Plant metric (# ferns and fern allies)
  svp1 <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1,
           FORM, ScientificName) |>
    #filter(DomVeg_Lev1 %in% c("forest", "shrub")) |>
    filter(FORM %in% c("fern")) |> unique() |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, FORM, DomVeg_Lev1) |>
    summarize(Num_SVP = sum(!is.na(ScientificName)), .groups= "drop") |>
    mutate(SVP_Score = case_when(is.na(Num_SVP) ~ NA_real_,
                                 Num_SVP == 0 ~ 0,
                                 Num_SVP == 1 ~ 3,
                                 Num_SVP == 2 ~ 7,
                                 Num_SVP >= 3 ~ 10,
                                 TRUE ~ NA_real_))

  # Add 0s where no sporophytes were found in emergent
  svp <- left_join(herbs_lj, svp1, by = c("LocationID", "FeatureID", "SampleYear", "ModuleNo", "DomVeg_Lev1"))
  svp$Num_SVP[is.na(svp$Num_SVP)] <- 0
  svp$SVP_Score[svp$Num_SVP == 0] <- 0
  svp$SVP_Score[svp$DomVeg_Lev1 == "emergent"] <- NA


  # FQAI Equation 7, page 7 of
    # https://dam.assets.ohio.gov/image/upload/epa.ohio.gov/Portals/35/wetlands/Ohio_FQAI.pdf
  FQAI <- herbs |>
    filter(!is.na(COFC)) |> # drop species without Coefs
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1,
           ScientificName, COFC) |> unique() |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1) |>
    summarize(CC = sum(COFC),
              NumSpp = sum(!is.na(ScientificName)),
              FQAI = CC/sqrt(NumSpp),
              .groups = 'drop') |>
    mutate(FQAI_Score = case_when(is.na(FQAI) ~ NA_real_,
                                  DomVeg_Lev1 %in% c("emergent", "shrub") & FQAI <= 9.9 ~ 0,
                                  DomVeg_Lev1 %in% c("emergent", "shrub") & FQAI > 9.9 & FQAI <= 14.3 ~ 3,
                                  DomVeg_Lev1 %in% c("emergent", "shrub") & FQAI > 14.3 & FQAI <= 21.4 ~ 7,
                                  DomVeg_Lev1 %in% c("emergent", "shrub") & FQAI > 21.4 ~ 10,

                                  DomVeg_Lev1 %in% c("forest") & FQAI <= 14.0 ~ 0,
                                  DomVeg_Lev1 %in% c("forest") & FQAI > 14.0 & FQAI <= 19.0 ~ 3,
                                  DomVeg_Lev1 %in% c("forest") & FQAI > 19.0 & FQAI <= 24.0 ~ 7,
                                  DomVeg_Lev1 %in% c("forest") & FQAI > 24.0 ~ 10,
                                  TRUE ~ NA_real_),

           FQAI_Score_FQ = case_when(is.na(FQAI) ~ NA_real_,
                                     FQAI <= 10 ~ 0,
                                     FQAI >= 10 & FQAI < 30 ~ ((FQAI - 10)/20) * 50,
                                     FQAI >= 30 ~ 50,
                                     TRUE ~ NA_real_))
  CovWt_CofC <- herbs |>
    mutate(cov_wt_C = rel_cov * COFC) |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1, tot_cov) |>
    summarize(cov_wt_C = sum(cov_wt_C, na.rm = T), .groups = 'drop') |>
    mutate(#Cov_Wt_C_Score = case_when(is.na(cov_wt_C) ~ NA_real_, # I don't think this is actually scored outside of FQ
           #                          cov_wt_C == 0 ~ 0,
           #                          cov_wt_C > 0 & cov_wt_C <= 6 ~ 3,
           #                          cov_wt_C > 6 ~ 10),
           Cov_Wt_C_Score_FQ = case_when(tot_cov < 0.75 & DomVeg_Lev1 == "forest" ~
                                            (((tot_cov/0.75) * cov_wt_C)/6) * 50, #from spreadsheet
                                         is.na(cov_wt_C) ~ NA_real_,
                                         cov_wt_C == 0 ~ 0,
                                         cov_wt_C > 0 & cov_wt_C <= 6 ~ (cov_wt_C/6) * 50,
                                         cov_wt_C > 6 ~ 50))

  # % Bryophyte using rel_cov
  pct_bryo1 <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1,
           ScientificName, FORM, rel_cov) |>
    #filter(DomVeg_Lev1 %in% c("forest", "shrub")) |>
    filter(FORM == "bryo") |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1) |>
    summarize(Pct_Bryo = sum(rel_cov, na.rm = T), .groups = 'drop') |>
    mutate(Pct_Bryo_Score = case_when(is.na(Pct_Bryo) ~ NA_real_,
                                     Pct_Bryo <= 0.01 ~ 0,
                                     Pct_Bryo > 0.01 & Pct_Bryo <= 0.03 ~ 3,
                                     Pct_Bryo > 0.03 & Pct_Bryo <= 0.06 ~ 7,
                                     Pct_Bryo > 0.06 ~ 10,
                                     TRUE ~ NA_real_))

  # Add 0s where no bryos were found in forest or shrub
  pct_bryo <- left_join(herbs_lj, pct_bryo1, by = c("LocationID", "FeatureID", "SampleYear", "ModuleNo", "DomVeg_Lev1"))
  pct_bryo$Pct_Bryo[is.na(pct_bryo$Pct_Bryo)] <- 0
  pct_bryo$Pct_Bryo_Score[pct_bryo$Pct_Bryo == 0] <- 0
  pct_bryo$Pct_Bryo_Score[pct_bryo$DomVeg_Lev1 %in% "emergent"] <- NA


  # % Hydrophyte using rel_cov: OH_STATUS = native, SHADE = shade or partial, WET/WETreg = FACW (FACW) OBL
  # *if total cover(sum of cover values for all species observed in sample plot is <10%, all % metrics scored as 0)
  pct_hydro_reg1 <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1,
           ScientificName, OH_STATUS, SHADE, WETreg, tot_cov, rel_cov) |>
    filter(OH_STATUS == "native" & WETreg %in% wet &
             SHADE %in% c("partial", "shade")) |> #unique() |>
    # filter(DomVeg_Lev1 %in% c("forest")) |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1, tot_cov) |>
    summarize(Pct_Hydro_reg = sum(rel_cov, na.rm = T),
              .groups = 'drop') |>
    mutate(Pct_Hydro_Score_reg = case_when(is.na(Pct_Hydro_reg) ~ NA_real_,
                                          tot_cov < 0.10 ~ 0, # first case
                                          Pct_Hydro_reg <= 0.1 ~ 0,
                                          Pct_Hydro_reg > 0.1 & Pct_Hydro_reg <= 0.15 ~ 3,
                                          Pct_Hydro_reg > 0.15 & Pct_Hydro_reg <= 0.28 ~ 7,
                                          Pct_Hydro_reg > 0.28 ~ 10,
                                          TRUE ~ NA_real_))

  # Add 0s where no hydros were found in forest or shrub
  pct_hydro_reg <- left_join(herbs_lj, pct_hydro_reg1, by = c("LocationID", "FeatureID", "SampleYear", "ModuleNo", "DomVeg_Lev1"))
  pct_hydro_reg$Pct_Hydro_reg[is.na(pct_hydro_reg$Pct_Hydro_reg)] <- 0
  pct_hydro_reg$Pct_Hydro_Score_reg[pct_hydro_reg$Pct_Hydro_reg == 0] <- 0
  pct_hydro_reg$Pct_Hydro_Score_reg[!pct_hydro_reg$DomVeg_Lev1 %in% "forest"] <- NA


  pct_hydro1 <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1,
           ScientificName, OH_STATUS, SHADE, WET, tot_cov, rel_cov) |> unique() |>
    filter(OH_STATUS == "native" & WET %in% wet) |>
          # & SHADE %in% c("partial", "shade"))  # VIBI spreadsheet doesn't filter on SHADE
    #filter(DomVeg_Lev1 %in% c("forest")) |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1, tot_cov) |>
    summarize(Pct_Hydro = sum(rel_cov, na.rm = T), .groups = 'drop') |>
    mutate(Pct_Hydro_Score = case_when(is.na(Pct_Hydro) ~ NA_real_,
                                      tot_cov < 0.10 ~ 0, # first case
                                      Pct_Hydro <= 0.1 ~ 0,
                                      Pct_Hydro > 0.1 & Pct_Hydro <= 0.15 ~ 3,
                                      Pct_Hydro > 0.15 & Pct_Hydro <= 0.28 ~ 7,
                                      Pct_Hydro > 0.28 ~ 10,
                                      TRUE ~ NA_real_))

  # Add 0s where no hydros were found in forest or shrub
  pct_hydro <- left_join(herbs_lj, pct_hydro1, by = c("LocationID", "FeatureID", "SampleYear", "ModuleNo", "DomVeg_Lev1"))
  pct_hydro$Pct_Hydro[is.na(pct_hydro$Pct_Hydro)] <- 0
  pct_hydro$Pct_Hydro_Score[pct_hydro$Pct_Hydro == 0] <- 0
  pct_hydro$Pct_Hydro_Score[!pct_hydro$DomVeg_Lev1 %in% "forest"] <- NA

  # % sensitive - rel cover of COFC >=6, for DomVeg_Lev1 = shrub, buttonbush is not included as %sensitive
  # *if total cover(sum of cover values for all species observed in sample plot is <10%, all % metrics scored as 0)
  pct_sens1 <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1,
           ScientificName, COFC, tot_cov, rel_cov) |>
    filter(COFC >= 6) |>
    filter(!(DomVeg_Lev1 == "shrub" & ScientificName %in% "Cephalanthus occidentalis")) |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1, tot_cov) |>
    summarize(Pct_Sens = sum(rel_cov, na.rm = T), .groups = 'drop') |>
    mutate(Pct_Sens_Score = case_when(is.na(Pct_Sens) ~ NA_real_,
                                     tot_cov < 10 ~ 0, # first case,
                                     DomVeg_Lev1 %in% c("emergent") & Pct_Sens <= 0.025 ~ 0,
                                     DomVeg_Lev1 %in% c("emergent") & Pct_Sens > 0.025 & Pct_Sens <= 0.10 ~ 3,
                                     DomVeg_Lev1 %in% c("emergent") & Pct_Sens > 0.10 & Pct_Sens <= 0.15 ~ 7,
                                     DomVeg_Lev1 %in% c("emergent") & Pct_Sens > 0.15 ~ 10,

                                     DomVeg_Lev1 %in% c("shrub") & Pct_Sens <= 0.02 ~ 0,
                                     DomVeg_Lev1 %in% c("shrub") & Pct_Sens > 0.02 & Pct_Sens <= 0.06 ~ 3,
                                     DomVeg_Lev1 %in% c("shrub") & Pct_Sens > 0.06 & Pct_Sens <= 0.13 ~ 7,
                                     DomVeg_Lev1 %in% c("shrub") & Pct_Sens > 0.13 ~ 10,

                                     DomVeg_Lev1 %in% c("forest") & Pct_Sens <= 0.035~ 0,
                                     DomVeg_Lev1 %in% c("forest") & Pct_Sens > 0.035 & Pct_Sens <= 0.12 ~ 3,
                                     DomVeg_Lev1 %in% c("forest") & Pct_Sens > 0.12 & Pct_Sens <= 0.30 ~ 7,
                                     DomVeg_Lev1 %in% c("forest") & Pct_Sens > 0.30 ~ 10,
                                     TRUE ~ NA_real_))

  # Add 0s where no hydros were found in forest or shrub
  pct_sens <- left_join(herbs_lj, pct_sens1, by = c("LocationID", "FeatureID", "SampleYear", "ModuleNo", "DomVeg_Lev1"))
  pct_sens$Pct_Sens[is.na(pct_sens$Pct_Sens)] <- 0
  pct_sens$Pct_Sens_Score[pct_sens$Pct_Sens == 0] <- 0

  # % tolerant
  # *if total cover(sum of cover values for all species observed in sample plot is <10%, all % metrics scored as 0)
  pct_tol1 <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1,
           ScientificName, COFC, tot_cov, rel_cov) |>
    filter(COFC <= 2) |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1, tot_cov) |>
    summarize(Pct_Tol = sum(rel_cov, na.rm = T), .groups = 'drop') |>
    mutate(Pct_Tol_Score = case_when(is.na(Pct_Tol) ~ NA_real_,
                                    tot_cov < 0.10 ~ 0, # first case
                                    DomVeg_Lev1 %in% c("emergent") & Pct_Tol >= 0.60 ~ 0,
                                    DomVeg_Lev1 %in% c("emergent") & Pct_Tol >= 0.40  & Pct_Tol < 0.60 ~ 3,
                                    DomVeg_Lev1 %in% c("emergent") & Pct_Tol >= 0.20 & Pct_Tol < 0.40 ~ 7,
                                    DomVeg_Lev1 %in% c("emergent") & Pct_Tol < 0.20 ~ 10,

                                    DomVeg_Lev1 %in% c("forest") & Pct_Tol >= 0.45 ~ 0,
                                    DomVeg_Lev1 %in% c("forest") & Pct_Tol >= 0.30  & Pct_Tol < 0.45 ~ 3,
                                    DomVeg_Lev1 %in% c("forest") & Pct_Tol >= 0.15 & Pct_Tol < 0.30 ~ 7,
                                    DomVeg_Lev1 %in% c("forest") & Pct_Tol < 0.15 ~ 10,

                                    DomVeg_Lev1 %in% c("shrub") & Pct_Tol >= 0.15 ~ 0,
                                    DomVeg_Lev1 %in% c("shrub") & Pct_Tol >= 0.10  & Pct_Tol < 0.15 ~ 3,
                                    DomVeg_Lev1 %in% c("shrub") & Pct_Tol >= 0.05 & Pct_Tol < 0.10 ~ 7,
                                    DomVeg_Lev1 %in% c("shrub") & Pct_Tol < 0.05 ~ 10,
                                    TRUE ~ NA_real_))

  # Add 0s where no hydros were found in forest or shrub
  pct_tol <- left_join(herbs_lj, pct_tol1, by = c("LocationID", "FeatureID", "SampleYear", "ModuleNo", "DomVeg_Lev1"))
  pct_tol$Pct_Tol[is.na(pct_tol$Pct_Tol)] <- 0
  pct_tol$Pct_Tol_Score[pct_tol$Pct_Tol == 0] <- 0

  # Invasive graminoids: Phalaris arundinaceae, Typha spp. Phragmites australis
  # *if total cover(sum of cover values for all species observed in sample plot is <10%, all % metrics scored as 0)
  inv_grams <- c("Phalaris arundinacea", "Phragmites australis ssp. australis",
                 "Typha angustifolia", "Typha latifolia", "Typha x glauca",
                 "Typha minima", "Typha sp.")

  pct_invgram1 <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1, DomVeg_Lev2,
           ScientificName, tot_cov, rel_cov) |>
    filter(ScientificName %in% inv_grams) |> #unique() |>
    # filter(DomVeg_Lev1 %in% c("emergent") |
    #          (DomVeg_Lev1 == "shrub" & DomVeg_Lev2 == "Bog Shrub Swamp")) |>
    # leatherleaf bog isn't in the CUVA data, but including it here because of pg 19 of
    # EPA manual states that this metric replaces the subcanopy IV metric for this community.
    # Based on the VIBI spreadsheet and the HTLN database, Bog Shrub Swamp seems to cover
    # that, although there's not a site like this in the data.
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1, DomVeg_Lev2, tot_cov) |>
    summarize(Pct_InvGram = sum(rel_cov, na.rm = T), .groups = 'drop') |>
    mutate(Pct_InvGram_Score = case_when(is.na(Pct_InvGram) ~ NA_real_,
                                        tot_cov < 10 ~ 0, # first case
                                        Pct_InvGram >= 0.31 ~ 0,
                                        Pct_InvGram >= 0.15 & Pct_InvGram < 0.31 ~ 3,
                                        Pct_InvGram >= 0.03 & Pct_InvGram < 0.15 ~ 7,
                                        Pct_InvGram < 0.03 ~ 10,
                                        TRUE ~ NA_real_))

  # Add 0s where no invgrams were found in forest or shrub
  pct_invgram <- left_join(herbs_lj, pct_invgram1, by = c("LocationID", "FeatureID", "SampleYear", "ModuleNo", "DomVeg_Lev1"))
  pct_invgram$Pct_InvGram[is.na(pct_invgram$Pct_InvGram)] <- 0
  pct_invgram$Pct_InvGram_Score[pct_invgram$Pct_InvGram == 0] <- 0
  pct_invgram$Pct_InvGram_Score[pct_invgram$DomVeg_Lev1 == "forest"] <- NA
  pct_invgram$Pct_InvGram_Score[pct_invgram$DomVeg_Lev1 == "shrub" &
                                 (is.na(pct_invgram$DomVeg_Lev2)  |
                                    !pct_invgram$DomVeg_Lev2 == "Bog Shrub Swamp")] <- NA

  #---- Compile Woody and Big Tree metrics ----
  # For woody metrics to work, need to drop big tree records from tbl_VIBI_Woody and
  # then row bind the Big Tree DBH records
  woody1 <- getWoody(years = years, survey_type = survey_type, hgm_class = hgm_class,
                     dom_veg1 = dom_veg1, plotID = plotID, nativity = 'all', intens_mods = intens_mods)

  woody1$Count[woody1$Count == -9999] <- NA_real_
  woody1 <- woody1 |> mutate(ba_cm2 = (pi*(DBH_MidPt/2)^2)*Count)
  # Set wet status based on the column chosen, like in the macros code
  woody1$WETreg <- ifelse(is.na(woody1[,region]), woody1$WET, woody1[,region])
  woody1$WETreg <- ifelse(is.na(woody1$WETreg), "ND", woody1$WETreg)
  woody1$AreaHA <- as.numeric(woody1$AreaHA)
  woody2 <- woody1 |> filter(!DiamID %in% "BIG")

  bigt1 <- getBigTrees(years = years, survey_type = survey_type, hgm_class = hgm_class,
                       dom_veg1 = dom_veg1, plotID = plotID, nativity = 'all', intens_mods = intens_mods) |>
    mutate(BIG_ba_cm2 = pi*(DBH/2)^2)

  # Set wet status based on the column chosen, like in the macros code
  bigt1$WETreg <- ifelse(is.na(bigt1[,region]), bigt1$WET, bigt1[,region])
  bigt1$WETreg <- ifelse(is.na(bigt1$WETreg), "ND", bigt1$WETreg)
  bigt1$AreaHA <- as.numeric(bigt1$AreaHA)

  bigt <- bigt1 |> group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear,
                            ModuleNo, AreaHA, DomVeg_Lev1, DomVeg_Lev2, OH_STATUS,
                            SHADE, FORM, WET, WETreg,
                            ScientificName) |>
    summarize(BIG_BA = sum(BIG_ba_cm2, na.rm = T),
              BIG_Count = sum(!is.na(DBH)),
              DiamID = "BIG",
              DiamVal = ">= 40",
              .groups = 'drop')

  woody_comb <- rbind(woody2 |> select(LocationID, FeatureID, EventID, SampleDate, SampleYear,
                                       ModuleNo, AreaHA, DomVeg_Lev1, DomVeg_Lev2, OH_STATUS, SHADE, FORM,
                                       # WET, WETreg, # don't need b/c covered by first filter
                                       DiamID, DiamVal, ScientificName, Count, ba_cm2),
                      bigt |> select(LocationID, FeatureID, EventID, SampleDate, SampleYear,
                                     ModuleNo, AreaHA, DomVeg_Lev1, DomVeg_Lev2, OH_STATUS, SHADE, FORM,
                                     # WET, WETreg, # don't need b/c covered by first filter
                                     DiamID, DiamVal, ScientificName, Count = BIG_Count,
                                     ba_cm2 = BIG_BA))

  # Calc rel. density
  woody_rc <- woody_comb |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1) |>
    summarize(tot_stems = sum(Count, na.rm = T),
              tot_stems_per_ha = tot_stems/first(AreaHA),
              tot_ba_cm2 = sum(ba_cm2, na.rm = T),
              .groups = 'drop')

  woody <- left_join(woody_comb, woody_rc,
                     by = c("LocationID", "FeatureID", "EventID", "SampleDate",
                            "SampleYear", "ModuleNo", "DomVeg_Lev1")) |>
    filter(!is.na(Count)) # this filter drops FeatureID 305 from 2015 for Mod 3 Carya ovata C5 with -9999.
    # It's also a duplicate record, as there's a Carya ovata in the same size class with a count in Mod 3.

  # Create table to left_join with herb vibi metrics; There are no sample qualifiers for sampled, but non present
  # So assuming if there's a record in this df below, and the community matches, the VIBI should be 0 for woody vibis
  woody_lj <- woody |> select(LocationID, FeatureID, DomVeg_Lev1, SampleYear, ModuleNo) |> unique()

  # woody_check <- woody |> group_by(FeatureID, EventID, SampleYear, ModuleNo) |>
  #   summarize(rel_sum = sum(rel_stems))
  #
  # table(woody_check$rel_sum, useNA = 'always') # all sum to 1

  # Pole Timber rel dens
  #table(woody$DiamID, woody$DiamVal) C5-C7
  #**If no or only a few woody stems >1m tall in sample plot or if stems per ha <10, score metric as 0.
  # Interpreting this as <= 3 stems >1m tall
  pole1 <- woody |> #filter(DomVeg_Lev1 == "forest") |>
    filter(DiamID %in% c("C5", "C6", "C7")) |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1) |>
    summarize(RelDen_SmTree = sum(Count)/first(tot_stems),
              tot_stems_per_ha = first(tot_stems_per_ha),
              .groups = 'drop') |>
    mutate(SmTree_Score = case_when(is.na(RelDen_SmTree) ~ NA_real_,
                                    tot_stems_per_ha < 10 ~ 0, # ** Table 2
                                    RelDen_SmTree >= 0.32 ~ 0,
                                    RelDen_SmTree >= 0.22 & RelDen_SmTree < 0.32 ~ 3,
                                    RelDen_SmTree >= 0.11 & RelDen_SmTree < 0.22 ~ 7,
                                    RelDen_SmTree < 0.11 ~ 10,
                                    TRUE ~ NA_real_))

  # Add 0s where no pole trees were found in forest
  pole <- left_join(woody_lj, pole1, by = c("LocationID", "FeatureID", "SampleYear", "ModuleNo", "DomVeg_Lev1"))
  pole$RelDen_SmTree[is.na(pole$RelDen_SmTree)] <- 0
  pole$SmTree_Score[pole$RelDen_SmTree == 0] <- 0
  pole$SmTree_Score[!pole$DomVeg_Lev1 %in% "forest"] <- NA
  # Canopy and Subcanopy IV
  # Had to rbind all woody <40cm to Big Trees records to get correct DBH and BA for IV
  IV <- woody |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo,
             DomVeg_Lev1, DomVeg_Lev2, ScientificName, OH_STATUS, FORM, SHADE, DiamID) |>
    summarize(Count = sum(Count),
              BA = sum(ba_cm2),
              tot_ba_cm2 = first(tot_ba_cm2),
              tot_stems = first(tot_stems),
              tot_stems_per_ha = first(tot_stems_per_ha),
              .groups = 'drop') |>
    pivot_wider(values_from = c(Count, BA), names_from = DiamID, values_fill = 0,
                names_glue = "{DiamID}_{.value}") |>
    data.frame()

  # Add class columns potentially missing
  count_cols <- c("C0_Count", "C1_Count", "C2_Count", "C3_Count", "C4_Count",
                  "C5_Count", "C6_Count", "C7_Count", "C8_Count", "C9_Count",
                  "C10_Count", "BIG_Count")
  ba_cols <- c("C0_BA", "C1_BA", "C2_BA", "C3_BA", "C4_BA",
               "C5_BA", "C6_BA", "C7_BA", "C8_BA", "C9_BA",
               "C10_BA", "BIG_BA")

  miss_cols <- setdiff(c(count_cols, ba_cols), c(names(IV)))
  IV[miss_cols] <- 0

  # Calc rel class freq
  IV$rel_class_freq = rowSums(IV[,count_cols] > 0, na.rm = T) / length(count_cols) # 12; in case # classes changes
  head(IV)
  IV$rel_ba = rowSums(IV[,ba_cols], na.rm = T)/IV$tot_ba_cm2
  IV$rel_dens = rowSums(IV[,count_cols], na.rm = T)/IV$tot_stems
  IV[,c("rel_class_freq", "rel_ba", "rel_dens")][is.na(IV[,c("rel_class_freq", "rel_ba", "rel_dens")])] <- 0
  IV$IV = (IV$rel_class_freq + IV$rel_ba + IV$rel_dens)/3

  #---- Subcanopy IV ----
  # for F and SH
  # Manual states that subcan IV is the sum IV of
  # 1. native shade tolerant subcanopy species (FORM shrub and sm tree), plus
  # 2. IV of native FAC subcanopy (shrub and sm tree) species.
  # However, in the spreadsheet there's no calc of the 2nd, likely assuming
  # it's covered by the first, because it's all the same except wetness
  # For leatherleaf bogs, substitute invasive graminoid metric
  # ** If no or only a few woody stems >1m tall in sample plot or if stems per ha <10, score metric as 0.

  subcan_IV1 <- IV |>
    #filter(DomVeg_Lev1 %in% c("forest", "shrub")) |>
    #filter(!(DomVeg_Lev2 %in% "Bog Shrub Swamp")) |>
    filter(OH_STATUS == "native") |>
    filter(SHADE %in% c("partial", "shade")) |>
    filter(FORM %in% c("shrub", "sm tree")) |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear,
             ModuleNo, DomVeg_Lev1, tot_stems_per_ha) |>
    summarize(SubcanIV_num = sum(IV),
              SubcanIV_den = sum(IV > 0), # sums a logical statement, so gives count
              SubcanIV = SubcanIV_num/SubcanIV_den,
              .groups = 'drop') |>
    mutate(SubcanIV_Score = case_when(tot_stems_per_ha < 10 ~ 0, #** Table 2
                                      DomVeg_Lev1 == "forest" & SubcanIV <= 0.02 ~ 0,
                                      DomVeg_Lev1 == "forest" & SubcanIV > 0.02 & SubcanIV <= 0.072 ~ 3,
                                      DomVeg_Lev1 == "forest" & SubcanIV > 0.072 & SubcanIV <= 0.13 ~ 7,
                                      DomVeg_Lev1 == "forest" & SubcanIV > 0.13 ~ 10,

                                      DomVeg_Lev1 == "shrub" & SubcanIV <= 0.02 ~ 0,
                                      DomVeg_Lev1 == "shrub" & SubcanIV > 0.02 & SubcanIV <= 0.05 ~ 3,
                                      DomVeg_Lev1 == "shrub" & SubcanIV > 0.05 & SubcanIV <= 0.10 ~ 7,
                                      DomVeg_Lev1 == "shrub" & SubcanIV > 0.10 ~ 10,

                                      TRUE ~ NA_real_))

  # Add 0s where no subcanopy trees were found in forest
  subcan_IV <- left_join(woody_lj, subcan_IV1, by = c("LocationID", "FeatureID", "SampleYear", "ModuleNo", "DomVeg_Lev1"))
  subcan_IV$SubcanIV[is.na(subcan_IV$SubcanIV)] <- 0
  subcan_IV$SubcanIV_Score[subcan_IV$SubcanIV == 0] <- 0
  subcan_IV$SubcanIV_Score[subcan_IV$DomVeg_Lev1 == "emergent"] <- NA
  subcan_IV$SubcanIV_Score[subcan_IV$DomVeg_Lev2 == "Bog Shrub Swamp"] <- NA


  canopy_IV1 <- IV |>
    #filter(DomVeg_Lev1 %in% c("forest")) |>
    filter(OH_STATUS == "native") |>
    filter(FORM %in% c("tree")) |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1) |>
    summarize(CanopyIV_num = sum(IV),
              CanopyIV_den = sum(IV > 0), # sums a logical statement, so gives count
              CanopyIV = CanopyIV_num/CanopyIV_den,
              .groups = 'drop') |>
    mutate(CanopyIV_Score = case_when(CanopyIV >= 0.21 ~ 0,
                                      CanopyIV >= 0.17 & CanopyIV < 0.21 ~ 3,
                                      CanopyIV >= 0.14 & CanopyIV < 0.17 ~ 7,
                                      CanopyIV > 0 & CanopyIV < 0.14 ~ 10,
                                      CanopyIV == 0 ~ 0, # from *** in Table 2
                                      TRUE ~ NA_real_))

  # Add 0s where no canopy trees were found in forest
  canopy_IV <- left_join(woody_lj, canopy_IV1, by = c("LocationID", "FeatureID", "SampleYear", "ModuleNo", "DomVeg_Lev1"))
  canopy_IV$CanopyIV[is.na(canopy_IV$CanopyIV)] <- 0
  canopy_IV$CanopyIV_Score[canopy_IV$CanopyIV == 0] <- 0
  canopy_IV$CanopyIV_Score[!canopy_IV$DomVeg_Lev1 %in% "forest"] <- NA


  # % Unvegetated
  # **** This metric should be calculated for wetland mitigation sites where perennial
  # hydrophyte vegetation is not well established or where g/m2 of biomass is less than 100.
  # It can also be used as a biomass metric substitute for mitigation wetlands
  # or other emergent sites where biomass cannot be collected (Table 2)
  # It does not appear to be a metric collected by CUVA, so I won't include it. I'm guessing it's
  # because there are no mitigation wetlands with minimal plant establishment

  #---- Compile Biomass Metrics ----
  # Assumes area sampled is always 0.01m2 to get grams/m2
  bmass1 <- getBiomass(years = years, survey_type = survey_type, hgm_class = hgm_class,
                      dom_veg1 = dom_veg1, plotID = plotID, intens_mods = intens_mods)

  bmass <- bmass1 |>
           filter(DomVeg_Lev1 == "emergent") |>
           mutate(weight_g_m2 = DryWt/0.01) |>
           group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, TotalMods, AreaHA,
                    ModuleNo, DomVeg_Lev1) |>
           summarize(num_bmass_samp = sum(!is.na(DryWt)),
                     Avg_Bmass = sum(weight_g_m2)/num_bmass_samp,
                     .groups = 'drop') |>
           mutate(Biomass_Score = case_when(is.na(Avg_Bmass) ~ NA_real_,
                                            Avg_Bmass > 800 ~ 0,
                                            Avg_Bmass >= 451 & Avg_Bmass <= 800 ~ 3,
                                            Avg_Bmass >= 201 & Avg_Bmass < 451 ~ 7,
                                            Avg_Bmass >= 100 & Avg_Bmass < 201 ~ 10,
                                            Avg_Bmass < 100 ~ 0,
                                            TRUE ~ NA_real_
                                            ))

  # Not adding 0s for Biomass, because not every module is sampled for biomass every year.
  # I don't have a way to logically changes 0s to NA

  #---- Combine metrics for VIBI score and final rating ----

  # Create table to left_join with vibi metrics; Dropped EventID and SampleDate because not identical withins sample period
  herbs_recs <- herbs |> select(LocationID, FeatureID, DomVeg_Lev1, SampleYear, ModuleNo) |> mutate(herb = 1) |> unique()
  woody_recs <- woody |> select(LocationID, FeatureID, DomVeg_Lev1, SampleYear, ModuleNo) |> mutate(woody = 1) |> unique()
  bmass_recs <- bmass |> select(LocationID, FeatureID, DomVeg_Lev1, SampleYear, ModuleNo) |> mutate(bmass = 1) |> unique()

  full_evs1 <- purrr::reduce(list(herbs_recs, woody_recs, bmass_recs), full_join,
                            by = c("LocationID", "FeatureID", "DomVeg_Lev1", "SampleYear", "ModuleNo"))

  full_evs1$num_samps <- rowSums(full_evs1[,c("herb", "woody", "bmass")], na.rm = T)

  # Drop records that only have biomass sampled
  full_evs <- full_evs1 |> filter(!(bmass == 1 & num_samps == 1)) |> select(-herb, -woody, -bmass, -num_samps)

  # Removed EventID, SampleDate, from list, because not identical across herb, woody, biomass data
  vibi_list <- list(
    carex |> select(LocationID, FeatureID, SampleYear, ModuleNo, Num_Carex, Carex_Score),
    cyper |> select(LocationID, FeatureID, SampleYear, ModuleNo, Num_Cyper, Cyper_Score),
    dicot |> select(LocationID, FeatureID, SampleYear, ModuleNo, Num_Dicot, Dicot_Score),
    shade |> select(LocationID, FeatureID, SampleYear, ModuleNo, Num_Shade, Shade_Score),
    shrub_reg |> select(LocationID, FeatureID, SampleYear, ModuleNo, Num_Shrub_reg, Shrub_Score_reg),
    shrub |> select(LocationID, FeatureID, SampleYear, ModuleNo, Num_Shrub, Shrub_Score),
    hydrop_reg |> select(LocationID, FeatureID, SampleYear, ModuleNo, Num_Hydro_reg, Hydro_Score_reg),
    hydrop |> select(LocationID, FeatureID, SampleYear, ModuleNo, Num_Hydro, Hydro_Score),
    ap_ratio |> select(LocationID, FeatureID, SampleYear, ModuleNo, AP_Ratio, AP_Score),
    svp |> select(LocationID, FeatureID, SampleYear, ModuleNo, Num_SVP, SVP_Score),
    FQAI |> select(LocationID, FeatureID, SampleYear, ModuleNo, NumSpp, FQAI, FQAI_Score, FQAI_Score_FQ),
    CovWt_CofC |> select(LocationID, FeatureID, SampleYear, ModuleNo, tot_cov, cov_wt_C, Cov_Wt_C_Score_FQ),
    pct_bryo |> select(LocationID, FeatureID, SampleYear, ModuleNo, Pct_Bryo, Pct_Bryo_Score),
    pct_hydro_reg |> select(LocationID, FeatureID, SampleYear, ModuleNo, Pct_Hydro_reg, PctHydro_Score_reg),
    pct_hydro |> select(LocationID, FeatureID, SampleYear, ModuleNo, Pct_Hydro, Pct_Hydro_Score),
    pct_sens |> select(LocationID, FeatureID, SampleYear, ModuleNo, Pct_Sens, Pct_Sens_Score),
    pct_tol |> select(LocationID, FeatureID, SampleYear, ModuleNo, Pct_Tol, Pct_Tol_Score),
    pct_invgram |> select(LocationID, FeatureID, SampleYear, ModuleNo, Pct_InvGram, Pct_InvGram_Score),
    pole |> select(LocationID, FeatureID, SampleYear, ModuleNo, RelDen_SmTree, SmTree_Score),
    subcan_IV |> select(LocationID, FeatureID, SampleYear, ModuleNo, SubcanIV, SubcanIV_Score),
    canopy_IV |> select(LocationID, FeatureID, SampleYear, ModuleNo, CanopyIV, CanopyIV_Score),
    bmass |> select(LocationID, FeatureID, SampleYear, ModuleNo, Avg_Bmass, Biomass_Score)
  )

  vibi_comb <- purrr::reduce(vibi_list, left_join,
                       by = c("LocationID", "FeatureID", "SampleYear", "ModuleNo")) |> data.frame()

  vibi_comb2 <- left_join(full_evs, vibi_comb, by = c("LocationID", "FeatureID", "SampleYear", "ModuleNo"))

  #names(vibi_comb2)[grepl("Score", names(vibi_comb2))]

  score_reg_cols <- c("Carex_Score", "Cyper_Score",
                      "Dicot_Score", "Shade_Score",
                     "Shrub_Score_reg", "Hydro_Score_reg", "SVP_Score", "AP_Score", "FQAI_Score",
                     "PctBryo_Score", "PctHydro_Score_reg", "PctSens_Score", "PctTol_Score", "PctInvGram_Score",
                     "SmTree_Score", "SubcanIV_Score", "CanopyIV_Score", "Biomass_Score")

  score_state_cols <- c("Carex_Score", "Cyper_Score",
                        "Dicot_Score", "Shade_Score",
                        "Shrub_Score", "Hydro_Score", "SVP_Score", "AP_Score", "FQAI_Score",
                        "PctBryo_Score", "PctHydro_Score", "PctSens_Score", "PctTol_Score", "PctInvGram_Score",
                        "SmTree_Score", "SubcanIV_Score", "CanopyIV_Score", "Biomass_Score")

  vibi_fq_cols <- c("FQAI_Score_FQ", "Cov_Wt_C_Score_FQ")

  final_dat <- left_join(plots_abbr, vibi_comb2, by = c("LocationID", "FeatureID", "DomVeg_Lev1"))


  final_dat$VIBI_Score_ACOEReg <- rowSums(final_dat[,score_reg_cols], na.rm = T)
  final_dat$VIBI_Score_State <- rowSums(final_dat[,score_state_cols], na.rm = T)
  final_dat$VIBI_Score_FQ <- rowSums(final_dat[,vibi_fq_cols], na.rm = T)

  final_dat <- final_dat |> arrange(FeatureID, SampleYear, ModuleNo)

  # write.csv(final_dat |> select(-Park, -County, -PlotConfig, -AreaHA, -(X1oPlants:HGMClass),
  #                               -(DomVeg_Lev2:DomVeg_Lev3)),
  #           "./testing_scripts/vibi_check3.csv", row.names = F)

  return(final_dat)
  }
