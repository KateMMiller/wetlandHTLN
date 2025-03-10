#' @title sumVIBI: summarize plot-level VIBI
#'
#' @importFrom dplyr between c_across case_when filter first group_by left_join mutate select summarize
#' @importFrom tidyr pivot_wider
#' @importFrom tidyselect starts_with
#'
#' @description This function summarizes plot-level VIBI and filters by plot, year, and plot types.
#'
#' @param years Numeric. Filter on years of survey. Default is all.
#'
#' @param survey_type Filter on survey type of plots. Options are "all" (default), "reference",
#' "survey", "survey, womc", "womc", and "womc, reference". Can choose multiple options.
#'
#' @param hgm_class Filter on HGM class. Options are "all" (default), "Depression",
#' "Impoundment", "Riverine", "Slope". Can choose multiple options.
#'
#' @param dom_veg1 Filter on level 1 dominant vegetation type. Options are "all" (default), "Emergent",
#' "Forest", "Shrub". Can choose multiple options.
#'
#' @param plotID Quoted string. Default is 'all'. If specified will return data for only plots specified.
#' Can choose multiple plots. Based on FeatureID in database.
#'
#' @param nativity Quoted string. Filter on native status. Options are "all" (default), "adventive",
#' "cryptogeni", or "native". Can choose multiple statuses.
#'
#' @param region Quoted string. Default is "NCNE". Specifies the Army Corps Region for OH. Options are "EMP", "MW", and "NCNE".
#'
#' @examples
#' \dontrun{
#' # run first
#' importData()
#'
#'  # ADD EXAMPLES
#'
#' }
#'
#' @return Returns a data frame of VIBI calculations for each plot
#' @export
#'

sumVIBI <- function(years = 2008:as.numeric(format(Sys.Date(), format = "%Y")),
                    survey_type = 'all', hgm_class = 'all', dom_veg1 = 'all',
                    plotID = 'all', region = "NCNE"){

  #---- Bug handling ----
  survey_type <- match.arg(survey_type, several.ok = T,
                           choices = c("all", "reference", "survey", "survey, womc", "womc", "womc, reference"))
  hgm_class <- match.arg(hgm_class, choices = c("all", "Depression", "Impoundment", "Riverine", "Slope"),
                         several.ok = T)
  dom_veg1 <- match.arg(dom_veg1, choices = c("all", "Emergent", "Forest", "Shrub"), several.ok = T)
  nativity <- match.arg(nativity, choices = c("all", "adventive", "cryptogeni", "native"), several.ok = T)
  region <- match.arg(region, choices = c("NCNE", "EMP", "MW"))
  stopifnot(class(years) == "numeric" | class(years) == "integer", years >= 2008)


  #---- Compile Plot-level metrics ----
  env <- if(exists("HTLNwetlands")){HTLNwetlands} else {.GlobalEnv}

  plots <- getPlots(plot_type = "VIBIplotID", survey_type = survey_type, hgm_class = hgm_class,
                    dom_veg1 = dom_veg1, plotID = plotID)

  tryCatch(tluSpp <- get("tluSpp", envir = env),
           error = function(e){stop("tlu_WetlndSpeciesList not found. Please run importData() first.")})

  #---- Compile Herb Metrics ----
  herbs1 <- getHerbs(years = years, survey_type = survey_type, hgm_class = hgm_class,
                     dom_veg1 = dom_veg1, plotID = plotID, nativity = 'all')

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
    mutate(rel_cov = MidPoint/tot_cov)

  # herbs_check <- herbs |> group_by(LocationID, FeatureID, EventID, SampleYear, ModuleNo) |>
  #   summarize(sum_rel = sum(rel_cov))
  # table(herbs_check$sum_rel, useNA = 'always') # all sum to 1

  # Using macros cases to determine the exact ranges of VIBI scores (ie whether <= or < vs >= or >)
  # Carex Community E, SH
  carex <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, DomVeg_Lev1, ModuleNo, genus, ScientificName) |>
    filter(DomVeg_Lev1 %in% c("emergent", "shrub")) |>
    filter(genus == "Carex") |> unique() |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, genus) |>
    summarize(Num_Carex = sum(!is.na(genus)), .groups = 'drop') |>
    mutate(Carex_Score = case_when(is.na(Num_Carex) ~ NA_real_,
                                   Num_Carex <= 1 ~ 0,
                                   between(Num_Carex, 2, 3) ~ 3,
                                   Num_Carex == 4 ~ 7,
                                   Num_Carex >= 5 ~ 10,
                                   TRUE ~ NA_real_))

  #cyperaceae Community Ecoastal only
  cyper <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear,
           DomVeg_Lev1, ModuleNo, FAMILY, ScientificName) |>
    filter(DomVeg_Lev1 == "emergent-coastal") |>
    filter(FAMILY == "Cyperaceae") |> unique() |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, FAMILY) |>
    summarize(Num_Cyper = sum(!is.na(FAMILY)), .groups = 'drop') |>
    mutate(Cyper_Score = case_when(is.na(Num_Cyper) ~ NA_real_,
                                   Num_Cyper <= 1 ~ 0,
                                   between(Num_Cyper, 2, 3) ~ 3,
                                   between(Num_Cyper, 4, 6) ~ 7,
                                   Num_Cyper >= 7 ~ 10,
                                   TRUE ~ NA_real_))

  # dicot Community E SH
  dicot <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1,
           OH_STATUS, TYPE, ScientificName) |>
    filter(DomVeg_Lev1 %in% c("emergent", "shrub")) |>
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

  # Shade community F
  shade <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1,
           OH_STATUS, SHADE, ScientificName) |>
    filter(DomVeg_Lev1 == "forest") |>
    filter(OH_STATUS == "native" & SHADE %in% c("shade", "partial")) |> unique() |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo) |>
    summarize(Num_Shade = sum(!is.na(ScientificName)), .groups = 'drop') |>
    mutate(Shade_Score = case_when(is.na(Num_Shade) ~ NA_real_,
                                   Num_Shade <= 7 ~ 0,
                                   between(Num_Shade, 8, 13) ~ 3,
                                   between(Num_Shade, 14, 20) ~ 7,
                                   Num_Shade >= 21 ~ 10,
                                   TRUE ~ NA_real_))

  # Shrub- region Community E, SH
  shrub_reg <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1,
           OH_STATUS, FORM, WETreg, ScientificName) |>
    filter(DomVeg_Lev1 %in% c("emergent", "shrub")) |>
    filter(OH_STATUS == "native" & FORM == "shrub" & WETreg %in% c("FACW", "OBL", "(FACW)")) |> unique() |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo) |>
    summarize(Num_Shrub_reg = sum(!is.na(ScientificName)), .groups= "drop") |>
    mutate(Shrub_Score_reg = case_when(is.na(Num_Shrub_reg) ~ NA_real_,
                                       Num_Shrub_reg <= 1 ~ 0,
                                       Num_Shrub_reg == 2 ~ 3,
                                       between(Num_Shrub_reg, 3, 4) ~ 7,
                                       Num_Shrub_reg >= 5 ~ 10,
                                       TRUE ~ NA_real_))
  # Shrub- statewide
  shrub <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1,
           OH_STATUS, FORM, WET, ScientificName) |>
    filter(DomVeg_Lev1 %in% c("emergent", "shrub")) |>
    filter(OH_STATUS == "native" & FORM == "shrub" & WET %in% c("FACW", "OBL", "(FACW)")) |> unique() |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo) |>
    summarize(Num_Shrub = sum(!is.na(ScientificName)), .groups= "drop") |>
    mutate(Shrub_Score = case_when(is.na(Num_Shrub) ~ NA_real_,
                                   Num_Shrub <= 1 ~ 0,
                                   Num_Shrub == 2 ~ 3,
                                   between(Num_Shrub, 3, 4) ~ 7,
                                   Num_Shrub >= 5 ~ 10,
                                   TRUE ~ NA_real_))

  # Hydrophyte- region # native FACW and OBL
  hydrop_reg <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1,
           OH_STATUS, WETreg, ScientificName) |>
    filter(DomVeg_Lev1 %in% c("emergent", "shrub")) |>
    filter(OH_STATUS == "native" & WETreg %in% c("FACW", "OBL", "(FACW)")) |> unique() |>
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

  # hydro - statewide
  hydrop <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1,
           OH_STATUS, WET, ScientificName) |>
    filter(DomVeg_Lev1 %in% c("emergent", "shrub")) |>
    filter(OH_STATUS == "native" & WET %in% c("FACW", "OBL", "(FACW)")) |> unique() |>
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

  # annual : perennial ratio - did not explicitly state native, so not including OH_STATUS in summary
  ap_ratio <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1,
           HABIT, ScientificName) |>
    filter(DomVeg_Lev1 %in% c("emergent")) |>
    filter(HABIT %in% c("AN", "PE")) |> unique() |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, HABIT) |>
    summarize(Num_Spp = sum(!is.na(ScientificName)), .groups = 'drop') |>
    pivot_wider(names_from = HABIT, values_from = Num_Spp, values_fill = 0) |>
    mutate(AP_Ratio = ifelse(PE > 0, AN/PE, 1),
           AP_Score = case_when(is.na(AP_Ratio) ~ NA_real_,
                                AP_Ratio > 0.48 ~ 0,
                                between(AP_Ratio, 0.32, 0.48) ~ 3,
                                between(AP_Ratio, 0.20, 0.30) ~ 7,
                                AP_Ratio <= 0.20 ~ 10,
                                TRUE ~ NA_real_
                                ))

  # Seedless Vascular Plant metric (# ferns and fern allies)
  svp <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1,
           FORM, ScientificName) |>
    filter(DomVeg_Lev1 %in% c("forest", "shrub")) |>
    filter(FORM %in% c("fern")) |> unique() |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, FORM) |>
    summarize(Num_SVP = sum(!is.na(ScientificName)), .groups= "drop") |>
    mutate(SVP_Score = case_when(is.na(Num_SVP) ~ NA_real_,
                                 Num_SVP == 0 ~ 0,
                                 Num_SVP == 1 ~ 3,
                                 Num_SVP == 2 ~ 7,
                                 Num_SVP >= 3 ~ 10,
                                 TRUE ~ NA_real_))

  # FQAI Equation 7, page 7 of
    # https://dam.assets.ohio.gov/image/upload/epa.ohio.gov/Portals/35/wetlands/Ohio_FQAI.pdf
  FQAI <- herbs |>
    filter(!is.na(COFC)) |> # drop species without Coefs
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1,
           ScientificName, COFC) |>
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
                                  TRUE ~ NA_real_
                                  ))
  # % Bryophyte using rel_cov
  pct_bryo <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1,
           ScientificName, FORM, rel_cov) |>
    filter(DomVeg_Lev1 %in% c("forest", "shrub")) |>
    filter(FORM == "bryo") |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo) |>
    summarize(pct_bryo = sum(rel_cov, na.rm = T), .groups = 'drop') |>
    mutate(PctBryo_Score = case_when(is.na(pct_bryo) ~ NA_real_,
                                     pct_bryo <= 0.01 ~ 0,
                                     pct_bryo > 0.01 & pct_bryo <= 0.03 ~ 3,
                                     pct_bryo > 0.03 & pct_bryo <= 0.06 ~ 7,
                                     pct_bryo > 0.06 ~ 10,
                                     TRUE ~ NA_real_))


  # % Hydrophyte using rel_cov: OH_STATUS = native, SHADE = shade or partial, WET/WETreg = FACW (FACW) OBL
  pct_hydro_reg <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1,
           ScientificName, OH_STATUS, SHADE, WETreg, rel_cov) |>
    filter(OH_STATUS == "native" & WETreg %in% c("FACW", "OBL", "(FACW)") &
             SHADE %in% c("partial", "shade")) |> #unique() |>
    filter(DomVeg_Lev1 %in% c("forest")) |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo) |>
    summarize(pct_hydro_reg = sum(rel_cov, na.rm = T), .groups = 'drop') |>
    mutate(PctHydro_Score = case_when(is.na(pct_hydro_reg) ~ NA_real_,
                                      pct_hydro_reg <= 0.1 ~ 0,
                                      pct_hydro_reg > 0.1 & pct_hydro_reg <= 0.15 ~ 3,
                                      pct_hydro_reg > 0.15 & pct_hydro_reg <= 0.28 ~ 7,
                                      pct_hydro_reg > 0.28 ~ 10,
                                      TRUE ~ NA_real_))

  pct_hydro <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1,
           ScientificName, OH_STATUS, SHADE, WET, rel_cov) |>
    filter(OH_STATUS == "native" & WET %in% c("FACW", "OBL", "(FACW)") & SHADE %in% c("partial", "shade")) |> #unique() |>
    filter(DomVeg_Lev1 %in% c("forest")) |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo) |>
    summarize(pct_hydro = sum(rel_cov, na.rm = T), .groups = 'drop') |>
    mutate(PctHydro_Score = case_when(is.na(pct_hydro) ~ NA_real_,
                                      pct_hydro <= 0.1 ~ 0,
                                      pct_hydro > 0.1 & pct_hydro <= 0.15 ~ 3,
                                      pct_hydro > 0.15 & pct_hydro <= 0.28 ~ 7,
                                      pct_hydro > 0.28 ~ 10,
                                      TRUE ~ NA_real_))

  # % sensitive - rel cover of COFC >=6, for DomVeg_Lev1 = shrub, buttonbush is not included as %sensitive
  pct_sens <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1,
           ScientificName, COFC, rel_cov) |>
    filter(COFC >= 6) |>
    filter(!(DomVeg_Lev1 == "shrub" & ScientificName %in% "Cephalanthus occidentalis")) |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1) |>
    summarize(pct_sens = sum(rel_cov, na.rm = T), .groups = 'drop') |>
    mutate(PctSens_Score = case_when(is.na(pct_sens) ~ NA_real_,
                                     DomVeg_Lev1 %in% c("emergent") & pct_sens <= 0.025 ~ 0,
                                     DomVeg_Lev1 %in% c("emergent") & pct_sens > 0.025 & pct_sens <= 0.10 ~ 3,
                                     DomVeg_Lev1 %in% c("emergent") & pct_sens > 0.10 & pct_sens <= 0.15 ~ 7,
                                     DomVeg_Lev1 %in% c("emergent") & pct_sens > 0.15 ~ 10,

                                     DomVeg_Lev1 %in% c("shrub") & pct_sens <= 0.02 ~ 0,
                                     DomVeg_Lev1 %in% c("shrub") & pct_sens > 0.02 & pct_sens <= 0.06 ~ 3,
                                     DomVeg_Lev1 %in% c("shrub") & pct_sens > 0.06 & pct_sens <= 0.13 ~ 7,
                                     DomVeg_Lev1 %in% c("shrub") & pct_sens > 0.13 ~ 10,

                                     DomVeg_Lev1 %in% c("forest") & pct_sens <= 0.035~ 0,
                                     DomVeg_Lev1 %in% c("forest") & pct_sens > 0.035 & pct_sens <= 0.12 ~ 3,
                                     DomVeg_Lev1 %in% c("forest") & pct_sens > 0.12 & pct_sens <= 0.30 ~ 7,
                                     DomVeg_Lev1 %in% c("forest") & pct_sens > 0.30 ~ 10,
                                     TRUE ~ NA_real_))

  # % tolerant
  pct_tol <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1,
           ScientificName, COFC, rel_cov) |>
    filter(COFC <= 2) |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1) |>
    summarize(pct_tol = sum(rel_cov, na.rm = T), .groups = 'drop') |>
    mutate(PctTol_Score = case_when(is.na(pct_tol) ~ NA_real_,
                                    DomVeg_Lev1 %in% c("emergent") & pct_tol >= 0.60 ~ 0,
                                    DomVeg_Lev1 %in% c("emergent") & pct_tol >= 0.40  & pct_tol < 0.60 ~ 3,
                                    DomVeg_Lev1 %in% c("emergent") & pct_tol >= 0.20 & pct_tol < 0.40 ~ 7,
                                    DomVeg_Lev1 %in% c("emergent") & pct_tol < 0.20 ~ 10,

                                    DomVeg_Lev1 %in% c("shrub") & pct_tol >= 0.45 ~ 0,
                                    DomVeg_Lev1 %in% c("shrub") & pct_tol >= 0.30  & pct_tol < 0.45 ~ 3,
                                    DomVeg_Lev1 %in% c("shrub") & pct_tol >= 0.15 & pct_tol < 0.30 ~ 7,
                                    DomVeg_Lev1 %in% c("shrub") & pct_tol < 0.15 ~ 10,

                                    DomVeg_Lev1 %in% c("forest") & pct_tol >= 0.15 ~ 0,
                                    DomVeg_Lev1 %in% c("forest") & pct_tol >= 0.10  & pct_tol < 0.15 ~ 3,
                                    DomVeg_Lev1 %in% c("forest") & pct_tol >= 0.05 & pct_tol < 0.10 ~ 7,
                                    DomVeg_Lev1 %in% c("forest") & pct_tol < 0.05 ~ 10,
                                    TRUE ~ NA_real_))

  # Invasive graminoids: Phalaris arundinaceae, Typha spp. Phragmites australis
  inv_grams <- c("Phalaris arundinacea", "Phragmites australis ssp. australis",
                 "Typha angustifolia", "Typha latifolia", "Typha x glauca",
                 "Typha minima", "Typha sp.")

  pct_invgram <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1, DomVeg_Lev2,
           ScientificName, rel_cov) |>
    filter(ScientificName %in% inv_grams) |> #unique() |>
    filter(DomVeg_Lev1 %in% c("emergent") |
             (DomVeg_Lev1 == "shrub" & DomVeg_Lev2 == "Bog Shrub Swamp")) |>
    # leatherleaf bog isn't in the CUVA data, but including it here because of pg 19 of
    # EPA manual states that this metric replaces the subcanopy IV metric for this community.
    # Based on the VIBI spreadsheet and the HTLN database, Bog Shrub Swamp seems to cover
    # that, although there's not a site like this in the data.
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo) |>
    summarize(pct_invgram = sum(rel_cov, na.rm = T), .groups = 'drop') |>
    mutate(PctInvGram_Score = case_when(is.na(pct_invgram) ~ NA_real_,
                                        pct_invgram >= 0.31 ~ 0,
                                        pct_invgram >= 0.15 & pct_invgram < 0.31 ~ 3,
                                        pct_invgram >= 0.03 & pct_invgram < 0.15 ~ 7,
                                        pct_invgram < 0.03 ~ 10,
                                        TRUE ~ NA_real_))

  #---- Compile Woody and Big Tree metrics ----
  # For woody metrics to work, need to drop big tree records from tbl_VIBI_Woody and
  # then row bind the Big Tree DBH records
  woody1 <- getWoody(years = years, survey_type = survey_type, hgm_class = hgm_class,
                     dom_veg1 = dom_veg1, plotID = plotID, nativity = 'all')

  woody1$Count[woody1$Count == -9999] <- NA_real_
  woody1 <- woody1 |> mutate(ba_cm2 = (pi*(DBH_MidPt/2)^2)*Count)
  # Set wet status based on the column chosen, like in the macros code
  woody1$WETreg <- ifelse(is.na(woody1[,region]), woody1$WET, woody1[,region])
  woody1$WETreg <- ifelse(is.na(woody1$WETreg), "ND", woody1$WETreg)

  woody2 <- woody1 |> filter(!DiamID %in% "BIG")

  bigt1 <- getBigTrees(years = years, survey_type = survey_type, hgm_class = hgm_class,
                       dom_veg1 = dom_veg1, plotID = plotID, nativity = 'all') |>
    mutate(BIG_ba_cm2 = pi*(DBH/2)^2)

  # Set wet status based on the column chosen, like in the macros code
  bigt1$WETreg <- ifelse(is.na(bigt1[,region]), bigt1$WET, bigt1[,region])
  bigt1$WETreg <- ifelse(is.na(bigt1$WETreg), "ND", bigt1$WETreg)

  bigt <- bigt1 |> group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear,
                            ModuleNo, DomVeg_Lev1, DomVeg_Lev2, OH_STATUS,
                            SHADE, FORM, WET, WETreg,
                            ScientificName) |>
    summarize(BIG_BA = sum(BIG_ba_cm2, na.rm = T),
              BIG_Count = sum(!is.na(DBH)),
              DiamID = "BIG",
              DiamVal = ">= 40",
              .groups = 'drop')

  woody_comb <- rbind(woody2 |> select(LocationID, FeatureID, EventID, SampleDate, SampleYear,
                                       ModuleNo, DomVeg_Lev1, DomVeg_Lev2, OH_STATUS, SHADE, FORM,
                                       WET, WETreg,
                                       DiamID, DiamVal, ScientificName, Count, ba_cm2),
                      bigt |> select(LocationID, FeatureID, EventID, SampleDate, SampleYear,
                                     ModuleNo, DomVeg_Lev1, DomVeg_Lev2, OH_STATUS, SHADE, FORM,
                                     WET, WETreg,
                                     DiamID, DiamVal, ScientificName, Count = BIG_Count,
                                     ba_cm2 = BIG_BA))

  # Calc rel. density
  woody_rc <- woody_comb |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1) |>
    summarize(tot_stems = sum(Count, na.rm = T),
              tot_ba_cm2 = sum(ba_cm2, na.rm = T),
              .groups = 'drop')

  woody <- left_join(woody_comb, woody_rc,
                     by = c("LocationID", "FeatureID", "EventID", "SampleDate",
                            "SampleYear", "ModuleNo", "DomVeg_Lev1")) |>
    filter(!is.na(Count)) # dropping FeatureID 305 from 2015 for Mod 3 Carya ovata C5 with -9999.
    # It's also a duplicate record, as there's a Carya ovata in the same size class with a count in Mod 3.

  # woody_check <- woody |> group_by(FeatureID, EventID, SampleYear, ModuleNo) |>
  #   summarize(rel_sum = sum(rel_stems))
  #
  # table(woody_check$rel_sum, useNA = 'always') # all sum to 1

  # Pole Timber rel dens
  #table(woody$DiamID, woody$DiamVal) C5-C7
  pole <- woody |> filter(DomVeg_Lev1 == "forest") |> filter(DiamID %in% c("C5", "C6", "C7")) |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo, DomVeg_Lev1) |>
    summarize(relden_smtree = sum(Count)/first(tot_stems), .groups = 'drop') |>
    mutate(SmTree_Score = case_when(is.na(relden_smtree) ~ NA_real_,
                                    relden_smtree >= 0.32 ~ 0,
                                    relden_smtree >= 0.22 & relden_smtree < 0.32 ~ 3,
                                    relden_smtree >= 0.11 & relden_smtree < 0.22 ~ 7,
                                    relden_smtree < 0.11 ~ 10,
                                    TRUE ~ NA_real_))

  # Canopy and Subcanopy IV
  # Had to rbind all woody <40cm to Big Trees records to get correct DBH and BA for IV
  IV1 <- woody |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, ModuleNo,
             DomVeg_Lev1, DomVeg_Lev2, ScientificName, OH_STATUS, FORM, WET, WETreg, SHADE, DiamID) |>
    summarize(Count = sum(Count),
              BA = sum(ba_cm2),
              tot_ba_cm2 = first(tot_ba_cm2),
              tot_stems = first(tot_stems),
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

  miss_cols <- setdiff(count_cols, c(names(IV1), names(ba_cols)))
  IV1[miss_cols] <- 0

  # Calc rel class freq
  IV1$rel_class_freq = rowSums(IV1[,count_cols] > 0, na.rm = T) / length(count_cols) # 12; in case # classes changes
  IV1$rel_ba = rowSums(IV1[,ba_cols], na.rm = T)/IV1$tot_ba_cm2
  IV1$rel_dens = rowSums(IV1[,count_cols], na.rm = T)/IV1$tot_stems
  IV1[,c("rel_class_freq", "rel_ba", "rel_dens")][is.na(IV1[,c("rel_class_freq", "rel_ba", "rel_dens")])] <- 0
  IV1$IV = (IV1$rel_class_freq + IV1$rel_ba + IV1$rel_dens)/3

  #---- Subcanopy IV ----
  # for F and SH
  # Manual states that subcan IV is the sum IV of
  # 1. native shade tolerant subcanopy species (FORM shrub and sm tree), plus
  # 2. IV of native FAC subcanopy (shrub and sm tree) species.
  # However, in the spreadsheet there's no calc of the 2nd, likely assuming
  # it's covered by the first, because it's all the same except wetness
  # For leatherleaf bogs, substitute invasive graminoid metric


  subcan_IV <- IV1 |>
    filter(DomVeg_Lev1 %in% c("forest", "shrub")) |>
    filter(!(DomVeg_Lev2 %in% "Bog Shrub Swamp")) |>
    filter(OH_STATUS == "native") |>
    filter(SHADE %in% c("partial", "shade")) |>
    filter(FORM %in% c("shrub", "sm tree")) |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear,
             ModuleNo, DomVeg_Lev1) |>
    summarize(subcanIV_num = sum(IV),
              subcanIV_den = sum(IV > 0), # sums a logical statement, so gives count
              subcanIV = subcanIV_num/subcanIV_den,
              .groups = 'drop') |>
    mutate(SubcanIV_Score = case_when(DomVeg_Lev1 == "forest" & subcanIV <= 0.02 ~ 0,
                                      DomVeg_Lev1 == "forest" & subcanIV > 0.02 & subcanIV <= 0.072 ~ 3,
                                      DomVeg_Lev1 == "forest" & subcanIV > 0.072 & subcanIV <= 0.13 ~ 7,
                                      DomVeg_Lev1 == "forest" & subcanIV > 0.13 ~ 10,

                                      DomVeg_Lev1 == "shrub" & subcanIV <= 0.02 ~ 0,
                                      DomVeg_Lev1 == "shrub" & subcanIV > 0.02 & subcanIV <= 0.05 ~ 3,
                                      DomVeg_Lev1 == "shrub" & subcanIV > 0.05 & subcanIV <= 0.10 ~ 7,
                                      DomVeg_Lev1 == "shrub" & subcanIV > 0.10 ~ 10,

                                      TRUE ~ NA_real_))

  canopy_IV <- IV1 |>
    filter(DomVeg_Lev1 %in% c("forest")) |>
    filter(OH_STATUS == "native") |>
    filter(FORM %in% c("tree")) |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear,
             ModuleNo, DomVeg_Lev1) |>
    summarize(canopyIV_num = sum(IV),
              canopyIV_den = sum(IV > 0), # sums a logical statement, so gives count
              canopyIV = canopyIV_num/canopyIV_den,
              .groups = 'drop') |>
    mutate(CanopyIV_Score = case_when(canopyIV >= 0.21 ~ 0,
                                      canopyIV >= 0.17 & canopyIV < 0.21 ~ 3,
                                      canopyIV >= 0.14 & canopyIV < 0.17 ~ 7,
                                      canopyIV < 0.14 ~ 10,
                                      TRUE ~ NA_real_))


  #---- Compile Biomass Metrics ----
  bmass <- getBiomass(years = years, survey_type = survey_type, hgm_class = hgm_class,
                      dom_veg1 = dom_veg1, plotID = plotID)



  }
