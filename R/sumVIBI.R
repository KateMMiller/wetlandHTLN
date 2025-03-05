#' @title sumVIBI: summarize plot-level VIBI
#'
#' @importFrom dplyr between case_when filter group_by left_join mutate select summarize
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

  #---- Compile Herb metrics ----
  herbs <- getHerbs(years = years, survey_type = survey_type, hgm_class = hgm_class,
                    dom_veg1 = dom_veg1, plotID = plotID, nativity = 'all')

  # Need to account for differences in Statewide vs Region in Wetness Adj
  # The ACOE region uses the region wet score. If the region doesn't have a score,
  # it uses the WET column. Statewide is WET column
  # Dicot has differnt ratings for different habitats
  # Use X1oPlants

  # Set wet status based on the column chosen, like in the macros code
  herbs$WETreg <- ifelse(is.na(herbs[,region]), herbs$WET, herbs[,region])
  herbs$WETreg <- ifelse(is.na(herbs$WETreg), "ND", herbs$WETreg)

  # Extract genus from scientific name
  herbs$genus <- gsub("([A-Za-z]+).*", "\\1", herbs$ScientificName)

  # Carex Community E, SH
  carex <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, genus, ScientificName) |>
    filter(genus == "Carex") |> unique() |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, genus) |>
    summarize(Num_Carex = sum(!is.na(genus)), .groups = 'drop') |>
    mutate(Carex_Score = case_when(is.na(Num_Carex) ~ NA_real_,
                                   Num_Carex <= 1 ~ 0,
                                   between(Num_Carex, 2, 3) ~ 3,
                                   Num_Carex == 4 ~ 7,
                                   Num_Carex >= 5 ~ 10,
                                   TRUE ~ NA_real_))

  #cyperaceae Community Ecoastal
  cyper <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, FAMILY, ScientificName) |>
    filter(FAMILY == "Cyperaceae") |> unique() |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, FAMILY) |>
    summarize(Num_Cyper = sum(!is.na(FAMILY)), .groups = 'drop') |>
    mutate(Cyper_Score = case_when(is.na(Num_Cyper) ~ NA_real_,
                                   Num_Cyper <= 1 ~ 0,
                                   between(Num_Cyper, 2, 3) ~ 3,
                                   between(Num_Cyper, 4, 6) ~ 7,
                                   Num_Cyper >= 7 ~ 10,
                                   TRUE ~ NA_real_))

  # dicot Community E SH
  dicot <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, X1oPlants,
           OH_STATUS, TYPE, ScientificName) |>
    filter(OH_STATUS == "native" & TYPE == "DI") |> unique() |> # native dicots
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, X1oPlants) |>
    summarize(Num_Dicot = sum(!is.na(ScientificName)), .groups = 'drop') |>
    mutate(Dicot_Score = case_when(is.na(Num_Dicot) ~ NA_real_,
                                   X1oPlants == "PEM" & Num_Dicot <= 10 ~ 0,
                                   X1oPlants == "PEM" & between(Num_Dicot, 11, 17) ~ 3,
                                   X1oPlants == "PEM" & between(Num_Dicot, 18, 24) ~ 7,
                                   X1oPlants == "PEM" & Num_Dicot >= 25 ~ 10,

                                   X1oPlants == "PSS" & Num_Dicot <= 9 ~ 0,
                                   X1oPlants == "PSS" & between(Num_Dicot, 10, 14) ~ 3,
                                   X1oPlants == "PSS" & between(Num_Dicot, 15, 23) ~ 7,
                                   X1oPlants == "PSS" & Num_Dicot >= 24 ~ 10,
                                   TRUE ~ NA_real_))

  # Shade community F
  shade <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear,
           OH_STATUS, SHADE, ScientificName) |>
    filter(OH_STATUS == "native" & SHADE %in% c("shade", "partial")) |> unique() |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear) |>
    summarize(Num_Shade = sum(!is.na(ScientificName)), .groups = 'drop') |>
    mutate(Shade_Score = case_when(is.na(Num_Shade) ~ NA_real_,
                                   Num_Shade <= 7 ~ 0,
                                   between(Num_Shade, 8, 13) ~ 3,
                                   between(Num_Shade, 14, 20) ~ 7,
                                   Num_Shade >= 21 ~ 10,
                                   TRUE ~ NA_real_))

  # Shrub- region Community E, SH
  shrub_reg <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear,
           OH_STATUS, FORM, WETreg, ScientificName) |>
    filter(OH_STATUS == "native" & FORM == "shrub" & WETreg %in% c("FACW", "OBL", "(FACW)")) |> unique() |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear) |>
    summarize(Num_Shrub_reg = sum(!is.na(ScientificName)), .groups= "drop") |>
    mutate(Shrub_Score_reg = case_when(is.na(Num_Shrub_reg) ~ NA_real_,
                                       Num_Shrub_reg <= 1 ~ 0,
                                       Num_Shrub_reg == 2 ~ 3,
                                       between(Num_Shrub_reg, 3, 4) ~ 7,
                                       Num_Shrub_reg >= 5 ~ 10,
                                       TRUE ~ NA_real_))
  # Shrub- statewide
  shrub <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear,
           OH_STATUS, FORM, WET, ScientificName) |>
    filter(OH_STATUS == "native" & FORM == "shrub" & WET %in% c("FACW", "OBL", "(FACW)")) |> unique() |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear) |>
    summarize(Num_Shrub = sum(!is.na(ScientificName)), .groups= "drop") |>
    mutate(Shrub_Score = case_when(is.na(Num_Shrub) ~ NA_real_,
                                   Num_Shrub <= 1 ~ 0,
                                   Num_Shrub == 2 ~ 3,
                                   between(Num_Shrub, 3, 4) ~ 7,
                                   Num_Shrub >= 5 ~ 10,
                                   TRUE ~ NA_real_))

  # Hydrophyte- region # native FACW and OBL
  hydrop_reg <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear,
           OH_STATUS, WETreg, ScientificName) |>
    filter(OH_STATUS == "native" & WETreg %in% c("FACW", "OBL", "(FACW)")) |> unique() |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear) |>
    summarize(Num_Hydro_reg = sum(!is.na(ScientificName)), .groups = 'drop') |>
    mutate(Hydro_Score_reg = case_when(is.na(Num_Hydro_reg) ~ NA_real_,
                                   X1oPlants == "PEM" & Num_Hydro_reg <= 10 ~ 0,
                                   X1oPlants == "PEM" & between(Num_Hydro_reg, 11, 20) ~ 3,
                                   X1oPlants == "PEM" & between(Num_Hydro_reg, 21, 30) ~ 7,
                                   X1oPlants == "PEM" & Num_Hydro_reg >= 31 ~ 10,

                                   X1oPlants == "PSS" & Num_Hydro_reg <= 9 ~ 0,
                                   X1oPlants == "PSS" & between(Num_Hydro_reg, 10, 14) ~ 3,
                                   X1oPlants == "PSS" & between(Num_Hydro_reg, 15, 20) ~ 7,
                                   X1oPlants == "PSS" & Num_Hydro_reg >= 21 ~ 10,

                                   TRUE ~ NA_real_
                                   ))

  # hydro - statewide
  hydrop <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear,
           OH_STATUS, WET, ScientificName) |>
    filter(OH_STATUS == "native" & WET %in% c("FACW", "OBL", "(FACW)")) |> unique() |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear) |>
    summarize(Num_Hydro = sum(!is.na(ScientificName)), .groups = 'drop') |>
    mutate(Hydro_Score = case_when(is.na(Num_Hydro) ~ NA_real_,
                                       X1oPlants == "PEM" & Num_Hydro <= 10 ~ 0,
                                       X1oPlants == "PEM" & between(Num_Hydro, 11, 20) ~ 3,
                                       X1oPlants == "PEM" & between(Num_Hydro, 21, 30) ~ 7,
                                       X1oPlants == "PEM" & Num_Hydro >= 31 ~ 10,

                                       X1oPlants == "PSS" & Num_Hydro <= 9 ~ 0,
                                       X1oPlants == "PSS" & between(Num_Hydro, 10, 14) ~ 3,
                                       X1oPlants == "PSS" & between(Num_Hydro, 15, 20) ~ 7,
                                       X1oPlants == "PSS" & Num_Hydro >= 21 ~ 10,

                                       TRUE ~ NA_real_
    ))



  #---- Compile Woody metrics ----
  woody <- getWoody(years = years, survey_type = survey_type, hgm_class = hgm_class,
                    dom_veg1 = dom_veg1, plotID = plotID, nativity = 'all')

  bmass <- getBiomass(years = years, survey_type = survey_type, hgm_class = hgm_class,
                      dom_veg1 = dom_veg1, plotID = plotID)



  }
