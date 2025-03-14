#' @title sumVIBI: summarize plot-level VIBI metrics
#'
#' @importFrom dplyr filter group_by left_join mutate select summarize
#'
#' @description This function summarizes plot-level VIBI by averaging module-level VIBI scores, and
#' filters by plot, year, and plot types. Note that the Biomass metric assumes the area sampled is
#' always 0.01m2. Metric calculations and thresholds follow INTEGRATED WETLAND ASSESSMENT PROGRAM
#' Part 9: Field Manual for the Vegetation Index of Biotic Integrity for Wetlands v. 1.5.
#' Pages 17 - 20 and Table 2 were most useful. For plot-level VIBI scores that are comparable with
#' OH VIBI scores from other sites, use joinVIBI_plot(). Note however, that the joinVIBI_plot() only
#' rates sites with 4 intensive modules. This function allows sites with fewer than 4 modules to be
#' assessed on the same scale as sites with 4 modules. The VIBI scores with this function will be lower
#' than the original VIBI spreadsheet approach, because the VIBI spreadsheet sums across the 4 modules,
#' rather than averages.
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
#' "forest", "shrub". Can choose multiple options.
#'
#' @param plotID Quoted string. Default is 'all'. If specified will return data for only plots specified.
#' Can choose multiple plots. Based on FeatureID in database.
#'
#' @param region Quoted string. Default is "NCNE". Specifies the Army Corps Region for OH.
#' Options are "EMP", "MW", and "NCNE".
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
#' # calculate module-level VIBI for all sites sampled in 2023
#' vibi23 <- joinVIBI_module(years = 2023)
#'
#' # calculate module-level VIBI for all emergent wetlands
#' vibi_emerg <- joinVIBI_module(dom_veg1 = "emergent")
#'
#' # calculate module-level VIBI for depressional wetlands
#'
#' vibi_dep <- joinVIBI_module(hgm_class = "Depression")
#'
#'
#' }
#'
#' @return Returns a data frame of VIBI calculations for each module of each plot
#' @export
#'

sumVIBI <- function(years = 2008:as.numeric(format(Sys.Date(), format = "%Y")),
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

  # Compile the loc/visit table to left-join with final results
  plots_abbr <- plots[,c("LocationID", "FeatureTypes", "FeatureID", "Park", "County", "TotalMods",
                         "PlotConfig", "AreaHA",
                         "X1oPlants", "X2oVegID", "X1oHGM", "DomVegID", "HGM_ID", "HGMClass",
                         "DomVeg_Lev1", "DomVeg_Lev2", "DomVeg_Lev3")]

  vibi_mod <- joinVIBI(years = years, survey_type = survey_type, hgm_class = hgm_class,
                       dom_veg1 = dom_veg1, plotID = plotID, region = region)

  vibi_plot <- vibi_mod |> group_by(LocationID, FeatureTypes, FeatureID, Park, County, TotaMods, PlotConfig,
                                    AreaHA, X1oPlants, X2oVegIDm, X1oHGM, DomVegID, HGM_ID, HGMClass                                    )


  final_dat <- left_join(plots_abbr, vibi_comb2, by = c("LocationID", "FeatureID"))
  final_dat$VIBI_Score_ACOEReg <- rowSums(final_dat[,score_reg_cols], na.rm = T)
  final_dat$VIBI_Score_State <- rowSums(final_dat[,score_state_cols], na.rm = T)
  final_dat$VIBI_Score_FQ <- rowSums(final_dat[,vibi_fq_cols], na.rm = T)

  return(final_dat)
  }
