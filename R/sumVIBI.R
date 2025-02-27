#' @title sumVIBI: summarize plot-level VIBI
#'
#' @importFrom dplyr group_by summarize
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
                    plotID = 'all'){

  #---- Bug handling ----
  survey_type <- match.arg(survey_type, several.ok = T,
                           choices = c("all", "reference", "survey", "survey, womc", "womc", "womc, reference"))
  hgm_class <- match.arg(hgm_class, choices = c("all", "Depression", "Impoundment", "Riverine", "Slope"),
                         several.ok = T)
  dom_veg1 <- match.arg(dom_veg1, choices = c("all", "Emergent", "Forest", "Shrub"), several.ok = T)
  nativity <- match.arg(nativity, choices = c("all", "adventive", "cryptogeni", "native"), several.ok = T)
  stopifnot(class(years) == "numeric" | class(years) == "integer", years >= 2008)


  #---- Compile Plot-level metrics ----
  plots <- getPlots(plot_type = "VIBIplotID", survey_type = survey_type, hgm_class = hgm_class,
                    dom_veg1 = dom_veg1, plotID = plotID)

  #---- Compile Herb metrics ----
  herbs <- getHerbs(years = years, survey_type = survey_type, hgm_class = hgm_class,
                    dom_veg1 = dom_veg1, plotID = plotID, nativity = 'all')

  herbs$genus <- gsub("([A-Za-z]+).*", "\\1", herbs$ScientificName)

  carex <- herbs |> group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear) |>
    filter(genus == "Carex") |>
    summarize(Num_Carex = sum(!is.na(CovCode)))

  #+++++ ENDED HERE ++++++
  #cyperaceae

  dicot <- herbs |> group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear) |>
    filter(OH_STATUS == "native" & GROUP == "DI") # native dicots
    summarize(Num_Dicot = sum(!is.na(CovCode)))




  #---- Compile Woody metrics ----
  woody <- getWoody(years = years, survey_type = survey_type, hgm_class = hgm_class,
                    dom_veg1 = dom_veg1, plotID = plotID, nativity = 'all')

  bmass <- getBiomass(years = years, survey_type = survey_type, hgm_class = hgm_class,
                      dom_veg1 = dom_veg1, plotID = plotID)



  }
