#' @title getWoody: get woody data
#'
#' @importFrom dplyr filter
#'
#' @description This function filters woody data by plot, year, plot types, species, and species groupings.
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
#' @param plot Quoted string. Default is 'all'. If specified will return data for only plots specified.
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
#' # return all woody plant data
#' woody <- getWoody()
#'
#' # return only 2023 data
#' woody23 <- getWoody(years = 2023)
#'
#' # return 2020 and later
#' woody20 <- getWoody(years = 2020:2024)
#'
#' # return only reference sites
#' ref <- getWoody(survey_type = c("reference", "womc, reference"))
#'
#' # return only wetlands of management concern
#' womc <- getWoody(survey_type = c("womc", "womc, reference", "survey, womc"))
#'
#' # return only depressional wetlands
#' depr <- getWoody(hgm_class = "Depression")
#'
#' return only forested vegetation types
#' forest <- getWoody(dom_veg1 = "Forest")
#'
#' # return non-forested vegetation types
#' nonfor <- getWoody(dom_veg1 = c("Shrub", "Emergent"))
#'
#' # return native species only
#' nat <- getWoody(nativity = "native")
#'
#' # return woody species for subset of plots
#' herb_plots <- getWoody(plot = c("1007", "1017", "1034", "1036", "1043"))
#'
#' }
#'
#' @return Returns a data frame of woody count data
#' @export

getWoody <- function(years = 2008:as.numeric(format(Sys.Date(), format = "%Y")),
                     survey_type = 'all', hgm_class = 'all', dom_veg1 = 'all',
                     plot = 'all', nativity = 'all'){

  #---- Bug handling ----
  survey_type <- match.arg(survey_type, several.ok = T,
                           choices = c("all", "reference", "survey", "survey, womc", "womc", "womc, reference"))
  hgm_class <- match.arg(hgm_class, choices = c("all", "Depression", "Impoundment", "Riverine", "Slope"),
                         several.ok = T)
  dom_veg1 <- match.arg(dom_veg1, choices = c("all", "Emergent", "Forest", "Shrub"), several.ok = T)
  nativity <- match.arg(nativity, choices = c("all", "adventive", "cryptogeni", "native"), several.ok = T)
  stopifnot(class(years) == "numeric" | class(years) == "integer", years >= 2008)

  #---- Compile data ----
  env <- if(exists("HTLNwetlands")){HTLNwetlands} else {.GlobalEnv}

  tryCatch(woody <- get("woodyVIBI", envir = env),
           error = function(e){stop("tbl_VIBI_Woody not found. Please run importData() first.")})

  woody1 <- woody |> filter(SampleYear %in% years)

  woody2 <- if(any(survey_type == 'all')){woody1
  } else {woody1 |> dplyr::filter(SurveyType %in% survey_type)}

  woody3 <- if(any(hgm_class == 'all')){woody2
  } else {woody2 |> dplyr::filter(HGMClass %in% hgm_class)}

  woody4 <- if(any(dom_veg1 == 'all')){woody3
  } else {woody3 |> dplyr::filter(DomVeg_Lev1 %in% dom_veg1)}

  woody5 <- if(any(plot == 'all')){woody4
  } else {woody4 |> dplyr::filter(FeatureID %in% plot)}

  woody6 <- if(any(nativity == 'all')){woody5
  } else {woody5 |> dplyr::filter(OH_STATUS %in% nativity)}

  return(woody6)
}

