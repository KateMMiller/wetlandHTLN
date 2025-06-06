% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getBigTrees.R
\name{getBigTrees}
\alias{getBigTrees}
\title{getBigTrees: get big tree data}
\usage{
getBigTrees(
  years = 2008:as.numeric(format(Sys.Date(), format = "\%Y")),
  survey_type = "all",
  hgm_class = "all",
  dom_veg1 = "all",
  plotID = "all",
  nativity = "all",
  intens_mods = 1:4
)
}
\arguments{
\item{years}{Numeric. Filter on years of survey. Default is all.}

\item{survey_type}{Filter on survey type of plots. Options are "all" (default), "reference",
"survey", "survey, womc", "womc", and "womc, reference". Can choose multiple options.}

\item{hgm_class}{Filter on HGM class. Options are "all" (default), "Depression",
"Impoundment", "Riverine", "Slope". Can choose multiple options.}

\item{dom_veg1}{Filter on level 1 dominant vegetation type. Options are "all" (default), "emergent",
"forest", "shrub". Can choose multiple options.}

\item{plotID}{Quoted string. Default is 'all'. If specified will return data for only plots specified.
Can choose multiple plots. Based on FeatureID in database.}

\item{nativity}{Quoted string. Filter on native status. Options are "all" (default), "adventive",
"cryptogeni", or "native". Can choose multiple statuses.}

\item{intens_mods}{Filter on total number of intensive modules. Ranges from 1 to 4. Can select multiple.
Default is 1:4 (all).}
}
\value{
Returns a data frame of bigtree count data
}
\description{
This function filters big tree data by plot, year, plot types, and nativity.
}
\examples{
\dontrun{
# run first
importData()

# return all big tree data
bigtree <- getBigTrees()

# return only 2023 data
bigtree23 <- getBigTrees(years = 2023)

# return 2020 and later
bigtree20 <- getBigTrees(years = 2020:2024)

# return only reference sites
ref <- getBigTrees(survey_type = c("reference", "womc, reference"))

# return only wetlands of management concern
womc <- getBigTrees(survey_type = c("womc", "womc, reference", "survey, womc"))

# return only depressional wetlands
depr <- getBigTrees(hgm_class = "Depression")

# return only forested vegetation types
forest <- getBigTrees(dom_veg1 = "forest")

# return non-forested vegetation types
nonfor <- getBigTrees(dom_veg1 = c("shrub", "emergent"))

# return native species only
nat <- getBigTrees(nativity = "native")

# return bigtree species for subset of plots
bigtree_plots <- getBigTrees(plotID = c("1007", "1017", "1034", "1036", "1043"))

}

}
