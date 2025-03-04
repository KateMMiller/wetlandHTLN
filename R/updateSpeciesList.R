#' @title updateSpeciesList
#'
#' @importFrom dplyr full_join
#'
#' @description This function compares the tlu_WetlndSpeciesList in the HTLN database and the latest Ohio
#' EPA wetland species list with columns needed to rate the Ohio Vegetation IBI, and saves the table as
#' a csv to check against. This is mostly an internal function that may only be used whenever EPA updates
#' their list.
#'
#' @param html_path Quoted path, including file name, where the updated FQAI html table lives. Note that I tried
#' downloading the table directly from the epa.ohio.gov website, but the server did not allow it. You have
#' to download the latest html version of the FQAI list. The website housing the upated FQAI list is:
#'    https://epa.ohio.gov/divisions-and-offices/surface-water/reports-data/wetland-ecology.
#' The link to the html table of updated FQAI spreadsheet (2014) is:
#'    https://epa.ohio.gov/static/Portals/35/401/LU_Veg_Species.html.
#'
#' @param save Logical. If TRUE (default), saves to the data folder in the package. If FALSE, only returns
#' object to global environment if named (see example).
#'
#' @param out_path Quoted path you want to export joined species list to. Defaults to a data folder in your
#' working directory. Only needed if save = T.
#'
#' @examples
#' \dontrun{
#' # run first
#' importData()
#'
#'  # Write to file, but don't return output to global environment in R
#'  updateSpeciesList(html_path = "./data/fqai_update_2018_08_13.html", save = TRUE, out_path = "./data/")
#'
#'  # Return spp_check to global env, but don't save to file
#'  spp_check <- updateSpeciesList(html_path = "./data/fqai_update_2018_08_13.html", save = F)
#'
#' }
#'
#' @return Returns a data frame of VIBI calculations for each plot
#' @export
#'

updateSpeciesList <- function(html_path = "./data/fqai_update_2018_08_13.html", save = TRUE, out_path = "./data/"){

  #---- Bug Handling ----
  # check for XML package to be installed. If not installed, asks user to install in console.
  if(!requireNamespace("XML", quietly = TRUE)){
    stop("Package 'XML' needed for this function to work. Please install it.", call. = FALSE)
  }

  if(save == TRUE){
    if(is.na(out_path)){stop(paste0("Must specify a out_path to the database when save = TRUE"))
    } else if(!file.exists(out_path)){
      stop(paste0("Specified file path does not exist. ",
                  ifelse(grepl("sharepoint", out_path), " Note that file paths from Sharepoint or Teams are not accessible.",
                         "")))}
    if(!grepl("/$", out_path)){out_path <- paste0(out_path, "/")}} # add / to end of out_path if doesn't exist

  #---- Compile Data ----
  fqai_table <- data.frame(XML::readHTMLTable(html_path))
  colnames(fqai_table) <- sub("NULL.", "", names(fqai_table))

  env <- if(exists("HTLNwetlands")){HTLNwetlands} else {.GlobalEnv}

  tryCatch(
    tluSpp <- get("tluSpp", envir = env),
    error = function(e){stop("tlu_WetlndSpeciesList not found. Please run importData() first.")}
  )

  spp_check <- dplyr::full_join(tluSpp, fqai_table,
                        by = c("ScientificName" = "SCIENTIFIC_NAME"),
                        suffix = c("_db", "_epa"))

  alpha_cols <- sort(names(spp_check))
  alpha_cols <- alpha_cols[!grepl("ScientificName", alpha_cols)]
  spp_check <- spp_check[,c("ScientificName", alpha_cols)]

  if(save == TRUE){
  write.csv(spp_check, paste0(out_path, "FQAI_species_mismatches.csv"), row.names = F)
  }

  return(spp_check)
  }
