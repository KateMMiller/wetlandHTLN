library(wetlandHTLN)
library(dplyr)
library(purrr)
importData()


views <- names(HTLN_wetlands)
views

view_cols <-
rbind(
  data.frame(table_name = "bigtreesVIBI", column_name = names(HTLN_wetlands$bigtreesVIBI)),
  data.frame(table_name = "woodyVIBI", column_name = names(HTLN_wetlands$woodyVIBI)),
  data.frame(table_name = "locations", column_name = names(HTLN_wetlands$locations)),
  data.frame(table_name = "biomassVIBI", column_name = names(HTLN_wetlands$biomassVIBI)),
  data.frame(table_name = "tluSpp", column_name = names(HTLN_wetlands$tluSpp)),
  data.frame(table_name = "herbVIBI", column_name = names(HTLN_wetlands$herbVIBI))
) |> dplyr::arrange(table_name)

bigtrees <- HTLN_wetlands$bigtreesVIBI
names(bigtrees)

make_table_defs <- function(df, view_name, ord){
  df_defs <- map(seq_along(names(df)),
                 function(x){
                   df = data.frame(view = view_name,
                                   column = names(df)[x],
                                   class = as.character(typeof(df[,x])),
                                   order = ord)
                   return(df)
                 }) |> list_rbind()
  return(df_defs)
  }

vibi_table_defs <-
  rbind(
    make_table_defs(HTLN_wetlands$bigtreesVIBI, "bigtreesVIBI", ord = 5),
    make_table_defs(HTLN_wetlands$woodyVIBI, "woodyVIBI", ord = 4),
    make_table_defs(HTLN_wetlands$locations, "locations", ord = 1),
    make_table_defs(HTLN_wetlands$biomassVIBI, "biomassVIBI", ord = 2),
    make_table_defs(HTLN_wetlands$tluSpp, "tluSpp", ord = 6),
    make_table_defs(HTLN_wetlands$herbVIBI, "herbVIBI", ord = 3)
) |> arrange(order)

dates <- vibi_table_defs$column[grepl("Date", vibi_table_defs$column)]
vibi_table_defs$class[vibi_table_defs$column %in% dates] <- "Date"
vibi_table_defs$class[vibi_table_defs$class %in% c('integer', 'double')] <- "numeric"

vibi_table_defs <- vibi_table_defs[,c("order", "view", "column", "class")]

write.csv(vibi_table_defs, "./testing_scripts/VIBI_DP_table_defs.csv", row.names = F)
