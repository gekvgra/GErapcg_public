# Preparations
## Prepare PCG-Liste

## Setting up PCG list for non-standard input files:
## - Add thresholds according to VORA-EDI
# pcg_li_raw <- as.data.table(
#   openxlsx::read.xlsx(pcg_li_path, sheet = "Arzneimittel"))[
#   ,
#   `:=`(
#     Schwellenwert_DDD = fifelse(
#       Anzahl.DDD.pro.Packung != "-" & !is.na(Anzahl.DDD.pro.Packung),
#       180,
#       NA_real_
#     ),
#     Schwellenwert_Packungen = fifelse(
#       Anzahl.DDD.pro.Packung == "-" | is.na(Anzahl.DDD.pro.Packung),
#       3,
#       NA_real_
#     ),
#     Anzahl.DDD.pro.Packung = as.numeric(
#       fifelse(
#         Anzahl.DDD.pro.Packung == "-" | is.na(Anzahl.DDD.pro.Packung),
#         0,
#         Anzahl.DDD.pro.Packung
#       )
#     )
#   )
# ]
#
## Check if Pharmacode column exist (newer pcg-list do not provide this column)
# if (!("Pharmacode" %in% names(pcg_li_raw))) {
#   pcg_li_raw[, Pharmacode := NA_real_]
#   setcolorder(pcg_li_raw, "Pharmacode", after = "GTIN")
# }
#
# pcg_list <- pcg_li_raw

# Replace with PCG list included in the package if NULL
if (is.null(pcg_list)) {
  pcg_list <- test_pcglist
}

pcg_li <- GErapcg::pcg_import(
  pcg_list,
  pcg_kurzname_col = 1,
  gtin_col = 4,
  pharmacode_col = 5,
  pharmacode_ddd_col = 7,
  schwellenwert_ddd_col = 8,
  schwellenwert_packungen_col = 9
  ) %>%
  as.data.table()
