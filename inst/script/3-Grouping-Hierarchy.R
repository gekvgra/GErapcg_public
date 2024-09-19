# Grouping & Hierarchy
hierarched_T_1 <- GErapcg::pcg_hierarchy(
  GErapcg::pcg_grouping(
    data_input = ind_data[Jahr == Jahr_T_1 & MonateHorizont == 26],
    pcg_liste = pcg_li
  ),
  direction = "wide"
)

hierarched_T_2 <- GErapcg::pcg_hierarchy(
  GErapcg::pcg_grouping(
    data_input = ind_data[Jahr == Jahr_T_2 & MonateHorizont == 26],
    pcg_liste = pcg_li
  ),
  direction = "wide"
)

gc()
