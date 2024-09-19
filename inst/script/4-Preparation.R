# ---- Data Preparation ----
## Handle NAs
setnafill(ind_data, fill = 0, cols = c("Kosten", "Kobe"))

# ---- Aufenthalt & Altersklassen ----
T_14_auf <- GErapcg::make_age_groups(
  GErapcg::aufenthalt_fun(
    dt_T = ind_data[Jahr == Jahr_T & MonateHorizont == 14],
    dt_T_1 = ind_data[Jahr == Jahr_T_1 & MonateHorizont == 26]
  ),
  style = "sora"
)
T_1_14_auf <- GErapcg::make_age_groups(
  GErapcg::aufenthalt_fun(
    dt_T = ind_data[Jahr == Jahr_T_1 & MonateHorizont == 14],
    dt_T_1 = ind_data[Jahr == Jahr_T_2 & MonateHorizont == 26]
  ),
  style = "sora"
)
T_1_26_auf <- GErapcg::make_age_groups(
  GErapcg::aufenthalt_fun(
    dt_T = ind_data[Jahr == Jahr_T_1 & MonateHorizont == 26],
    dt_T_1 = ind_data[Jahr == Jahr_T_2 & MonateHorizont == 26]
  ),
  style = "sora"
)
T_2_26_auf <- GErapcg::make_age_groups(
  ind_data[Jahr == Jahr_T_2 & MonateHorizont == 26],
  style = "sora"
)

# Clear cache and remove unnecessary objects
if (Clean_Steps == TRUE) {
  rm(ind_data)
}
gc()

# ---- Versichererwechsler ----
vw_T <- GErapcg::vw_fun(T_1_26_auf, T_14_auf)

# ---- Versicherer ----
vers_T <- unique(T_14_auf[
  ,
  .(BagNr)
])


# ---- Teuerung ----
pi_T <- GErapcg::teuerung_fun(dt_T_14 = T_14_auf, dt_T_1_14 = T_1_14_auf)

# Clear cache and remove unnecessary objects
if (Clean_Steps == TRUE) {
  rm(T_1_14_auf)
}
gc()

# ---- Regression Prep ----
# Remove Kids and Monate==0
T_1_26_reg <- unique(T_1_26_auf[
  Altersklasse != "0-18" & Monate != 0,
  .(
    BagNr,
    AhvNr,
    Geburtsjahr,
    Kanton,
    Altersklasse,
    Geschlecht,
    Aufenthalt_Vj,
    Monate,
    Kosten,
    Kobe
  )
])

# Get all existing PCG for regression
existing_pcg <- names(hierarched_T_2[, -c("AhvNr", "Geburtsjahr")])

# Create all RGs
all_rg <- GErapcg::rg_grid(children = FALSE, kantonal = TRUE)
children_rg <- GErapcg::rg_grid(children = TRUE, kantonal = TRUE)

# Create CH RGs
ch_rg <- GErapcg::rg_grid(children = FALSE, kantonal = FALSE)

# Clear cache and remove unnecessary objects
if (Clean_Steps == TRUE) {
  rm(T_2_26_auf)
}
gc()

