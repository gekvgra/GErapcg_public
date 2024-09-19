# Regression
# Die Daten von Versicherten mit Versichererwechsel mit ueberlappenden Deckungen (d.h. Summe der Monate in einem gegebenen Behandlungsjahr > 12 Monate) werden in den Berechnungen vollstaendig beruecksichtigt.

# ---- PCG-Regression ----
# Extrapolate y, join hierarched data from T-2, 26M
T_1_26_pi <- hierarched_T_2[
  T_1_26_reg[
    pi_T[
      , .(Kanton, pi_Niv_k)
    ],
    on = .(Kanton)
  ][
    , `:=`(
      Y_star = (1 + pi_Niv_k) * (Kosten - Kobe),
      y_star = (1 + pi_Niv_k) * (Kosten - Kobe) / Monate
    )
  ][
    , c("Kosten", "Kobe", "pi_Niv_k") := NULL
  ],
  on = .(AhvNr, Geburtsjahr)
]
setnafill(T_1_26_pi, fill = 0, cols = existing_pcg)

# y per Rg and PCG
Ry_rg_pcg <- T_1_26_pi[
  , .(
    y_rg_pcg = sum(Y_star) / sum(Monate),
    monate_rg_pcg = sum(Monate)
  ),
  by = c("Kanton", "Altersklasse", "Geschlecht", "Aufenthalt_Vj", existing_pcg),
  # gives all existing combinations of RG and PCG
][
  , # the coercion to numeric is necessary
  (existing_pcg) := lapply(.SD, as.numeric),
  .SDcols = existing_pcg
]

# Save Monate per RG and PCG
monate_rg_pcg <- Ry_rg_pcg$monate_rg_pcg

# Calculate residual of y
Ry <- Ry_rg_pcg[
  ,
  # a_point_kr == gewichteter Mittelwert der 1560 RG
  a_point_kr := sum(y_rg_pcg * monate_rg_pcg) / sum(monate_rg_pcg),
  by = .(Kanton, Altersklasse, Geschlecht, Aufenthalt_Vj)
][
  ,
  residual_y := y_rg_pcg - a_point_kr
][
  ,
  residual_y
]

# RP == PCG residual matrix
RP <- as.matrix(
  Ry_rg_pcg[
    , ..existing_pcg
  ]
) - as.matrix(
  Ry_rg_pcg[
    ,
    (existing_pcg) := lapply(
      .SD,
      function(x) sum(x * monate_rg_pcg) / sum(monate_rg_pcg)
    ),
    by = .(Kanton, Geschlecht, Aufenthalt_Vj, Altersklasse),
    .SDcols = existing_pcg
  ][
    , ..existing_pcg
  ]
)

# GLM Regression
reg_output <- glm(Ry ~ 0 + RP, weights = monate_rg_pcg)

# Save coefficients
pcg_coef <- broom::tidy(reg_output) %>%
  mutate(pcg = substr(term, start = 3, stop = nchar(term))) %>%
  select(pcg, pcg_ansatz = estimate) %>%
  filter(pcg_ansatz > 0 & !is.na(pcg_ansatz))

# Clear cache and remove unnecessary objects
if (Clean_Steps == TRUE) {
  rm(hierarched_T_2, RP, Ry_rg_pcg, T_1_26_pi, T_1_26_reg)
}
gc()

# ---- Risikogruppen Ansatz ----
# Monate from T
mo_T <- unique(T_14_auf[
  Monate > 0 & Altersklasse != "0-18",
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
])[
  ,
  .(Monate_T = sum(Monate)),
  by = .(Kanton, Altersklasse, Geschlecht, Aufenthalt_Vj)
][
  all_rg,
  on = .(Kanton, Altersklasse, Geschlecht, Aufenthalt_Vj = Aufenthalt)
][
  is.na(Monate_T),
  Monate_T := 0
]

# Gruppendurchschnitte pro Kt und RG
D_1_1 <- unique(T_1_26_auf[
  Monate > 0 & Altersklasse != "0-18",
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
])[
  pi_T[, .(Kanton, pi_Niv_k)],
  on = "Kanton"
][
  , Y := Kosten - Kobe
][
  , -c("Kosten", "Kobe")
][
  ,
  .(
    Sum_MoT_1_26 = sum(Monate),
    y_bar_kr_T_1_26 = sum(Y) / sum(Monate)
  ),
  by = .(Kanton, Geschlecht, Aufenthalt_Vj, Altersklasse)
]

# Erwartete Gruppendurchschnitte pro Kt und RG
D_1_2_kt <- D_1_1[
  pi_T[, .(Kanton, pi_Niv_k)],
  on = .(Kanton)
][
  ,
  y_bar_star_kr := y_bar_kr_T_1_26 * (1 + pi_Niv_k)
][
  all_rg,
  on = .(Kanton, Geschlecht, Aufenthalt_Vj = Aufenthalt, Altersklasse)
]

# Swiss mean for missing cantonal RG in T-1
D_1_2_ch <- D_1_2_kt[
  !is.na(y_bar_star_kr),
  .(y_bar_star_r = sum(y_bar_star_kr * Sum_MoT_1_26) / sum(Sum_MoT_1_26)),
  by = .(Geschlecht, Aufenthalt_Vj, Altersklasse)
][
  ch_rg,
  on = .(Geschlecht, Aufenthalt_Vj = Aufenthalt, Altersklasse)
][
  D_1_2_kt[, -c("pi_Niv_k", "y_bar_kr_T_1_26")],
  on = .(Geschlecht, Aufenthalt_Vj, Altersklasse)
][
  is.na(y_bar_star_kr),
  y_bar_star_kr := y_bar_star_r
]

# Erwartete Gesamtnettoleistungen pro Kt und RG
D_1_3 <- D_1_2_ch[
  ,
  -c("Sum_MoT_1_26", "y_bar_star_r")
][
  mo_T,
  on = .(Kanton, Altersklasse, Geschlecht, Aufenthalt_Vj)
][
  ,
  Y_star_kr := y_bar_star_kr * Monate_T
]

# Erwartete Nettoleistungen pro Monat und Kt
D_1_4 <- copy(D_1_3)[
  ,
  y_bar_star_k := sum(Y_star_kr) / sum(Monate_T),
  by = .(Kanton)
]

# Make hierarched_T_1 to long format
pcg_grouped_T_1_long <- data.table::melt(hierarched_T_1,
                                         id.vars = c("AhvNr", "Geburtsjahr"),
                                         measure.vars = names(hierarched_T_1)[
                                           names(hierarched_T_1) %ni% c("AhvNr", "Geburtsjahr")
                                         ],
                                         variable.factor = F,
                                         variable.name = "pcg"
)[
  value == 1 & pcg != "hyp" # hyp should not be in here anyway
][
  ,
  value := NULL
]

# Join months from T, 14M to pcg_grouped_T_1_long
pcg_grouped_T_1_month_T <- as.data.table(pcg_coef)[
  pcg_grouped_T_1_long[
    unique(
      T_14_auf[
        ,
        -c("Gtin", "Pharmacode", "Packungen", "LinieNr")
      ]
    ),
    on = .(AhvNr, Geburtsjahr)
  ],
  on = .(pcg)
][
  ,
  .(
    Jahr, BagNr, AhvNr, Geburtsjahr, Kanton, Altersklasse, Geschlecht,
    Aufenthalt_Vj, Monate, pcg, pcg_ansatz
  )
][
  is.na(pcg_ansatz),
  pcg_ansatz := 0
]

# RG Ansatz Korrektur
D_1_5 <- pcg_grouped_T_1_month_T[
  ,
  pcg_sum_bkr := sum(Monate * pcg_ansatz),
  by = .(BagNr, Kanton, Altersklasse, Geschlecht, Aufenthalt_Vj)
][
  ,
  .(pcg_sum_kr = sum(Monate * pcg_ansatz)),
  by = .(Kanton, Altersklasse, Geschlecht, Aufenthalt_Vj)
][
  D_1_4[, -c("Y_star_kr")],
  on = .(Kanton, Altersklasse, Geschlecht, Aufenthalt_Vj)
][
  ,
  pcg_sum_kr := fifelse(is.na(pcg_sum_kr), 0, pcg_sum_kr)
][
  ,
  sum_pcg_corr_kr := fifelse(Monate_T == 0, 0, (pcg_sum_kr / Monate_T))
][
  ,
  a_tilde_kr := y_bar_star_kr - y_bar_star_k - sum_pcg_corr_kr
]

# Clear cache and remove unnecessary objects
if (Clean_Steps == TRUE) {
  rm(pcg_grouped_T_1_long, hierarched_T_1, T_1_26_auf)
}
gc()

# ---- Junge Erwachsene ----
# Entlastung JE (junge Erwachsene), Belastung AE (alte Erwachsene)
D_1_6 <- copy(D_1_5[, -c("y_bar_star_kr", "y_bar_star_k", "sum_pcg_corr_kr")])[
  Altersklasse == "19-25",
  `:=`(
    je_contrib_half = (sum(Monate_T * a_tilde_kr) + sum(pcg_sum_kr)) / 2,
    je_monate = sum(Monate_T)
  ),
  by = .(Kanton)
][
  Altersklasse == "19-25",
  D_je_k := max(0, -je_contrib_half / je_monate),
  by = .(Kanton)
][
  ,
  je_contrib_half := min(je_contrib_half, na.rm = TRUE),
  by = .(Kanton)
][
  Altersklasse != "19-25",
  `:=`(
    ae_monate = sum(Monate_T)
  ),
  by = .(Kanton)
][
  Altersklasse != "19-25",
  D_e_k := min(0, je_contrib_half / ae_monate),
  by = .(Kanton)
][
  ,
  `:=`(
    a_kr = fifelse(Altersklasse == "19-25",
                   a_tilde_kr + D_je_k,
                   a_tilde_kr + D_e_k
    ),
    D_k = fifelse(Altersklasse == "19-25",
                  D_je_k,
                  D_e_k
    ),
    je_ae_Sum_MoT_1_26 = fifelse(Altersklasse == "19-25",
                                 je_monate,
                                 ae_monate
    )
  )
][
  ,
  `:=`(
    D_je_k = NULL,
    D_e_k = NULL,
    je_monate = NULL,
    ae_monate = NULL
  )
]

# Clear cache and remove unnecessary objects
if (Clean_Steps == TRUE) {
  rm()
}
gc()


# ---- Saldo ----
saldo <- unique(T_14_auf[
  ,
  !c("Gtin", "Pharmacode", "Packungen", "LinieNr")
])[
  , .(Monate_T = sum(Monate)),
  by = .(BagNr, Kanton, Altersklasse, Geschlecht, Aufenthalt_Vj)
][
  as.data.table(expand.grid(
    unique(all_rg$Kanton),
    unique(children_rg$Altersklasse),
    unique(all_rg$Geschlecht),
    unique(all_rg$Aufenthalt),
    unique(vers_T$BagNr),
    stringsAsFactors = F
  ) %>%
    rename(
      Kanton = Var1,
      Altersklasse = Var2,
      Geschlecht = Var3,
      Aufenthalt_Vj = Var4,
      BagNr = Var5
    )),
  on = .(BagNr, Kanton, Altersklasse, Geschlecht, Aufenthalt_Vj)
][
  D_1_6,
  on = .(Kanton, Altersklasse, Geschlecht, Aufenthalt_Vj),
  `:=`(
    a_tilde_kr = i.a_tilde_kr,
    D_k = i.D_k
  )
][
  ,
  a_bkr_sum := Monate_T * a_tilde_kr
][
  unique(pcg_grouped_T_1_month_T[
    ,
    .(BagNr, Kanton, Altersklasse, Geschlecht, Aufenthalt_Vj, pcg_sum_bkr)
  ]),
  on = .(BagNr, Kanton, Altersklasse, Geschlecht, Aufenthalt_Vj),
  pcg_sum_bkr := i.pcg_sum_bkr
][
  ,
  `:=`(
    saldo_ohne_D = a_bkr_sum + pcg_sum_bkr,
    D_total = Monate_T * D_k
  )
][
  ,
  saldo_mit_D := saldo_ohne_D + D_total
][
  order(BagNr, Kanton, Geschlecht, Aufenthalt_Vj, Altersklasse)
]
setnafill(saldo, fill = 0, cols = c(6:ncol(saldo)))

# Clear cache and remove unnecessary objects
if (Clean_Steps == TRUE) {
  rm(T_14_auf, all_rg, ch_rg, children_rg, D_1_1, D_1_2_kt, D_1_3,
     D_1_4, mo_T, reg_output, vers_T, Ry, existing_pcg, monate_rg_pcg)
}
gc()

