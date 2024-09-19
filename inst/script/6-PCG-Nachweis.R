# Remove data if clean all intermediate steps is TRUE
if (Clean_Steps == TRUE) {
  rm(D_1_2_ch, D_1_5, D_1_6)
}

# Create PCG-Nachweis output
pcg_nachweis <- pcg_grouped_T_1_month_T[
  ,
  .(Jahr, BagNr, AhvNr, Geburtsjahr, pcg)
]

# Remove data if clean all intermediate steps is TRUE
if (Clean_Steps == TRUE) {
  rm(pcg_grouped_T_1_month_T)
}
