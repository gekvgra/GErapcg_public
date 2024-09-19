## ---- Setup

# Option presets
options(
  scipen = 999,
  max.print = 2000,
  digits = 10
)

# Functions
"%ni%" <- Negate("%in%")

# Set year
Jahr_T <- Year
Jahr_T_1 <- Jahr_T - 1
Jahr_T_2 <- Jahr_T - 2

# Use test_data if none provided, adjust test data to year, and
# remove rows with Monate == 0
if (is.null(ind_data)) {
  ind_data <- test_data
}

ind_data <- data.table::as.data.table(ind_data)[
  ,
  `:=`(
    Jahr = Jahr + (Jahr_T - max(test_data$Jahr)),
    Geburtsjahr = Geburtsjahr + (Jahr_T - max(test_data$Jahr))
  )
][Monate != 0]
