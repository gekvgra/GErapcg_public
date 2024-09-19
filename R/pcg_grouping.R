#' PCG Grouping
#'
#' Groups individual data into PCGs
#'
#' @param data_input A \code{data.table} or \code{data.frame} featuring RA
#' individual data according to naming and data type standards used in PB. For
#' renaming use function \code{GEPack::sora_naming}.
#' @param pcg_liste \code{data.table} or \code{data.frame} with the PCG-list or
#' character vector with the path to an excel file.
#' @param inter_returns Character vector with return objects from intermediate
#' steps of the function. Possible values are "data_input", "ddd_groupings",
#' "input_fun", "last_step", "pcg_gtin", "pcg_li", "pcg_li_all", "pcg_liste",
#' "pcg_pharm", "pcg_threshold", "show_all_steps".
#' @importFrom magrittr "%>%"
#' @importFrom data.table ":="
#' @export
#' @return A \code{data.table} containing
#'  \item{Jahr}{Data year}
#'  \item{MonateHorizont}{Horizon of data status, either \code{14}, \code{26}
#' or \code{NA}}
#'  \item{AhvNr}{Pseudo SSN}
#'  \item{Geburtsjahr}{Year of birthday - used with \code{AhvNr} to warrant
#' identification}
#'  \item{pcg}{Grouped PCG}
#' @author Magnus Vieten, Yannick Schwarz
#' @examples \dontrun{
#' pcg_grouping(
#'   data_input = some_RA_IndData_object,
#'   pcg_liste = "path/to/prepped/pcg_list.xlsx"
#' )
#' }
pcg_grouping <- function(data_input,
                         pcg_liste,
                         inter_returns = NULL) {
  # no visible binding NOTE in R CMD check
  PharmaCode <- GTIN <- PCG_Kurzname <- Pharmacode_DDD <- Gtin <- NULL
  Pharmacode <- DDD_pro_pack <- Schwellenwert_DDD <- NULL
  Schwellenwert_Packungen <- BagNr <- Jahr <- MonateHorizont <- NULL
  Monate <- Packungen <- AhvNr <- Geburtsjahr <- ddd_pack_sum <- pcg <- NULL
  ddd_pcg <- pack_sum <- NULL


  # Check data_input
  if (!data.table::is.data.table(data_input)) {
    stop("Input is not of class data.table.")
  }

  import <- GErapcg::pcg_import(pcg_liste)

  # Check import
  necessary_columns <- c(
    "Gtin", "PCG_Kurzname", "Pharmacode", "Pharmacode_DDD",
    "Schwellenwert_DDD", "Schwellenwert_Packungen"
  )

  missing_columns <- necessary_columns[!necessary_columns %in% colnames(import)]

  if (length(missing_columns) > 0) {
    stop(
      "Object pcg_liste in this function does not contain all necessary columns.
      Missing columns: ", paste(missing_columns, collapse = ", ")
    )
  }

  # Prepare PCG list
  pcg_li_all <- import %>%
    # Mutate columns to correct data types to avoid errors
    dplyr::mutate(
      PCG_Kurzname = as.character(PCG_Kurzname),
      Gtin = as.numeric(Gtin),
      Pharmacode = as.numeric(Pharmacode),
      Pharmacode_DDD = as.numeric(
        ifelse(
          is.na(Pharmacode_DDD), 0, Pharmacode_DDD
        )
      ),
      Schwellenwert_DDD = as.numeric(Schwellenwert_DDD),
      Schwellenwert_Packungen = as.numeric(Schwellenwert_Packungen)
    ) %>%
    dplyr::rename(
      DDD_pro_pack = Pharmacode_DDD
    ) %>%
    dplyr::mutate(
      DDD_pro_pack = trunc(DDD_pro_pack * 10^9) / 10^9,
      Schwellenwert_DDD = trunc(Schwellenwert_DDD * 10^2) / 10^2
      # See Confluence: Sora PCG > Spezifikationen > Berechnung Dokumentation
    ) %>%
    data.table::as.data.table()

  # PCG thresholds
  pcg_threshold <- unique(pcg_li_all[, .(
    PCG_Kurzname,
    Schwellenwert_DDD,
    Schwellenwert_Packungen
  )])

  # PCG list for joining
  pcg_li <- pcg_li_all[, .(PCG_Kurzname, Gtin, Pharmacode, DDD_pro_pack)]

  # Make data_input flexible for increased usability
  data_input[
    ,
    `:=`(
      BagNr = if (!"BagNr" %in% colnames(data_input)) NA else BagNr,
      Jahr = if (!"Jahr" %in% colnames(data_input)) NA else Jahr,
      MonateHorizont = if (
        !"MonateHorizont" %in% colnames(data_input)) {
        NA
      } else {
        MonateHorizont
      }
    )
  ]

  # Join PCG list to all Gtin which are not NA or == 0
  pcg_gtin <- data_input[
    Gtin != 0 & !is.na(Gtin) & Monate > 0
  ][pcg_li[, -"Pharmacode"],
    on = .(Gtin),
    nomatch = 0
  ][
    , .(Packungen = max(Packungen)),
    # Bei Kt-Wechsler und unterschiedlicher Packungsanzahl wird max() genommen
    by = .(
      BagNr,
      Jahr,
      MonateHorizont,
      AhvNr,
      Geburtsjahr,
      Gtin,
      PCG_Kurzname,
      DDD_pro_pack
    )
  ]

  # Join PCG list to all Pharmacodes which are not already
  # joined by the Gtin or the Pharmacode is NA or =0
  pcg_pharm <- data_input[
    Pharmacode != 0 & !is.na(Pharmacode) & Monate > 0
  ][!pcg_gtin,
    on = .(Jahr, MonateHorizont, BagNr, AhvNr, Geburtsjahr, Gtin)
  ][
    , -c("Gtin")
  ][pcg_li[, -"Gtin"],
    on = .(Pharmacode),
    nomatch = 0
  ][
    , .(Packungen = max(Packungen)),
    by = .(
      BagNr,
      Jahr,
      MonateHorizont,
      AhvNr,
      Geburtsjahr,
      Pharmacode,
      PCG_Kurzname,
      DDD_pro_pack
    )
  ]

  if (nrow(pcg_pharm) == 0) {
    warning("No Pharmacodes in data.")
  }

  # Bind Gtin and Pharmacode joins together
  # No inconsistencies! Every Gtin or Pharmacode is valid
  ddd_groupings <- rbind(pcg_gtin, pcg_pharm, fill = TRUE)[
    , ddd_pack_sum := Packungen * DDD_pro_pack
  ][
    , .(
      ddd_pcg = round(sum(ddd_pack_sum), digits = 2),
      # See Confluence: Sora PCG > Spezifikationen > Berechnung Dokumentation
      pack_sum = sum(Packungen)
    ),
    by = .(Jahr, MonateHorizont, AhvNr, Geburtsjahr, PCG_Kurzname)
  ][pcg_threshold,
    on = .(PCG_Kurzname),
    nomatch = 0
  ][ # Differentiate between min. DDD and Packungen
    , pcg := ifelse(!is.na(Schwellenwert_DDD),
      ifelse(ddd_pcg >= Schwellenwert_DDD, PCG_Kurzname, NA),
      ifelse(pack_sum >= Schwellenwert_Packungen, PCG_Kurzname, NA)
    )
  ]

  last_step <- ddd_groupings[
    !is.na(pcg),
    .(Jahr, MonateHorizont, AhvNr, Geburtsjahr, pcg)
  ]

  if (!is.null(inter_returns)) {
    if ("show_all_steps" %in% inter_returns) {
      inter_returns <- c("data_input", "ddd_groupings", "last_step", "pcg_gtin",
                         "pcg_li", "pcg_li_all", "pcg_liste", "pcg_pharm",
                         "pcg_threshold")
    }

    single_steps <- sapply(
      inter_returns,
      function(x) {
        if (exists(x) && !is.null(get(x))) {
          get(x)
        }
      },
      simplify = FALSE,
      USE.NAMES = TRUE
    )
    return(single_steps)
  }

  return(last_step)
}
