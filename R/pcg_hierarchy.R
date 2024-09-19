#' PCG Hierarchisation
#'
#' Apply hierarchy to grouped PCG data. Application accords to
#' \href{https://www.fedlex.admin.ch/eli/cc/2019/618/de#art_2}{Art. 2 & 3 VORA-EDI}
#'
#' @param data_input A \code{data.table} of grouped PCG data with necessary
#' variables \code{AhvNr}, \code{Geburtsjahr}, \code{pcg}.
#' @param direction Character vector which determines the output dimensions.
#' \code{"wide"} returns a dummy matrix output, \code{"long"} returns a long
#' format output.
#' @param display_all Boolean, whether to return all variables, even if no
#'   corresponding PCG entries exist. Default: FALSE.
#' @param display_hyp Logical with default \code{FALSE}. If \code{TRUE}
#' non-independent PCG "hyp" is treated as independent (and also still combined
#' with DM2 to DM2hyp).
#' @importFrom data.table ":="
#' @export
#' @return A \code{data.table} containing
#'  \item{AhvNr}{Pseudo SSN}
#'  \item{Geburtsjahr}{Year of birthday - used with \code{AhvNr}
#'      to warrant identification}
#'  \item{pcg}{Grouped PCG}
#' @author Magnus Vieten, Yannick Schwarz
#' @examples \dontrun{
#' pcg_hierarchy(
#'   data_input = grouped_pcg_dt,
#'   pcg_liste = "path/to/prepped/pcg_list.xlsx",
#'   direction = "wide",
#'   display_all = TRUE,
#'   display_hyp = TRUE
#' )
#' }
#' @seealso \href{https://www.fedlex.admin.ch/eli/cc/2019/618/de#art_2}{Art. 2 & 3 VORA-EDI}
pcg_hierarchy <- function(data_input,
                          direction,
                          display_all = FALSE,
                          display_hyp = FALSE) {
  # no visible binding NOTE in R CMD check
  PCG_Kurzname <- hierarchy <- value <- pcg <- pcg_nr <- i.pcg <- NULL
  pcg_1_nam <- pcg_2_nam <- pcg_einstufung_nam <- exklusiv <- NULL

  # Get pcg numbers for translating from char to num
  pcg_kurz <- data.table::fread(
    system.file("extdata", "pcg.csv", package = "GErapcg")
  )
  hierarchy <- data.table::fread(
    system.file("extdata", "hierarchy.csv", package = "GErapcg")
  )[
    pcg_kurz,
    on = .(pcg_1 = pcg_nr),
    pcg_1_nam := i.pcg
  ][
    pcg_kurz,
    on = .(pcg_2 = pcg_nr),
    pcg_2_nam := i.pcg
  ][
    pcg_kurz,
    on = .(einstufung = pcg_nr),
    pcg_einstufung_nam := i.pcg
  ]

  # Check column names of input data
  required_cols <- c("AhvNr", "Geburtsjahr", "Jahr", "MonateHorizont", "pcg")
  missing_cols <- setdiff(required_cols, names(data_input))

  if (length(missing_cols) > 0) {
    stop(
      "Input data is missing the following columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  # Check if input data is grouped
  if (sum(!is.na(data_input$pcg)) == 0) {
    stop("Input data does not have any entries in the 'pcg' column.")
  }

  # Create missing PCG cols (when in grouped data are not all PCGs)
  data_copy <- unique(data.table::copy(data_input))
  pcg_matrix <- data.table::dcast(data_copy,
    AhvNr + Geburtsjahr ~ pcg,
    value.var = "pcg",
    fun = length
  )
  missing_matrix_cols <- setdiff(pcg_kurz[pcg != "hyp"][[2]], names(pcg_matrix))
  pcg_matrix[, (missing_matrix_cols) := as.integer(0)]

  # NOTE: shifted away from retired get()-interface. Results checked with identical()
  lapply(
    seq_along(hierarchy[exklusiv == 1][[1]]),
    function(rule) {
      pcg_matrix[
        pcg_1_nam == 1 & pcg_2_nam == 1,
        pcg_einstufung_nam := 1,
        env = list(
          pcg_1_nam = hierarchy[exklusiv == 1, pcg_1_nam][[rule]],
          pcg_2_nam = hierarchy[exklusiv == 1, pcg_2_nam][[rule]],
          pcg_einstufung_nam = hierarchy[exklusiv == 1, pcg_einstufung_nam][[rule]]
        )
      ][
        pcg_einstufung_nam == 1,
        pcg_1_nam := 0,
          env = list(
            pcg_einstufung_nam = hierarchy[exklusiv == 1, pcg_einstufung_nam][[rule]],
            pcg_1_nam = hierarchy[exklusiv == 1, pcg_1_nam][[rule]]
          )
      ]
    }
  )

  lapply(
    seq_along(hierarchy[exklusiv == 0][[1]]),
    function(rule) {
      pcg_matrix[
        pcg_1_nam == 1,
        pcg_2_nam := 0,
        env = list(
          pcg_1_nam = hierarchy[exklusiv == 0, pcg_1_nam][[rule]],
          pcg_2_nam = hierarchy[exklusiv == 0, pcg_2_nam][[rule]]
        )
      ]
    }
  )

  # Create long format
  pcg_long <- data.table::melt(
    pcg_matrix,
    id.vars = c("AhvNr", "Geburtsjahr"),
    measure.vars = pcg_kurz[[2]],
    variable.factor = FALSE,
    variable.name = "pcg"
  )[
    value == 1 & if (display_hyp == FALSE) {
      pcg != "hyp"
    } else {
      value == 1
    }
  ]

  # Check if value column exists in pcg_long
  if (!("value" %in% names(pcg_long))) {
    stop(
      "The 'value' column does not exist in the pcg_long data:
     with high probability there were no groupings in the input data
    or the hierarchy could not be applied correctly."
    )
  }

  pcg_long[, value := NULL]

  # Make output long or wide
  if (direction == "long") {
    return(pcg_long)
  }

  if (direction == "wide") {
    pcg_wide <- data.table::dcast(pcg_long,
      AhvNr + Geburtsjahr ~ pcg,
      value.var = "pcg",
      fun = function(x) {
        as.numeric(length(x))
      }
    )

    # PCG WAS did not previously show up.
    if (display_all) {
      pcg_wide <- pcg_wide[
        , setdiff(
          pcg_kurz[pcg != "hyp"][[2]],
          names(pcg_wide[, -c("AhvNr", "Geburtsjahr")])
        ) := as.integer(0)
      ]
    }

    data.table::setcolorder(pcg_wide, neworder = order(colnames(pcg_wide)))
    data.table::setcolorder(pcg_wide,
      neworder = c(
        "AhvNr",
        "Geburtsjahr"
      )
    )
    return(pcg_wide)
  }
}
