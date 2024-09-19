#' Import PCG list
#'
#' Imports and modifies PCG list provided by BAG to be used in RA calculation.
#'
#' @param input \code{data.table} or \code{data.frame} with the PCG-list or
#' character vector with the path to an excel file.
#' @param export_path Character vector with a path where a excel file as
#' output should be generated.
#' @param pcg_kurzname_col,gtin_col,pharmacode_col,pharmacode_ddd_col,schwellenwert_ddd_col,schwellenwert_packungen_col
#' Integer vector with the column number of the PCG-Kurzname, GTIN, Pharmacode,
#' Anzahl DDD pro Packung, DDD-Schwellenwert, Packungen-Schwellenwert.
#' @importFrom magrittr "%>%"
#' @export
#' @return A \code{data.frame} with the modified PCG list to be used for upload in
#' SORA PCG and PB calculation.
#' @author Magnus Vieten, Yannick Schwarz
#' @examples \dontrun{
#' pcg_import(test_pcglist)
#' pcg_import("path/to/pcg_list.xlsx")
#' }
pcg_import <- function(
    input,
    export_path = NULL,
    pcg_kurzname_col = 1,
    gtin_col = 4,
    pharmacode_col = 5,
    pharmacode_ddd_col = 7,
    schwellenwert_ddd_col = 8,
    schwellenwert_packungen_col = 9) {

  # no visible binding NOTE in R CMD check
  Gtin <- Pharmacode <- PCG_Kurzname <- Pharmacode_DDD <- NULL
  Schwellenwert_DDD <- Schwellenwert_Packungen <- PCG_Nr <- NULL
  import <- NULL

  if (!is.data.frame(input) & !is.character(input)) {
    stop("Input is not of class data.frame or character")
  }

  if (is.character(input) && !file.exists(input)) {
    stop("File does not exist at the specified path: ", input)
  }

  if (is.character(input)) {
    sheetname <- "Arzneimittel"

    while (is.null(import)) {
      tryCatch(
        {
          import <- openxlsx::read.xlsx(input, sheet = sheetname)
        },
        error = function(cond) {
          if (grepl("Arzneimittel", cond$message)) {
            message("Sourced XLSX does not contain sheet Arzneimittel. ",
                    "Reading from first sheet.")
          } else {
            stop("An error occurred while reading the input: ", cond)
          }
        }
      )
      if (is.null(import)) {
        sheetname <- 1
      }
    }
  } else if (is.data.frame(input)) {
    import <- input
  }

  colnames(import) <- gsub("[- ]", "_", colnames(import))

  # Catch cases where required columns already exist. Note: need to handle
  # overridden column ids
  required_cols <- c("PCG_Kurzname", "Gtin", "Pharmacode", "Pharmacode_DDD",
                     "Schwellenwert_DDD", "Schwellenwert_Packungen")

  for (col in required_cols) {
    match <- grepl(paste0("^", col, "$"), colnames(import), ignore.case = TRUE)
    col_id_var <- paste0(tolower(col), "_col")

    if (any(match) && (which(match) != get(col_id_var) | is.null(col_id_var))) {
      assign(col_id_var, which(match))
    }
  }

  # Ensure that column indices are plausible. Will not be the case for the
  # public PCG list if default values are used.
  col_order <- c(pcg_kurzname_col, gtin_col, pharmacode_col,
                 pharmacode_ddd_col, schwellenwert_ddd_col,
                 schwellenwert_packungen_col)
  expected_ncols <- max(col_order)

  if (expected_ncols > length(colnames(import))) {
    message(
      "Expected at least ", expected_ncols, "columns, found ", length(colnames(import)),
      ". If input isn't a public PCG list, consider modifying the default column ids.")

    # If column GTIN exists, rename to Gtin.
    if (any(grepl("^(GTIN|gtin|Gtin)$", colnames(import)))) {
      colnames(import) <- gsub("^(GTIN|gtin|Gtin)$", "Gtin", colnames(import))
    } else {
      stop("Could not find column containing GTIN")
    }

    # Handle case where required column Pharmacode is not available.
    if (!("pharmacode" %in% tolower(colnames(import)))) {
      import <- import %>%
        dplyr::mutate(Pharmacode = NA) %>%
        dplyr::relocate(Pharmacode, .after = Gtin)
    }

    # Create threshold columns
    import <- import %>%
      dplyr::mutate(
        Schwellenwert_DDD = ifelse(PCG_Kurzname %in% c("KRE", "KRK"), NA, 180),
        Schwellenwert_Packungen = ifelse(PCG_Kurzname %in% c("KRE", "KRK"), 3, NA)
      )
  }

  # Check import object and select relevant columns
  tryCatch(
    {
      pcg_prep <- data.table::as.data.table(import)[, ..col_order] %>%
        `colnames<-`(required_cols) %>%
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
        )
    },
    error = function(cond) {
      stop("An error occurred while selecting the relevant columns: ", cond)
    }
  )

  # Anpassungen ----
  # Prepare Exklusiv-PCG
  exclusive_pcg <- data.frame(
    "(DM2+hyp)",
    as.numeric(-1),
    as.numeric(-1),
    as.numeric(-1),
    as.numeric(180),
    NA_real_
  )
  # Add names of PCG-Liste to Exklusiv-PCG
  names(exclusive_pcg) <- names(pcg_prep)

  # Bind them together
  pcg_li_binded <- rbind(pcg_prep, exclusive_pcg)

  # Add PCG-Nr column
  pcg_li <- dplyr::left_join(
    pcg_li_binded,
    data.frame(PCG_Kurzname = unique(pcg_li_binded$PCG_Kurzname)) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(PCG_Nr = as.integer(seq_along(PCG_Kurzname))),
    by = "PCG_Kurzname"
  ) %>%
    unique()

  if (!is.null(export_path)) {
    openxlsx::write.xlsx(pcg_li, file = export_path)
  }

  # Return ----
  return(pcg_li)
}
