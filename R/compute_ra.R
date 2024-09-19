#' Run Risk Adjustment calculations
#'
#' Runs all steps of the risk adjustment calculations and returns the results
#'   as a named list.
#' @param Year An integer specifying the year of the risk adjustment
#' @param ind_data \code{data.frame} or \code{data.table} object. Uses synthetic
#'   test data included in the package if NULL.
#' @param pcg_list Either a path to an .xlsx with the PCG list, or a \code{data.frame}/\code{data.table}
#'   Uses the public PCG list of year 2022 if NULL.
#' @param Clean_Steps Boolean. Whether to clean up intermediate results.
#' @param export_objects A character vector naming objects to export. By default, all objects are exported
#' @importFrom magrittr "%>%"
#' @export
#' @return A \code{list} of objects specified in export_objects.
#' @author Magnus Vieten, Yannick Schwarz, Thomas Fischer
#' @examples \dontrun{
#' res <- compute_ra(2022, Clean_Steps = TRUE)
#' }
compute_ra <- function(Year, ind_data = NULL, pcg_list = NULL, Clean_Steps = FALSE,
                       export_objects = "all") {

  ra_env <- environment()

  source(system.file("script/1-Setup.R", package = "GErapcg"), local = ra_env)
  source(system.file("script/2-Pcg-List.R", package = "GErapcg"), local = ra_env)
  source(system.file("script/3-Grouping-Hierarchy.R", package = "GErapcg"), local = ra_env)
  source(system.file("script/4-Preparation.R", package = "GErapcg"), local = ra_env)
  source(system.file("script/5-Berechnung.R", package = "GErapcg"), local = ra_env)
  source(system.file("script/6-PCG-Nachweis.R", package = "GErapcg"), local = ra_env)

  objects <- ls(envir = ra_env)[!(ls(envir = ra_env) %in% c("ra_env", "export_objects"))]

  if (!("all" %in% export_objects)) {
    objects <- base::intersect(export_objects, objects)
    if (length(objects) == 0) {
      stop("export_objects does not match any objects in environment")
    }
  }

  sapply(
    objects,
    function(x) {
      get(x, envir = ra_env)
    },
    USE.NAMES = TRUE,
    simplify = FALSE
  )
}

