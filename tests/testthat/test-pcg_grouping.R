
test_that(
  "Verify that pcg_grouping runs with test data and default parameters",
  {
    objects <- NULL
    expect_no_error(
      objects <- GErapcg::pcg_grouping(test_data, test_pcg_list, "show_all_steps")
    )

    skip_if(is.null(objects))

    object_names <- c("pcg_li_all", "pcg_gtin", "pcg_pharm", "ddd_groupings", "last_step")
    for (object_name in object_names) {
      # message("Running tests for intermediate object ", object_name)
      object <- objects[[object_name]]

      if (object_name != "pcg_pharm" || min(object$Jahr) <= 2022) {
        expect(nrow(object) > 0, "Empty data.table")
      }

      expect_s3_class(object, "data.table")

      empty_cols <- sapply(
        names(object),
        function(col_name) {
          all(is.na(object[,.(get(col_name))])) && (col_name != "Pharmacode")
        }, USE.NAMES = TRUE
      )

      expect(
        all(!empty_cols),
        paste(
          "Empty columns:",
          paste(names(empty_cols[empty_cols]), collapse = ","))
      )
    }

    testthat::expect(
      identical(as.data.frame(objects[["last_step"]]),
                as.data.frame(pcg_grouping_fixture)),
      "Results do not match reference object."
    )
  }
)

test_that(
  "Verify that pcg_grouping runs with objects from iterated GErapcg::pcg_import calls ...",
  {
    testthat::expect_no_error(
      GErapcg::pcg_grouping(test_data, GErapcg::pcg_import(test_pcglist))
    )
  }
)
