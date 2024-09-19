
test_that(
  "Verify that pcg_hierarchy runs with test inputs ...",
  {
    long <- NULL
    expect_no_error(
      long <- GErapcg::pcg_hierarchy(grouped_pcg, direction = "long")
    )

    skip_if(is.null(long))
    expect_s3_class(long, "data.table")
    expect(nrow(long) > 0, "Empty data.table returned")
    expect(
      identical(as.data.frame(long), as.data.frame(pcg_hierarchy_fixture)),
      "Results do not match reference object."
    )
  }
)

# Opting not to add a fixture test here
test_that(
  "Verify that wide-format output is generated ...",
  {
    wide <- NULL
    expect_no_error(
      wide <- GErapcg::pcg_hierarchy(grouped_pcg, direction = "wide",
                                     display_all = TRUE, display_hyp = TRUE)
    )
    skip_if(is.null(wide))
    missing_pcg <- setdiff(all_pcg, names(wide[, -c("AhvNr", "Geburtsjahr")]))
    expect(length(missing_pcg) == 0,
           paste0("Missing pcg: ", paste0(missing_pcg, collapse = ",")))

    expect(nrow(wide) > 0, "Empty data.table returned")
  }
)
