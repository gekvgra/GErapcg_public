
test_that(
  "Test that 'pcg_import' works with public BAG PCG list and default values.",
  {
    pcg_list <- NULL
    expect_no_error(pcg_list <- GErapcg::pcg_import(test_pcglist))

    testthat::skip_if(is.null(pcg_list))

    with(
      pcg_list,
      {
        # Correct types for BAG PCG list with default arguments
        testthat::expect_type(PCG_Kurzname, "character")
        testthat::expect_type(Gtin, "double")
        testthat::expect_type(Pharmacode, "double")
        testthat::expect_type(Pharmacode_DDD, "double")
        testthat::expect_type(Schwellenwert_DDD, "double")
        testthat::expect_type(Schwellenwert_Packungen, "double")
      }
    )

    # Content matches what it should be -> False, can have numbers. For example DM2
    with(
      pcg_list,
      {
        testthat::expect(
          all(grepl("[a-zA-Z]{1,}", PCG_Kurzname)),
          "Entries in PCG_Kurzname should contain at least one character."
        )

        testthat::expect(
          all((Gtin == -1 & PCG_Kurzname == "(DM2+hyp)") |
                is.na(Gtin) | grepl("^[0-9]{13}$", Gtin)),
          "Invalid values for Gtin. Check column order."
        )

        testthat::expect_false(all(is.na(Gtin)))

        testthat::expect(
          all((Pharmacode == -1 & PCG_Kurzname == "(DM2+hyp)") |
                is.na(Pharmacode) | grepl("^[0-9]{5,7}$", Pharmacode)),
          "Invalid values for Pharmacode. Check column order."
        )
      }
    )
  }
)
