test_that(
  "Test that compute_ra example runs",
  {
    result <- NULL
    expect_no_error(
      result <- GErapcg::compute_ra(2022, Clean_Steps = TRUE)
    )

    skip_if(is.null(result))

    matching <- FALSE
    expect(
      matching <- identical(
        as.data.frame(result[["saldo"]]),
        as.data.frame(saldo)),
      failure_message = "Results do not match reference object."
    )

    skip_if(matching)
    expect_equal(
      apply(result$saldo[,Monate_T:saldo_mit_D], 2, mean),
      apply(saldo[,Monate_T:saldo_mit_D], 2, mean)
    )
  }
)
