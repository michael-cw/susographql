test_that("check success result", {
  testthat::skip_on_cran()

  ep = Sys.getenv("SUSO_API_EP")
  usr =  Sys.getenv("SUSO_API_USER")
  pass =  Sys.getenv("SUSO_API_PASSWORD")

  testthat::skip_if(suso_gql_pwcheck(ep, usr, pass)==400, "No valid connection/credentials.")

  res<-suso_gql_assignments(
    endpoint = ep,
    user =  usr,
    password =  pass,
    workspace = "primary"
  )
  # top level
  expect_equal(names(res), "assignments")
  # next one
  expect_equal(names(res$assignments), c("totalCount","filteredCount","nodes"))
})
