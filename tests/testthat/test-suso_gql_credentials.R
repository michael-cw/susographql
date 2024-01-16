test_that("default credentials check", {
  expect_equal(suso_gql_pwcheck(), 400)
})


test_that("correct credentials check", {
  testthat::skip_on_cran()

  ep = Sys.getenv("SUSO_API_EP")
  usr =  Sys.getenv("SUSO_API_USER")
  pass =  Sys.getenv("SUSO_API_PASSWORD")

  testthat::skip_if(suso_gql_pwcheck(ep, usr, pass, workspace = "primary")==400,
                    "No valid connection/credentials.")

  res<-suso_gql_pwcheck(
    endpoint = Sys.getenv("SUSO_API_EP"),
    user =  Sys.getenv("SUSO_API_USER"),
    password =  Sys.getenv("SUSO_API_PASSWORD"),
    workspace = "primary"
  )
  expect_equal(res, 200)
})
