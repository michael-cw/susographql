test_that("check success result", {
  testthat::skip_on_cran()

  ep = Sys.getenv("SUSO_API_EP")
  usr =  Sys.getenv("SUSO_API_USER")
  pass =  Sys.getenv("SUSO_API_PASSWORD")

  testthat::skip_if(suso_gql_pwcheck(ep, usr, pass, workspace = "primary")==400, "No valid connection/credentials.")

  questlist<-suso_gql_questionnaires(
    endpoint = Sys.getenv("SUSO_API_EP"),
    user =  Sys.getenv("SUSO_API_USER"),
    password =  Sys.getenv("SUSO_API_PASSWORD"),
    workspace = "primary"
  )$questionnaires$nodes

  res<-suso_gql_interviews(
    endpoint = Sys.getenv("SUSO_API_EP"),
    user =  Sys.getenv("SUSO_API_USER"),
    password =  Sys.getenv("SUSO_API_PASSWORD"),
    workspace = "primary",
    questionnaireId = questlist$questionnaireId[1],
    questionnaireVersion = eq(questlist$version[1]),
    sortby_responsibleName = "ASC"
  )
  # top level
  expect_equal(names(res), "interviews")
  # next one
  expect_equal(names(res$interviews), c("totalCount","filteredCount","nodes"))
})
