# deploy/deploy-shinyapps.R
# usethis::use_build_ignore("deploy")
rsconnect::setAccountInfo(
  Sys.getenv("SHINYAPPS_ACCOUNT"),
  Sys.getenv("SHINYAPPS_TOKEN"),
  Sys.getenv("SHINYAPPS_SECRET")
)

# TODO: We currently have copy the file for the deployment to work
file.copy("inst/report/index.Rmd", "index.Rmd", overwrite = TRUE)

options(rsconnect.packrat = TRUE)

rsconnect::deployApp(
  account = "miraisolutions",
  appName = "covid19-vaccination-ch-test",
  forceUpdate = TRUE
)

unlink("index.Rmd")

# rsconnect::deployApp(
#   appDir = "inst/report",
#   appPrimaryDoc = "inst/report/index.Rmd",
#   appName = "covid19-vaccination-ch",
# )
# rsconnect::deployDoc(
#   doc = "inst/report/index.Rmd",
#   appDir = ".",
#   appName = "covid19-vaccination-ch",
# )
# rsconnect::deployApp(
#   appDir = ".",
#   appFiles = list.files(),
#   appPrimaryDoc = "inst/report/index.Rmd",
#   appName = "covid19-vaccination-ch"
# )
