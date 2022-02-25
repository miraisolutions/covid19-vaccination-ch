# deploy/deploy-shinyapps.R
# usethis::use_build_ignore("deploy")
rsconnect::setAccountInfo(
  Sys.getenv("SHINYAPPS_ACCOUNT"),
  Sys.getenv("SHINYAPPS_TOKEN"),
  Sys.getenv("SHINYAPPS_SECRET")
)

file.copy("inst/report/index.Rmd", "index.Rmd", overwrite = TRUE)
#
rsconnect::deployApp(
  # account = "miraisolutions",
  # appDir = "inst/report",
  # appPrimaryDoc = "inst/report/index.Rmd",
  appName = "covid19-vaccination-ch",
)

unlink("index.Rmd")

# rsconnect::deployDoc(
#   account = "miraisolutions",
#   doc = "inst/report/index.Rmd",
#   appDir = ".",
#   appName = "covid19-vaccination-ch",
# )
# rsconnect::deployApp(
#   account = "miraisolutions",
#   appDir = ".",
#   appFiles = list.files(),
#   appPrimaryDoc = "inst/report/index.Rmd",
#   appName = "covid19-vaccination-ch"
# )
