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
  account = "miraisolutions",
  #appDir = "inst/report",
  #appPrimaryDoc = "inst/report/index.Rmd",
  appName = "covid19-vaccination-ch",
)

# rsconnect::deployDoc(
#   account = "miraisolutions",
#   #appDir = "inst/report",
#   doc = "inst/report/Index.Rmd",
#   appName = "covid19-vaccination-ch",
# )
