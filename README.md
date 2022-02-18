
<!-- README.md is generated from README.Rmd. Please edit that file -->

# covid19vaccinationch

The goal of `covid19vaccinationch` is to provide a dashboard article
analyzing the weekly Vaccination report from
[**BAG**](https://www.bag.admin.ch/bag/en/home.html) (BundesAmt für
Gesundheit - Swiss Federal Office for Public Health) collecting the data
from the past 4 weeks..

“Vaccinated” are split into 3 categories:  
- *Fully Vaccinated with Booster*  
- *Fully Vaccinated without Booster*  
- *Partially Vaccinated*.

The categories above are compared with the “Unvaccinated”.

Hospitalized and Deaths rates withing the 4 populations are compared to
derive who is more at risk. The following measures are shown in the
article:  
- Hospitalized / Deaths counts  
- Hospitalized / Deaths per 100'000 people

Entries with “Unknown” vaccination status are allocated to the 4
populations proportionally. Infection cases cannot be used for
comparison because not provided by BAG.

The application fetches the data from BAG any time it is run. Given that
BAG updates the data daily also for the past weeks (delay in
communication) the presented results are also updated. A new week is
published by BAG on Monday.

It is expected that upon data structure changes from BAG this
application may fail to initialize. An update will be then provided
within short time.

## Installing package covid19vaccinationch

The **covid19vaccinationch** Shiny app is [deployed](gke#readme) to **ShinyApps.io** and can be accessed at https://mirai-solutions.ch/gallery/covid19-vaccination-ch.   
The App is structured as an R package that can be installed from GitHub with
<!-- argument build_vignettes not available anymore (r-lib/remotes#353), build_opts = "" for a full installation including vignettes  -->
``` r
remotes::install_github("miraisolutions/covid19vaccinationch", build_opts = "")
```
and used to serve the app locally from R via
``` r
covid19vaccinationch::run_report(
                      rmdname = "Index.Rmd",
                      out.html = "Index.html"
                      )
browseURL("Index.html")
```

## ShinyApps.io deployment

The article is being deployed to **Shinyapps.io** as an **Rmarkdown**
Rmd document.

`Index.Rmd` file contains the article text and the R code that reads and processes the data. Instead of deploying to Shinyapps.io the `app.R` file, it is possible to deploy an Index.Rmd file that will generate `index.html` once rendered. The Rmd article is written with `runtime: shiny` and contains both `ggplot2` / `plotly` graphs and `shiny` dynamic charts.


