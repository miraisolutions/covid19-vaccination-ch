# covid19-vaccination-ch

The goal of `covid19-vaccination-ch` is to provide a dashboard article
analyzing the weekly vaccination report from [**BAG**](https://www.bag.admin.ch/bag/en/home.html) (BundesAmt für Gesundheit - Swiss Federal Office for Public Health) collecting the data from the past 4 weeks.

The **Vaccinated** population is split into 3 categories:

- **Fully Vaccinated with Booster**
- **Fully Vaccinated without Booster**
- **Partially Vaccinated**

The categories above are compared with the **Unvaccinated** population.

Hospitalized and Deaths rates within the 4 populations are compared to analyze who is more at risk. The following measures are shown in the
article:

- Hospitalization / Deaths counts
- Hospitalization / Deaths per 100'000 people

Entries with "Unknown" vaccination status are allocated to the 4 populations proportionally. Infection cases cannot be used for comparison because not provided by *BAG*.

The application fetches the data from *BAG* on a daily basis as part of a [Continuous Integration / Deployment workflow](#data-building-with-github-actions). Given that *BAG* updates the data daily also for the past weeks (delay in communication) the presented results are also updated. A new week is published by *BAG* on Monday.

## Package covid19vaccinationch

The `covid19-vaccination-ch` dashboard article is implemented through the supporting R package **covid19vaccinationch** that can be installed from GitHub with

``` r
remotes::install_github("miraisolutions/covid19-vaccination-ch")
```

and used to render the article locally from R via

``` r
covid19vaccinationch::run_report()

```
See help file for using the latest data from *BAG* source.

## R Markdown article

The dashboard article is defined as an R Markdown document `inst/report/index.Rmd` shipped with the **covid19vaccinationch** package, containing the article text and the R code that reads and processes the data.

The R Markdown document uses `runtime: shiny_prerendered` and contains both `ggplot2` / `plotly` graphs and `shiny` dynamic charts. The usage of `runtime: shiny_prerendered` allows speeding up the rendering of the article for the users, see the corresponding [documentation](https://rmarkdown.rstudio.com/authoring_shiny_prerendered.HTML).

## Deployment to *shinyapps.io*

The `covid19-vaccination-ch` R Markdown article is deployed to **shinyapps.io** and can be accessed at <https://mirai-solutions.ch/gallery/covid19-vaccination-ch>.

## Data building with GitHub Actions

It is likely that upon data structure changes from *BAG* the article might fail to render. For this reason, data are programmatically updated and checked as part of the **Continuous Integration / Deployment GitHub Actions workflow** `.github/workflows/workflow.yml`. The report will therefore show the latest working data until the package is update to be compatible with the new data structure.

Data are stored as RDS files inside the package under `inst/bag_data`, and used when rendering the article. A function of the package `build_data()` rebuilds the RDS files with the latest *BAG* data and overwrites them in `inst/bag_data`. This function is executed by the GitHub Action step _"Fetch and rebuild latest BAG data"_ every Tuesday at 16:00 UTC (since April 2022 BAG switched to weekly updates), and the updated data are tested (relying on `R CMD check`) and then committed and pushed to the `main` branch. Finally, the article with updated data is deployed to *shinyapps.io*, ensuring the application can be visualized with the daily updates.
