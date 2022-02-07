
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vacreportch

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of `vacreportch` is to provide a dashboard article analyzing
the weekly Vaccination report from **BAG** (BundesAmt für Gesundheit
Switzerland).

“Vaccinated” are split into 3 categories:  
- Fully Vaccinated with Booster - Fully Vaccinated without Booster -
Partially Vaccinated.

The categories above are compared with the “Unvaccinated”.

Hospitalized and Deaths rates withing the 4 populations are compared to
derive who is more at risk. The following measures are shown in the
article: - Hospitalized / Deaths counts - Hospitalized / Deaths per
100’000 people

Entries with “Unknown” vaccination status are allocated to the 4
populations proportionally. Infection cases cannot be used for
comparison because not provided by BAG.

# ShinyApps.io deployment

The article is being deployed to **Shinyapps.io** as an **Rmarkdown**
Rmd document.
