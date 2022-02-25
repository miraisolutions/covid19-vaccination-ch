This directory contains RDS data used by the report.

- Data are programmatically updated as part of the Continuous Integration / Deployment GitHub Actions workflows, by means of the `build_data()` function.
- The update happens only in the main branch.
- The RDS files should not to be generated and pushed locally.
- A local Pull will be required any time the data are updated in the repository.
