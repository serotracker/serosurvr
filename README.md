# serosurvr

## Using serosurvr

### Installation

```
install.packages("devtools")
devtools::install_github("serotracker/serosurvr")
```

### Environment Variables

If you are a SeroTracker developer, researcher, or partner, or CITF modelling team member that needs to pull new data, add a .env file to the top level of the repository to store environment variables. This file should be formatted as follows:

```
RECORDS_URL=
FILTER_OPTIONS_URL=
```

Check the pinned message in the #data-modelling Slack channel or ask Harriet for the environment variables that you will need.

### Setting file options

Set `import_new_data = TRUE` to import new data or `import_new_data = FALSE` to use cached data.

Set the path to cache folder to read from and write to for this project, e.g.
`path_to_cache_folder = ".cache/example_analysis"`

### Creating a data request

A basic data request has parameters `reqname` and `estimates_subgroup`. The `estimates_subgroup` parameter takes 3 values:
- `all_estimates`: return all estimates [DEFAULT]
- `primary_estimates`: return only primary estimates (one summary estimate per study identified by SeroTracker)
- `prioritize_estimates`: return only prioritized estimates according to `prioritize_estimates_mode`

If using `prioritize_estimates`, `prioritize_estimates_mode` takes 3 values related to test adjustment:
- `dashboard`: Prioritizes study author's test adjusted ("dashboard primary") estimate.
- `analysis_static`: Prioritizes study author's test unadjusted ("academic primary") estimate.
- `analysis_dynamic`: Prioritizes SeroTracker's own test adjusted estimate. If we were unable to successfully adjust the seroprevalence estimate ourselves, returns the study author's test adjusted estimate. [DEFAULT]

The full estimate prioritization code is stored here: https://github.com/serotracker/iit-backend/blob/b5dfe5c8af42f652fded59303ab97d04847be16c/app/utils/estimate_prioritization/estimate_prioritization.py

Example:
```
fs_males_req <- serosurvr::datareq_params(reqname = "data_req",
                                          estimates_subgroup = 'prioritize_estimates',
                                          prioritize_estimates_mode = "dashboard")
```

### Requesting data

To request data, pipe the data request to the `get_data` function.

Example:
```
tbl <-
  data_req %>%
  serosurvr::get_data(import_new_data = import_new_data,
                      path_to_cache_folder = path_to_cache_folder)
```
                                          
## Improving serosurvr

### Pushing new versions of serosurvr

Before pushing a new version of serosurvr, please follow the following steps:
- Run `devtools::document()` to ensure that the package is fully documented
- Run `devtools::check()` to ensure that all checks pass and the updates are ready to push

### renv

If you are modifying this library, please note that it relies on [renv](https://rstudio.github.io/renv/articles/renv.html) for package management. 

To set up renv for this repo, follow the following steps:
- Clone this git repo
- Ensure you have the renv package installed (in R, run `install.packages("renv")`)
- Start R in the `sysreview_biostats` working directory, either by opening `sysreview_biostats` as a RStudio project or launching R from your console. 
- Call `renv::restore()` to restore the project library

If you add a new package or dependency to this repo, make sure you log this change by calling `renv::snapshot()` and then committing changes to the `renv.lock` file. See [this page](https://rstudio.github.io/renv/articles/renv.html) for more information on the renv workflow.

The [collaborating with renv](https://rstudio.github.io/renv/articles/collaborating.html) page provides further information on how to use renv with Git. 
