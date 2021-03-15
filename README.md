# serosurvr

### Environment Variables

If you are a SeroTracker developer, researcher, or partner, or CITF modelling team member, that needs to pull new data, add a .env file to the top level of the repository to store environment variables. This file should be formatted as follows:

```
RECORDS_URL=
FILTER_OPTIONS_URL=
```

Check the pinned message in the #data-modelling Slack channel or ask Rahul for the environment variables that you will need.

### renv

If you are modifying this library, please note that it relies on [renv](https://rstudio.github.io/renv/articles/renv.html) for package management. 

To set up renv for this repo, follow the following steps:
- Clone this git repo
- Ensure you have the renv package installed (in R, run `install.packages("renv")`)
- Start R in the `sysreview_biostats` working directory, either by opening `sysreview_biostats` as a RStudio project or launching R from your console. 
- Call `renv::restore()` to restore the project library

If you add a new package or dependency to this repo, make sure you log this change by calling `renv::snapshot()` and then committing changes to the `renv.lock` file. See [this page](https://rstudio.github.io/renv/articles/renv.html) for more information on the renv workflow.

The [collaborating with renv](https://rstudio.github.io/renv/articles/collaborating.html) page provides further information on how to use renv with Git. 
