# nyc-tip-estimator

Deployment example for a tip estomator app using NYC taxi data

## Build notes

1. Render `code/01_download_tlc_data.qmd`

This downloads the parquet files for 2024 into the `data` directory. Change the `year` parameter to download data from additional years. A rudimentary check is made to prevent duplicate downloads.

2. Render `code/02_load_data.qmd`

- creates a .duckdb file/database from the parquet files downloaded in step 1.
- writes an aggregated dataset to Posit Connect
    - default pin name `nyc-pin-workshop` can be set in the yaml params
    - Connect account is read from the `rsconnect` API. You should have already connected your Connect account before running this step.

3. Publish the `plumber` API `code/03_predict_tip/plumber.R`

- make a note of the content URL. [optionally create a custom URL]

4. Setup authentication for the plumber API

- Create an API key for your account with viewer privileges
- Create a .Renviron file in your project directory
- Add the line: NYC_TIPS_API_KEY=<your_api_key_here>
- Restart your R session

5. Setup and run `tip-estimator-shiny`

- set the `api_base_url` to the content URL from step 3.
- `Run App` to test
- Publish


