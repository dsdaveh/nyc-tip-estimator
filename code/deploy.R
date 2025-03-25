library(rsconnect)

# Deploy the API
rsconnect::deployAPI(
  api = ".",
  appTitle = "NYC Taxi Tip Predictor",
  appFiles = c(
    "03_predict.R",
    "manifest.json"
  ),
  lint = FALSE
) 