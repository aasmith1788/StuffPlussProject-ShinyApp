library(rsconnect)
deployApp(
  appName = "MLBStuffPlus",
  appFiles = c("app.R", "EnhancedPrediction.csv"),
  appTitle = "MLB STUFF PLUS"
)

