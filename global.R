packagesToInstall <- c("shiny", 
                       "shinydashboard",           # elaborate shiny dashboard features
                       "shinydashboardPlus", 
                       "shinyWidgets",             # widgets
                       "shinythemes" ,             # themes
                       "shinyBS", 
                       "shinyjs",                  # allow for using js
                       "dplyr",                # Manipulating data
                       "readxl",                   # read excel files
                       "lobstr",   
                       "data.table",               # showing and manipulating data.tables in shiny 
                       "stringr",                  # manipulating strings
                       "plotly",                   # interactive plots
                       "matrixStats",              # mathematical operations
                       "xts",                      # time series
                       "dygraphs",                 # interative graphs
                       "openxlsx",                 # reqd xlsx files
                       "YieldCurve",
                       "mailR",                # sending emails
                       "magrittr",                 # Using the pipe operator
                       "rapportools")              # Use of is.empty function

# install_success <- lapply(packagesToInstall, install.packages, character.only = TRUE)
success <- lapply(packagesToInstall, require, character.only = TRUE)

email <- envelope()
class(email)

email <- envelope(
  to = "calebmigosi@gmail.com",
  from = "calebmigosi@gmail.com",
  subject = "This is a plain text message!",
  text = "Hello!"
)

# Create email function
smtp <- server(host = "smtp.gmail.com",
               port = 465,
               username = "calebmigosi@gmail.com",
               password = "Iwillbegr8")

smtp(email, verbose = TRUE)

# Mailing List
mailing_list <- c("calebmigosi@gmail.com",
               "migosi@gmail.com",
               "cmigosi@gmail.com",
               "migos@gmail.com")