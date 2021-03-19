library(shiny)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(rhandsontable)
library(lubridate)
library(bizdays)
library(pdftools)
library(stringr)
library(stringi)
library(curl)
library(jsonlite)
library(openxlsx)
library(purrr)
library(dplyr)
library(tibble)
library(openxlsx)

source('../lib/parserFrGoldman.R')
source('../lib/parserFrLeonteq.R')
source('../lib/parserFrSocgen.R')
source('../lib/parserEnLeonteq.R')
source('../lib/parserEnBnp.R')
source('../lib/parserEnSocgen.R')
source('../lib/parserEnEfg.R')
source('../lib/parserEnBarclays.R')
source('../lib/parserEnRaiffeisen.R')
source('../lib/utils.R')
source('keywords.R')
source('parser.R')


cal <- create.calendar("business", weekdays = c("saturday","sunday"), financial = T)

nextBusinessDay <- function(theDate) {
  if(is.bizday(theDate, cal))
    theDate
  else
    offset(theDate, 1, cal)
}



today <- as.character(as.Date(Sys.time()))
startDate <- as.Date("2018-01-01")
lastDate <- nextBusinessDay(startDate + years(12))
dates <- seq.Date(startDate,lastDate,by="1 day")
disabledDays = dates[format(dates,"%w") %in% c(0,6)]

AUD_flag="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAADAAAAAgCAMAAABjCgsuAAAAllBMVEUAIH8BIH8CIYAJJ4MSL4gYNIsaNowvSZYyS5g0TZlHXqJNY6VTaKhWa6pXbKpYbKtkd7Fxgrd2h7qOnMaVosmWo8qXpMqmsdKnstKtt9W6wdvRDSTRDybRh5rSZHjSiJvTGjDURFjVKz/VqrvX2eneytbo4urp4urp6fHq6vLr7fTu7PLx8vf19vn3+Pr4+Pv7+/z///9M1mOaAAABEElEQVQ4y+WTWROCIBSFUSoz28u2m9Gepi38/z8X5BhIYPHUQ2eccRzuJ5d7DmgJm+MYIyYKQPkbj48bWCKTDltY7/aeALz9bg3bgxEYpCuAVTpwcsApvo1A8cdFnQP1hdjRKByeWM8k5kBM2JlOIZbXa75fU5AgjlgdBxgZxYHSwv0ub0iFGCAkKnrnc08CwCCppN1GdkBZn1vSKEiIODRRD/02qzwKUcKBJBJRMc2qwjjZgdesqqJRcqCYVVX4FAdyiSi84v2MisYBG/X7lkC3i36iZtOm2m3hyQS33O+J0eV2vYyqU1QOC8+o7i4rN65QJ+NA1nlf0frN1JhTOm/oVkx+z4bDqdVUvefzD3oALqcw5PUAVWwAAAAASUVORK5CYII="
CAD_flag="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAADAAAAAgCAMAAABjCgsuAAAAVFBMVEX/AAD/AwP/BQX/CQn/R0f/Skr/S0v/Tk7/cHD/cXH/hYX/lJT/q6v/r6//srL/t7f/xMT/zc3/zs7/z8//0ND/1dX/1tb/2Nj/29v/5ub/9/f////rrEQTAAAAbklEQVQ4y+3Ttw6AMAxF0QsJvRMI7f//kwExICFkjwje5uEMbnDJdhOe8oO3g7bVgSGKBxWwYOVgWasUsmpdhKAITAKJCQohqM+qFoIyPIqwlDbdGADTyKeUA+SKsXrXd86rNj3NytNw4/9AHwc7pig+ByeVQt0AAAAASUVORK5CYII="
CHF_flag="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAADAAAAAgAQMAAABuGmlfAAAABlBMVEXXKA////8DMMfZAAAAGElEQVQI12NgIBswHyCW+v8fN0W8KWQBAHkbER3fLCbmAAAAAElFTkSuQmCC"
CZK_flag="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAADAAAAAgBAMAAACm+uYvAAAAJ1BMVEURRX4hQXYlVYlSNV1TNFxhg6mVJDuwwdSxwtXHGCLXFBrs8PX///8pWPwdAAAAg0lEQVQoz3XNTRWDMBhE0eGnFmIBLFQCGnBQDZVQBUh4GiqholiQnoRkvlnOXTztPz+NnwD0+AagNQLtEdiMJJ+R5DMXmEyGPpOhz/xBSwRtpsD4DqDJVHDP1JCoVv3T4WF44eGJh4SHe6BAEyiw4SHhYT489IEMGx4SHlwA8AHAB4AT5FKgDEnRXtsAAAAASUVORK5CYII="
EUR_flag="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAADAAAAAgAgMAAAApuhOPAAAACVBMVEUAU6eApFP/9ABWIW2TAAAAQ0lEQVQY02NgoD5gdEDisE1AsEXYJojAOZmMDpkIDhjBlIERMbbAbQLZArcJhYOiDA9AsRTFOSgORfECiueIsoVkAACg9AqhFj835AAAAABJRU5ErkJggg=="
GBP_flag="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAADAAAAAgCAMAAABjCgsuAAAAyVBMVEUAIH8BIH8CIYAHJoIOLIYPLYYZNYsaNowfO44nQpI3UJo4UJtLYaRhdK9idbBmebJnerJoerN4iLt5ibuQncenstK7w9y8xN29xd3P1ebRDSTe4u7fMh3fNiHgOyfgPCjhQi/hRDHjTjzlW0rlW0vnalvna1vpdWfpdmjpe27qfG/qjYTqj4bqoJnq7PTropvrs7DrtLHrurfrurjr7fTsxcTsxcXu1NXx4ePy8/j06+309fn17e/49PX49Pb7+vv7+/z+/v7///8ZBiYZAAABQklEQVQ4y82U2U4CURBEW3ABRR2VdShkQFTcFUQYQIH6/4/ywbm5KwYeTKzHSarOTU9XC4D+nGkkhsg4jmNSfzmoc/XSAi5fpQ8gGZLVwi+G0jOnNwCuJhQOEg/iGPJl8qMD4GHBhqSc37oQ21BscPEIoDMiy3kp1KghpwHD8YyTHoDrKd9LIiJy5kEMw26FfGsDracV6/vZCzyINhw2+XUPoDvm8iKnx+JAlCF3vuS4C+Duk80jc/BSqBqQSBlqavhkZU8cRQoyIJXhZ/i9CWcn4suEZIaRGn5RQtqJUs77ABJl0MO39iak7EkBbW+It9R/NPz9lJztsH5cEiq8tRpDezX8LobizeULFd6P1+sdpesgZrxRIKcma+LtTocgTrxzNTxIiGrfJSfQi/cvn5W5yam0IOLFB6+3hshm514X/hucN8Af5X8PRgAAAABJRU5ErkJggg=="
HKD_flag="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAADAAAAAgCAMAAABjCgsuAAABaFBMVEXeKRDeKhHeKxLeLBPeLRTeLhXeLhbfLxffMxvfNR7gNR7gOCHgOSLgOyTgPCXhPijhQSviQy3iRjDiRjHiRzHjTTjjTTnjTjrjUD3kUTzkUj7kUz/kVkLlWEXlWkblW0jlXEnmYk/mYlDnaVjna1rnbFvobVzqd2frfW7rfm/rgHLrgXPshHXshnjtjH/tjoDukYXulIfulonvmY3vm4/vn5TxpJnyp5zyqJ7yrKPyrqXyr6Xzsajzta3ztq70sqn0ubD0u7P1vrb1v7f1wbr1w7z1xL32vbb2xsD2x8H2ycP2ysP2zcf3zMb3z8n30Mr4zMb40cz408741M/41dH41tH419L51ND52tX53dn619L64N364d774+D75OH75eL76uf85eL85uP85+T86OX86+j86+n88fD97ev97uz98O/98vH99PP+9PL+9fT+9vX++Pj++fn+/f3+/v7/+vn/+/v//v7///+ebWIhAAAAilEQVQYGe3BV0NBYQAG4DentKNoaKIkaWhZDVKUGaFonYaGaByV9+/3E8537lx4HrS1MoM3OQQNzPeMQovTz3elF2I6ABh45aMZYtZ0wBRlxsYCZxGnBFWv9XrapDwGHN+Vao3Heqi5YO7jcKvcV3zipZ/0QM0qQw+KcX3OPrpP5sctUNN1xDc5tTKxHN+etXVCgORKFLwjO4PXlVIPxAxsBjO3My8yNyCm8Mu7rwWePOcgxMpQ+C8z/JOtliFi+pxN3hjhafAAIqSl3T13N4D5xiQ06V9EW8v6B5p+Kt/cqbu/AAAAAElFTkSuQmCC"
JPY_flag="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAADAAAAAgBAMAAACm+uYvAAAAIVBMVEW8AC3ADDfGI0rGJk3UWnjbdY7pqrnqrr3++vv//f7////GoaNjAAAAaUlEQVQoz2NYhQMwDA6JFSmGwm5dWCSKGIBAHVNiKgMYRGJIBEAkWNElljNAQRWaRBNMQgNNIgEmwYYm4QCTYEGTMIBJMKNJCMAkGImVwGkUTstxOhenB3EGCc5AxB3sOCMKd9QOppQIAHJMlppj7t83AAAAAElFTkSuQmCC"
NOK_flag="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAADAAAAAgAgMAAAApuhOPAAAACVBMVEUAJWryJyL///88DZz4AAAAJUlEQVQY02MIDQ1dwBUKAQyDjLNq1aoFXKsgAJVDDsBt2iAOAwB573zZ1odDEgAAAABJRU5ErkJggg=="
RUB_flag="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAADAAAAAgAgMAAAApuhOPAAAACVBMVEUANKnXKA/////2gFzsAAAAFklEQVQY02NYhQQYBoQzmEEoEhgQDgBowXeJbih2qwAAAABJRU5ErkJggg=="
SEK_flag="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAADAAAAAgAQMAAABuGmlfAAAABlBMVEUAUpP+ywDfFTtxAAAAGElEQVQI12NgYD7AAAK0o/6DAXaK5rYDANc7LT3YjsDKAAAAAElFTkSuQmCC"
SGD_flag="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAADAAAAAgCAMAAABjCgsuAAAASFBMVEX0KkD0L0X0MEX0Mkf1QVT1TmD1T2H2Y3P3ZXX3bHv6qbL6rrf6r7f7tLz7tb380db81dn81tv94OP94+b96uz96u3+7/H///9kSRtrAAAAUklEQVQ4y+2QwRGAIAwEFxAUFVFE03+ntkC+yL5v5y6BXphSPWdN/pXNaAqS3AbwvlWosgIx56gSKEUxyYILwbUfvVvdW4/nWvgPomQIQ+hY+AA5OkdE+VBF4QAAAABJRU5ErkJggg=="
USD_flag="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAADAAAAAgBAMAAACm+uYvAAAAD1BMVEUAJWq0vtO1v9PBASv////7AfyZAAAAOUlEQVQoz2NgQALGSIABt4QSHDK4IAEGBkU4RJMg3Q4kS9CMQlhCqR3DxB8uOABuCWMcALfEiLUDADkOhlU5rxL6AAAAAElFTkSuQmCC"


currencies <- c("AUD","CAD","CHF","CZK","EUR", "GBP", "HKD", "JPY", "NOK", "RUB", "SEK", "SGD", "USD")
currencies_content <- c()
currencies_content <- c()
for(currency in currencies) {
  currencies_content <- c(currencies_content, sprintf("<span><img src='%s' style='display:inline-block;width:21px;height:14px;margin-right:5px'></img>%s</span>", get(paste0(currency, "_flag")), currency))
}
  
maturities <- c("Custom", "3 Months", "6 Months", "9 Months", "1 Year", "1 Year, 3 Months", "1 Year, 6 Months", 
                "2 Years", "2 Years, 6 Months", "3 Years", "3 Years, 6 Months", "4 Years", "4 Years, 6 Months", 
                "5 Years", "5 Years, 6 Months", "6 Years", "7 Years", "8 Years", "9 Years", "10 Years")


underlyingsChoices = c("KN FP", "CS FP", "EN FP", "LHN SW", "DG FP", "GOOGL UQ","AIR FP",
                       "MT NA Equity", "RNO FP Equity", "BNP FP Equity", "SOLEWN Index", "FP FP", "BP/ LN", "ENI IM", "AEX", "CAC",
                       "VINCI SA", "BP PLC", "ENI SPA", "MT NA", "WDI GY", "BNP FP", "GLE FP", "NDX",
                       "ABBV UN", "RIO LN", "SLB UN", "CFR SW", "LOGN SW", "LONN SW", "C UN", "GS UN", "WFC UN", "NFLX UQ", "SPOT UN", "DIS UN"
                       ,"AAPL UQ","MSFT UQ","NVDA UQ")

issuersList <- list(Leonteq = "LTQ", "Societe Generale" = "SG", EFG = "EFG", Raiffeisen = "RAIF",
                    Barclays = "BARC", "BNP Paribas" = "BNP", "Goldman Sachs" = "GOLDSW")



############################### UI ##################################
ui <- navbarPage(
  id= "body",
  title="Parsing App",
  selected = "Parsing",
  inverse=T,
  shinyjs::useShinyjs(),
  inlineCSS(list(
    .redClass = "border-style: solid;
    border-width: 1px;
    border-radius: 5px;
    border-color: rgba(200, 0, 0, .8);",
    .greenClass = "border-style: solid;
    border-width: 1px;
    border-radius: 5px;
    border-color: rgba(0, 80, 0, .8);")),
    tabPanel("Parsing",
    sidebarLayout(
      sidebarPanel(width = 4,
           tags$br(),
           tags$br(),
           
           fluidRow(column(9, 
                           fileInput("bulkTS",
                                        "Please select multiple termsheets (only PDF format):",
                                     accept = '.pdf',
                                        multiple = T)),
           
           # Hidden download button
           column(3, conditionalPanel("false", downloadButton('download')))),

           fluidRow(column(9, textInput("issuer","Issuer:"))),
           fluidRow(column(9, pickerInput("investmentCurrency", label = "Currency:", choices=currencies, choicesOpt = list(content = currencies_content)))),
           fluidRow(column(9, textInput("nominal", label = "Notional Amount"))),
           fluidRow(column(9, textInput("underlyingsAll", label = "Underlyings (Tickers)"))),
           fluidRow(column(9, textInput("strike", label = "Strike Level"))),
           fluidRow(column(9, textInput("initialFixingDate", label = "Initial Fixing Date:"))),
           fluidRow(column(9, textInput("issueDate", label = "Issue Date:"))),
           fluidRow(column(9, textInput("finalFixingDate", label = "Final Fixing Date:"))),
           fluidRow(column(9, textInput("redemptionDate", label = "Redemption Date:"))),
           fluidRow(column(9, textInput("coupon", label = "Coupon"))),
           fluidRow(column(9, textInput("isin", label = "ISIN"))),
           tags$div(id = "triggersZone", rHandsontableOutput("triggers")),
           tags$div(id = "condtionalZone", rHandsontableOutput("couponDates")),
      ),
      mainPanel(
        column(6, align="right", fileInput('file1', 'Select a termsheet')),
        uiOutput("display")
      )
    )
  )
)





############################# SERVER ################################
server <- function(session, input, output) {
 
  # INDIVIDUAL TERMSHEET PARSING
  ##################################################################
  observe({
    req(input$file1)
    
    inFile <- input$file1
    pathFile <- inFile$datapath
    
    # Copy File to www
    wwwFile <- pathFile%>%str_extract('[0-9]{1,10}.pdf')
    inputFolder <- pathFile%>%str_extract('.*(?=/[0-9]{1,10}.pdf)')
    
    # Add temporary path 
    addResourcePath('tmp',inputFolder)
    inputFile <- paste0('tmp/', wwwFile)
    
    if (is.null(inFile)){return(NULL)
    }else {
      output$display <- renderUI({
        tags$iframe(style="height:1000px; width:100%", src=inputFile)
      })

      # Read Termsheet
      results = TSParser(pathFile)
      issuer_name <- results$issuer_name
      
      if (issuer_name == "") {
        showNotification("Issuer not recognized")
      } else {
        tryCatch(
          expr = {

            fillFields(results)
          },
          error = function(e){
            print("Error in the fields filling :")
            print(e)
          }
        )
      }
    }
  })
  
  # Function to fill in the respective fields
  fillFields <- function(results) {
    
    # Single Data
    #--------------------------------------------
    
    # Fill Issuer
    updateTextInput(session, "issuer", value = results$issuer)
    
    # Investment Currency
    if (is.na(results$investment_currency)) {
      showNotification("Error in the investment currency")
      shinyjs::removeClass(selector = "button.btn", class = "greenClass")
      shinyjs::addClass(selector = "button.btn", class = "redClass")
    } else {
      updatePickerInput(session, "investmentCurrency", selected=results$investment_currency)
      shinyjs::removeClass(selector = "button.btn", class = "redClass")
      shinyjs::addClass(selector = "button.btn", class = "greenClass")
    }
    
    # Notional Amount
    if (is.na(results$investment_nominal)) {
      showNotification("Error in the notional amount")
      updateTextInput(session, "nominal", value = "")
      shinyjs::removeClass(id = "nominal", "greenClass")
      shinyjs::addClass(id = "nominal", "redClass")
    } else {
      updateTextInput(session, "nominal", value=results$investment_nominal)
      shinyjs::removeClass(id = "nominal", "redClass")
      shinyjs::addClass(id = "nominal", "greenClass")
    }
    
    # underlyings and strike
    if (is.na(results$underlyings)) {
      showNotification("Error in the underlyings")
      shinyjs::runjs("$( '#underlyings-selectized' ).parent('div').removeClass( 'greenClass' );")
      shinyjs::runjs("$( '#underlyings-selectized' ).parent('div').addClass( 'redClass' );")
    } else {
      updateTextAreaInput(session, "underlyingsAll", value = results$underlyings)
      updateTextInput(session, "strike", value=results$strike)
      shinyjs::runjs("$( '#underlyings-selectized' ).parent('div').removeClass( 'redClass' );")
      shinyjs::runjs("$( '#underlyings-selectized' ).parent('div').addClass( 'greenClass' );")
    }
    
    # Initial Fixing Date
    if (is.na(results$initial_fixing_date)) {
      showNotification("Error in the initial fixing date")
      updateTextInput(session, "initialFixingDate", value = "")
      shinyjs::removeClass(id = "initialFixingDate", "greenClass")
      shinyjs::addClass(id = "initialFixingDate", "redClass")
    } else {
      updateTextInput(session, "initialFixingDate", value=format(results$initial_fixing_date, "%d/%m/%Y"))
      shinyjs::removeClass(id = "initialFixingDate", "redClass")
      shinyjs::addClass(id = "initialFixingDate", "greenClass")
    }
    
    # Issue Date
    if (is.na(results$issue_date)) {
      showNotification("Error in the issue date")
      updateTextInput(session, "issueDate", value = "")
      shinyjs::removeClass(id = "issueDate", "greenClass")
      shinyjs::addClass(id = "issueDate", "redClass")
    } else {
      updateTextInput(session, "issueDate", value=format(results$issue_date, "%d/%m/%Y"))
      shinyjs::removeClass(id = "issueDate", "redClass")
      shinyjs::addClass(id = "issueDate", "greenClass")
    }
    
    # Final Fixing Date
    if (is.na(results$final_fixing_date)) {
      showNotification("Error in the final fixing date")
      updateTextInput(session, "finalFixingDate", value = "")
      shinyjs::removeClass(id = "finalFixingDate", "greenClass")
      shinyjs::addClass(id = "finalFixingDate", "redClass")
    } else {
      updateTextInput(session, "finalFixingDate", value=format(results$final_fixing_date, "%d/%m/%Y"))
      shinyjs::removeClass(id = "finalFixingDate", "redClass")
      shinyjs::addClass(id = "finalFixingDate", "greenClass")
    }
    
    # Redemption Date
    if (is.na(results$redemption_date)) {
      showNotification("Error in the dates")
      updateTextInput(session, "redemptionDate", value = "")
      shinyjs::removeClass(id = "redemptionDate", "greenClass")
      shinyjs::addClass(id = "redemptionDate", "redClass")
    } else {
      updateTextInput(session, "redemptionDate", value=format(results$redemption_date, "%d/%m/%Y"))
      shinyjs::removeClass(id = "redemptionDate", "redClass")
      shinyjs::addClass(id = "redemptionDate", "greenClass")
    }
    
    # Coupon
    if (is.null(results$coupon_value)) {
      showNotification("Error in the notional amount")
      updateTextInput(session, "coupon", value = "")
      shinyjs::removeClass(id = "coupon", "greenClass")
      shinyjs::addClass(id = "coupon", "redClass")
    } else {
      updateTextInput(session, "coupon", value=results$coupon_value)
      shinyjs::removeClass(id = "coupon", "redClass")
      shinyjs::addClass(id = "coupon", "greenClass")
    }
    
    # ISIN
    if (is.na(results$ISIN)) {
      showNotification("Error in ISIN")
      updateTextAreaInput(session, "isin", value = "")
      shinyjs::removeClass(id = "isin", "greenClass")
      shinyjs::addClass(id = "isin", "redClass")
    } else {
      updateTextInput(session, "isin", value = results$ISIN)      
      shinyjs::removeClass(id = "isin", "redClass")
      shinyjs::addClass(id = "isin", "greenClass")
    }
    #-------------------------------------------- 

    ### AUTOCALL
    #--------------------------------------------
    # Process Triger Observation and Pyment Dates
    if (results$autocall && results$trigger_observation == "") {
      shinyjs::show(id = "triggersZone")
      showNotification("Error in triggers dates")
      df <- data.frame(observation=format(structure(rep(NA_real_, 0), class="Date")), 
                       payment=format(structure(rep(NA_real_, 0), class="Date")),
                       threshold=structure(rep(NA_real_, 0), class="numeric"))
      
      output$triggers <- renderRHandsontable(rhandsontable(df, height=350)%>% 
                                               hot_cols(colWidths = c(120, 120, 85)))
      
      # If full table with Dates and Triggers exists
    } else if (results$autocall) {
      
      shinyjs::show(id = "triggersZone")
      
      # Find the Trigger observation dates
      trig_obs <- results$trigger_observation%>%
        str_split(',')%>%    # Split String
        unlist()%>%          
        as.Date()%>%         # Format Dates
        format('%d/%m/%Y')
      
      # Find the Trigger Payment Dates
      trig_pmt <- results$trigger_payment%>%
        str_split(',')%>%
        unlist()%>%
        as.Date()%>%
        format('%d/%m/%Y')
      
      # Process triggers
      triggers <- results$autocall_trigger%>%
        str_split(',')%>%
        unlist()
      
      # Regroup Data in DT
      triggerTable <- data.frame("Autocall Obs" = trig_obs,
                                 "Autocall Pmt" = trig_pmt,
                                 "Trigger" = triggers)
      
      output$triggers <- renderRHandsontable(rhandsontable(triggerTable, height=350)%>% 
                                               hot_cols(colWidths = c(120, 120, 85)))
      
    } else {
      output$triggers <- renderRHandsontable(NULL)
      shinyjs::hide(id = "triggersZone")
    }
    #--------------------------------------------
    
    ### COUPONS
    #--------------------------------------------
    # Missing Trigger but dates exist
    if (is.null(results$coupon_trigger) && !is.null(results$coupon_observation)) {
      shinyjs::show("condtionalZone")
      showNotification("No coupon trigger for issue")
      
      # Find the Trigger observation dates
      cpn_obs <- results$coupon_observation%>%
        str_split(',')%>%    # Split String
        unlist()%>%          
        as.Date()%>%         # Format Dates
        format('%d/%m/%Y')
      
      # Find the Trigger Payment Dates
      cpn_pmt <- results$coupon_payment%>%
        str_split(',')%>%
        unlist()%>%
        as.Date()%>%
        format('%d/%m/%Y')
      
      # Regroup Data in DT
      coupTable <- data.frame("Coupon Obs" = cpn_obs,
                                 "Coupon Pmt" = cpn_pmt)
      
      output$couponDates <- renderRHandsontable(rhandsontable(coupTable, height=350)%>% 
                                                  hot_cols(colWidths = c(120, 120)))
      
      # If A coupon Trigger Exists create a full table
    } else if (!is.null(results$coupon_trigger)) {
      shinyjs::show("condtionalZone")
      
      # Find the Trigger observation dates
      cpn_obs <- results$coupon_observation%>%
        str_split(',')%>%    # Split String
        unlist()%>%          
        as.Date()%>%         # Format Dates
        format('%d/%m/%Y')
      
      # Find the Trigger Payment Dates
      cpn_pmt <- results$coupon_payment%>%
        str_split(',')%>%
        unlist()%>%
        as.Date()%>%
        format('%d/%m/%Y')
      
      # Process triggers
      triggers <- results$coupon_trigger%>%
        str_split(',')%>%
        unlist()
      
      # Regroup Data in DT
      coupTable <- data.frame("Coupon Obs" = cpn_obs,
                                 "Coupon Pmt" = cpn_pmt,
                                 "Trigger" = triggers)
      
      output$couponDates <- renderRHandsontable(rhandsontable(coupTable, height=350)%>% 
                                                  hot_cols(colWidths = c(120, 120, 85)))
                                                
    } else {
      shinyjs::hide("condtionalZone")
      output$couponDates <- renderRHandsontable(NULL)
    }
    #--------------------------------------------
  }
  
  ##################################################################
  # END OF INDIVIDUAL PARSING
  
  
  
  # MULTIPLE TERMSHEET PARSING
  ##################################################################
  # Observe for multiple TS input
  observe({
    req(input$bulkTS)
    
    # Follow progress
    progressSweetAlert(session = session,
                       'parser',
                       value = 0,
                       total = 100,
                       title = "Parsing term sheets:",
                       display_pct = TRUE,
                       striped = TRUE)
    
    
    filext <- tools::file_ext(input$bulkTS$datapath)
    
    # List of missing ISINs and unmapped ISINs
    unmappedTSISIN <- c()
    missingBDDISIN <- c()
    
    # Find the list of names of files
    TS_list <- input$bulkTS$datapath
    TS_names <- input$bulkTS$name%>%
                  str_extract('[A-Z]{2}[0-9]{10}')
    
    TS_infos <- vector("list", length = length(TS_list))

    
    # browser()
    # PARSING
    ################################################
    lapply(1:length(TS_list),
           function(i){  
             updateProgressBar(
               session = session,
               id = "parser",
               value = i*50/length(TS_list),
               total = 100,
               title = paste0("Parsing TS: ", i, " of ", length(TS_list)))
             
      TS_infos[[i]] <<- TSParser(TS_list[i])})
  
    # Find the index of unparsed Termsheets
    indexList <- lapply(1:length(TS_infos), function(i) length(TS_infos[[i]]) == 1)%>%
                    unlist()
    
    # To be used in the output file
    unmappedTSISIN <- TS_names[indexList]
    
    # Filter out for empty issuer field
    TS_infos <- Filter(function(a) {!is.na(a['issuer'])}, TS_infos)
    
    # browser()
    # Parsing completed
    updateProgressBar(
      session = session,
      id = "parser",
      value = 50,
      title = "Curling data from Database")
    
    
    
    
    # DATA CURLING FROM SERVER
    ################################################
    BDD_infos <- lapply(1:length(TS_list),
                        
                        function(i){
                          updateProgressBar(
                            session = session,
                            id = "parser",
                            value = 55 + (i*35/length(TS_list)),
                            total = 100,
                            title = paste0("Curling DB Data: ", i, " of ", length(TS_list)))
                          
                          print(i)
                          tryCatch({
                          BDDCurler(TS_infos[[i]]$ISIN)},
                          
                          error = function(e){
                            print(paste0("Missing BDD Data ")) 
                                         # TS_infos[[i]]$ISIN))
                            return(NA)})})
    
    # find the index of uncurled data
    indexList <- lapply(BDD_infos, function(x) all(is.na(x)))%>%
                    unlist()
    
    # To be used in the output file
    missingBDDISIN <- TS_names[indexList]
    
    # index of rows for the longest column
    sq <- seq(max(length(unmappedTSISIN), 
                  length(missingBDDISIN)))
  
    # Create a dataframe of missing data
    MissingData <- data.frame("Unkown Format" = unmappedTSISIN[sq],
                       "Missing From BDD" = missingBDDISIN[sq])
    
    # Remove NA values from BDD Infos
    BDD_infos <- Filter(function(a) any(!is.na(a)), BDD_infos)
    
    # Obtain all ISINs in BDD_infos
    BDD_ISINs <- lapply(1:length(BDD_infos),
                        
                        function(i) {
                          BDD_infos[[i]]$ISIN})%>%
                      unlist()
    
    # Filter TS_infos for ISINs in BDD_infos
    # Remove empty elements
    TS_infos <- Filter(function(a) {!is.character(a)}, TS_infos)
    
    # Filter out for ISINs existing in BDD
    TS_infos <- Filter(function(a) {any(a['ISIN'] %in% BDD_ISINs)}, TS_infos)
    
    # FORMATTING AND WRITING TO XLSX
    ################################################
    updateProgressBar(
      session = session,
      id = "parser",
      value = 90,
      title = "List to DataFrame")
    
    
    # Dataframe to be coverted to the xlsx output
    TS_DF <<- data.frame(data.frame(matrix(ncol = length(fields_list), 
                                          nrow = length(BDD_infos))))
    BDD_DF <<- data.frame(data.frame(matrix(ncol = length(fields_list), 
                                            nrow = length(BDD_infos))))
    colnames(BDD_DF) <<- fields_list
    colnames(TS_DF) <<- fields_list
    
    # Fill the dataframe to output in Excel
    lapply(1:length(BDD_infos), function(j){
        lapply(1:length(fields_list), function(i){
          try(TS_DF[j, i] <<- TS_infos[[j]][[fields_list[i]]])
          try(BDD_DF[j, i] <<- BDD_infos[[j]][[fields_list[i]]])
          })
      })
    
    # Assign the BDD coupon frequency to TS
    # Logic behind this is to check the annualized coupon rate
    TS_DF['frequency'] <- BDD_DF['frequency']
    TS_DF$coupon_value[is.na(TS_DF$coupon_value)] <- 0 
    TS_DF$frequency[is.na(TS_DF$frequency)] <- 'NA'
    TS_DF <- TS_DF%>%
              mutate(coupon_value =  frequencyMapper(frequency)*coupon_value)
    
    # Assign cpPa value to the coupno_value if it exists
    TS_DF[!is.na(TS_DF$cpPa), 'coupon_value'] <- TS_DF[!is.na(TS_DF$cpPa), 'cpPa']

    # Change date formats
    TS_DF$initial_fixing_date <- as.Date(TS_DF$initial_fixing_date%>%as.numeric(), 
                                    origin = '1970-01-01')%>%
                                  as.character()
    
    TS_DF$issue_date <- as.Date(TS_DF$issue_date%>%as.numeric(), 
                                         origin = '1970-01-01')%>%
                        as.character()
    
    TS_DF$final_fixing_date <- as.Date(TS_DF$final_fixing_date%>%as.numeric(), 
                                         origin = '1970-01-01')%>%
                                as.character()
    
    TS_DF$redemption_date <- as.Date(TS_DF$redemption_date%>%as.numeric(), 
                                       origin = '1970-01-01')%>%
                                as.character()
    

    # ERROR DETECTION
    ################################################
    # Change NAs to space
    TS_DF <- TS_DF%>%
                mutate_all(~replace(., is.na(.), ""))%>%
                select(-cpPa)
    
    BDD_DF <- BDD_DF%>%
                mutate_all(~replace(., is.na(.), ""))%>%
                select(-cpPa)
    
    # Easier for lisibility
    # Compares each value in the Parsed TS with DB
    comparisonFuntion <- function(i, j){
                            TS_DF[i,j] == BDD_DF[i,j]}
    
    # Declare Errors Table
    Errors <- data.frame(data.frame(matrix(ncol = 4, nrow = 1)))
    colnames(Errors) <- c('rowname', 'ISIN', 'Parsed', 'BDD')
    
    fields_list <- fields_list[!fields_list %in% 'cpPa']
    
    # Obtain errors and append them to the Errors dataframe
    lapply(1:nrow(TS_DF), 
           function(i){
             
             # Compare BDD and Parsed Data
             index <- sapply(1:length(fields_list),
                             function(j){
                               comparisonFuntion(i, j)}) 
             
             # Extract errors in long format
             error_Table <- TS_DF[i,'ISIN']%>%
                               rbind(TS_DF[i, index == FALSE])%>%
                               rbind(BDD_DF[i, index == FALSE])%>%
                               t()%>%
                               as.data.frame()%>%
                               tibble::rownames_to_column()
             
             # Rename column
             colnames(error_Table) <- c('rowname', 'ISIN', 'Parsed', 'BDD')
             
             # RBind to results container
             Errors <<- Errors%>%
                          rbind(error_Table)
             
             })
    
    # Filter out rows containing Issuer Name
    colnames(Errors) <- c('Field', 'ISIN', 'Parsed', 'Database')
    
    # Remove first row
    Errors <- Errors%>%
                na.omit()
    Errors <- Errors[Errors$Field != 'issuer',]

   
    # OBSERVATION & PAYMENT DATE TREATMENT
    ###############################################
    # Fields with concatenated values
    concatenated_values <- c('trigger_observation',
                           'trigger_payment',
                           'coupon_payment',
                           'coupon_observation',
                           'autocall_observation',
                           'coupon_trigger',
                           'autocall_trigger')
    
    # Filter out data with concatenated values
    concat_errors_index <- Errors$Field %in% concatenated_values
    concat_data <- Errors[concat_errors_index, ]
    
    # Separate strings by comma
    parsed_data <- concat_data$Parsed%>%strsplit(',')
    database_data <- concat_data$Database%>%strsplit(',')
    concat_fields <- concat_data$Field
    concat_ISIN <- concat_data$ISIN
    
    # browser()
    # Compare each of the fields of the concatenated data
    concat_comparison <- function(i){
      a = character(0)
      
      # Find if there are any values that are similar
        if(any(parsed_data[[i]] == database_data[[i]])){
          matched_indices <- parsed_data[[i]] == database_data[[i]]
          parsed_values <- parsed_data[[i]][!matched_indices]
          db_values <- database_data[[i]][!matched_indices]
          
          errored_data <- cbind(concat_fields[[i]],
                                      concat_ISIN[[i]],
                                      parsed_values,
                                      db_values)
          
        }else{ # Otherwise one is empty
          
          # Find the empty field
          a <- character(0)
          
          # Fill empty list with space
          if(identical(parsed_data[[i]], a)){
              parsed_data[[i]] <- ""
          }else if(identical(database_data[[i]], a)){
              database_data[[i]] <- ""
          }  
          
          errored_data <- cbind(concat_fields[[i]],
                                concat_ISIN[[i]],
                                parsed_data[[i]],
                                database_data[[i]])
        }
      
        errored_data <- errored_data%>%
                          as.data.frame()
        
        colnames(errored_data) <- c('Field', 'ISIN', 'Parsed', 'Database')

        return(errored_data)
        }
    
    # Replace concatenated values with individual cells in the Error table
    concatErrors <- lapply(1:nrow(concat_data), concat_comparison)
    Errors <- Errors[!concat_errors_index,] 
    Errors <- Errors%>%
                rbind(bind_rows(concatErrors))
    
    Errors <- Errors%>%
                arrange(ISIN, Field)
    
    # Split main dataframe into separate ones by ISIN
    ErrList <- split(Errors, Errors$ISIN)%>%
                lapply(select, -ISIN)
    

    # ERROR EXCEL FORMATTING
    ###############################################
    # Contains both the parsed and Database data  
    FileOutput <<- list("Missing Data" = MissingData,
                        "BDD Data" = BDD_DF,
                        "Parsed Data" = TS_DF,
                        "Errors" = Errors)%>%
                    append(ErrList)
    
    # Create Excel worksheet
    wb <<- createWorkbook()
    
    # Mapping function from list of dataframes to excel sheets
    list_to_sheet_mapper <- function(data, sheet_name){
                                addWorksheet(wb, sheet_name)
                                writeData(wb, sheet = sheet_name, data)
                              }
    
    # Map all elements in file output to wb
    sheet_list <- c("Missing Data","BDD Data", "Parsed Data", "Errors", names(ErrList))
    map2(FileOutput, sheet_list, list_to_sheet_mapper)
    

    # List of critical fields
    critical_fields <- c('underlyings', 
                         'strike',
                         'coupon',
                         'coupon_value',
                         'barrier',
                         'coupon_trigger',
                         'autocall_trigger',
                         'trigger_observation',
                         'coupon_observation',
                         'initial_fixing_date',
                         'final_fixing_date',
                         'product_type',
                         'investment_nominal',
                         'denomination',
                         'issue_date')
    
    # Create style on excel
    style <- createStyle(textDecoration = 'bold',
                          bgFill = 'orange')
    
    # Conditional Formatting function to apply to all sheets
    formatter <- function(sheet){
      lapply(1:length(critical_fields),
          function(i){
            conditionalFormatting(wb, 
                                  sheet = sheet,  
                                  cols = 1:4, 
                                  rows = 1:100000,
                                  rule = paste0('$A1="',critical_fields[i],'"'),
                                  style = style)})}
    
    formatSheets <- c('Errors', names(ErrList))
    
    lapply(formatSheets, formatter)
    
    closeSweetAlert(session = session)
    
    sendSweetAlert(session = session,
                   title = "Completed",
                   type = "success")
    # Click hidden button
    runjs("$('#download')[0].click();")

})

# Download Excel file
output$download <- downloadHandler(
  filename = function(){
    paste0("Parsed_TS-", 
           Sys.Date(), ".xlsx", sep="")},
  content = function(file){
    saveWorkbook(wb, file = file)
  })
##################################################################


}

# Run the app ----
shinyApp(ui = ui, server = server)
