# Print Function
print <- function(str){
  cat(file=stderr(), paste0("[", Sys.time(), "] ",str, "\n"))
}


"0. PRELIMINARIES"
####################################################
# Amethyst fields
fields_list <- c('issuer',
                 'valoren',
                 'listing',
                 'denomination',
                 'currency_hedge',
                 'ISIN',
                 'product_type',
                 'underlyings',
                 'investment_currency',
                 'investment_nominal',
                 'frequency',
                 'autocall',
                 'autocall_trigger',
                 'trigger_observation',
                 'trigger_payment',
                 'strike',
                 'trigger_override',
                 'coupon',
                 'coupon_type',
                 'coupon_memory',
                 'coupon_trigger',
                 'coupon_observation',
                 'coupon_payment',
                 'coupon_value',
                 'strike_level',
                 'barrier_type',
                 'barrier',
                 'put_strike',
                 'call_strike',
                 'bonus_level',
                 'capped',
                 'cap_level',
                 'capital_protection',
                 'issue_price',
                 'participation',
                 'upside_participation',
                 'downside_participation',
                 'gearing',
                 'initial_fixing_date',
                 'issue_date',
                 'final_fixing_date',
                 'redemption_date',
                 'first_observation',
                 'cpPa',
                 'soft_call')
##########################################################


"1. BDD CURLER (DATA EXTRACTION FROM DATABASE)"
##########################################################

# DATABASE FIELDS
###################################
BDDMapping <- c("id", "active","askDate","askPrice","asset","bloomAdd","bloomIssuer","boolAutocall","boolCoupon","boolDownside","boolUpside",
                "comonCode","currencyHedge","currency","deliveryType","denom","details","finalObsDate", 
                "internalRef","isin","issueAmount","issueDate","booker", "issuer","listing","listingPlace","marge","maturityDate", 
                "name","quoteSubType","quoteType","reutersCode","strikeDate","subType","telekunaAdd","template","tradeDate","type", "urlTermsheet",         
                "valorenCode","spUnderlyings","spAutocalls","spCoupons","spMatDownSides","spMatUpsides")


mappedFields <- c('active','denomination','investment_currency',
                  'investment_nominal','currency_hedge',
                  'comonCode','final_fixing_date','ISIN',
                  'listing','valoren',
                  'initial_fixing_date','issue_date','product_type',
                  'autocall','downside_participation',
                  'upside_participation','coupon','quote_type',
                  'quote_subtype','sub_type','initial_fixing_date')



#Mapping Amethyst fields with the BDD
mapper <- function(amethyst_field){
  
  # With Exception of observation andd payment dates
  result  = switch(amethyst_field,
                   'active'                = 'active',
                   'denomination'          = 'denom',
                   'investment_currency'   = 'currency',
                   'investment_nominal'    = 'issueAmount',
                   'currency_hedge'        = 'currencyHedge',
                   'comonCode'             = 'comonCode',
                   'final_fixing_date'     = 'finalObsDate',
                   'ISIN'                  = 'isin',
                   'listing'               = 'listing',
                   'maturity'              = 'maturityDate',
                   'valoren'               = 'valorenCode',
                   'initial_fixing_date'   = 'strikeDate',
                   'issue_date'            = 'issueDate',
                   'product_type'          = 'type',
                   'product_name'          = 'name',
                   'autocall'              = 'boolAutocall',
                   'downside_participation'= 'boolDownside',  
                   'upside_participation'  = 'boolUpside',
                   'coupon'                = 'boolCoupon',
                   'quote_type'            = 'quoteType',
                   'quote_subtype'         = 'quoteSubType',
                   'sub_type'              = 'subType',       
                   'template'              = 'template',
                   'quote_type'            = 'quoteType',
                   'quote_subtype'         = 'quoteSubType',
                   'initial_fixing_date'   = 'tradeDate')
  
  return(result)
}

# Map days to frequency
# Vectorized function
frequencyMapper <- function(freq){
  interval <- vector(mode = 'numeric', length = length(freq))
  
  sapply(1:length(freq),
         function(i){
          interval[i] <<- switch(freq[i],
                         'Daily' = 365,
                         'Weekly' = 52,
                         'Monthly' = 12,
                         'Quarterly' = 4,
                         'Bi_Annual' = 2,
                         'Annually' = 1,
                         'Yearly' = 1,
                         'NA' = 0)})
          
  return(interval)
}

######################################################

# Explicit listing of tickers
# Easier to use than regex
index_tickers <-'NKY|SMI|RTY|IND1GLDI|NDQ|NDX|SPX|XAUEUR|DAX|SX5E|MXIT|(GDX UP)'


"BDD CURLER FUNCTION"
######################################################
# Function to extract data from BDD
BDDCurler <- function(ISIN){
  con <- curl(paste0('http://entityserver.silexteq.com/entitymanager/searchassets?isin=', ISIN))
  out <- readLines(con)
  test <- jsonlite::fromJSON(out)
  
  # Results container
  results <- vector(mode = 'list',
                    length = length(mappedFields))
  
  # Map out all the mapped lists
  lapply(1:length(mappedFields),
                 function(j){
                   results[mappedFields[j]] <<- test[mapper(mappedFields[j][1])]
                 })
  
  results$investment_currency <- results$investment_currency$shortName
    
  
  # Find the ISIN
  url <- paste0("http://entityserver.silexteq.com/entitymanager/assetinfo?assetId=", test$asset$id)
  curledURL <- curl(url)
  data <- readLines(curledURL, warn = F)%>%
            jsonlite::fromJSON()
  
  results$ISIN <- data$isin
  
  
  # Obtain the underlyings
  underlyingID <- test$spUnderlyings[[1]]$asset$id
  tickers <- lapply(1:length(underlyingID), 
              function(j){
                  url <- paste0("http://entityserver.silexteq.com/entitymanager/bbticker?assetId=", underlyingID[j])
                  con <- curl(url)
                  lines <- readLines(con)%>%
                        str_extract(paste0('([A-Z0-9]{1,5})\\s[A-Z]{2}|',
                                           index_tickers))%>%
                        unlist()
                  close(con)
                  return(lines)
            })
  
  results$underlyings <- tickers%>%
                            unlist()%>%
                            sort()%>%
                            paste0(collapse = ',')
  
  # Obtain the strikes
  results$strike <- test$spUnderlyings[[1]]$strike%>%
                      sort()%>%
                      paste0(collapse = ',')
  
  # Find the coupon type
  results$coupon_type <- test$spCoupons[[1]]$type
  
  # Find the coupon value
  results$coupon_value <- test$spCoupons[[1]]$cpPa
  
  # Coupon frequency
  results$frequency <- test$spCoupons[[1]]$obsFrequency
  
  # Find if coupon is memory
  results$coupon_memory <- ifelse(test$spCoupons[[1]]$subtype == 'Memory',
                                  TRUE,
                                  FALSE)
  
  # Obtain the dates
  results$trigger_observation <- test$spAutocalls[[1]]$spAutocallObsDates[[1]]$obsDate%>%
                                    as.Date()
  results$coupon_observation <- test$spCoupons[[1]]$spCouponObsDates[[1]]$obsDate%>%
                                    as.Date()
  
  # Obtain the barriers
  triggers <- test$spAutocalls[[1]]$spAutocallObsDates[[1]]$barrier
  
  # Trigger override
  results$trigger_override <- length(unique(triggers)) > 1
  
  # Triggers
  results$autocall_trigger <- ifelse(results$trigger_override, 
                                     triggers%>%
                                       paste0(collapse = ','),
                                     unique(triggers))
  
  # Coupon triggers
  triggers <- test$spCoupons[[1]]$spCouponObsDates[[1]]$barrier
  override <- length(unique(triggers)) > 1
  
  results$coupon_trigger <- ifelse(override, 
                                     triggers%>%
                                       paste0(collapse = ','),
                                     unique(triggers))  
  
  # Product type
  results$product_type <- test$spCoupons[[1]]$basketType
  results$product_type <- ifelse(is.na(results$product_type),
                                 test$spMatDownSides[[1]]$basketType,
                                 results$product_type)
  
  results$barrier_type <- test$spMatDownsides[[1]]$barrierType
  results$barrier <- test$spMatDownsides[[1]]$barrier
  
  # Convert dates to text
  results$trigger_observation <- results$trigger_observation%>%
                                    paste0(collapse = ',')
  
  results$coupon_observation <- results$coupon_observation%>%
                                    paste0(collapse = ',')
  
  results$coupon_payment <- results$coupon_payment%>%
                                    paste0(collapse = ',')
  
  results$issuer <- test$issuer$sigle
  
  results$final_fixing_date <- as.Date(results$final_fixing_date)%>%
                                  as.character()
  
  results$initial_fixing_date<- as.Date(results$initial_fixing_date)%>%
                                  as.character()
  
  results$issue_date<- as.Date(results$issue_date)%>%
                                  as.character()
  
  results$redemption_date <- as.Date(test$maturity)%>%
                                  as.character()
  
  
  # Close connection after completion
  close(con)
  close(curledURL)
  return(results)
}

# Test for BDD Curler
# test <- BDDCurler('CH0407978383')
# BDDCurler(ISIN)
# ISIN <- 'CH0508230833'
# ISIN <- 'CH0590636053'
# Test for mapper
# mapper('initial_fixing_date')
# mapper('sub_type')
# mapper('downside_participation')
###################################



# Simple Date Parser
#######################################
dateParser <- function(date){
  suppressWarnings({if(is.na(dmy(date))){
    mdy(date)
  }else{
    dmy(date)
  }})
}

#Test
# dateParser('November 10 2000')
# dateParser('10 November 2000')
# dateParser('10/11/2000')
# dateParser('1/2/2000')
# dateParser('1/1/2000')

#######################################

##########################################################



"2) KEYWORD DECLARATIONS"
##########################################################
# Issuer List to be used to parse Issuer
issuerVect <- c('BNP', 'EFG', 'Leonteq', 'Barclays', 'SG', 'Raiffeisen', 'Citigroup', 'Morgan', 'Credit')
issuerList <- issuerVect%>%
  paste0(collapse = '|')

# Possibly add this information to the database
issuerVect <- issuerList%>%
  strsplit('\\|')


# Capital Protection List (To be updated with other issuers)
leonteqCapitalProtection <- "[0-9.]{2,8}\\% Capital Protection"
leonteqCappedParticipation <- "[0-9.]{2,8}\\% Capped Participation"
leonteqParticipation <- "[0-9.]{2,8}\\% Participation"
SGParticipation <- "the investor can participate in [0-9 %.]{4,8} "

capitalProtectionList <- c(leonteqCapitalProtection)%>%
                            paste0(collapse = '|')

participationList <- c(leonteqParticipation,
                       SGParticipation)%>%
                        paste0(collapse = '|')

cappedProtectionList <- c(leonteqCappedParticipation)%>%
                          paste0(collapse = '|')

# declare list of keywords to look for for each issuer
keyWordDictionary <- vector(mode = 'list',
                            length = length(issuerVect[[1]]))

# Name of each issuer/parser function
names(keyWordDictionary) <- issuerVect[[1]]
###################################################





"3. KEY WORD DICTIONARY"
##########################################################
# Define the keywords
# To be used to detect values we're earching for
keyWordDictionary$BNP <- c('Automatic Early If, on',
                           'Underlying Shares i',
                           'Automatic Early Redemption Automatic Early Redemption|Automatic Early Automatic Early Automatic Early',
                           'Automatic Early Automatic Early (Coupon|Interest) Payment',
                           'Optional Exercise n Optional Exercise',
                           'Optional Redemption n Optional Redemption',
                           '(Coupon|Interest) Valuation (Coupon|Interest) Payment',
                           'Paid on')

keyWordDictionary$EFG <- c('(UNDERLYING|Underlying) Underlying',
                           'Observation and Conditional Observation Date',
                           'Observation and Early Autocall',
                           'Following Business Day Convention applies. [0-9]{3}')


keyWordDictionary$Leonteq <- c('(UNDERLYING|Underlying) Underlying',
                               'Coupon Observation Coupon Trigger',
                               'Autocall Observation Autocall Trigger',
                               'Following Business Day Convention applies. [0-9]{3}')

keyWordDictionary$Barclays <- c('APPENDIX i',
                                'Interest Valuation Date\\(s\\) Interest Payment Date\\(s\\)',
                                'Autocall Valuation Date\\(s\\) Specified Early Cash Redemption',
                                'adjusted in accordance with the Business Day Convention')

keyWordDictionary$SG <- c('k Company Bloomberg',
                          'Valuation Date\\(i\\) \\(i from 1 to [0-9]{1,3}\\)',
                          'Valuation Date\\([0-9]{1,3}\\)',
                          'Interest Payment Date\\((i|s)\\)',
                          'Automatic Early Redemption Automatic Early Redemption')

keyWordDictionary$Raiffeisen <- c('(UNDERLYING|Underlying) Underlying',
                                  'Coupon Observation Coupon Trigger',
                                  'Autocall Observation Autocall Trigger',
                                  'Following Business Day Convention applies [0-9]{3}')

# Results file should contain the following information
# This is intended to be a global variable
resultNames <- c('autocall_trigger',
                 'trigger_observation',
                 'trigger_payment',
                 'strike',
                 'trigger_override',
                 'coupon_observation',
                 'coupon_payment',
                 'coupon_trigger',
                 'underlyings',
                 'strike_level',
                 'barrier',
                 'call_strike',
                 'put_strike',
                 'capped',
                 'coupon_value',   # Used for Morgan Stanley because of particular format
                 'cap_level',
                 'capital_protection',
                 'participation',
                 'final_fixing_date')


# DATE FORMATS
BNPDateFormat <- '[A-Za-z]{3,12}\\s[0-9]{2}, [0-9]{4}'
EFGDateFormat <- "[0-9]{2}/[0-9]{2}/[0-9]{4}"
LeonteqDateFormat <- "[0-9]{2}/[0-9]{2}/[0-9]{4}"
BarclaysDateFormat <- "[0-9]{1,2} [A-Za-z]{3,12} [0-9]{4}"
SocGenDateFormat <- "([0-9]{1,2} [A-Za-z]{3,12} [0-9]{4})|([0-9]{2}/[0-9]{2}/[0-9]{4})"
RaiffeisenDateFormat <- "[0-9]{2}/[0-9]{2}/[0-9]{4}"
CitiFormat<- "(([0-9]{1,2} [A-Za-z]{3,12} [0-9]{4})|([A-Za-z]{3,12} [0-9]{1,2}, [0-9]{4}))"
CreditDateFormat <- "[0-9]{1,2} [A-Za-z]{3} [0-9]{4}"
MorganDateFormat <- '[0-9]{1,2} [A-Za-z]{3,12} [0-9]{4}'
##########################################################


"4. PARSER FUNCTIONS"
##########################################################
# Declare list of parser functions specific to each issuer
parserFunction <- vector(mode = 'list',
                         length = length(issuerVect[[1]]))

names(parserFunction) <- issuerVect[[1]]

"i) BNP function"
#############################################################################
parserFunction$BNP <- function(pdf_loc){
    results <- vector(mode = 'list',
                      length = length(resultNames))
    names(results) <- resultNames
    
    # Read the PDF
    PDF <- pdf_text(pdf_loc)%>%
              str_squish()%>%
              paste0(collapse = " ")%>%
              str_replace_all(.,"(?<=([0-9]{1}))(th|rd|nd|st)", "")%>%
              gsub("\\sDaten", "",.)
    
    # Set trigger_override to FALSE by default ( for American Barrier Issues)
    results$trigger_override <- FALSE
    
    # Added to the trigger and coupon observation dates
    redDate <- str_extract(PDF, paste('(?<=(Redemption Date))(', BNPDateFormat, ')'))
    redValDate <- str_extract(PDF, paste('(?<=(Redemption Valuation))(', BNPDateFormat, ')'))
    
    # 1) Single early redemption date
    early_redemption <- PDF%>%
                          str_detect(pattern = paste0('(?<=(',
                                                       keyWordDictionary$BNP[1],
                                                       ' ))([A-Za-z]{3,12} [0-9 a-z ,]{5} [0-9]{4})'))
    
    # Ignore if is na
    if(early_redemption){
        early_redemption <- PDF%>%
                              str_extract(pattern = paste0('(?<=(',
                                                          keyWordDictionary$BNP[1],
                                                          ' ))([A-Za-z]{3,12} [0-9 a-z ,]{5} [0-9]{4})'))%>%
                              unlist()
                            
        results$trigger_payment <- early_redemption[[1]]%>%
                                      dateParser()
    }
    
    # 2) Underlyings (Bloomberg Ticker)
    if(PDF%>%str_detect('Name of (Underlying|Bloomberg Code)|Underlying Shares')){
        underlyinglist<- PDF%>%
                    str_extract_all(pattern = paste('(\\s([A-Z]{1,5}|[0-9]{1,5})\\s[A-Z]{2}|',
                                                    index_tickers,
                                                    ') [0-9.]{3,10}'))%>%
                    unlist()%>%
                    unique()
        
        results$underlyings <-underlyinglist%>%
                                str_extract_all(pattern = paste('(\\s([A-Z]{1,5}|[0-9]{1,5})\\s[A-Z]{2}|',
                                                                index_tickers, ')'))%>%
                                str_extract(paste0('(([A-Z]{1,5}|[0-9]{1,5})\\s[A-Z]{2})|(', index_tickers, ')'))%>%
                                unlist()%>%
                                sort()%>%
                                paste0(collapse = ',')
                              
        results$strike <- underlyinglist%>%
                            str_extract_all(pattern ='(?!([0-9]{1,5}+(?=\\s[A-Z]{2})))[0-9]{1,6}.[0-9]{1,4}')%>%
                            unlist()%>%
                            as.numeric()%>%
                            sort()%>%
                            paste0(collapse = ',')
        
        results$put_strike <- PDF%>%
                                str_extract('Strike Price (.*?) Share1Initial')%>%
                                str_extract('(?<=(\\())([0-9]{1,6})')%>%
                                as.numeric()
    }
    
    # 3) 
    # Automatic Early Redemption Valuation
    # Automatic Early Redemption Dates
    if(PDF%>%str_detect(keyWordDictionary$BNP[3])){
        redemption_dates <- PDF%>%
                              str_extract(paste0('(', keyWordDictionary$BNP[3], 
                                               ") (.*?) Business Day Following"))%>%
                              str_extract_all(pattern = paste0(BNPDateFormat,'\\s', # E. Redempt. Val. Date
                                                               BNPDateFormat, '(\\s[0-9]{1,5}%)?',            # E. Redempt. Date
                                                                     sep = ''))
        results$trigger_observation <- redemption_dates[[1]]%>%
                                          str_extract(pattern = BNPDateFormat)%>%
                                          c(redValDate)%>%
                                            dateParser()%>%
                                          paste(collapse = ',')
        
        # obtain the second date in the substring
        results$trigger_payment <- redemption_dates[[1]]%>%
                                        str_extract(paste0("(?<=(", 
                                                    BNPDateFormat," ))(",
                                                    BNPDateFormat,")"))%>%
                                        c(redDate)%>%
                                        dateParser()%>%
                                        paste(collapse = ',')
        
        # Autocall Triggers
        autocall_trigger <- redemption_dates[[1]]%>%
                                        str_extract('([0-9]+(?=\\%))')
        # Check for trigger override
        results$trigger_override <- ifelse(length(unique(autocall_trigger)) > 1,
                                           TRUE,
                                           FALSE)
        
        results$autocall_trigger <- ifelse(results$trigger_override,
                                           autocall_trigger%>%paste0(collapse = ','),
                                           unique(autocall_trigger))
    
      # In the case of a single autocaall date
    }else if(PDF%>%str_detect(paste0('then the Issuer shall redeem each Certificate on ',BNPDateFormat ))){
        triggerDates <- PDF%>%
                          str_extract('Automatic Early If, on .* N x [0-9.]{2,9}%')
        
        # Find the trigger date
        results$trigger_observation <- triggerDates%>%
                                          str_extract(paste0('(?<=(If, on ))(', BNPDateFormat,')'))%>%
                                          dateParser()%>%
                                          paste0(collapse=',')
        
        # Find the payment date
        results$trigger_payment <- triggerDates%>%
                                          str_extract(paste0('(?<=(each Certificate on ))(', BNPDateFormat,')'))%>%
                                          dateParser()%>%
                                          paste0(collapse=',')      
        
        results$coupon_observation <- results$trigger_observation
        results$coupon_payment <- results$trigger_payment
        
        
        # Find the trigger
        results$coupon_trigger <- PDF%>%
                                    str_extract('Automatic Early .* Redemption Price')%>%
                                    str_extract('(?<=(Automatic Early ))([0-9.]{1,5})')%>%
                                    as.numeric()
        # Find the coupon value
        results$coupon_value <- triggerDates%>%
                                  str_extract('[0-9.]{1,6}%')%>%
                                  str_extract('[0-9.]{1,6}')%>%
                                  as.numeric()
        
        results$coupon_value <- results$coupon_value - 100
        
    }
    
    # 4) 
    # Coupon Payment Dates
    if(PDF%>%str_detect(keyWordDictionary$BNP[4])){
        coupon_dates <- PDF%>%
                          str_extract_all(pattern = paste(BNPDateFormat, # E. Redempt. Val. Date
                                                           BNPDateFormat, # E. Redempt. Date
                                                           BNPDateFormat, # Coupon Pmt Date
                                             sep = ' '))
        
        results$trigger_observation <- coupon_dates[[1]]%>%
                                          str_extract(pattern = BNPDateFormat)%>%
                                          c(redValDate)%>%
                                          dateParser()%>%
                                          paste(collapse = ',')
        
        # obtain the second date in the substring
        results$trigger_payment <- coupon_dates[[1]]%>%
                                      str_extract(paste0("(?<=(", 
                                                         BNPDateFormat," ))(",
                                                         BNPDateFormat,")"))%>%
                                      c(redDate)%>%
                                      dateParser()%>%
                                      paste(collapse = ',')
                                    
        # Extract the last date from each line
        results$coupon_payment <- coupon_dates[[1]]%>%
                                    str_split('[0-9]{4} ')%>%
                                    lapply(function(x){
                                              return(x[3])})%>%
                                    unlist()%>%
                                    c(redDate)%>%
                                    dateParser()%>%
                                    paste0(collapse = ',')
        
        results$coupon_observation <- results$trigger_observation
        
        if(PDF%>%str_detect('Conditional (Coupon|Interest)')){
            # Define coupon trigger
            coupon_trigger <- PDF%>%
                                str_extract('Conditional (Coupon|Interest) (.*?) (Coupon|Interest) Payment Date')
    
            # Coupon trigger defined in the TS
            if(coupon_trigger%>%str_detect('each Underlying Share is greater than or equal to ShareiInitial')){
              results$coupon_trigger <- 100
            
            }else if(coupon_trigger%>%str_detect('each Underlying Share is greater than or equal to [0-9]{1,9}% of ShareiInitial')){
              results$coupon_trigger <- coupon_trigger%>%
                                            str_extract('each Underlying Share is greater than or equal to [0-9]{1,9}% of ShareiInitial')%>%
                                            str_extract('[0-9.]{1,6}')%>%
                                            as.numeric()
            }else{
              results$coupon_trigger <- coupon_trigger%>%
                str_extract('(?<=(equal to ))([0-9.]{1,6})')%>%
                as.numeric()
            } 
        }
        # Trigger override by default
        results$trigger_override <- FALSE
        
        results$autocall_trigger <- PDF%>%
                                        str_extract('Automatic Early [0-9.%]{2,9} x (ShareiInitial|ReferenceInitial|(Best-of))')%>%
                                        str_extract('[0-9.]{2,6}')%>%
                                        as.numeric()
                                      
        results$barrier <- results$autocall_trigger
                      
    }
    # 5)
    # Optional Exercise Date
    # Remove all Daten and Issuer's Internal use strings
    PDF <- PDF%>%
            gsub("\\(Issuer's internal use only\\)\\s", "",.)
          
    
    if(PDF%>%str_detect(keyWordDictionary$BNP[5])){
        # Select strings btwn BNP[5] and 6
        optExerciseText <- str_match(PDF, 
                                     paste0(keyWordDictionary$BNP[5],        # Optional Exercise n Optional Exercise
                                            "\\s*(.*?)\\s",                  # between
                                            keyWordDictionary$BNP[6]))[,2]   # Optional Redemption n Optional Redemption
        
        # Add dates to results
        trigger_observation <- optExerciseText%>%
                                          str_extract_all(BNPDateFormat)
        
        results$trigger_observation <- trigger_observation[[1]]%>%
                                          c(redValDate)%>%
                                          dateParser()%>%
                                          paste0(collapse = ',')
    }
    
    # 6)
    # Optional Redemption Date
    if(PDF%>%str_detect(keyWordDictionary$BNP[6])){
        trigger_payment <-PDF%>%
                            str_match(paste0(keyWordDictionary$BNP[6], 
                                             ".* Optional Redemption"))
        
        trigger_payment <- trigger_payment[,1]%>% # Filter out all text after the Optional Redemption n Optional Redemption
                                  str_extract_all(BNPDateFormat) # Apply the same function as in 5
        
        results$trigger_payment <- trigger_payment[[1]]%>%
                                      str_extract(BNPDateFormat)%>%
                                      c(redDate)%>%
                                      dateParser()%>%
                                      paste0(collapse = ',')
    
    }
    # 7)
    # Coupon Valuation Coupon Payment
    if(PDF%>%str_detect(keyWordDictionary$BNP[7])){
        # Add dates to results
        couponVDate <- PDF%>%
                        str_match(paste0(keyWordDictionary$BNP[7],        # Optional Exercise n Optional Exercise
                                         "\\s*(.*?)\\s(",                  # between
                                         keyWordDictionary$BNP[3], ')'))  # Automatic Early Redemption Automatic Early Redemption
        
        # Assign values to dictionary
        coup_dates <- couponVDate[1, ][1]%>%
                                  str_extract_all(paste0(BNPDateFormat,'\\s', # Coup. Val. Date
                                                         BNPDateFormat,            # Coup. Pmt. Date
                                                         sep = ''))
        
        
        results$coupon_observation <- coup_dates[[1]]%>%
                                      str_extract(BNPDateFormat)%>%
                                      dateParser()%>%
                                       paste0(collapse = ',')
                    
        results$coupon_payment <- coup_dates[[1]]%>%
                                  str_extract(paste0('(?<=(',
                                                     BNPDateFormat,
                                                     ' ))(',
                                                     BNPDateFormat, ')'))%>%
                                  dateParser()%>%
                                  paste0(collapse = ',')
        
        coupon_trigger <- PDF%>%
                                    str_extract('Conditional (Coupon|Interest) (.*?) (Coupon|Interest) Payment Date')
        
        if(coupon_trigger%>%str_detect('each Underlying Share is greater than or equal to ShareiInitial')){
          results$coupon_trigger <- 100
          
        }else{
          results$coupon_trigger <- coupon_trigger%>%
                                      str_extract('(?<=(equal to ))([0-9.]{1,6})')%>%
                                      as.numeric()
        }
                                                                                        
    }
    
    #8) 
    # Non conditional coupon date
    # A list of dates is provided not under a table but listed
    if(!(PDF%>%str_detect(keyWordDictionary$BNP[4])) && PDF%>%str_detect('Coupon N')){
        # Add dates to results
        couponFDate <- PDF%>%
                        str_match(paste0(keyWordDictionary$BNP[8],        # Optional Exercise n Optional Exercise
                             "\\s*(.*?) and on the Redemption Date"))
        
        # NA couponFDate means there is only one coupon payment date which is the redemption date
        conditional <-!is.na(couponFDate[1]) && str_detect(couponFDate, BNPDateFormat)
        
        if(conditional){
            results$coupon_payment <- couponFDate[1]%>%
                                        str_extract_all(BNPDateFormat)%>%
                                        unlist()%>%
                                        dateParser()%>%
                                        c(redDate)%>%
                                        paste(collapse = ',')
        }else{
          results$coupon_payment <-PDF%>%
                                    str_extract(paste0('(?<=(Redemption Date ))(',
                                                BNPDateFormat, ')'))%>%
                                    dateParser()
        }
    }
    
    # 9)
    # Autocall Trigger
    
    # In the case of a single autocall trigger 
    if(PDF%>%str_detect('Automatic Early [0-9.%]{2,9} x (ShareiInitial|ReferenceInitial|(Best-of))')){
          results$trigger_override <- FALSE
          results$autocall_trigger <- PDF%>%
                                        str_extract('Automatic Early [0-9.%]{2,9} x (ShareiInitial|ReferenceInitial|(Best-of))')%>%
                                        str_extract('[0-9.]{2,6}')%>%
                                        as.numeric()
          results$barrier <- results$autocall_trigger
    }
    
    
    # 10)
    # Find the Barrier
    if(PDF%>%str_detect('Knock-in Price')){
      # Consider References as well
      field1 <-  '[0-9.]+(?=(% of (Share([0-9]{1})|Reference)Initial\\) Knock-in Determination))'
      field2 <- '(?<=(Knock-in Pricei ))([0-9.]{1,6})'
      results$barrier <- PDF%>%
                          str_extract(paste('(', field1, '|',
                                            field2, ')',
                                            sep = ''))%>%
                          as.numeric()
      
      if (is.na(results$barrier)){
        # Consider a phrase taking into account references
        results$barrier <- PDF%>%
                            str_extract('[0-9.]+(?=(% x (ShareiInitial with i from 1 to [0-9]{1,3}|ReferenceInitial) Knock-in Determination))')%>%
                            as.numeric()
        results$barrier <- ifelse(is.na(results$barrier), 
                                  results$autocall_trigger,
                                  results$barrier)
      }
      
    }else if(PDF%>%str_detect('WO ShareFinal is greater than or equal to')){
      results$barrier <- PDF%>%
                          str_extract('(?<=(WO ShareFinal is greater than or equal to ))([0-9.]{2,9})')%>%
                          as.numeric()
    }
    
    # 11)
    # Capital protection/Participation and Cap
    
    # Capital Protection, Participation and Capped Participation
    # Lazy conditional. Should be individual but this works as well
    stringsTodetect <- paste0(c(capitalProtectionList,
                                participationList,
                                cappedProtectionList),
                              collapse = "|")
    
    if(str_detect(PDF, stringsTodetect)){
        results$capital_protection <- PDF%>%
                                        str_extract(capitalProtectionList)%>%
                                        unlist()%>%
                                        str_extract('[0-9.]{4,8}')%>%
                                        as.numeric()
        
        results$participation <- PDF%>%
                                    str_extract(participationList)%>%
                                    unlist()%>%
                                    str_extract('[0-9.]{4,8}')%>%
                                    as.numeric()
        
        results$capped <- ifelse(PDF%>%
                                   str_detect(cappedProtectionList),
                                 TRUE, 
                                 FALSE)
        
        results$cap_level <- PDF%>%
                                str_extract(cappedProtectionList)%>%
                                unlist()%>%
                                str_extract('[0-9.]{4,8}')%>%
                                as.numeric()
    }
    return(results)
}

# TESTS
# pdf_loc <- "termsheets/bnp/XS2149558657.pdf"
# pdf_loc <- "silex/DavidCyvoct/localParser/termsheets/bnp/XS2049219467.pdf"
# pdf_loc <- 'termsheets/TermSheets/XS2221815777.pdf'
# pdf_loc <- 'termsheets/bnp/XS2113435676.pdf'
# pdf_loc <- 'termsheets/1) BNP/XS2186728502.pdf'
# parserFunction$BNP(pdf_loc)

#############################################################################



"ii) EFG function"
#############################################################################
# pdf_loc <- "silex/DavidCyvoct/localParser/termsheets/TermSheets/termsheet-CH0508206338-en.pdf"
# setwd("termsheets/efg")
# pdfs <- list.files(pattern = ".pdf$", recursive = TRUE)
# pdfs[3]
# lapply(1:length(pdfs),
#        function(i) {
#          parserFunction$EFG(pdfs[i])
#           print(i)})
# 
# pdf_loc <- 'termsheets/TermSheets/CH0584949231.pdf'
# parserFunction$EFG(pdf_loc)
parserFunction$EFG <- function(pdf_loc){
    results <- vector(mode = 'list',
                      length = length(resultNames))
    names(results) <- resultNames
    
      # Read the PDF
    PDF <- pdf_text(pdf_loc)%>%
              str_squish()%>%
              paste0(collapse = " ")
    
    # Default values
    results$capped <- FALSE
    
    
    #1)
    # Extract all text between the Underlyings and Product details sections
    # Conditional on there being underlyings
    if(str_detect(PDF, keyWordDictionary$EFG[1])){
      underlyings_text <- PDF%>%
                          str_match(paste0(keyWordDictionary$EFG[1],
                                     '\\s*(.*?)\\s',
                                     '(PRODUCT DETAILS|Product Details)'))
                  
      # Presence of indices in the underlying
      if(str_detect(underlyings_text[1], index_tickers)){
        results$underlyings <- underlyings_text%>%
                                  str_extract_all(index_tickers)%>%
                                  unlist()%>%
                                  unique()%>%
                                  sort()%>%
                                  paste0(collapse = ',')

        underlyings <- underlyings_text[1]%>%
          str_extract_all(paste0('(', index_tickers, ') ',
                                 '([A-Z]{3}|GBp) ([0-9.]{2,10}|TBA) ',
                                 '(([A-Z]{3}|GBp) ([0-9.]{2,10}|TBA) )?',
                                 '(([A-Z]{3}|GBp) ([0-9.]{2,10}|TBA) )?',
                                 '(([A-Z]{3}|GBp) ([0-9.]{2,10}|TBA) )?'))                   
        
        # Find the corresponding columns. Order matters in this case
        column_keywords <- c('Barrier Level',
                             'Strike Level',
                             'Autocall Trigger',
                             'Coupon Trigger')
        
        # Order of these keywords corresponds to the order of column_keywords
        keywords <- c('barrier', 
                      'strike_level',
                      'autocall_trigger',
                      'coupon_trigger')
        
        # Cases exist where no barrier/strike columns exists in the underlying
        tryCatch({
          # Find columns in the PDF Underlying
          underlying_headers <- underlyings_text%>%
                                  str_extract_all(column_keywords%>%paste0(collapse = '|'))%>%
                                  unlist()%>%
                                  unique()
          
          # Assign values to their corresponding fields
          fields <- underlyings[[1]]%>%
                      strsplit(" ")
          
          keys <- keywords[column_keywords %in% underlying_headers]
          
          # Assign values to their corresponding
          results[keys] <- fields[[1]][seq(3,(length(keys)+1)*2, by =2)+2]%>%
                              as.numeric()
          
          # Initial Value
          initial <- fields[[1]][3]%>%
            as.numeric()
          
          results[keys] <- map2(results[keys], initial,  ~ round(.x*100 / .y, 1))
          
          
          results$strike <- lapply(1:length(underlyings[[1]]),
                                   function(j){
                                     fields <- underlyings[[1]][j]%>%
                                       strsplit(" ")
                                     
                                     fields[[1]][3]})
          results$strike <- results$strike %>%
            unlist()%>%
            as.numeric()%>%
            sort()%>%
            paste0(collapse = ',')
          
          
        },
        error = function(e)(print(paste0(pdf_loc, ': missing barrier/strike values'))))
        
        
        # Conditional on there being a cap level in the text
      }else if(str_detect(PDF, 'Cap Level') ){
        underlyings <- underlyings_text[1]%>%
                          str_extract_all(paste0('(([A-Z]{1,5} [A-Z]{2} |[A-Z]{2}[0-9]{1}\\s[A-Z]{1}[a-z]{5} )',
                                                 '|', index_tickers, ' )',
                                                 '[A-Z]{3} ([0-9.]{2,10}|TBA) '))
        
        results$underlyings <- underlyings[[1]]%>%
                                  str_extract_all(paste0('(([A-Z]{1,5} [A-Z]{2} |[A-Z]{2}[0-9]{1}\\s[A-Z]{1}[a-z]{5} )',
                                                         '|', index_tickers, ' )'))%>%
                                  unlist()%>%
                                  sort()%>%
                                  paste0(collapse = ',')
        
        results$capped = TRUE
        results$cap = underlyings[[1]]%>%
                        str_extract_all('[0-9.]{2,12}')%>%
                        unlist()
        
      }else{
        # Small conditional because of a GBp vs GBp error in TS
        underlyings <- underlyings_text[1]%>%
                          str_extract_all(paste0('(([A-Z]{1,5}(\\/)? [A-Z]{2} |[A-Z]{2}[0-9]{1}\\s[A-Z]{1}[a-z]{5} )',
                                                 '|(', index_tickers, ' ))',
                                                 '([A-Z]{3}|GBp) ([0-9.]{2,10}|TBA) ',
                                                 '(([A-Z]{3}|GBp) ([0-9.]{2,10}|TBA) )?',
                                                 '(([A-Z]{3}|GBp) ([0-9.]{2,10}|TBA) )?',
                                                 '(([A-Z]{3}|GBp) ([0-9.]{2,10}|TBA) )?', 
                                                 '([A-Z]{3}|GBp) ([0-9.]{2,10}|TBA)'))%>%
                          unlist()
        
        # Find the corresponding columns. Order matters in this case
        column_keywords <- c('Barrier Level',
                             'Strike Level',
                             'Autocall Trigger',
                             'Coupon Trigger')
        
        # Order of these keywords corresponds to the order of column_keywords
        keywords <- c('barrier', 
                      'strike_level',
                      'autocall_trigger',
                      'coupon_trigger')
        
        # Find columns in the PDF Underlying
        colKeys <- column_keywords%>%
                    paste0(collapse = "|")
        
        underlying_headers <- underlyings_text%>%
                                str_extract_all(colKeys)%>%
                                unlist()%>%
                                unique()
        
        # Assign values to their corresponding fields
        fields <- underlyings[1]%>%
                     strsplit(" ")
        
        keys <- keywords[column_keywords %in% underlying_headers]

        # Assign values to their corresponding keys as percentages             
        results[keys] <- fields[[1]][seq(4,(length(keys)+1)*2, by =2)+2]%>%
                            as.numeric()
        
        # Initial Value
        initial <- fields[[1]][4]%>%
                      as.numeric()

        results[keys] <- map2(results[keys], initial,  ~ round(.x*100 / .y, 1))
        
        results$strike <- lapply(1:length(underlyings),
                                        function(j){
                                          fields <- underlyings[j]%>%
                                                          strsplit(" ")
                                            
                                          fields[[1]][4]})
        results$strike <- results$strike%>%
                                  unlist()%>%
                                  as.numeric()%>%
                                  sort()%>%
                                  paste0(collapse = ',')
                                          
        
         
          
          
        # Find the underlying tickers
        results$underlyings  <-underlyings%>%
                                  str_extract_all('[A-Z]{1,5}(\\/)?\\s[A-Z]{2}')%>%# Last space needed to identify only tickers
                                  str_extract('[A-Z]{1,5}(\\/)?\\s[A-Z]{2}')%>% # Remove the last space
                                  unlist()%>%
                                  sort()%>%
                                  paste0(collapse = ',')
      }
    }
    
    
    #2)
    # Coupon Observation and payment dates
    # Find if the Autocall Dates exist
    if(PDF%>%str_detect(keyWordDictionary$EFG[2])){
      
      if(PDF%>%str_detect(keyWordDictionary$EFG[3])){ 
        # If the above is true then find all strings between keywords 2 and 3:
        coupDates <- PDF%>%
          str_match(paste0(keyWordDictionary$EFG[2],
                           "\\s*(.*?)\\s",
                           keyWordDictionary$EFG[3]))
        
        # Extract all dates and triggers
        dateTriggers <- coupDates[1]%>%
          str_extract_all(paste0("[0-9]{1,2}\\s", EFGDateFormat, 
                                 "[*]{0,3} [0-9 . %]{4,8}\\s",
                                 EFGDateFormat, "[*]{0,3}",
                                 sep = ''))
        
        # Automatically finds the first date in each line = coupon observation
        results$coupon_observation <- dateTriggers[[1]]%>%
                                  str_extract(EFGDateFormat)%>%
                                  lapply(dateParser)%>%
                                  unlist()%>%
                                  as.Date(origin = '1970-01-01')%>%
                                  paste0(collapse = ',')
        
        # Find the coupon payment dates
        results$coupon_payment <- dateTriggers[[1]]%>%
                                    str_extract(paste0('(?<=(%\\s))(', 
                                                       EFGDateFormat, ')'))%>%
                                    lapply(dateParser)%>%
                                    unlist()%>%
                                    as.Date(origin = '1970-01-01')%>%
                                    paste0(collapse = ',')
        
        # (results$coupon_observation <- do.call('c', coupon_observation))
        # (results$coupon_payment <- do.call('c', coupon_payment))
        
      }else{ # Otherwise consider the coupon table as the only table and use keywords
        # Extract all dates and triggers
        dateTriggers <- PDF%>%
          str_extract_all(paste0("[0-9]{1,2}\\s", EFGDateFormat, 
                                 "[*]{0,3} [0-9 . %]{4,8}\\s",
                                 EFGDateFormat, "[*]{0,3}",
                                 sep = ''))
        
        # Automatically finds the first date in each line = coupon observation
        results$coupon_observation <- dateTriggers[[1]]%>%
                                        str_extract(EFGDateFormat)%>%
                                        lapply(dateParser)%>%
                                        unlist()%>%
                                        as.Date(origin = '1970-01-01')%>%
                                        paste0(collapse = ',')
        
        # Find the coupon payment dates
        results$coupon_payment <- dateTriggers[[1]]%>%
                                    str_extract(paste0('(?<=(%\\s))(', 
                                                       EFGDateFormat, ')'))%>%
                                    lapply(dateParser)%>%
                                    unlist()%>%
                                    as.Date(origin = '1970-01-01')%>%
                                    paste0(collapse = ',')
        
        # (results$coupon_observation <- do.call('c', coupon_observation))
        # (results$coupon_payment <- do.call('c', coupon_payment))
        
      }
    }
    
    #3)
    # Autocall dates within the same conditional
    # If this is true then find all strings between keywords 2 and 3:
    if(PDF%>%str_detect(keyWordDictionary$EFG[3])){
      autocallDates <- PDF%>%
                         str_extract(paste0(keyWordDictionary$EFG[3],".*"))%>%
                        gsub('of the Strike Level ', '', .) #  To be used for an old version of the TS
        
      # Extract all dates and triggers
      dateTriggers <- autocallDates%>%
                        str_extract_all(paste0("[0-9]{1,2}\\s", EFGDateFormat, 
                               "[*]{0,3} [0-9 . %]{4,7}\\s",
                               EFGDateFormat, "[*]{0,3}",
                               sep = ''))
      
      autocallTriggers <- dateTriggers%>%
                            str_extract_all('[0-9.]{5,7}%')%>%
                            unlist()%>%
                            gsub('%', '',.)%>%
                            as.numeric()
      
      uniqueTriggers <- unique(autocallTriggers)
      
      # Trigger override when we have more than one trigger until maturity
      results$trigger_override <- ifelse(length(uniqueTriggers)>1,
                                         TRUE,
                                         FALSE)
      
      results$autocall_trigger <- ifelse(length(uniqueTriggers) >1,
                                         autocallTriggers%>%paste0(collapse = ','),
                                         uniqueTriggers)
        
      # Automatically finds the first date in each line = coupon observation
      results$trigger_observation <- dateTriggers[[1]]%>%
                                        str_extract(EFGDateFormat)%>%
                                        lapply(dateParser)%>%
                                        unlist()%>%
                                        as.Date(origin = '1970-01-01')%>%
                                        paste0(collapse = ',')
      

      # Find the coupon payment dates
      results$trigger_payment <- dateTriggers[[1]]%>%
                                    str_extract(paste0('(?<=(%\\s))(', 
                                                       EFGDateFormat, ')'))%>%
                                    lapply(dateParser)%>%
                                    unlist()%>%
                                    as.Date(origin = '1970-01-01')%>%
                                    paste0(collapse = ',')
                                    
      
      # (results$trigger_observation <- do.call('c', trigger_observation))
      # (results$trigger_payment <- do.call('c', trigger_payment))
    } 
    #4)
    # Guaranteed coupon dates
    if(PDF%>%str_detect(keyWordDictionary$EFG[4])){
      
      # Find all dates relating to a specific coupon to be paid
      coupon_payment <- PDF%>%
                          str_extract_all(paste0("[A-Z]{3} [0-9.]{3,10} paid on ", EFGDateFormat))
      
      results$coupon_payment <- coupon_payment[[1]]%>%
                          str_extract(EFGDateFormat)%>%
                          lapply(dateParser)%>%
                          unlist()%>%
                          as.Date(origin = '1970-01-01')%>%
                          paste0(collapse = ',')
                        
      # (results$coupon_payment <- do.call('c', coupon_payment))
    }
    
    # Capital Protection, Participation and Capped Participation
    # Lazy conditional. Should be individual but this works as well
    stringsTodetect <- paste0(c(capitalProtectionList,
                                participationList,
                                cappedProtectionList),
                              collapse = "|")
    
    #5)
    # Capital protection/Participation and Cap
    if(str_detect(PDF, stringsTodetect)){
      results$capital_protection <- PDF%>%
                                      str_extract(capitalProtectionList)%>%
                                      unlist()%>%
                                      str_extract('[0-9.]{4,8}')%>%
                                      as.numeric()
      
      results$participation <- PDF%>%
                                  str_extract(participationList)%>%
                                  unlist()%>%
                                  str_extract('[0-9.]{4,8}')%>%
                                  as.numeric()
      
      results$capped <- ifelse(PDF%>%
                          str_detect(cappedProtectionList),
                          TRUE, 
                          FALSE)
      
      results$cap_level <- PDF%>%
                              str_extract(cappedProtectionList)%>%
                              unlist()%>%
                              str_extract('[0-9.]{4,8}')%>%
                              as.numeric()
    }
    
    
    return(results)
}
#############################################################################



"iii) Leonteq function"
#############################################################################
# pdf_loc <- "termsheets/leonteq/CH0521177268.pdf"
pdf_loc <- "termsheets/leonteq/termsheet-CH0539859964-en.pdf"
# pdf_loc <- "termsheets/leonteq/CH0508210488.pdf"
# pdf_loc <- "termsheet-CH0546949865-en.pdf"
# pdf_loc <- "termsheets/TermSheets/CH0584949231.pdf"
# pdf_loc <- 'termsheets/TermSheets/CH0584949231.pdf'
parserFunction$Leonteq <- function(pdf_loc){
      # Read the PDF 
      'Should probably use the global PDF variable'
      PDF <- pdf_text(pdf_loc)%>%
              str_squish()%>%
              paste0(collapse = " ")

      
      results <- vector(mode = 'list',
                        length = length(resultNames))
      names(results) <- resultNames
      
      
      
      #1)
      # Extract all text between the Underlyings and Product details sections
      # Conditional on there being underlyings
      if(str_detect(PDF, keyWordDictionary$Leonteq[1])){
            underlyings_text <- PDF%>%
                            str_extract(paste0(keyWordDictionary$Leonteq[1],
                                             '\\s*(.*?)\\s',
                                             '(PRODUCT DETAILS|Product Details)'))
            
            # Presence of indices in the underlying
            if(str_detect(underlyings_text, index_tickers)){
                results$underlyings <- underlyings_text%>%
                                          str_extract_all(index_tickers)%>%
                                          unlist()%>%
                                          unique()%>%
                                          sort()%>%
                                          paste0(collapse = ',')
                
                underlyings <- underlyings_text[1]%>%
                                  str_extract_all(paste0('(', index_tickers, ') ',
                                                         '([A-Z]{3}|GBp) ([0-9.]{2,10}|TBA) ',
                                                         '(([A-Z]{3}|GBp) ([0-9.]{2,10}|TBA) )?',
                                                         '(([A-Z]{3}|GBp) ([0-9.]{2,10}|TBA) )?',
                                                         '(([A-Z]{3}|GBp) ([0-9.]{2,10}|TBA) )?'))                   
                                
                # Find the corresponding columns. Order matters in this case
                column_keywords <- c('Barrier Level',
                                     'Strike Level',
                                     'Autocall Trigger',
                                     'Coupon Trigger')
                
                # Order of these keywords corresponds to the order of column_keywords
                keywords <- c('barrier', 
                              'strike_level',
                              'autocall_trigger',
                              'coupon_trigger')
                
                # Cases exist where no barrier/strike columns exists in the underlying
                tryCatch({
                    # Find columns in the PDF Underlying
                    underlying_headers <- underlyings_text%>%
                                            str_extract_all(column_keywords%>%paste0(collapse = '|'))%>%
                                            unlist()%>%
                                            unique()
                    
                    # Assign values to their corresponding fields
                    fields <- underlyings[[1]]%>%
                                strsplit(" ")
                    
                    keys <- keywords[column_keywords %in% underlying_headers]
                    
                    # Assign values to their corresponding
                    results[keys] <- fields[[1]][seq(3,(length(keys)+1)*2, by =2)+2]%>%
                                        as.numeric()
                    
                    # Initial Value
                    initial <- fields[[1]][3]%>%
                                  as.numeric()
                    
                    results[keys] <- map2(results[keys], initial,  ~ round(.x*100 / .y, 1))
                    
                    
                    results$strike <- lapply(1:length(underlyings[[1]]),
                                             function(j){
                                               fields <- underlyings[[1]][j]%>%
                                                              strsplit(" ")
                                               
                                               fields[[1]][3]})
                    results$strike <- results$strike %>%
                                        unlist()%>%
                                        as.numeric()%>%
                                        sort()%>%
                                        paste0(collapse = ',')
                    
                    
                },
                error = function(e)(print(paste0(pdf_loc, ': missing barrier/strike values'))))
                
              # Conditional on there being a cap level in the text
            }else if(str_detect(PDF, 'Cap Level') ){
              underlyings <- underlyings_text[1]%>%
                                str_extract_all(paste0('(([A-Z]{1,5} [A-Z]{2}|[A-Z]{2}[0-9]{1}\\s[A-Z]{1}[a-z]{5})',
                                                       '|', index_tickers, ' )',
                                                       '[A-Z]{3} ([0-9.]{2,10}|TBA) '))
              
              results$underlyings <- underlyings[[1]]%>%
                                        str_extract_all(paste0('(([A-Z]{1,5} [A-Z]{2} |[A-Z]{2}[0-9]{1}\\s[A-Z]{1}[a-z]{5} )',
                                                               '|', index_tickers, ' )'))%>%
                                        unlist()%>%
                                        sort()%>%
                                        paste0(collapse = ',')
              
              results$capped = TRUE
              results$cap = underlyings[[1]]%>%
                              str_extract_all('[0-9.]{2,12}')%>%
                              unlist()
              
            }else{
              underlyings <- underlyings_text%>%
                                str_extract_all(paste0('(([A-Z]{1,5}(\\/)? [A-Z]{2} |[A-Z]{2}[0-9]{1}\\s[A-Z]{1}[a-z]{5} )',
                                                       '|(', index_tickers, ' ))',
                                                       '([A-Z]{3}|GBp) ([0-9.]{2,10}|TBA) ',
                                                       '(([A-Z]{3}|GBp) ([0-9.]{2,10}|TBA) )?',
                                                       '(([A-Z]{3}|GBp) ([0-9.]{2,10}|TBA) )?',
                                                       '(([A-Z]{3}|GBp) ([0-9.]{2,10}|TBA) )?', 
                                                       '([A-Z]{3}|GBp) ([0-9.]{2,10}|TBA)'))%>%
                                unlist()
              
              # Find the corresponding columns. Order matters in this case
              column_keywords <- c('Barrier Level',
                                   'Strike Level',
                                   'Autocall Trigger',
                                   'Coupon Trigger')
              
              # Order of these keywords corresponds to the order of column_keywords
              keywords <- c('barrier', 
                            'strike_level',
                            'autocall_trigger',
                            'coupon_trigger')
              
              # Cases exist where no barrier/strike columns exists in the underlying
              tryCatch({
                  # Find columns in the PDF Underlying
                  underlying_headers <- underlyings_text%>%
                                          str_extract_all(column_keywords%>%paste0(collapse = '|'))%>%
                                          unlist()%>%
                                          unique()
                  
                  # Assign values to their corresponding fields
                  fields <- underlyings[1]%>%
                              strsplit(" ")
                  
                  keys <- keywords[column_keywords %in% underlying_headers]
                  
                  # Assign values to their corresponding
                  results[keys] <- fields[[1]][seq(4,(length(keys)+1)*2, by =2)+2]%>%
                                        as.numeric()
                  
                  # Initial Value
                  initial <- fields[[1]][4]%>%
                                  as.numeric()
                  
                  results[keys] <- map2(results[keys], initial,  ~ round(.x*100 / .y, 1))
                  
                    
                  results$strike <- lapply(1:length(underlyings),
                                                    function(j){
                                                      fields <- underlyings[j]%>%
                                                        strsplit(" ")
                                                      
                                                      fields[[1]][4]})
                  results$strike <- results$strike %>%
                                                    unlist()%>%
                                                    as.numeric()%>%
                                                    sort()%>%
                                                    paste0(collapse = ',')
                    
                  
              },
              error = function(e)(print(paste0(pdf_loc, ': missing barrier/strike values'))))

              # Find the underlying tickers
              results$underlyings <-underlyings%>%
                                        str_extract_all('[A-Z]{1,5}(\\/)?\\s[A-Z]{2} ')%>% # Last space needed to identify only tickers
                                        str_extract('[A-Z]{1,5}(\\/)?\\s[A-Z]{2}')%>% # Remove the last space
                                        unlist()%>%
                                        sort()%>%
                                        paste0(collapse = ',')
            }
      }
      
      
      #2)
      # Coupon Observation and payment dates
      # Find if the Autocall Dates exist
      if(PDF%>%str_detect(keyWordDictionary$Leonteq[2])){
        
        if(PDF%>%str_detect(keyWordDictionary$Leonteq[3])){ 

          # If the above is true then find all strings between keywords 2 and 3:
          coupDates <- PDF%>%
                      str_match(paste0(keyWordDictionary$Leonteq[2],
                                       "\\s*(.*?)\\s",
                                       keyWordDictionary$Leonteq[3]))
          
          # Extract all dates and triggers
           dateTriggers <- coupDates[1]%>%
                                        str_extract_all(paste0("[0-9]{1,2}\\s", LeonteqDateFormat, 
                                                               "[*]{0,3} [0-9 . %]{4,8}\\s",
                                                               "(of the Strike Level([a-z]{1,2})?\\s)?", # If this phrase exists
                                                               LeonteqDateFormat, "[*]{0,3}",
                                                               sep = ''))
          
           # Automatically finds the first date in each line = coupon observation
           results$coupon_observation <- dateTriggers[[1]]%>%
                                            str_extract(LeonteqDateFormat)%>%
                                            lapply(dateParser)%>%
                                             unlist()%>%
                                             as.Date(origin = '1970-01-01')%>%
                                             paste0(collapse = ',')
          
          # Find the coupon payment dates
           results$coupon_payment <- dateTriggers[[1]]%>%
                               str_extract(paste0('(?<=(% (of the Strike Level([a-z]{1,2})? )?))(', 
                                                  LeonteqDateFormat, ')'))%>%
                               lapply(dateParser)%>%
                               unlist()%>%
                               as.Date(origin = '1970-01-01')%>%
                               paste0(collapse = ',')
           
           # (results$coupon_observation <- do.call('c', coupon_observation))
           # (results$coupon_payment <- do.call('c', coupon_payment))
           
        }else{ # Otherwise consider the coupon table as the only table and use keywords
          # Extract all dates and triggers
          dateTriggers <- PDF%>%
                            str_extract_all(paste0("[0-9]{1,2}\\s", LeonteqDateFormat, 
                                                   "[*]{0,3} [0-9 . %]{4,8}\\s",
                                                   "(of the Strike Level([a-z]{1,2})?\\s)?", # If this phrase exists
                                                   LeonteqDateFormat, "[*]{0,3}",
                                                   sep = ''))
                          
          # Automatically finds the first date in each line = coupon observation
          results$coupon_observation <- dateTriggers[[1]]%>%
                                  str_extract(LeonteqDateFormat)%>%
                                  lapply(dateParser)%>%
                                  unlist()%>%
                                  as.Date(origin = '1970-01-01')%>%
                                  paste0(collapse = ',')
                                
          # Find the coupon payment dates
          results$coupon_payment <- dateTriggers[[1]]%>%
                                  str_extract(paste0('(?<=(% (of the Strike Level([a-z]{1,2})? )?))(', 
                                                     LeonteqDateFormat, ')'))%>%
                                  lapply(dateParser)%>%
                                  unlist()%>%
                                  as.Date(origin = '1970-01-01')%>%
                                  paste0(collapse = ',')
                                
          # (results$coupon_observation <- do.call('c', coupon_observation))
          # (results$coupon_payment <- do.call('c', coupon_payment))
          
        }
      }
      
       #3)
      # Autocall dates within the same conditional
      # If this is true then find all strings between keywords 2 and 3:
      if(PDF%>%str_detect(keyWordDictionary$Leonteq[3])){
          autocallDates <- PDF%>%
                          str_extract(paste0(keyWordDictionary$Leonteq[3],".*"))
          
          # Extract all dates and triggers
          dateTriggers <- autocallDates%>%
                              str_extract_all(paste0("[0-9]{1,2}\\s", LeonteqDateFormat, 
                                                       "[*]{0,3} [0-9 . %]{4,7}\\s",
                                                     "(of the Strike Level([a-z]{1,2})? )?", # If this phrase exists
                                                       LeonteqDateFormat, "[*]{0,3}",
                                                       sep = ''))
          # Find all the triggers
          autocallTriggers <- dateTriggers%>%
                                str_extract_all('[0-9.]{5,7}%')%>%
                                unlist()%>%
                                gsub('%', '',.)%>%
                                as.numeric()
          
          uniqueTriggers <- unique(autocallTriggers)
          
          # Trigger override when we have more than one trigger until maturity
          results$trigger_override <- ifelse(length(uniqueTriggers)>1,
                                             TRUE,
                                             FALSE)
          
          results$autocall_trigger <- ifelse(length(uniqueTriggers) >1,
                                             autocallTriggers%>%paste0(collapse = ','),
                                             uniqueTriggers)
          
          
          # Automatically finds the first date in each line = coupon observation
          results$trigger_observation <- dateTriggers[[1]]%>%
                                          str_extract(LeonteqDateFormat)%>%
                                          lapply(dateParser)%>%
                                          unlist()%>%
                                          as.Date(origin = '1970-01-01')%>%
                                          paste0(collapse = ',')
          
          # Find the coupon payment dates
          results$trigger_payment <- dateTriggers[[1]]%>%
                                        str_extract(paste0('(?<=(% (of the Strike Level([a-z]{1,2})? )?))(', 
                                                           LeonteqDateFormat, ')'))%>%
                                        lapply(dateParser)%>%
                                        unlist()%>%
                                        as.Date(origin = '1970-01-01')%>%
                                        paste0(collapse = ',')
          
          # (results$trigger_observation <- do.call('c', trigger_observation))
          # (results$trigger_payment <- do.call('c', trigger_payment))
      } 
      #4)
      # Guaranteed coupon dates
      if(PDF%>%str_detect(keyWordDictionary$Leonteq[4])){
      
      # Find all dates relating to a specific coupon to be paid
      coupon_payment <- PDF%>%
                                str_extract_all(paste0("[A-Z]{3} [0-9.]{3,10} paid on ", LeonteqDateFormat))
      
      results$coupon_payment <- coupon_payment[[1]]%>%
                          str_extract(LeonteqDateFormat)%>%
                          lapply(dateParser)%>%
                          unlist()%>%
                          as.Date(origin = '1970-01-01')%>%
                          paste0(collapse = ',')
      
      # (results$coupon_payment <- do.call('c', coupon_payment))
      }
      
      
      # Capital Protection, Participation and Capped Participation
      # Lazy conditional. Should be individual but this works as well
      stringsTodetect <- paste0(c(capitalProtectionList,
                                  participationList,
                                  cappedProtectionList),
                                collapse = "|")
      
      #5)
      # Capital protection/Participation and Cap
      if(str_detect(PDF, stringsTodetect)){
        results$capital_protection <- PDF%>%
                                        str_extract(capitalProtectionList)%>%
                                        unlist()%>%
                                        str_extract('[0-9.]{4,8}')%>%
                                        as.numeric()
        
        results$participation <- PDF%>%
                                  str_extract(participationList)%>%
                                  unlist()%>%
                                  str_extract('[0-9.]{4,8}')%>%
                                  as.numeric()
        
        results$capped <- ifelse(is.na(PDF%>%
                                  str_detect(cappedProtectionList)), 
                            FALSE, 
                            TRUE)
        
        results$cap_level <- PDF%>%
                              str_extract(cappedProtectionList)%>%
                              unlist()%>%
                              str_extract('[0-9.]{4,8}')%>%
                              as.numeric()
      }
      
      return(results)
}
#############################################################################



"iv) Barclays function"
#############################################################################
# pdf_loc <- "termsheets/TermSheets/XS2236851700.pdf"
# parserFunction$Barclays(pdf_loc)

parserFunction$Barclays <- function(pdf_loc){
      results <- vector(mode = 'list',
                        length = length(resultNames))
      names(results) <- resultNames
      
      # Read the PDF
      PDF <- pdf_text(pdf_loc)%>%
                  str_squish()%>%
                  paste0(collapse = " ")
      
      #1) 
      # Underlyings
      if(PDF%>%str_detect(keyWordDictionary$Barclays[1])){
          underlyings <- PDF%>%
                              str_extract(paste0(keyWordDictionary$Barclays[1],
                                                 ' (.*)'))
          
          underlyings_isin <- underlyings%>%
                              str_extract_all(paste0('[A-Z]{1,5} [A-Z]{2} [A-Z]{2}[0-9]{5}|', index_tickers))
          
          results$underlyings <- underlyings_isin[[1]]%>%
                            str_extract(paste0('[A-Z]{1,5} [A-Z]{2}|', index_tickers))%>%
                            sort()%>%
                            unique()%>%
                            paste0(collapse = ',')
          
          # To be joined to the respective tickers(Ref Currency, Initial price, Strike, barrier)
          initialsTriggers <- PDF%>%
                                str_extract_all('[A-Z]{3} [0-9 ,.]{3,16} [0-9 ,.]{3,16} [0-9, .]{3,16} [0-9 ,.]{3,16} [0-9 ,.]{3,16}')
          
          # Find keywords
          keywords <- c('Initial Level', 'Strike Level',  'Interest' ,'Autocall','Knock-in')
          keys <- c('strike_level', 'coupon_trigger', 'autocall_trigger', 'barrier')
          
          # Identify key words
          index <- underlyings%>%str_detect(keywords)
          index <- which(index, TRUE)
          
          # Fill keys using index from keywords
          triggers <- initialsTriggers[[1]]%>%
                          str_split(' ')
          
          # First value is the strike
          results$strike <- lapply(1:length(triggers), function(i) triggers[[i]][2])%>%
                              gsub(",", "",.)%>%
                              as.numeric()
          
          # Next values depend on presence in the appendix table
          index = index[index != 1]
          results[keys[index-1]] <- lapply(index+1,
                                           function(j){
                                             lapply(1:length(triggers), function(i) triggers[[i]][j])
                                           })
          
          # Unlist results for each of the fields and convert ti numeric
          results[keys[index-1]] <- lapply(results[keys[index-1]], 
                                             function(x){
                                               value <- gsub(",", "", x)%>%
                                                            as.numeric()
                                               value <- round(100*value/results$strike, 1)
                                               value%>%unique()
                                               })
          results$strike <- results$strike%>%
                              sort()%>%
                              paste0(collapse = ',')
          
      }else{
        results$underlyings <- PDF%>%
                                  str_extract('(?<=(Bloomberg Code \\(for identification purposes ))([A-Z]{2,5} [A-Z]{2})')
        
        # Find the strike
        results$strike <- PDF%>%str_extract('(?<=(Initial Price" means [A-Z]{3} ))([0-9.]{2,12})')%>%
                            as.numeric()
        
        # Find the strike
        results$autocall_trigger <- PDF%>%str_extract('(?<=(Autocall Barrier" means [A-Z]{3} ))([0-9.]{2,12})')%>%
                              as.numeric()
        
        results$autocall_trigger <- round(results$autocall_trigger * 100/ results$strike, 1)
        
        # Find the barrier price
        barrier <- PDF%>%
                        str_extract('(?<=(Barrier Price" means [A-Z]{3} ))([0-9.]{2,12})')%>%
                        as.numeric()
        
        results$barrier <- round(barrier *100/results$strike, 1)
      
      }
      
      #2)
      # Conditional Coupon
      # Find if the Autocall Dates exist
      if(PDF%>%str_detect(keyWordDictionary$Barclays[2])){
            if(PDF%>%str_detect(keyWordDictionary$Barclays[3])){
                  
                  # If this is true then find all strings between keywords 2 and 3:
                  coupDates <- PDF%>%
                                str_match(paste0(keyWordDictionary$Barclays[2],
                                                 "\\s*(.*?)\\s",
                                                 keyWordDictionary$Barclays[3]))
                  
                  # Extract all dates and triggers
                  dates <- coupDates[1]%>%
                                str_extract_all(paste0(BarclaysDateFormat, '\\s',
                                                       BarclaysDateFormat,
                                                       sep = ''))
            
                  # Extract observation dates
                  results$coupon_observation <- dates[[1]]%>%
                                                    str_extract(BarclaysDateFormat)%>%
                                                    dateParser()%>%
                    paste0(collapse = ',')
                  
                  # Extract redemption dates
                  results$coupon_payment <- dates[[1]]%>%
                                                  str_extract(paste0('(?<=(',
                                                                     BarclaysDateFormat,
                                                                     ' ))(',
                                                                     BarclaysDateFormat, ')'))%>%
                                              dateParser()%>%
                    paste0(collapse = ',')
      
                  # Autocall dates within the same conditional
                  # If this is true then find all strings between keywords 2 and 3:
                  autocallDates <- PDF%>%
                                    str_extract(paste0(keyWordDictionary$Barclays[3],".*"))
                  
                  # Extract all dates and triggers
                  dates <- autocallDates%>%
                                str_extract_all(paste0(BarclaysDateFormat, "\\s", 
                                                       BarclaysDateFormat,
                                                       sep = ''))
            
                  # Extract observation dates
                  results$trigger_observation <- dates[[1]]%>%
                                                    str_extract(BarclaysDateFormat)%>%
                                                    dateParser()%>%
                    paste0(collapse = ',')
                    
                  # Extract redemption dates
                  results$trigger_payment <- dates[[1]]%>%
                                              str_extract(paste0('(?<=(',
                                                                 BarclaysDateFormat,
                                                                 ' ))(',
                                                                 BarclaysDateFormat, ')'))%>%
                                              dateParser()%>%
                                              paste0(collapse = ',')    
            }else{
              # If false then find all strings after keywords 2
              coupDates <- PDF%>%
                            str_match(paste0(keyWordDictionary$Barclays[2],
                                             "\\s*(.*?)\\s",
                                             keyWordDictionary$Barclays[3]))
                          
              # Extract all dates and triggers
              dates <- coupDates[1]%>%
                          str_extract_all(paste0(BarclaysDateFormat, "\\s", 
                                                 BarclaysDateFormat,
                                                 sep = ''))
                        
              # Extract observation dates
              results$trigger_observation <- dates[[1]]%>%
                                              str_extract(BarclaysDateFormat)%>%
                                              dateParser()%>%
                paste0(collapse = ',')
                                            
              # Extract redemption dates
              results$trigger_payment <- dates[[1]]%>%
                                            str_extract(paste0('(?<=(',
                                                               BarclaysDateFormat,
                                                               ' ))(',
                                                               BarclaysDateFormat, ')'))%>%
                                            dateParser()%>%
                paste0(collapse = ',')
              
            }
      }
      
      return(results)
}

#############################################################################                              



"v) SocGen function"
#############################################################################
exchanges <- c('New York Stock Ex-',
               'Frankfurt Stock Ex-',
               'Tokyo Stock Ex-',
               'Nasdaq Stock Ex-',
               'London Stock Ex-',
               'Six Swiss Exchange',
               'Bolsa de Madrid')%>%
                paste0(collapse = '|')

parserFunction$SG <- function(pdf_loc){
    # Read the PDF
    PDF <- pdf_text(pdf_loc)%>%
              str_squish()%>%
              paste0(collapse = " ")
    
    # Results container
    results <- vector(mode = 'list',
                      length = length(resultNames))
    names(results) <- resultNames
    
    #0)
    # Maturty Date
    final_fixing_date <- PDF%>%
                            str_extract(paste0('(?<=(Maturity Date (\\(DD/MM/YYYY\\) )?))(', SocGenDateFormat, ')'))
    
    #1)
    # Underlying bloomberg tickers, Initial fixing and strike
    if(PDF%>%str_detect('Company Bloomberg Ticker Exchange Website Strike')){
      # Example: ML FP Euronext Paris www.michelin.com EUR 91.677
      underlyings <- PDF%>%
                          str_extract_all(paste0('[A-Z0-9]{1,5} [A-Z]{2} (([A-Za-z]{2,15} ([A-Za-z]{2,15})?)|',
                                                 exchanges,
                                                  ') www\\.[a-z\\-\\.\\/]{2,30} ([A-Z]{3}|GBp) ([0-9.,]{2,9}|\\[TBD\\])'))
                        
      results$underlyings <- underlyings[[1]]%>%
                                str_extract_all('[A-Z0-9]{1,5} [A-Z]{2}')%>%
                                unlist()%>%
                                sort()%>%
                                paste0(collapse = ',')
      
      results$strike <- underlyings[[1]]%>%
                        str_extract_all('(?<=(([A-Z]{3}|GBp) ))([0-9,.]{1,12})')%>%
                        gsub(',', '', .)%>%
                        as.numeric()%>%
                        sort()%>%
                        paste0(collapse = ',')
      
    }else if(PDF%>%str_detect(keyWordDictionary$SG[1])){
    
      # Extract all the data from KEY PARAMETERS to DISCLAIMER
      PDFFilt <- PDF%>%
                    str_extract('KEY PARAMETERS\\s*(.*?) S\\(')
    
      # Pattern matching
      underlyings <- PDFFilt%>%
                                str_extract_all(paste0('([A-Z0-9]{1,5} [A-Z]{2} )|', 
                                                       index_tickers))
                                
      
      results$underlyings <- underlyings[[1]]%>%
                                str_extract(paste0('([A-Z0-9]{1,5} [A-Z]{2})|', 
                                                   index_tickers))%>%
                                sort()%>%
                                paste0(collapse = ',')
                                
      
      # Conditional on these being existent                          
      # Should correspond to the tickers
      results$strike <- PDF%>%
                        str_extract_all('(?<=((S\\(0\\) [A-Z]{3})|(S\\(0\\,[0-9]{1}\\) \\=)) )([0-9.]{2,6})')%>%
                        unlist()%>%
                        as.numeric()%>%
                        sort()%>%
                        paste0(collapse = ',')
      
    }else{
      # Extract all the data from KEY PARAMETERS to DISCLAIMER
      PDFFilt <- PDF%>%
        str_extract('KEY PARAMETERS\\s*(.*?) S\\(')
      
      # Ticker Pattern matching
      results$underlyings <- PDFFilt%>%
                                str_extract_all('[A-Z0-9]{1,5} [A-Z]{2} ')%>%
                                unlist()%>%
                                sort()%>%
                                paste0(collapse =',')
      results$strike <- PDF%>%
                          str_extract_all('(?<=(S\\(0\\) ([A-Z]{3})? ))([0-9.\\,]{2,12})')%>%
                          unlist()%>%
                          gsub(',','',.)%>%
                          as.numeric()%>%
                          sort()%>%
                          paste0(collapse = ',')
      
      if(PDF%>%str_detect(index_tickers)){
        results$underlyings <- PDFFilt%>%
                                    str_extract_all(index_tickers)%>%
                                    unlist()%>%
                                    unique()%>%
                                    sort()%>%
                                    paste0(collapse =',')
        
        results$strike <-  PDFFilt%>%
                            str_extract_all('(?<=([a-z \\.\\-\\/]{5} pts ))([0-9,\\.]{2,10})')%>%
                            unlist()%>%
                            gsub(",","",.)%>%
                            as.numeric()%>%
                            paste0(collapse = ',')
        
      }
    }
    
    #2)
    # Valuation Dates
    # Conditional on there being Valuation Dates
    if(PDF%>%str_detect(keyWordDictionary$SG[2])){
        # Extract the valuation dates from a section of string
        firstValDates <- PDF%>%
                          str_extract(paste0(keyWordDictionary$SG[2],
                                           '\\s*(.*?)\\s',
                                           keyWordDictionary$SG[3])) # Last valuation date
        
        firstValDates <- firstValDates[1]%>%
                            gsub('- ', '', .)%>%
                            str_extract_all(SocGenDateFormat)
        
        firstDates <- firstValDates[[1]]%>%
                        dateParser()
        
        # Select the final date (Written separately)
        finalDate <- PDF%>%
                      str_extract_all(paste0('(?<=(',
                                         keyWordDictionary$SG[3],
                                         '))(\\s',
                                         SocGenDateFormat,')', sep = ''))
        
        finalDate <- dateParser(finalDate[[1]][2])
        results$final_fixing_date <- finalDate
        
        # Concatenate the dates to obtain the full list
        results$trigger_observation <- c(firstDates, finalDate)%>%
                                          paste0(collapse = ',')
        
        # Use the same valuation dates in the coupon and trigger dates
        results$coupon_observation <- results$trigger_observation
        
    }else if(PDF%>%str_detect('Valuation Date\\(1\\)')){# Conditional on there only being one valuation date
        results$trigger_observation <- PDF%>%
                                        str_extract(paste0('(?<=(Valuation Date\\(1\\) ))(',
                                                           SocGenDateFormat,')'))%>%
                                        dmy()
        
        results$trigger_payment <- PDF%>%
                                        str_extract(paste0('(?<=(Maturity Date ))(',
                                                           SocGenDateFormat,
                                                           ')'))%>%
                                        dmy()
        
        results$coupon_payment <- PDF%>%
                                    str_extract(paste0('(?<=(Interest Payment Date\\(s\\) ))(',
                                                      SocGenDateFormat,
                                                      ')'))%>%
                                    dmy()%>%
                                    c(results$trigger_payment)%>% # Add the maturity date to the list of dates
                                    paste0(collapse = ',')
    }
    
    #3)
    # Interest Payment Dates
    # Conditional Coupon always comes before the early redemption
    if(PDF%>%str_detect(paste0('Rate of Interest [0-9.\\%]{2,9} payable in arrear ', keyWordDictionary$SG[4]))){
      # Select the second date
      IPDates <- PDF%>%
                  str_extract_all(paste0('(Rate of Interest [0-9.\\%]{2,9} payable in arrear )?',
                                         keyWordDictionary$SG[4],
                                         ' (.*) and the Maturity Date')) # Last valuation date
                
      results$coupon_payment <- IPDates%>%
                                  str_extract_all(SocGenDateFormat)%>%
                                  unlist()%>%
                                  c(final_fixing_date)%>%
                                  dateParser()%>%
                                  paste0(collapse = ',')
      
    }else if(PDF%>%str_detect(keyWordDictionary$SG[4])){

        # Select the second date
        IPDates <- PDF%>%
                    str_extract(paste0(keyWordDictionary$SG[4],
                           ' (.*) and the Maturity (\\(i from 1 to [0-9]{1,2}\\) )?Date')) # Last valuation date
        
        results$coupon_payment <- IPDates%>%
                                    str_extract_all(SocGenDateFormat)%>%
                                    unlist()%>%
                                    c(final_fixing_date)%>%
                                    dateParser()%>%
                                    paste0(collapse = ',')
        
    } 
    
    #4)
    # Early Redemption Dates
    # Early Redemption always comes before the general information
    if(PDF%>%str_detect(keyWordDictionary$SG[5])){
      # Select the early redemption dates
      ERDates <- PDF%>%
                  str_extract(paste0(keyWordDictionary$SG[5],
                         '\\s*(.*?)\\sGENERAL INFORMATION')) # Last valuation date
      
      results$trigger_payment <- ERDates%>%
                                    str_extract_all(paste0('(', SocGenDateFormat,
                                                           '|[0-9]{1,2}/[0-9]{2}/[0-9]{4})'))%>%
                                    unlist()%>%
                                    dateParser()%>%
                                    paste0(collapse = ',')
      
      ##
      # Modify trigger observation dates
      # Trigger Dates already obtained in section 2
      index <- ERDates%>%
                  str_extract('(?<=(Valuation Date\\(i\\) \\(i from ))([0-9]{1,2} to [0-9]{1,2})')
      
      # Find the index of trigger dates to consider (Example: Valuation(i) from 3 to 11)
      min <- index%>%
                str_extract('[0-9]{1,2}')%>%
                as.numeric()
      
      max <- index%>%
                str_extract('(?<=(to ))([0-9]{1,2})')%>%
                as.numeric()
      
      # Filter by min and max
      trigger_obs <- results$trigger_observation%>%
                          str_split(',')%>%
                          unlist()
      
      results$trigger_observation <- trigger_obs[min:(max+1)]%>%
                                        paste0(collapse = ',')
      
      # Soft Call trigger dates
    }else if(PDF%>%str_detect('Optional Redemption Date\\(i\\)')){
      # Select the early redemption dates
      ERDates <- PDF%>%
                    str_extract(paste0('Optional Redemption Date\\(i\\)',
                                       '\\s*(.*?)\\sGENERAL INFORMATION')) # Last valuation date
                  
      results$trigger_payment <- ERDates%>%
                                    str_extract_all(paste0('(', SocGenDateFormat,
                                                           '|[0-9]{1,2}/[0-9]{2}/[0-9]{4})'))%>%
                                    unlist()%>%
                                    dateParser()
      
      results$trigger_observation <- results$trigger_payment - days(7)
      
      # Dates to strings
      results$trigger_payment <- results$trigger_payment%>%
                                        paste0(collapse = ',')
      
      results$trigger_observation <- results$trigger_observation%>%
                                        paste0(collapse = ',')
    }
      
    
    #5
    # Trigger Levels
    if(PDF%>%str_detect('AutocallBarrier\\(i\\)')){
      triggers <- PDF%>%
                    str_extract_all('(?<=(Autocall Barrier\\([0-9]{1,2}\\) = (-)?))([0-9.]{1,9})')%>%
                    unlist()%>%
                    as.numeric()
      
      results$autocall_trigger <- 100 - triggers
      
      results$trigger_override <- ifelse(length(unique(triggers))>1,
                                         TRUE,
                                          FALSE)
      # Convert list to strings
      results$autocall_trigger <- results$autocall_trigger%>%
                                    paste0(collapse = ',')
      
    }else if(PDF%>%str_detect('Event WorstPerformance\\(i\\) is higher than or equal to (-)?')){
      trigger <- PDF%>%
                  str_extract(paste0('Event WorstPerformance\\(i\\) is higher than or equal to (-)?', 
                                     '[0-9]{1,2}'))%>%
                  str_extract('[0-9]{1,2}')%>%
                  as.numeric()
      
      results$autocall_trigger <- 100 - trigger

      results$trigger_override <- FALSE
    }
    
    
    # 6
    # Barrier and autocall trigger
    if(PDF%>%str_detect('Knock-In Threshold')){
      results$barrier <- PDF%>%
                          str_extract('(?<=(Knock-In(\\s)?Threshold(\\([a-z0-9]{1}\\))? ))([0-9.]{2,12})')%>%
                          as.numeric()
      
    }
    
    if(PDF%>%str_detect('WorstPerformance\\(i\\) is lower than -')){
      coupon_trigger <- PDF%>%
                          str_extract('(?<=(WorstPerformance\\(i\\) is lower than -))([0-9]{1,2})')%>%
                          as.numeric()
      results$coupon_trigger <- 100 - coupon_trigger
    }
    
    
    
    # Capital Protection, Participation and Capped Participation
    # Lazy conditional. Should be individual but this works as well
    stringsTodetect <- paste0(c(capitalProtectionList,
                                participationList,
                                cappedProtectionList),
                              collapse = "|")
    
    
    #7)
    # Capital protection/Participation and Cap
    # Removed Capital protection because of conflict with Leonteq keyword
    if(str_detect(PDF, stringsTodetect)){

      results$participation <- PDF%>%
                                str_extract(participationList)%>%
                                unlist()%>%
                                str_extract('[0-9.]{2,8}')%>%
                                as.numeric()
      
      results$capped <- PDF%>%
                          str_detect(cappedProtectionList)
      
      results$cap_level <- PDF%>%
                              str_extract(cappedProtectionList)%>%
                              unlist()%>%
                              str_extract('[0-9.]{4,8}')%>%
                              as.numeric()
    }
    
   return(results)
}

# pdf_loc <- "termsheets/socgen/TS_XS2059571203.pdf"
# pdf_loc <- 'termsheets/2) Societe Generale/CH0408526330.pdf'
# parserFunction$SG(pdf_loc)

#############################################################################



"vi) Raiffeisen function"
#############################################################################
 # pdf_loc <- "silex/DavidCyvoct/termsheets/raiffeisen/CH0499875927.pdf"

parserFunction$Raiffeisen <- function(pdf_loc){
    results <- vector(mode = 'list',
                      length = length(resultNames))
    names(results) <- resultNames
    
    # Read the PDF
    PDF <- pdf_text(pdf_loc)%>%
      str_squish()%>%
      paste0(collapse = " ")
    
    #1)
    # Extract all text between the Underlyings and Product details sections
    # Conditional on there being underlyings
    if(str_detect(PDF, keyWordDictionary$Raiffeisen[1])){
      underlyings_text <- PDF%>%
                            str_match(paste0(keyWordDictionary$Raiffeisen[1],
                                '\\s*(.*?)\\s',
                                'Product Details'))
      
        # Presence of indices in the underlying
        if(str_detect(underlyings_text[1], index_tickers)){
            results$underlyings <- underlyings_text%>%
                                    str_extract_all(index_tickers)%>%
                                    unlist()%>%
                                    unique()%>%
                                    sort()%>%
                                    paste0(collapse = ',')
            
            underlyings <- underlyings_text[1]%>%
                            str_extract_all(paste0('(', index_tickers, ') ',
                                                   '([A-Z]{3}|GBp) ([0-9.]{2,10}|TBA) ',
                                                   '(([A-Z]{3}|GBp) ([0-9.]{2,10}|TBA) )?',
                                                   '(([A-Z]{3}|GBp) ([0-9.]{2,10}|TBA) )?',
                                                   '(([A-Z]{3}|GBp) ([0-9.]{2,10}|TBA) )?'))                   
            
            # Find the corresponding columns. Order matters in this case
            column_keywords <- c('Barrier Level',
                                 'Strike Level',
                                 'Autocall Trigger',
                                 'Coupon Trigger')
            
            # Order of these keywords corresponds to the order of column_keywords
            keywords <- c('barrier', 
                          'strike_level',
                          'autocall_trigger',
                          'coupon_trigger')
            
            # Cases exist where no barrier/strike columns exists in the underlying
            tryCatch({
              # Find columns in the PDF Underlying
              underlying_headers <- underlyings_text%>%
                                      str_extract_all(column_keywords%>%paste0(collapse = '|'))%>%
                                      unlist()%>%
                                      unique()
              
              # Assign values to their corresponding fields
              fields <- underlyings[[1]]%>%
                          strsplit(" ")
              
              keys <- keywords[column_keywords %in% underlying_headers]
              
              # Assign values to their corresponding
              results[keys] <- fields[[1]][seq(3,(length(keys)+1)*2, by =2)+2]%>%
                                  as.numeric()
              
              # Initial Value
              initial <- fields[[1]][3]%>%
                            as.numeric()
              
              results[keys] <- map2(results[keys], initial,  ~ round(.x*100 / .y, 1))
              
              
              results$strike <- lapply(1:length(underlyings[[1]]),
                                       function(j){
                                         fields <- underlyings[[1]][j]%>%
                                           strsplit(" ")
                                         
                                         fields[[1]][3]})
              results$strike <- results$strike %>%
                                    unlist()%>%
                                    as.numeric()%>%
                                    sort()%>%
                                    paste0(collapse = ',')
              
              
            },
            error = function(e)(print(paste0(pdf_loc, ': missing barrier/strike values'))))
            
        
        # Conditional on there being a cap level in the text
        }else if(str_detect(PDF, 'Cap Level') ){
          underlyings <- underlyings_text[1]%>%
                            str_extract_all(paste0('(([A-Z]{1,5} [A-Z]{2} |[A-Z]{2}[0-9]{1}\\s[A-Z]{1}[a-z]{5} )',
                                   '|', index_tickers, ' )',
                                   '[A-Z]{3} ([0-9.]{2,10}|TBA) '))
          
          results$underlyings <- underlyings[[1]]%>%
                                    str_extract_all(paste0('(([A-Z]{1,5} [A-Z]{2} |[A-Z]{2}[0-9]{1}\\s[A-Z]{1}[a-z]{5} )',
                                        '|', index_tickers, ' )'))%>%
                                    unlist()%>%
                                    sort()%>%
                                    paste0(collapse = ',')
          
          results$capped = TRUE
          results$cap = underlyings[[1]]%>%
                          str_extract_all('[0-9.]{2,12}')%>%
                          unlist()
          
        }else{
            underlyings <- underlyings_text[1]%>%
                              str_extract_all(paste0('(([A-Z]{1,5}(\\/)? [A-Z]{2} |[A-Z]{2}[0-9]{1}\\s[A-Z]{1}[a-z]{5} )',
                                                     '|(', index_tickers, ' ))',
                                                     '([A-Z]{3}|GBp) ([0-9.]{2,10}|TBA) ',
                                                     '(([A-Z]{3}|GBp) ([0-9.]{2,10}|TBA) )?',
                                                     '(([A-Z]{3}|GBp) ([0-9.]{2,10}|TBA) )?',
                                                     '(([A-Z]{3}|GBp) ([0-9.]{2,10}|TBA) )?', 
                                                     '([A-Z]{3}|GBp) ([0-9.]{2,10}|TBA)'))%>%
                              unlist()
            
            # Find the corresponding columns. Order matters in this case
            column_keywords <- c('Barrier Level',
                                 'Strike Level',
                                 'Autocall Trigger',
                                 'Coupon Trigger')
            
            # Order of these keywords corresponds to the order of column_keywords
            keywords <- c('barrier', 
                          'strike_level',
                          'autocall_trigger',
                          'coupon_trigger')
            
            # Find columns in the PDF Underlying
            underlying_headers <- underlyings_text%>%
                                    str_extract_all(column_keywords)%>%
                                    unlist()%>%
                                    unique()
            
            # Assign values to their corresponding fields
            fields <- underlyings[1]%>%
                          strsplit(" ")
            
            keys <- keywords[column_keywords %in% underlying_headers]
            
            # Assign values to their corresponding              
            results[keys] <- fields[[1]][seq(4,(length(keys)+1)*2, by =2)+2]%>%
                                as.numeric()
            # Initial Value
            initial <- fields[[1]][4]%>%
              as.numeric()
            
            results[keys] <- map2(results[keys], initial,  ~ round(.x*100 / .y, 1))

            results$strike <- lapply(1:length(underlyings),
                                              function(j){
                                                fields <- underlyings[j]%>%
                                                  strsplit(" ")
                                                
                                                fields[[1]][4]})
            
            results$strike <- results$strike%>%
                                    unlist()%>%
                                    as.numeric()%>%
                                    sort()%>%
                                    paste0(collapse = ',')
              
            
            # Find the underlying tickers
            results$underlyings  <-underlyings%>%
                                      str_extract_all('[A-Z]{1,5}(\\/)?\\s[A-Z]{2} ')%>%# Last space needed to identify only tickers
                                      str_extract('[A-Z]{1,5}(\\/)?\\s[A-Z]{2}')%>% # Remove the last space
                                      unlist()%>%
                                      sort()%>%
                                      paste0(collapse = ',')
          }
    }
    
    #2)
    # Coupon Observation and payment dates
    # Find if the Autocall Dates exist
    if(PDF%>%str_detect(keyWordDictionary$Raiffeisen[2])){
      
      if(PDF%>%str_detect(keyWordDictionary$Raiffeisen[3])){ 
        # If the above is true then find all strings between keywords 2 and 3:
        coupDates <- PDF%>%
          str_match(paste0(keyWordDictionary$Raiffeisen[2],
                           "\\s*(.*?)\\s",
                           keyWordDictionary$Raiffeisen[3]))
        
        # Extract all dates and triggers
        dateTriggers <- coupDates[1]%>%
          str_extract_all(paste0("[0-9]{1,2}\\s", RaiffeisenDateFormat, 
                                 "[*]{0,3} [0-9 . %]{4,8}\\s",
                                 RaiffeisenDateFormat, "[*]{0,3}",
                                 sep = ''))
        
        # Automatically finds the first date in each line = coupon observation
        results$coupon_observation <- dateTriggers[[1]]%>%
                                        str_extract(RaiffeisenDateFormat)%>%
                                        lapply(dateParser)%>%
                                        unlist()%>%
                                        as.Date(origin = '1970-01-01')%>%
                                        paste0(collapse = ',')
        
        # Find the coupon payment dates
        results$coupon_payment <- dateTriggers[[1]]%>%
                                    str_extract(paste0('(?<=(%\\s))(', 
                                                       RaiffeisenDateFormat, ')'))%>%
                                    lapply(dateParser)%>%
                                    unlist()%>%
                                    as.Date(origin = '1970-01-01')%>%
                                    paste0(collapse = ',')
        
        # (results$coupon_observation <- do.call('c', coupon_observation))
        # (results$coupon_payment <- do.call('c', coupon_payment))
        
      }else{ # Otherwise consider the coupon table as the only table and use keywords
        # Extract all dates and triggers
        dateTriggers <- PDF%>%
          str_extract_all(paste0("[0-9]{1,2}\\s", RaiffeisenDateFormat, 
                                 "[*]{0,3} [0-9 . %]{4,8}\\s",
                                 RaiffeisenDateFormat, "[*]{0,3}",
                                 sep = ''))
        
        # Automatically finds the first date in each line = coupon observation
        results$coupon_observation <- dateTriggers[[1]]%>%
                                        str_extract(RaiffeisenDateFormat)%>%
                                        lapply(dateParser)%>%
                                        unlist()%>%
                                        as.Date(origin = '1970-01-01')%>%
                                        paste0(collapse = ',')
        
        # Find the coupon payment dates
        results$coupon_payment <- dateTriggers[[1]]%>%
                                    str_extract(paste0('(?<=(%\\s))(', 
                                                       RaiffeisenDateFormat, ')'))%>%
                                    lapply(dateParser)%>%
                                    unlist()%>%
                                    as.Date(origin = '1970-01-01')%>%
                                    paste0(collapse = ',')
        
        # (results$coupon_observation <- do.call('c', coupon_observation))
        # (results$coupon_payment <- do.call('c', coupon_payment))
        
      }
    }
    
    #3)
    # Autocall dates within the same conditional
    # If this is true then find all strings between keywords 2 and 3:
    if(PDF%>%str_detect(keyWordDictionary$Raiffeisen[3])){
      autocallDates <- PDF%>%
        str_extract(paste0(keyWordDictionary$Raiffeisen[3],".*"))
      
      # Extract all dates and triggers
      dateTriggers <- autocallDates%>%
                         str_extract_all(paste0("[0-9]{1,2}\\s", RaiffeisenDateFormat, 
                               "[*]{0,3} [0-9 . %]{4,7}\\s",
                               RaiffeisenDateFormat, "[*]{0,3}",
                               sep = ''))
      
      # Find all the triggers
      autocallTriggers <- dateTriggers%>%
                            str_extract_all('[0-9.]{5,7}%')%>%
                            unlist()%>%
                            gsub('%', '',.)%>%
                            as.numeric()
      
      uniqueTriggers <- unique(autocallTriggers)
      
      # Trigger override when we have more than one trigger until maturity
      results$trigger_override <- ifelse(length(uniqueTriggers)>1,
                                         TRUE,
                                         FALSE)
      
      results$autocall_trigger <- ifelse(length(uniqueTriggers) >1,
                                         autocallTriggers%>%paste0(collapse = ','),
                                         uniqueTriggers)
      
      # Automatically finds the first date in each line = coupon observation
      results$trigger_observation <- dateTriggers[[1]]%>%
                                        str_extract(RaiffeisenDateFormat)%>%
                                        lapply(dateParser)%>%
                                        unlist()%>%
                                        as.Date(origin = '1970-01-01')%>%
                                        paste0(collapse = ',')
      
      # Find the coupon payment dates
      results$trigger_payment <- dateTriggers[[1]]%>%
                                    str_extract(paste0('(?<=(%\\s))(', 
                                                       RaiffeisenDateFormat, ')'))%>%
                                    lapply(dateParser)%>%
                                    unlist()%>%
                                    as.Date(origin = '1970-01-01')%>%
                                    paste0(collapse = ',')
      
      
      # (results$trigger_observation <- do.call('c', trigger_observation))
      # (results$trigger_payment <- do.call('c', trigger_payment))
    } 
    
    #4)
    # Guaranteed coupon dates
    if(PDF%>%str_detect(keyWordDictionary$Raiffeisen[4])){
      
      # Find all dates relating to a specific coupon to be paid
      coupon_payment <- PDF%>%
                          str_extract_all(paste0("[A-Z]{3} [0-9.]{3,10} paid on ", RaiffeisenDateFormat))
      
      results$coupon_payment <- coupon_payment[[1]]%>%
                                  str_extract(RaiffeisenDateFormat)%>%
                                  lapply(dateParser)%>%
                                  unlist()%>%
                                  as.Date(origin = '1970-01-01')
      
      # (results$coupon_payment <- do.call('c', coupon_payment))
    }
    
    # Capital Protection, Participation and Capped Participation
    # Lazy conditional. Should be individual but this works as well
    stringsTodetect <- paste0(c(capitalProtectionList,
                                participationList,
                                cappedProtectionList),
                              collapse = "|")
    
    #5)
    # Capital protection/Participation and Cap
    if(str_detect(PDF, stringsTodetect)){
      results$capital_protection <- PDF%>%
                                      str_extract(capitalProtectionList)%>%
                                      unlist()%>%
                                      str_extract('[0-9.]{4,8}')%>%
                                      as.numeric()
      
      results$participation <- PDF%>%
                                str_extract(participationList)%>%
                                unlist()%>%
                                str_extract('[0-9.]{4,8}')%>%
                                as.numeric()
      
      results$capped <- PDF%>%
                          str_detect(cappedProtectionList)
      
      results$cap_level <- PDF%>%
                              str_extract(cappedProtectionList)%>%
                              unlist()%>%
                              str_extract('[0-9.]{4,8}')%>%
                              as.numeric()
    }
    
    
    return(results)
}
#############################################################################



"vii) Citigroup function"
#############################################################################
# notice should be given 8 days in advance for soft call
citiBusinessDaysNotice <- 8

parserFunction$Citigroup <- function(pdf_loc){
  results <- vector(mode = 'list',
                    length = length(resultNames))
  names(results) <- resultNames
  
  # Read the PDF
  PDF <- pdf_text(pdf_loc)%>%
    str_squish()%>%
    paste0(collapse = " ")
  
  # Final observation and redemption Date
  finalDate <- PDF%>%str_extract(paste0('(?<=(Final Valuation Date ))(',CitiFormat, ')'))
  matDate <- PDF%>%str_extract(paste0('(?<=(Maturity Date ))(',CitiFormat, ')'))
  
  #1
  # Find the underlyings and strike
  underlyings <- PDF%>%
                   str_extract('([0-9]{4})? The Underlyings (.*) Initial Level For')
  
  results$underlyings <- underlyings%>%
                          str_extract_all(paste0(' [A-Z0-9]{1,5} [A-Z]{2} |', index_tickers))%>%
                          str_extract_all(paste0('[A-Z0-9]{1,5} [A-Z]{2}|', index_tickers))%>%
                          unlist()%>%
                          sort()%>%
                          paste0(collapse = ',')
  #Check for null value
  a = character(0)
  strike <- PDF%>%
              str_extract_all('(?<=(Market( Inc.)?|Inc.|Exchange|Index|\\(SIBE\\))\\s)(([A-Z]{3} )?[0-9.,]{2,12})')
  
  # Directly detect index barrier values
  if(underlyings%>%str_detect('([A-Z]{3} )?[0-9.,]{2,15} ([A-Z]{3} )?[0-9.,]{2,15} ([A-Z]{3} )?[0-9.,]{2,15} ([A-Z]{3} )?[0-9.,]{2,15} ([A-Z]{3} )?[0-9.,]{2,15}')){
    results$strike <- underlyings%>%
                        str_extract_all(' ([A-Z]{3} )?[0-9.,]{2,15} ([A-Z]{3} )?[0-9.,]{2,15} ([A-Z]{3} )?[0-9.,]{2,15} ([A-Z]{3} )?[0-9.,]{2,15} ([A-Z]{3} )?[0-9.,]{2,15}')%>%
                        unlist()%>%
                        str_extract('[0-9.\\,]{3,10}')%>% # Extract first number (Initial level)
                        gsub(',', '', .)%>%
                        as.numeric()%>%
                        sort()%>%
                        paste0(collapse = ',')
    
    # Possible exception if there are only 3 barriers available
  }else if(underlyings%>%str_detect('([A-Z]{3} )?[0-9.,]{2,15} ([A-Z]{3} )?[0-9.,]{2,15} ([A-Z]{3} )?[0-9.,]{2,15} ([A-Z]{3} )?[0-9.,]{2,15}')){
    results$strike <- underlyings%>%
                          str_extract_all(' ([A-Z]{3} )?[0-9.,]{2,15} ([A-Z]{3} )?[0-9.,]{2,15} ([A-Z]{3} )?[0-9.,]{2,15} ([A-Z]{3} )?[0-9.,]{2,15}')%>%
                          unlist()%>%
                          str_extract('[0-9.\\,]{3,10}')%>% # Extract first number (Initial level)
                          gsub(',', '', .)%>%
                          as.numeric()%>%
                          sort()%>%
                          paste0(collapse = ',')
    
  }else if(underlyings%>%str_detect('([A-Z]{3} )?[0-9.,]{2,15} ([A-Z]{3} )?[0-9.,]{2,15} ([A-Z]{3} )?[0-9.,]{2,15}')){
    
    results$strike <- underlyings%>%
                          str_extract_all(' ([A-Z]{3} )?[0-9.,]{2,15} ([A-Z]{3} )?[0-9.,]{2,15} ([A-Z]{3} )?[0-9.,]{2,15}')%>%
                          unlist()%>%
                          str_extract('[0-9.\\,]{3,10}')%>% # Extract first number (Initial level)
                          gsub(',', '', .)%>%
                          as.numeric()%>%
                          sort()%>%
                          paste0(collapse = ',')
  }else{
      results$strike <- strike%>%
                          str_extract_all('[0-9.\\,]{3,10}')%>% # Extract first number (Initial level)
                          unlist()%>%
                          gsub(',', '', .)%>%
                          as.numeric()%>%
                          sort()%>%
                          paste0(collapse = ',')
  }
  
  #2
  # Autocall Coupon Triggers and Barrier
  # Coupon Trigger
  results$coupon_trigger <- PDF%>%
                              str_extract('(?<=(Coupon Barrier Level For each Underlying, ))([0-9.]{1,9})')%>%
                              as.numeric()
  
  # Barrier
  results$barrier <- PDF%>%
                      str_extract('(?<=((Final|Knock-In) Barrier Level For each Underlying, ))([0-9.]{1,9})')%>%
                      as.numeric()
                    
  # Autocall Trigger/ Trigger payment and observation dates
  if(PDF%>%str_detect('Autocall Barrier Level (Mandatory Early|Redemption Date)')){
        results$trigger_override <- TRUE
        
        # Autocall Observation and Payment dates
        results$trigger_observation <- PDF%>%
                                        str_extract('Autocall Valuation Date Autocall Barrier Level (Mandatory Early|Redemption Date) (.*)')%>%
                                        str_extract_all(paste0(CitiFormat, 
                                                               ' [0-9.]{2,9}% of the Initial Level ', 
                                                               CitiFormat))
        
        results$autocall_trigger <- results$trigger_observation[[1]]%>%
                                        str_extract(paste0('(?<=(',CitiFormat ,' ))([0-9.]{2,6})'))%>%
                                        as.numeric()%>%
                                        paste0(collapse = ',')
          
        # Trigger Payment
        results$trigger_payment <- results$trigger_observation[[1]]%>%
                                      str_extract(paste0('(?<=( Initial Level ))(', CitiFormat,')'))%>%
                                      dateParser()%>%
                                      paste0(collapse= ',')
        
        # Trigger Observation
        results$trigger_observation <- results$trigger_observation[[1]]%>%
                                          str_extract(CitiFormat)%>%
                                          dateParser()%>%
                                          paste0(collapse= ',')
        
        # Soft callable conditional
        
      }else if(PDF%>%str_detect('Optional Early Redemption')){
            # Find the start of the callable option for the emitter
            startDate <- PDF%>%
                          str_extract('Optional Early Redemption (.*) Optional Early Redemption')%>%
                          str_extract(paste0('(?<=(starting from ))(', CitiFormat ,')'))
                          
            # Find all coupon payment dates
           coupDates <- PDF%>%
                          str_extract('Coupon Payment Date .* Maturity Date Coupon Amount')%>%
                          str_extract_all(CitiFormat)%>%
                          unlist()%>%
                          c(matDate)
           
           # Find the start of the optional redemption dates
          index <- which(startDate == coupDates)
          
          # Maturity date is not included
          trigger_payment <- coupDates[index:(length(coupDates)-1)]
          
          #Trigger observation dates
          results$trigger_observation = trigger_payment%>%
                                            dateParser()%>%
                                            add.bizdays(-citiBusinessDaysNotice)%>%
                                            paste0(collapse = ',')
          
          results$coupon_observation <- results$trigger_observation
          
          # Coupon payment Dates
          results$coupon_payment <- coupDates%>%
                                      dateParser()%>%
                                      paste0(collapse = ',')
          
          # Early Redemption Dates (In case issue is called by issuer)
          results$trigger_payment <-  trigger_payment%>%
                                          dateParser()%>%
                                          paste0(collapse = ',')
          
          
      }else{
        results$autocall_trigger <- PDF%>%
                                      str_extract('(?<=(Autocall Barrier Level For each Underlying, ))([0-9.]{1,9})')%>%
                                      as.numeric()
        results$trigger_override <- FALSE
        
        # Autocall Observation and Payment dates
        results$trigger_observation <- PDF%>%
                                        str_extract('Autocall Valuation Date Mandatory Early Redemption Date (.*)')%>%
                                        str_extract_all(paste0(CitiFormat, 
                                                               ' ', CitiFormat))
        
        results$trigger_payment <- results$trigger_observation[[1]]%>%
                                      str_extract(paste0('(?<=( [0-9]{4} ))(', CitiFormat,')'))%>%
                                      c(matDate)%>%
                                      dateParser()%>%
                                      paste0(collapse= ',')
        
        results$trigger_observation <- results$trigger_observation[[1]]%>%
                                          str_extract(CitiFormat)%>%
                                          c(finalDate)%>%
                                          dateParser()%>%
                                          paste0(collapse= ',')
  }


  
  #3
  # Coupon Observation and Payment dates
  if(is.null(results$coupon_observation)){
      results$coupon_observation <- PDF%>%
                                      str_extract('Valuation Date Contingent Coupon Payment Date (.*) Final Valuation Date Maturity Date')%>%
                                      str_extract_all(paste0(CitiFormat, 
                                                             ' ', CitiFormat))
      
      results$coupon_payment <- results$coupon_observation[[1]]%>%
                                          str_extract(paste0('(?<=( [0-9]{4} ))(', CitiFormat,')'))%>%
                                          c(matDate)%>%
                                          dateParser()%>%
                                          paste0(collapse= ',')
                                        
      results$coupon_observation <- results$coupon_observation[[1]]%>%
                                        str_extract(CitiFormat)%>%
                                        c(finalDate)%>%
                                        dateParser()%>%
                                        paste0(collapse= ',')
  }
  
  return(results)
}

# Test
# pdf_loc <- 'termsheets/TermSheets/XS2284511347.pdf'
# parserFunction$Citigroup(pdf_loc)
#############################################################################



"vii) Morgan function"
#############################################################################
parserFunction$Morgan <- function(pdf_loc){
  results <- vector(mode = 'list',
                    length = length(resultNames))
  names(results) <- resultNames
  
  # Read the PDF
  PDF <- pdf_text(pdf_loc)%>%
            str_squish()%>%
            paste0(collapse = " ")
  
  #1
  # Find the underlyings and strike
  results$underlyings <- PDF%>%
                          str_extract('UNDERLYING(.*) GENERAL INFORMATION')%>%
                          str_extract_all(paste0(' ([A-Z]{1,5}|[0-9]{1,5}) [A-Z]{2} |',
                                                 index_tickers))%>%
                          str_extract_all(paste0('([A-Z]{1,5}|[0-9]{1,5}) [A-Z]{2}|', index_tickers))%>%
                          unlist()%>%
                          sort()%>%
                          paste0(collapse = ',')
  
  results$strike <- PDF%>%
                          str_extract('UNDERLYING(.*) GENERAL INFORMATION')%>%
                          str_replace('(This is not .* Global ID)', '')%>%
                          str_extract_all(' ((?!([0-9]{1,5}+(?=\\s[A-Z]{2})))[0-9.]{2,9}) ')%>%
                          unlist()%>%
                          as.numeric()%>%
                          sort()%>%
                          paste0(collapse = ',')
  
  #2 
  #Autocall observation & payment dates and triggers
  trigger_dates <- PDF%>%
                    str_extract('EARLY REDEMPTION:(.*) FINAL REDEMPTION AMOUNT:')%>%
                    str_extract_all(paste0('[0-9]{1,2} ([0-9.]{2,6}% )?([0-9.]{2,6}% )?',
                                           MorganDateFormat, ' ',
                                           MorganDateFormat))

  autocall_trigger <- trigger_dates[[1]]%>%
                                str_extract(' [0-9.]{1,6}')%>%
                                as.numeric()
  
  results$trigger_override <- length(unique(autocall_trigger)) > 1
  results$autocall_trigger <- ifelse(results$trigger_override,
                                     autocall_trigger%>%paste0(collapse = ','),
                                     unique(autocall_trigger))
  
  # Select the date after the trigger observation Date
  results$trigger_observation <- trigger_dates[[1]]%>%
                                    str_extract(MorganDateFormat)%>%
                                    as.Date(format = '%d %B %Y')%>%
                                    paste0(collapse = ',')
  
  results$trigger_payment <- trigger_dates[[1]]%>%
                                str_extract(paste0('(?<=([0-9]{4} ))(', MorganDateFormat, ')'))%>%
                                as.Date(format = '%d %B %Y')%>%
                                paste0(collapse = ',')

  #3
  #Coupon observation & payment dates and triggers 
  if(PDF%>%str_detect('Number Coupon Barrier')){
        trigger_dates <- PDF%>%
                            str_extract('Interest Observation(.*) EARLY REDEMPTION:')%>%
                            str_extract_all(paste0('[0-9]{1,2} ([0-9.]{2,6}% )?([0-9.]{2,6}% )?',
                                                   MorganDateFormat, ' ',
                                                   MorganDateFormat))
        
        coupon_trigger <- trigger_dates[[1]]%>%
                                str_extract(' [0-9.]{1,6}')%>%
                                as.numeric()
        
        trigger_override <- length(unique(coupon_trigger)) > 1
        
        results$coupon_trigger <- ifelse(trigger_override,
                                           coupon_trigger%>%paste0(collapse = ','),
                                           unique(coupon_trigger))
                              
        results$coupon_observation <- trigger_dates[[1]]%>%
                                          str_extract(MorganDateFormat)%>%
                                          as.Date(format = '%d %B %Y')%>%
                                          paste0(collapse = ',')
                                        
        results$coupon_payment <- trigger_dates[[1]]%>%
                                      str_extract(paste0('(?<=([0-9]{4} ))(', MorganDateFormat, ')'))%>%
                                      as.Date(format = '%d %B %Y')%>%
                                      paste0(collapse = ',')
        
        results$coupon_value <- trigger_dates[[1]]%>%
                              str_extract('([0-9.]+(?=\\% [0-9]{1,2} [A-Z]{1}))')%>%
                              unique()%>%
                              as.numeric()
        
        
  }else if(PDF%>%str_detect('Coupon Coupon Payment Date')){
        trigger_dates <- PDF%>%
                            str_extract('Coupon payout(.*) EARLY REDEMPTION:')%>%
                            str_extract_all(paste0('[0-9]{1,2} ([0-9.]{2,6}% )?([0-9.]{2,6}% )?',
                                                   MorganDateFormat))
        
        results$coupon_payment <- trigger_dates[[1]]%>%
                                    str_extract(MorganDateFormat)%>%
                                    as.Date(format = '%d %B %Y')
        
        results$coupon <- trigger_dates[[1]]%>%
                            str_extract('([0-9.]+(?=\\%))')%>%
                            unique()%>%
                            as.numeric()
        
        results$coupon_observation <- results$trigger_observation
  }
  
  #4
  # Find the barrier
  results$barrier <- PDF%>%
                      str_extract("(?<=(Barrier Level\\” means ))([0-9.]{2,5})")%>%
                      as.numeric()
    
  results$put_strike <- PDF%>%
                          str_extract("(?<=(Geared Put Strike\\” means ))([0-9.]{2,5})")%>%
                          as.numeric()
      

  return(results)                
}
# 
# # # Test
# pdf_loc <- 'termsheets/5) Morgan Stanley/XS2239708857.pdf'
# parserFunction$Morgan(pdf_loc)
#############################################################################



"viii) Credit function"
#############################################################################
parserFunction$Credit <- function(pdf_loc){
    results <- vector(mode = 'list',
                      length = length(resultNames))
    names(results) <- resultNames
    
    # Read the PDF
    PDF <- pdf_text(pdf_loc)%>%
      str_squish()%>%
      paste0(collapse = " ")
    
    #1
    # Find the underlyings and strike
    results$underlyings <- PDF%>%
                        str_extract_all(paste0('(([A-Z0-9]{1,5}\\s[A-Z]{2})|',
                                               index_tickers,')(?= (Index|Equity) [A-Z]{2}[0-9]{5})'))%>%
                        unlist()%>%
                        sort()%>%
                        paste0(collapse = ',')
    

    
    if(PDF%>%str_detect('Name( \\[i\\])? Initial Level( \\[i\\])? (Strike Level|Coupon Barrier Level)')){
      results$strike <- PDF%>%
                          str_extract('Initial Level (.*) Final Level')%>%
                          str_extract_all('([A-Z]{3} [0-9,.]{2,12} [A-Z]{3} )')%>%
                          str_extract_all('[0-9,.]{2,12} ')%>%
                          unlist()%>%
                          gsub(",", '',.)%>%
                          as.numeric()%>%
                          sort()%>%
                          paste0(collapse = ',')
    }else{
      results$strike <- PDF%>%
                          str_extract('Initial Level (.*) Final Level')%>%
                          str_extract_all('(?<=((Index|Equity) ))([0-9.,]{2,10})')%>%
                          gsub(",", '',.)%>%
                          as.numeric()%>%
                          sort()%>%
                          paste0(collapse = ',')
    }
    
    #2
    # Autocall valuation, payment and triggers
    results$trigger_observation <- PDF%>%
                                    str_extract('Valuation Dates (.*) Maturity Date')%>%
                                    str_extract_all(paste0('(?<=([0-9]{1,2} ))(', CreditDateFormat, ' (?!\\(Non-Callable\\)))'))%>% # Remove non callable dates
                                    unlist()%>%
                                    as.Date(format = '%d %b %Y')%>%
                                     paste0(collapse=',')
    
    results$trigger_payment <- PDF%>%
                                      str_extract('Trigger Payment Date (.*) Settlement Currency')%>%
                                      str_extract_all(paste0('(?<=([0-9]{1,2} ))(', CreditDateFormat, ')'))%>%
                                      unlist()%>%
                                      as.Date(format = '%d %b %Y')%>%
                                      paste0(collapse=',')

    autocall_trigger <- PDF%>%
                          str_extract('Trigger Barrier Level(.*) Trigger Event:')%>%
                          str_extract_all('([0-9.]+(?=\\%))')%>%
                          unlist()%>%
                          as.numeric()

    results$trigger_override <- length(unique(autocall_trigger)) > 1
    
    results$autocall_trigger <- ifelse(results$trigger_override,
                                       autocall_trigger%>%paste0(collapse=','),
                                       unique(autocall_trigger))
    results$coupon_observation = results$trigger_observation
    
    #3
    # Coupon valuation and triggers
    results$coupon_payment <- PDF%>%
                                str_extract('Interest Payment Date(.*) Trigger Payment')%>%
                                str_extract_all(paste0('(?<=([0-9]{1,2} ))(', CreditDateFormat, ')'))%>%
                                unlist()%>%
                                as.Date(format = '%d %b %Y')%>%
                                paste0(collapse=',')    
    
    # if(is.null(results$coupon_observation)){
    #   results$coupon_observation <- results$trigger_observation
    # }
    # Barrier
    results$barrier <- PDF%>%
                        str_extract('(?<=(Knock-In Barrier: ))([0-9.]{2,6})')%>%
                        as.numeric()
    
    
    # Trigger values
    # Coupon Barrier
    if(PDF%>%str_detect('Coupon Barrier Level \\[i\\]:')){
     results$coupon_trigger <- PDF%>%
                                str_extract('(?<=(Coupon Barrier Level \\[i\\]: ))([0-9.]{2,6})')%>%
                                as.numeric()%>%
                                paste0(collapse =',')
    }
    
    
    return(results)
    
}

# Test
# pdf_loc <- 'termsheets/9) Credit Suisse/XS2289399789.pdf'
# parserFunction$Credit(pdf_loc)
#############################################################################
