source('keywords.R')

"1. GENERAL VARIABLE DICTIONARY"
####################################################
# Issuer List to be used to parse Issuer
issuerList <- c('BNP', 'EFG', 'Leonteq', 'Barclays', 'SG', 'Raiffeisen', 'Citigroup', 'Morgan', 'Credit')%>%
                  paste0(collapse = '|')

# Text for settlement type/method
settlementList <- c('Settlement Type','Settlement Method')%>%
                    paste0(collapse = '|')

# Notional Amount
notionalAmountList <- c('Aggregate Nominal',
                        'Principal',
                        'Aggregate Nominal Amount(:)?', 
                        'Aggregate Nominal Amount: Up to', 
                        'Aggregate Nominal Amount of the',
                        'Issue Size', 'Issue Amount')%>%
                        paste0(collapse = '|')

# Denomination
denominationList <- c('Denomination',
                      'Denomination \\(Denom\\):',
                      'Denomination \\(Par\\)', 
                      'Minimum Tradable Amount',
                      '\\=', 
                      'Specified Currency / Specified' # Societe Generale
                      )%>%
                          paste0(collapse = '|')

# The type of security
# Any mention of the names on these list should imply that thet descibe the asset
currencyHedge <- c('Quanto')%>%
                  paste0(collapse = '|')

subtype <- c('Warrant', 'Certificate', 'Note')%>%
               paste0(collapse = '|')

# Listing
listingList <- c('Listing////Exchange',
                 'Listing and Admission to Trading',
                 'Listing////Trading')%>%
                  paste0(collapse = '|')


# Specify the dates
issueDateList <- c('Issue Date',
                   'following the Initial Setting Date \\(expected to be')%>%
                    paste0(collapse ='|')

initialFixingDate <- c('Initial Fixing Date', 
                       'Initial Setting Date:',
                       'Trade Date',
                       'Valuation Date\\(0\\)')%>%
                        paste0(collapse = '|')

redemptionDateList <- c('\\) Redemption Date',
                        'following the Final Valuation Date \\(expected to be',
                        'Maturity Date( \\(DD/MM/YYYY\\))?',
                        'Redemption Date')%>%
                    paste0(collapse = '|')

# Societe Generale has a particular date format. Done in the individual parser function
finalFixingDate <- c('(Final Valuation" means\\,)',      # Barclays
                     '(Final Valuation Date" means)', # Barclays
                  '(Final Fixing Date)',               # EFG, Leonteq
                  '(Redemption Valuation Date)',
                  "(Determination Date)",             # Morgan Stanley
                  '(Final Valuation Date)',           # Citigroup
                  '(Valuation Date\\([1-9]{1,2}\\))',
                  'including the Final Valuation Date \\(expected to be', # Credit Suisse
                  '(Redemption Valuation)')%>%         # BNP
                  paste0(collapse = '|')

# Coupon Rate
couponRateList <- c('Coupon Rate', 
                    'Interest Rate \\=',
                    'Coupon Amount',
                    'Where Coupon is:',                                         # Credit Suisse
                    'Valuation Date is at or above its Interest Barrier:',     # Barclays
                    'Coupon payout',                                           # Morgan Stanley
                    '\\(corresponding to approximately',                       # Citigroup
                    'CONDITIONAL COUPON',
                    'in respect of the relevant Interest Payment Date:',
                    'Specified Denomination x \\(i x',                          # Societe generale
                    'Coupon N x',
                    'Certificate: N x', # Conditional Coupon for BNP
                    'Rate of Interest' # SocGen
                    )%>%
                     paste0(collapse = '|')

# DATE FORMATS
dateFormats <- c(BNPDateFormat,
                 LeonteqDateFormat,
                 SocGenDateFormat,
                 EFGDateFormat,
                 RaiffeisenDateFormat,
                 BarclaysDateFormat,
                 CitiFormat,
                 CreditDateFormat,
                 '[0-9]{2}-[A-Za-z]{3,12}-[0-9]{2}')%>% #Old Barclays format
                  paste0(collapse = ')|(')


# Barrier Type
barrierTypeList <- c('Barrier Observation at maturity only|Autocallable|Phoenix')%>%
                      paste0(collapse = '|')
barrierTypeListUS <- c('American barrier')%>%
                      paste0(collapse = '|')
  

# Valoren 
valorenList <- c('Valoren',
                 'Valoren:',
                 'Swiss Security Number')%>%
                  paste0(collapse = "|")

softCallList <- c('Redemption at the option of the Issuer',   # Societe generale
                  'Issuer Callable',                          # Barclays
                  'Optional Exercise Date')%>%
                   paste0(collapse = "|")


couponPerAnnumList <- c('p\\.a\\.' # Leonteq, EFG & Raiffeisen
                        )%>%
                        paste0(collapse = "|")

dateFormats <-paste0('(',dateFormats, ')')

couponMemory <- c("Memory Coupon",
                  "(%\\) - SumCouponsPaid\\(i-1\\))",
                  "(Previously Paid Coupons means)")%>%
                      paste0(collapse = '|')

####################################################




"2. UNIVERSAL PARSER FUNCTION"
####################################################
TSParser <- function(pdf_loc){
  tryCatch({
  # Read the PDF
  PDF <- pdf_text(pdf_loc)%>%
            str_squish()%>%
            paste0(collapse = " ")%>%
            str_replace_all(.,"(?<=([0-9]{1}))(th|rd|nd|st)", "")%>%
            gsub("\\sDaten", "",.)

  # Vector of results
  results <- vector('list', length = length(fields_list))%>%
              lapply(replace, NULL, ' ')
  names(results) <- fields_list

   # Issuer
  results$issuer_name <- str_extract( PDF,
                          pattern = paste0("(?<=Issuer(:)?\\s)(",issuerList,")\\s[a-zA-Z]+"))
  results$issuer <- results$issuer_name
  
   # Ensure that the issuer exists in the list      
   if(!is.na(results$issuer)){          

        
        # Values to be changed if condition os false
        results$cap_protection <- FALSE
        results$capped <- FALSE
        results$bonus_level <- FALSE
        
        # Find the ISIN
        results$ISIN <- str_extract(PDF, 
                                    pattern = "(?<=(ISIN|ISIN\\:|ISIN Code)\\s)([A-Z]{2})([0-9]{9,11})")
        
        # Dates from the individual parser functions
        #####################################################
        # Select the first string from result$issuer
        issuer <- results$issuer_name%>%
                        str_split(' ')
        issuer = issuer[[1]][1]
        
        results[resultNames] <- parserFunction[[issuer]](pdf_loc)
        #####################################################
        
        # Soft Callable
        results$soft_call <- PDF%>%
                              str_detect(softCallList)

         # Find whether this is an autocall
         results$autocall <- ifelse(str_detect(PDF, '.utocallable|Phoenix|Autocall'),
                                   TRUE, 
                                   FALSE)
         
            
         # Find the denomination
         results$denomination <- str_extract(PDF,
                                     pattern = paste0("(?<=(", denominationList,")\\s)([A-Z]{3}\\s[0-9 ’',]{1,12})"))%>%
                                    str_extract("[0-9’ ',]{1,12}")%>%
                                    gsub("'|\\s|,|’", "", .)%>%
                                    as.numeric()
      
         # Currency hedge
         results$currency_hedge <- ifelse(str_detect(PDF, currencyHedge),
                                          "Quanto",
                                          NA)
         
         
         # Product type (contingent on there being keywords in the text)
         # Worst of using key word
         if(str_detect(PDF,"Worst Performance|Worst-of|Worst Of|Worst Performing of")){
            results$product_type <- "Worst_Of"
            
         }else if(str_detect(PDF,"Best Performance|Best-of|Best Of|Best Performing of")){
            # Best of using key word
            results$product_type <- "Best_Of"
            
         }else if(str_detect(PDF,"the Underlying, as described|Reference Asset on the Final|the official closing price of the Underlying|Underlying Asset:")){
            results$product_type <- "Mono"
         }
         
         # Asset sub type (contingent on there being keywords in the text)
         if(grepl(subtype, PDF)){
            product_subtype <- PDF%>%
                              str_extract(subtype)%>%
                              unlist()
            
            results$sub_type <- product_subtype
         }
         
         # Find the issue price
         results$issue_price <- str_extract(PDF,
                                   pattern = "(?<=(Issue Price(:)?\\s)|(Issue Price per\\s))([0-9.]{2,5})")%>%
                                    as.numeric()
         
         
         # Find the investment Currency from the notional
         results$investment_currency <- str_extract(PDF,
                                                    pattern =  paste0("(?<=(", notionalAmountList,")\\s)([A-Z]{3}\\s[0-9,'’]{4,15})"))%>%
                                                      str_extract('[A-Z]{3}')
            
            
         # Find the settlement method
         if(grepl(settlementList,PDF)){
            cash <- PDF%>%
                     str_detect("(c|C)ash (s|S)ettlement")
            
            # String of conditionals for each emitter
            physical <- PDF%>%
                           str_detect("(d|D)elivery of (u|U)nderlying") # leonteq, EFG, Raiffeisen
           
            # Conditional on both or either being true 
            results$settlement_type <- ifelse(physical && cash, 
                                              "Physical/Cash",
                                              ifelse(physical,
                                                      "Physical",
                                                      "Cash"))
         }
         
         # Find the barrier type
         if(PDF%>%str_detect(barrierTypeList)){
           results$barrier_type <- 'EU'  
         }else if(PDF%>%str_detect(barrierTypeListUS)){
           results$barrier_type <- 'US'
         }
         
         # Find the Valoren Code
         if(str_detect(PDF, valorenList)){
            valoren <- PDF%>%
                        str_extract(paste0('(', valorenList,') ([0-9]{5,12})'))%>%
                        unlist()%>%
                        str_extract('[0-9]{5,12}')
            
            results$valoren <- valoren
         }
         
                                         
         
         # Find the notional and currency
         results$investment_nominal <- str_extract(PDF,
                                         pattern =  paste0("(?<=(", notionalAmountList,")\\s)([A-Z]{3}\\s[0-9,'’\\s]{5,15})"))%>%
                                          str_extract("[0-9,'’\\s]{1,15}")%>%
                                             gsub("'|\\s|,|’", "", .)%>%
                                             as.numeric()
         # Find the Initial fixing date
         results$initial_fixing_date <- str_extract(PDF,
                                           pattern =  paste0("(?<=(",initialFixingDate,")\\s)(", dateFormats,")"))%>%
                                             dateParser()%>%
                                             as.Date(origin = '1970-01-01')
         
         # Find the Final fixing date
         results$final_fixing_date <- str_extract(PDF,
                                                pattern =  paste0('(?<=(',finalFixingDate,')\\s)(', dateFormats,')'))%>%
                                                dateParser()%>%
                                                as.Date(origin = '1970-01-01')
         
         # Find the Issue Date
         results$issue_date <- str_extract(PDF,
                                  pattern =  paste0("(?<=(",issueDateList,")\\s)(", dateFormats,")"))%>%
                                 dateParser()%>%
                                 as.Date(origin = '1970-01-01')
      
         # Find the Redemption Date
         results$redemption_date <- str_extract(PDF,
                                       pattern =  paste0("(?<=(", redemptionDateList,")\\s)(", dateFormats,")"))%>%
                                       dateParser()%>%
                                       as.Date(origin = '1970-01-01')
         
         #Find the listing or the exchange
         listing <- str_extract(PDF,pattern = paste0('(?<=(',listingList,')\\s)([a-zA-Z]+)'))
         results$listing <- ifelse(is.na(listing), FALSE, TRUE)
         
         # Conditional on there being a coupon, find coupon frequency and rate
         #Coupon conditionality
         #Guaranteed coupon
         guaranteeConditional <- c('((.uaranteed|nconditional) coupon)','
                                   ( paid on [0-9]{1,2} [A-Za-z]{3,12} [0-9]{4})',
                                   'FIXED COUPON',
                                   'Interest Rate =',
                                   'On each Coupon Payment Date, investors will receive a coupon amount')%>% # Citigroup
                          paste0(collapse = '|')
         
         Guaranteed <- str_detect(PDF,
                                  pattern = guaranteeConditional)
         
         Conditional <- str_detect(PDF,
                                    pattern = '(c|C)onditional (c|C)oupon|CONDITIONAL COUPON|Contingent Coupon Amount|Coupon Barriern|Conditional Interest')
         
      
         results$coupon_type <- ifelse(Guaranteed,
                                       "Guaranteed",
                                       ifelse(Conditional,
                                              "Conditional",
                                              NA))
         # Coupon Rate
         results$coupon <- str_detect(PDF, paste0("(", couponRateList, ")"))
          
         if(results$coupon){
                    # Added this conditional foor Morgan Stanley or other issuers where coupon value defined in keywords
                  if(is.null(results$coupon_value)){
                    results$coupon_value <- str_extract(PDF,
                                               pattern =  paste0("(?<=(", couponRateList, ")\\s)([0-9.]{1,6})"))%>%
                                                as.numeric()
                  }
                # Find if the coupon is annual^
                results$cpPa <- str_extract(PDF,
                                           pattern =  paste0("(?<=(", couponRateList, ")\\s)([0-9.]{1,6}% (p.a.|per annum))"))%>%
                                            str_extract('[0-9.]{1,6}')%>%
                                            as.numeric()
           
         }
         
         
         # Coupon Frequency
         results$frequency <- str_extract(PDF,
                                   pattern =  "(?<=Coupon Frequency\\s)([a-zA-Z]+)")
        
         
         # Coupon Memory (SumCouponsPaid used in SG termsheet)
         results$coupon_memory <- str_detect(PDF, pattern =  couponMemory)
         
         # Upside and downside participation
         results$downside_participation <- ifelse(PDF%>%str_detect('Increased (d|D)ownside (r|R)isk'),
                                                  TRUE,
                                                  FALSE)
         
         results$upside_participation <- ifelse(PDF%>%str_detect('Increased (u|U)pside (r|R)isk'),
                                                  TRUE,
                                                  FALSE)
         
         
        return(results)

  }else{
     results$ISIN <- str_extract(PDF, 
                 pattern = "(?<=(ISIN|ISIN\\:)\\s)([A-Z]{2})([0-9]{9,11})")
     
     results$ISIN <- ifelse(is.na(results$ISIN), 'ISIN MISSING', results$ISIN)
     
      print(paste0("Issuer unmapped: ", results$ISIN))
     return(results)
   }},
  
  error = function(e){
    print(paste0(pdf_loc, 
          ": Error Loading File"))
    return(pdf_loc)
    })
  
}
  
# Tests
#########################
# pdf_loc <- 'termsheets/barclays/XS1812387634.pdf'
# pdf_loc <- 'termsheets/barclays/XS2149634425.pdf'
# pdf_loc <- 'termsheets/barclays/XS2111142316.pdf'
# pdf_loc <- 'termsheets/barclays/XS2149634425.pdf'
# pdf_loc <- "silex/DavidCyvoct/localParser/termsheets/TermSheets/termsheet-ch0506332128-en.pdf"
# pdf_loc <- "termsheets/TermSheets/termsheet-ch0506332128-en.pdf"
# pdf_loc <- 'termsheets/TermSheets/XS2242005101.pdf'
# TSParser(pdf_loc)
# BDDCurler('CH0557001036')

# Parse all TS
# pdf_loc <- 'termsheets/barclays/Barclays_InterBank_Termsheet__BCIS676091_.pdf'
# pdfs <- list.files(pattern = ".pdf$", recursive = TRUE)
# pdfList <- pdfs%>%
#             str_extract_all('[A-Z]{2}[0-9]{10}')%>%
#             unlist()
# 
# DBData <<- lapply(1:length(pdfList),
#                   function(i){
#                      BDDCurler(pdfList[i])
#                   })

# fields <- vector(mode = "list",
#                  length(pdfs))
# names(fields) <- pdfs
# lapply(150:length(pdfs),function(i){
#       TSParser(pdf_loc = pdfs[i])
#       print(i)})

# Test BDD Curler vs TS Parser
# test <- BDDCurler('ch0407978383')
# parsed <- TSParser("termsheets/efg/ch0407978383.pdf")
# pdf_loc <- "termsheets/raiffeisen/CH0461202142.pdf"
