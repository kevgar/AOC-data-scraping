get_query <- function(begin_date="01/17/2018", end_date="01/17/2018", case_type="CD"){
    
    # begin_date <- "01/17/2018"
    # end_date <- "01/17/2018"
    
    baseURL <- "https://caseinfo.aoc.arkansas.gov/cconnect/PROD/public/ck_public_qry_doct.cp_dktrpt_new_case_report?backto=C&case_id="
    
    
    queryURL <- paste0("&begin_date=", begin_date, "&end_date=", end_date,
                       "&county_code=ALL&cort_code=ALL&locn_code=ALL&case_type=",
                       case_type,"&docket_code=")
    
    url <- paste0(baseURL,queryURL)
    
    return(url)
    
}

# Get data for specified time period and case type
library(rvest)

doc <- read_html(get_query("01/01/2017","01/07/2017", "CF%20-%20PROPERTY%20FORFEITURE"), options = "HUGE")
# doc <- read_html(get_query("01/18/2017","01/18/2018", "CF%20-%20PROPERTY%20FORFEITURE"), options = "HUGE")

# Extract column names
columnNames <- html_nodes(doc, "th") %>% html_text()
# columnNames
# [1] "Case Action Filing Date"
# [2] "Case Id / Description"  
# [3] "Case Type Description"  
# [4] "Judge"                  
# [5] "Court (Court/Location)" 


# Extract data for each column
case_action_filing_date <- html_nodes(doc, "td:nth-child(1)") %>% html_text() %>% .[-c(1:6)]
temp                    <- html_nodes(doc, "td:nth-child(2)") %>% html_text() %>% .[-c(1:6)] 
case_id                 <- trimws(substr(temp,1,10), "both")
case_description        <- trimws(substr(temp,11,nchar(temp)), "both")
pat                     <- "\\$\\(?[0-9.,]+\\)?"
hasDollars              <- grepl(pattern = pat, x = case_description)
dollar_ammount          <- ifelse(hasDollars,
                                  regmatches(case_description,regexpr(pat,case_description)),
                                  NA_character_)
dollar_ammount          <- gsub(pattern="[$,]", replacement="", dollar_ammount, perl=TRUE)
case_type_description   <- html_nodes(doc, "td:nth-child(3)") %>% html_text() %>% .[-c(1:6)]
judge                   <- html_nodes(doc, "td:nth-child(4)") %>% html_text() %>% trimws(which="both")
court_location          <- html_nodes(doc, "td:nth-child(5)") %>% html_text()


# Create dataframe
df <- data.frame(
    case_action_filing_date=case_action_filing_date,
    case_id=case_id,
    case_description=case_description,
    dollar_ammount=as.numeric(dollar_ammount),
    case_type_description=case_type_description,
    court_location=court_location,
    stringsAsFactors = F)


# Write dataframe to csv
write.csv(df,"AOC_Extract.csv", row.names = FALSE)

# Convert list of row elements to dataframe

