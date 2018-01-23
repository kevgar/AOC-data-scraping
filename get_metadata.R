get_query <- function(start_dayMonthYear, end_dayMonthYear, case_type){
    
    # Properly format user inputs for the query url
    start_dayMonthYear <- gsub(pattern="[-. ]", replacement = "/", start_dayMonthYear)
    end_dayMonthYear <- gsub(pattern="[-. ]", replacement = "/", end_dayMonthYear)
    case_type <- gsub(pattern=" ", replacement = "%20", case_type)
    
    baseURL <- "https://caseinfo.aoc.arkansas.gov/cconnect/PROD/public/ck_public_qry_doct.cp_dktrpt_new_case_report?backto=C&case_id="
    
    queryURL <- paste0("&begin_date=", start_dayMonthYear, "&end_date=", end_dayMonthYear,
                       "&county_code=ALL&cort_code=ALL&locn_code=ALL&case_type=",
                       case_type,"&docket_code=")
    
    url <- paste0(baseURL,queryURL)
    
    return(url)
    
}

# Get data for specified time period and case type
library(rvest)

start_dayMonthYear <- "04/01/1989"
end_dayMonthYear <- "01/20/2018"
case_type <- "CF - PROPERTY FORFEITURE"

# Query the url
doc <- read_html(get_query(start_dayMonthYear,end_dayMonthYear,case_type),options = "HUGE")

# Get the column names
html_nodes(doc, "th") %>% html_text()
# [1] "Case Action Filing Date"
# [2] "Case Id / Description"  
# [3] "Case Type Description"  
# [4] "Judge"                  
# [5] "Court (Court/Location)" 

# Get data from "Cases Filed by Date Search" results
case_action_filing_date    <- html_nodes(doc, "td:nth-child(1)") %>% html_text() %>% .[-c(1:6)]
case_action_filing_date    <- as.Date(case_action_filing_date, format="%d-%B-%y")
temp                       <- html_nodes(doc, "td:nth-child(2)") %>% html_text() %>% .[-c(1:6)] 
case_id                    <- regmatches(temp,regexpr("(^[^ ]+)", temp))
case_description           <- trimws(gsub(pattern = "(^[^ ]+)",replacement="",temp))
pat                        <- "\\$\\(?[0-9.,]+\\)?"
areDollars                 <- which(grepl(pat, case_description, perl=TRUE))
dollar_ammount             <- rep(NA_character_, length(case_description))
dollar_ammount[areDollars] <- regmatches(case_description,regexpr(pat,case_description))
dollar_ammount[areDollars] <- gsub(pattern="[$,]",replacement="",dollar_ammount[areDollars],perl=TRUE)
case_type_description      <- html_nodes(doc, "td:nth-child(3)") %>% html_text() %>% .[-c(1:6)]
judge                      <- html_nodes(doc, "td:nth-child(4)") %>% html_text() %>% trimws(which="both")
court_location             <- html_nodes(doc, "td:nth-child(5)") %>% html_text()

# Get data from docket reports
docket_report_url          <- paste0("https://caseinfo.aoc.arkansas.gov/cconnect/PROD/public/ck_public_qry_doct.cp_dktrpt_frames?backto=C&case_id=",case_id,"&begin_date=&end_date=")

# test
read_html(docket_report_url[2]) #%>% html_nodes("table, tbody, tr, td")

# if case was monetary, extract the claimant name
# claimant <- 
# if case was monetary, extract the claimant name
# claimant_id <- 

# defendant_name
# defendant_id
# judge_id

# Create dataframe
df <- data.frame(
    case_action_filing_date=case_action_filing_date,
    case_id=case_id,
    case_description=case_description,
    dollar_ammount=as.numeric(dollar_ammount),
    case_type_description=case_type_description,
    court_location=court_location,
    docket_report_url=docket_report_url,
    stringsAsFactors = F)

# Write dataframe to .csv
outFileName <- gsub(pattern = "/",replacement="-",paste0("AOC_Extract_",start_dayMonthYear,"_",end_dayMonthYear,".csv"), fixed=T)
write.csv(df,outFileName, row.names = FALSE)
