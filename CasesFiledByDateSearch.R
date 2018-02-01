
CasesFiledByDateSearch <- function(start_MMDDYYYY, end_MMDDYYYY, case_type){

    # use parameters to construct query url
    get_query <- function(...){
        
        # format user inputs for the query url
        start_MMDDYYYY <- gsub(pattern="[-. ]", replacement = "/", start_MMDDYYYY)
        end_MMDDYYYY <- gsub(pattern="[-. ]", replacement = "/", end_MMDDYYYY)
        case_type <- gsub(pattern=" ", replacement = "%20", case_type)
        
        baseURL <- "https://caseinfo.aoc.arkansas.gov/cconnect/PROD/public/ck_public_qry_doct.cp_dktrpt_new_case_report?backto=C"
        
        queryURL <- paste0("&begin_date=", start_MMDDYYYY, 
                           "&end_date=", end_MMDDYYYY,
                           "&county_code=","ALL",
                           "&cort_code=","ALL",
                           "&locn_code=","ALL",
                           "&case_type=",case_type)
        
        url <- paste0(baseURL,queryURL)
        
        return(url)
        
    }
    
    query_string <- get_query(start_MMDDYYYY, end_MMDDYYYY, case_type)
    
    # Get data for specified time period and case type
    library(rvest)
    
    # Query the url
    doc <- read_html(query_string, options = "HUGE")
    
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
    judge                      <- html_nodes(doc, "td:nth-child(4)") %>% html_text() %>% trimws()
    court_location             <- html_nodes(doc, "td:nth-child(5)") %>% html_text()
    docket_report_url          <- paste0("https://caseinfo.aoc.arkansas.gov/cconnect/PROD/public/ck_public_qry_doct.cp_dktrpt_docket_report?backto=C&case_id=",case_id)
    
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
    
    return(df)
    
    }

# system.time(df <- CasesFiledByDateSearch(start_MMDDYYYY = "07/26/1989", end_MMDDYYYY = "01/31/2018", case_type = "CF - PROPERTY FORFEITURE"))

# Write dataframe to .csv
# outFileName <- gsub(pattern="/", replacement="-", paste0("AOC_Extract_", start_MMDDYYYY, "_", end_MMDDYYYY, ".csv"), fixed=T)
# write.csv(df,outFileName, row.names = FALSE)

###############
# Compare CourtConnect data and FOIA'd data
# Read in both files
df <- read.csv("CAF_07261989_01202018.csv", stringsAsFactors = FALSE)
df_subset <- df[year(df$case_action_filing_date) == 2014,]
df2 <- read.csv("2014 Asset Seizures.csv", stringsAsFactors = FALSE)

# Check if they have any Case ID in common
any(df$case_id %in% df2$Tracking.Number) # [1] FALSE

# Check if they have any defendant names in common
get_defendant <- function(vec) vapply(X=vec, FUN= function(i) {
    vec <- gsub("VS","V",vec, fixed=TRUE)
    vec <- strsplit(i, split="[Vv]", perl=TRUE)[[1]][2]
    vec <- gsub("\\$\\(?[0-9.,]+\\)?","",vec, perl=TRUE)
    vec <- gsub("IN US CURRENCY","",vec, fixed=TRUE)
    vec <- gsub("US CURRENCY","",vec, fixed=TRUE)
    vec <- gsub("CURRENCY","",vec, fixed=TRUE)
    vec <- gsub(".","",vec, fixed=TRUE)
    vec <- gsub("[0-9]","",vec, perl=TRUE)
    vec <- trimws(vec,"both")
    return(vec)
    }, FUN.VALUE = character(1))

# compare defendant names
dname <- get_defendant(df_subset$case_description)
dname1 <- df2$Defendant.Name

intersect(dname,dname1) # [1] ""
###############

###############
# Compare CourtConnect data and "1999-2015" data
df_subset <- df[year(df$case_action_filing_date) %in% 1999:2015,]
df3 <- read.csv("AR Civil Forfeiture 1999-2015 (version 1).csv", stringsAsFactors = FALSE)

names(df3)
# [1] "CASE_ID"        "CASE_DESC"      "COUNTY"         "FILING_DATE"   
# [5] "UNIQUE_PIDM"    "PARTY_TYPE"     "SEX"            "RACE_CODE"     
# [9] "ETHNICITY_CODE"

# Replace values of PARTY_TYPE
df3$PARTY_TYPE <- ifelse(df3$PARTY_TYPE=="", "Property", 
                         ifelse(df3$PARTY_TYPE=="D", "Defendant", 
                                ifelse(df3$PARTY_TYPE=="P", "Plaintiff", 
                                       df3$PARTY_TYPE)))

table(df3$PARTY_TYPE)
# Defendant Plaintiff  Property 
# 10491      7908     17217 

df3_property <- df3[df3$PARTY_TYPE=="Property",]
df3_plaintiff <- df3[df3$PARTY_TYPE=="Plaintiff",]
df3_defendant <- df3[df3$PARTY_TYPE=="Plaintiff",]

# Rename the columns and join the subsets back together
df3_property <- df3_property[,1:6]
names(df3_property) <- c("CASE_ID","CASE_DESC","COUNTY","FILING_DATE",
                         "UNIQUE_PIDM","PARTY_TYPE","SEX","RACE_CODE",
                         "ETHNICITY_CODE")



library(dummies)

df3 <- dummy.data.frame(df3, "PARTY_TYPE")

table(df3[df3$PARTY_TYPEDefendant==1,"SEX"])
table(df3[df3$PARTY_TYPEProperty==1,"SEX"])
table(df3[df3$PARTY_TYPEPlaintiff==1,"SEX"])

# Number of unique Case IDs
length(unique(df_subset$case_id)) # [1] 23846
length(unique(df3$CASE_ID)) # [1] 25738

# Number of duplicate Case IDs
length(which(duplicated(df_subset$case_id))) # [1] 0
length(which(duplicated(df3$CASE_ID))) # [1] 9878

# Case ID from "1999-2015" data that are not in CourtConnect
cases_missing <- setdiff(df3$CASE_ID,df_subset$case_id)

# For the missing Case IDs, get rows where PARTY_TYPE = D
df3_discrep <- df3[df3$CASE_ID %in% missing_case_id,]

# Sources of discrepency
# "1999-2015" data has duplicate CASE_IDs (it looks like cases have a 
# seperate row for each plaintiff and defendant. The "1999-2015" data
# includes 30 Case IDs which are missing in courtconnect. Those each 
# start with 29CV-14, 34CV-14 or 43CV-14 which means they are
# Hempstead (COUNTY=29), Jackson (COUNTY=34) or Lonoke (COUNTY=43) 
# civil cases from 2014
# docs: https://courtinfo.aoc.arkansas.gov/docs/helpinternal.htm.


# I think we can use the Tracking System data use to identify defendants
# and then scrape the criminal criminal data.
