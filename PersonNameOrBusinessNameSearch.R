append_parameters <- function(filename, start_date, end_date, person_type = "D%20-%20DEFENDANT"){
    
    # browser()
    # Read in the CAF data file
    df <- read.csv(filename, stringsAsFactors = FALSE)
    
    # Create a copy of Defendant.Name to modify
    df$Defendant.Name2 <- df$Defendant.Name
    
    # Remove suffixes and other junk from Defendant.Name 
    pat <- c(" Jr."," Jr"," Sr."," Sr"," III"," II"," 3rd"," 2nd"," & No Name","\' ","\'")
    for(i in pat) df$Defendant.Name2 <- gsub(i,"", df$Defendant.Name2, fixed = TRUE)
    # Remove spanish stopwords
    pat <- c(" De "," La ")
    for(i in pat) df$Defendant.Name2 <- gsub(i," ", df$Defendant.Name2, fixed = TRUE)
    
    # Count the number of words in Defendant.Name
    df$numWords <- vapply(strsplit(df$Defendant.Name2, "\\W+"), length, integer(1))
    
    # distribution of word count
    # table(df$numWords)
    # 0    1    2    3    4    5 
    # 22    1 1191  395   56    6
    
    # If word count > 0, assume the last word is last_name
    df$last_name <- vapply(X=1:nrow(df), FUN=function(i){
        ifelse(df$numWords[i]>0,
            unlist(strsplit(df$Defendant.Name2[i], "\\W+"))[df$numWords[i]],
            NA_character_
        )
    }, FUN.VALUE=character(1))
    
    # Attempt to extract first name based how many words are in Defendant.Name
    df$first_name <- vapply(X=1:nrow(df), FUN=function(i){
        # If defendent_name has 2 words, assume first word is first_name
        ifelse(df$numWords[i]==2,unlist(strsplit(df$Defendant.Name2[i], "\\W+"))[1],
            # If defendent_name has 3 words, assume first word is first_name
            ifelse(df$numWords[i]==3, unlist(strsplit(df$Defendant.Name2[i], "\\W+"))[1],
                # If defendent_name has 3 words and "/" or "&", assume the first is first_name
                ifelse(df$numWords[i]==3 & any(unlist(strsplit(df$Defendant.Name2[i]," ")) %in% c("/","&")),unlist(strsplit(df$Defendant.Name2[i], "\\W+"))[1],
                    # If defendent_name has 4 words, one of them "or", assume the third is first_name 
                    ifelse(df$numWords[i]==4 & any(unlist(strsplit(df$Defendant.Name2[i]," ")) =="or"),unlist(strsplit(df$Defendant.Name2[i], "\\W+"))[1],
                        # If defendent_name has 4 words and "/" or "&", assume the third is first_name 
                        ifelse(df$numWords[i]==4 & any(unlist(strsplit(df$Defendant.Name2[i]," ")) %in% c("/","&")),unlist(strsplit(df$Defendant.Name2[i], "\\W+"))[3],
                            # If defendent_name has 5 words and "&" or "/" assume the fourth is first_name
                            # ifelse(df$numWords[i]==5 & any(unlist(strsplit(df$Defendant.Name[i]," ")) %in% c("/","&")),unlist(strsplit(df$Defendant.Name[i], "\\W+"))[4],
                                # If defendent_name has 5 words one of them "or", assume the fourth is first_name
                                ifelse(df$numWords[i]==5 & any(unlist(strsplit(df$Defendant.Name2[i]," ")) == "or"),unlist(strsplit(df$Defendant.Name2[i], "\\W+"))[4],
                                    # Otherwise leave first_name empty
                                    NA_character_)
                                )
                            # )
                        )
                    )
                )
            )
        }, FUN.VALUE=character(1))
    
    # replace NAs with ""
    df$first_name <- ifelse(is.na(df$first_name),"", df$first_name)
    
    # Get parameter value for county
    county_lookup <- read.csv("county_lookup.csv", stringsAsFactors = FALSE)
    
    # prepare county_names to merge
    county_lookup$county_name <- tolower(county_lookup$county_name)
    df$County <- tolower(df$County)
    
    # Merge to get the county parameter value
    df <- merge(df, county_lookup, by.x = "County", by.y = "county_name", all.x = TRUE)
    
    # Format parameters for the url
    df$county_code <- gsub(" ","%20",df$county_code,fixed=TRUE)
    df$county_code <- ifelse(is.na(df$county_code), "", df$county_code)
    
    # Exclude cases where Defendant.Name is empty
    # nrow(df) # [1] 1672
    df <- df[df$Tracking.Number != "",]
    # nrow(df) # [1] 1650
    
    # Create column for start_date
    df$start_date <- start_date
    # Create column for start_date
    df$end_date <- end_date
    # Create column for person_type
    df$person_type <- person_type
    
    # use columns to construct query url
    get_query <- function(...){
        
        baseURL <- "https://caseinfo.aoc.arkansas.gov/cconnect/PROD/public/ck_public_qry_cpty.cp_personcase_srch_details?"
        
        queryURL <- paste0("last_name=", df$last_name,
                           "&first_name=", df$first_name,
                           "&partial_ind=",ifelse(nchar(df$first_name)==1,'checked',''),
                           "&begin_date=", df$start_date, 
                           "&end_date=", df$end_date,
                           "&person_type=", df$person_type, 
                           "&county_code=", df$county_code, 
                           "&locn_code=","ALL",
                           "&PageNo=","1")
        
        url <- paste0(baseURL,queryURL)
        
        return(url)
        
    }
    
    # Create column for query url
    df$url <- get_query(df)
    
    # Select columns for result
    df <- df[,c("Tracking.Number","Receive.Date","Agency","Defendant.Name","Address","City.State.Zip",
                "STATE","Currency.Seized","Personal.Property.Seized","Real.Property.Seized","FilingStatus",
                "County","Judicial.District","Disposition..Money.","Disposition..Property.",
                "first_name","last_name","person_type","county_code","start_date","end_date","url")]
    
    # Remove dots from column names
    names(df) <- gsub("\\.+","", names(df))
    
    # Prefix columns to denote query inputs and query results
    names(df)[1:15] <- paste0("caf_",names(df)[1:15])
    names(df)[16:22] <- paste0("query_",names(df)[16:22])
    
    return(df)
    
    }

## get search parameters from 2010 Asset Tracker file
df <- append_parameters("2010 Asset Seizures.csv", start_date = "01/01/2010", end_date = "12/31/2011")
## get search parameters from 2011 Asset Tracker file
# df <- get_parameters("2011 Asset Seizures.csv", start_date = "01/01/2011", end_date = "12/31/2012")
## get search parameters from 2012 Asset Tracker file
# df <- get_parameters("2012 Asset Seizures.csv", start_date = "01/01/2012", end_date = "12/31/2013")
## get search parameters from 2013 Asset Tracker file
# df <- get_parameters("2013 Asset Seizures.csv", start_date = "01/01/2013", end_date = "12/31/2014")
## get search parameters from 2014 Asset Tracker file
# df <- get_parameters("2014 Asset Seizures.csv", start_date = "01/01/2014", end_date = "12/31/2015")
## get search parameters from 2015 Asset Tracker file
# df <- get_parameters("2015 Asset Seizures.csv", start_date = "01/01/2015", end_date = "12/31/2016")
## get search parameters from 2016 Asset Tracker file
# df <- get_parameters("2016 Asset Seizures.csv", start_date = "01/01/2016", end_date = "12/31/2017")

PersonNameOrBusinessNameSearch <- function(url){

    library(rvest)
    
    # Query the url
    doc <- read_html(url, options = "HUGE")

    # Get the column names
    # html_nodes(doc, "th") %>% html_text()
    # [1] "ID"
    # [2] "Names/Corporation"
    # [3] "Case Description"
    # [4] "Party Type"
    # [5] "Filing Date"
    # [6] "Judge"

    # Get data from "Cases Filed by Date Search" results
    id                 <- html_nodes(doc, "td:nth-child(1)") %>% html_text() %>% .[2]
    names_corporation  <- html_nodes(doc, "td:nth-child(2)") %>% html_text() %>% .[2]
    names_corporation  <- regmatches(names_corporation,regexpr("(^[^:]+)", names_corporation))
    names_corporation  <- gsub("Aliases","",names_corporation, fixed = TRUE)
    case_description   <- html_nodes(doc, "td:nth-child(3)") %>% html_text() %>% .[2]
    case_status        <- strsplit(case_description, split = ":")[[1]][3]
    case_id            <- html_nodes(doc, "a") %>% html_text() %>% .[2]
    party_type         <- html_nodes(doc, "td:nth-child(4)") %>% html_text() %>% .[2]
    filing_date        <- html_nodes(doc, "td:nth-child(5)") %>% html_text() %>% .[2]
    filing_date        <- as.Date(filing_date, format="%d-%B-%y")
    judge              <- html_nodes(doc, "td:nth-child(6)") %>% html_text() %>% .[2]
    judge              <- trimws(gsub("[[:punct:]]","",judge), "both")
    docket_report_url  <- paste0("https://caseinfo.aoc.arkansas.gov/cconnect/PROD/public/ck_public_qry_doct.cp_dktrpt_docket_report?backto=P&case_id=",case_id)

    # Create dataframe
    df <- data.frame(id=id,
                     names_corporation=names_corporation,
                     case_description=case_description,
                     case_status=case_status,
                     case_id=case_id,
                     party_type=party_type,
                     filing_date=filing_date,
                     judge=judge,
                     docket_report_url=docket_report_url,
                     stringsAsFactors = F)
    return(df)
}


## test scraper function on a single url
# result <- PersonNameOrBusinessNameSearch(df$query_url[10])
# str(result)


###
# TO DO:
# create a wrapper function that takes the dataframe of parameters as input 
# and returns dataframe augmented with criminal search result as output
###

#####
# Code to put in function

# Search criminal filings in CourtConnect for each caf_PersonName
l <- lapply(X=1:nrow(df), FUN=function(i){
    
    # print progress
    cat(paste0(i,": ", df$query_first_name[i]," ",df$query_last_name[i], "... "))

    result <- PersonNameOrBusinessNameSearch(df$query_url[i])
    
    # print number of rows in progress
    nRows <- ifelse(result$id != "",nrow(result), 0)
    cat(paste0(nRows," rows","\n"))

    return(result)
    })

result <- do.call("rbind", l)

## Create unique identifier for join
result$key_col <- 1:nrow(result)
names(result)
df$key_col <- 1:nrow(df)
names(df)

nrow(result) # [1] 1650
nrow(df) # [1] 1650

result <- merge(df, result, by="key_col")

## Set column values to NA for empty results
result$id <- ifelse(result$id=="", NA_character_, result$id)
result$names_corporation <- ifelse(result$id=="", NA_character_, result$names_corporation)
result$case_description <- ifelse(result$id=="", NA_character_, result$case_description)
result$case_id <- ifelse(result$id=="", NA_character_, result$case_id)
result$party_type <- ifelse(result$id=="", NA_character_, result$party_type)
result$filing_date <- ifelse(result$id=="", NA_character_, result$filing_date)
result$judge <- ifelse(result$id=="", NA_character_, result$judge)
result$docket_report_url <- ifelse(result$id=="", NA_character_, result$docket_report_url)

# Prefix columns to denote query results
names(result)[23:31] <- paste0("result_",names(result)[23:31])

#####

## output results to csv file
write.csv(result, "2010_criminal_search(4).csv", row.names = FALSE)
