###################################
# function that appends CourtConnect search parameters to 
# the Asset Tracker dataset
###################################

append_parameters <- function(filename, start_date, end_date, person_type = "D%20-%20DEFENDANT"){
    # browser() # for debugging
    # Read in the CAF data file
    # df <- read.csv("2010 Asset Seizures.csv", stringsAsFactors = FALSE)
    df <- read.csv(filename, stringsAsFactors = FALSE)
    
    # Only keep rows with a Tracking.Number
    df <- df[df$Tracking.Number!="",]
    
    # Create a copy of Defendant.Name to modify
    df$Defendant.Name2 <- df$Defendant.Name
    
    # Remove suffixes and other junk from Defendant.Name
    df$Defendant.Name2 <- gsub(", Jr.","", df$Defendant.Name2, fixed = TRUE)
    df$Defendant.Name2 <- gsub(", Sr.","", df$Defendant.Name2, fixed = TRUE)
    df$Defendant.Name2 <- gsub(", Jr","", df$Defendant.Name2, fixed = TRUE)
    df$Defendant.Name2 <- gsub(", Sr","", df$Defendant.Name2, fixed = TRUE)
    df$Defendant.Name2 <- gsub(" Jr.","", df$Defendant.Name2, fixed = TRUE)
    df$Defendant.Name2 <- gsub(" Sr.","", df$Defendant.Name2, fixed = TRUE)
    df$Defendant.Name2 <- gsub(" & No Name","", df$Defendant.Name2, fixed = TRUE)
    df$Defendant.Name2 <- gsub("\\bJr\\b","", df$Defendant.Name2, perl = TRUE)
    df$Defendant.Name2 <- gsub("\\bSr\\b","", df$Defendant.Name2, perl = TRUE)
    df$Defendant.Name2 <- gsub("\\bIV\\b","", df$Defendant.Name2, perl = TRUE)
    df$Defendant.Name2 <- gsub("\\bIII\\b","", df$Defendant.Name2, perl = TRUE)
    df$Defendant.Name2 <- gsub("\\bII\\b","", df$Defendant.Name2, perl = TRUE)
    df$Defendant.Name2 <- gsub("\\b4th\\b","", df$Defendant.Name2, perl = TRUE)
    df$Defendant.Name2 <- gsub("\\b3rd\\b","", df$Defendant.Name2, perl = TRUE)
    df$Defendant.Name2 <- gsub("\\b2nd\\b","", df$Defendant.Name2, perl = TRUE)
    
    # Remove spanish stopwords
    df$Defendant.Name2 <- gsub("\\bDe\\b","", df$Defendant.Name2, perl = TRUE)
    df$Defendant.Name2 <- gsub("\\bLa\\b","", df$Defendant.Name2, perl = TRUE)
    
    # replace "or" with "/" and "and" with "&"
    df$Defendant.Name2 <- gsub("\\bor\\b","/", df$Defendant.Name2, perl = TRUE)
    df$Defendant.Name2 <- gsub("\\band\\b","&", df$Defendant.Name2, perl = TRUE)
    
    # remove trailing spaces
    df$Defendant.Name2 <- trimws(df$Defendant.Name2)
    
    ######################################################################
    # For Defendant.name that have a comma, split Defendant.name
    # on "," and use name that has the most words
    # Note: in cases where Defendant.name has concatinated values
    #   this procedure will only captures one of the names
    ######################################################################
    
    # index of rows where Defendant.Name has a ","
    ind <- which(grepl(",",df$Defendant.Name2,fixed = TRUE))
    # list of vectors whose elements are parts of the name
    l <- lapply(X=strsplit(df[ind,"Defendant.Name2"], ",", fixed=TRUE), trimws)
    # assigning the part of the name that has the most words to "Defendant.Name2"
    df[ind,"Defendant.Name2"] <- vapply(1:length(l), function(i){
        # list of names for each row
        ll <- strsplit(l[[i]], "\\s+", perl=TRUE)
        # list of name lengths for each row
        lll <- unlist(lapply(ll,length))
        # vector of longest name indexes
        ind.max <- which.max(lll)
        # vector of longest names
        paste(ll[[ind.max]], collapse = " ")
    }, FUN.VALUE = character(1L))
    
    # # Count the total number of words in Defendant.Name
    df$numWords <- vapply(strsplit(df$Defendant.Name2, "\\W+"), length, integer(1))
    
    # Count the number of words before "&" or "/" in Defendant.Name
    # list of names for each row
    l <- lapply(X=strsplit(df$Defendant.Name2, "&|/", perl=TRUE), trimws)
    # number of words person1's name has
    df$p1NumWords <- vapply(1:length(l), function(i){
        length(unlist(strsplit(l[[i]][1], "\\s+", perl=TRUE)))
    }, FUN.VALUE = integer(1L))
    
    # create indicator for having two names
    df$isTwoNames <- grepl("&", df$Defendant.Name2, fixed=TRUE) | 
        grepl("/", df$Defendant.Name2, fixed=TRUE)
    
    # select the rows that have two names
    person1 <- df[df$isTwoNames, ]
    
    # create a copy of each row that has two names
    person2 <- df[df$isTwoNames, ]
    
    # select the rows that have only one name
    df <- df[!df$isTwoNames,]
    
    # distribution of word count
    # table(df$numWords)
    # 1    2    3    4 
    # 2 1197  368    4
        
    ###################################
    # Parse Defendant.name for the single-person records
    ###################################
    
    # Assume the first word is first_name
    df$first_name <- vapply(X=1:nrow(df), FUN=function(i){
        unlist(strsplit(df$Defendant.Name2[i], "\\W+"))[1]
    }, FUN.VALUE=character(1))
    
    # Assume the last word is last_name
    df$last_name <- vapply(X=1:nrow(df), FUN=function(i){
        unlist(strsplit(df$Defendant.Name2[i], "\\W+"))[df$numWords[i]]
    }, FUN.VALUE=character(1))
    
    ###################################
    # Parse Defendant.name for the double-person records
    ###################################
    
    # table(person1$numWords)
    #  3  4  5 
    # 26 49  3 
    
    # we have three scenarios..
    # 
    # scenario 1: name has 3 words
    #   person1 first word is first name, last word is last name
    #   person2 second word is first name, last word is last name
    # 
    # scenario2: name has 4 words
    #   person1 first word is first name, second word is last name
    #   person2 third word is first name, last word is last name
    # 
    # scenario3: name has 5 words
    #   scenario 3A: "&" is preceeded by 2 words
    #       person1 first word is first name, second word is last name
    #       person2 third word is first name, last last word is last name
    # 
    #   scenario 3B: "&" is preceeded by 3 words
    #       person1 first word is first name, third word is last name
    #       person2 fourth word is first name, last word is last name
    
    # function to extract nth word
    get_word <- function(vec, n) {vapply(X=1:length(vec), FUN=function(i){
        trimws(strsplit(vec[i], "\\W+", perl=TRUE)[[1]][n])
        }, FUN.VALUE = character(1L))}
    
    ## PERSON 1
    
    # first name is ALWAYS the first word
    person1$first_name <- get_word(person1$Defendant.Name2,1)
    
    # last name
    person1$last_name <- NA_character_
    person1$last_name <- ifelse(person1$numWords==3, get_word(person1$Defendant.Name2,3),person1$last_name)
    person1$last_name <- ifelse(person1$numWords==4, get_word(person1$Defendant.Name2,2),person1$last_name)
    person1$last_name <- ifelse(person1$numWords==5 & person1$p1NumWords==2, get_word(person1$Defendant.Name2,2),person1$last_name)
    person1$last_name <- ifelse(person1$numWords==5 & person1$p1NumWords==3, get_word(person1$Defendant.Name2,3),person1$last_name)
   
    ## PERSON 2
    
    # first name
    person2$first_name <- NA_character_
    person2$first_name <- ifelse(person2$numWords==3, get_word(person2$Defendant.Name2,2),person2$first_name)
    person2$first_name <- ifelse(person2$numWords==4, get_word(person2$Defendant.Name2,3),person2$first_name)
    person2$first_name <- ifelse(person2$numWords==5 & person2$p1NumWords==2, get_word(person2$Defendant.Name2,3),person2$first_name)
    person2$first_name <- ifelse(person2$numWords==5 & person2$p1NumWords==3, get_word(person2$Defendant.Name2,4),person2$first_name)
    
    # last name
    person2$last_name <- NA_character_
    person2$last_name <- ifelse(person2$numWords==3, get_word(person2$Defendant.Name2,3),person2$last_name)
    person2$last_name <- ifelse(person2$numWords==4, get_word(person2$Defendant.Name2,4),person2$last_name)
    person2$last_name <- ifelse(person2$numWords==5, get_word(person2$Defendant.Name2,5),person2$last_name)
    
    # combine the rows back into single dataframe 
    df <- do.call("rbind", list(df, person1, person2))
    
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
    # nrow(df) # [1] 1728
    df <- df[df$Tracking.Number != "",]
    df <- df[df$last_name != "",]
    # nrow(df) # [1] 1728
    
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
    
    # Replace spaces or slashes with dot in column names
    # names(df) <- gsub("State","STATE", names(df))
    names(df) <- gsub("Seizure.Date", "Receive.Date", names(df))
    names(df) <- gsub("Recieve.Date", "Receive.Date", names(df))
    names(df) <- gsub("City.State", "City.State.Zip", names(df))
    names(df) <- gsub("City.State.Zip.Zip", "City.State.Zip", names(df))
    
    
    # browser()
    # Select columns for result
    df <- df[,c("Tracking.Number","Receive.Date","Agency","Defendant.Name","Address","City.State.Zip",
                #"STATE",
                "Currency.Seized","Personal.Property.Seized","Real.Property.Seized","FilingStatus",
                "County","Judicial.District","Disposition..Money.","Disposition..Property.",
                "first_name","last_name","person_type","county_code","start_date","end_date","url"
                )]
    
    # Remove dots from column names
    names(df) <- gsub("\\.+","", names(df))
    
    # Prefix columns to denote query inputs and query results
    names(df)[1:14] <- paste0("caf_",names(df)[1:14])
    names(df)[15:21] <- paste0("query_",names(df)[15:21])
    
    # Order by Tracking.Number
    df <- df[order(df$caf_TrackingNumber),]
    
    return(df)
    
} # end append_parameters

## get search parameters from 2010 Asset Tracker file
# df <- append_parameters("2010 Asset Seizures.csv", start_date = "01/01/2010", end_date = "12/31/2011")
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


###################################
# function for scraping search results from a CourtConnect 
# "Person Name Or Business Name Search" URL
###################################

PersonNameOrBusinessNameSearch <- function(url){
    
    
    # URLs to test
    # url <- df$query_url[1] # 0 results
    # url <- df$query_url[5] # 1 results
    # url <- df$query_url[2] # 2 results
    # url <- df$query_url[3] # 3 results
    # url <- df$query_url[343] # 4 results
    # url <- df$query_url[203] # 10 results
    
    
    library(rvest, quietly = TRUE)
    
    # function for trying to read from a url
    function_that_may_fail <- function(i, ...) {
        # Query the url
        doc <- read_html(url, options = "HUGE")
        # stop if read_html() failed
        stopifnot(is.list(doc), length(doc)==2, class(doc) %in% c("xml_document","xml_node"))
        # return contents read from the url
        return(doc)
    }
    
    # attempt to read from the url, and retry up to 10 times if needed
    for(i in 1:10){
        try({doc <- function_that_may_fail(i, url)
        break #break/exit the for-loop
        }, silent = FALSE)
        Sys.sleep(2)
    }
    
    # Get the column names
    # html_nodes(doc, "th") %>% html_text()
    # [1] "ID"
    # [2] "Names/Corporation"
    # [3] "Case Description"
    # [4] "Party Type"
    # [5] "Filing Date"
    # [6] "Judge"
    
    # Count the number of columns
    numColumns <- length(html_nodes(doc, "th") %>% html_text()) 
    # numColumns # [1] 6
    
    df <- data.frame(id=NA_character_,
                     names_corporation=NA_character_,
                     case_description=NA_character_,
                     case_status=NA_character_,
                     case_id=NA_character_,
                     party_type=NA_character_,
                     filing_date=as.Date(NA_character_),
                     judge=NA_character_,
                     docket_report_url=NA_character_,
                     stringsAsFactors = F)
    
    ## Get data from "Cases Filed by Date Search" results
    
    case_desc_l <- html_nodes(doc, "td:nth-child(3)") %>% 
        html_text() %>% .[-c(1, length(.)-1, length(.))] %>% 
        strsplit(x=., split = ":")
    
    # Create function for slitting case description into three data elements
    cdsplit <- function(ind, myList=case_desc_l) 
        unlist(lapply(1:length(myList), function(i){ gsub("Status.*","",trimws(gsub("\\s+", " ", myList[[i]][ind]))) }))
    
    # Create dataframe
    
    if(numColumns==6) df <- data.frame(
        id                = html_nodes(doc, "td:nth-child(1)") %>% html_text() %>% .[-c(1, length(.)-1, length(.))],
        names_corporation = html_nodes(doc, "td:nth-child(2)") %>% html_text() %>% .[-c(1, length(.)-1, length(.))] %>% gsub("Aliases","",., fixed = TRUE),
        case_description  = cdsplit(2),
        case_status       = cdsplit(3),
        case_id           = gsub("\\s.*","", cdsplit(2)),
        party_type        = html_nodes(doc, "td:nth-child(4)") %>% html_text() %>% .[-c(1, length(.))],
        filing_date       = html_nodes(doc, "td:nth-child(5)") %>% html_text() %>% .[-c(1, length(.))] %>% as.Date(., format="%d-%B-%y") %>% suppressMessages(),
        judge             = html_nodes(doc, "td:nth-child(6)") %>% html_text() %>% .[-c(1, length(.))] %>% gsub(", ","",.),
        docket_report_url = paste0("https://caseinfo.aoc.arkansas.gov/cconnect/PROD/public/ck_public_qry_doct.cp_dktrpt_docket_report?case_id=",gsub("\\s.*","",cdsplit(2))),
        stringsAsFactors  = F)
    
    return(df)
    
} # end PersonNameOrBusinessNameSearch function


###################################
# wrapper function that takes the dataframe of parameters as input 
# and returns dataframe augmented with criminal search result as output
###################################

wrapper_fun <- function(df){
    
    # Search criminal filings in CourtConnect for each caf_PersonName
    l <- lapply(X=1:nrow(df), FUN=function(i){
        
        # print progress
        cat(paste0(i,": ", df$query_first_name[i]," ",df$query_last_name[i], "... "))
        
        # search for filings
        result <- PersonNameOrBusinessNameSearch(df$query_url[i])
        
        # Create unique identifier for join
        result$key_col <- i
        
        # add result row count to progress
        nRows <- ifelse(all(is.na(result[,-ncol(result)])), 0, nrow(result))
        cat(paste0(nRows," rows","\n"))
        
        return(result)
        
    }) # end lapply
    
    # Combine list of dataframe using recursive rbind
    result <- do.call("rbind", l)
    
    ## Create unique identifier for join
    df$key_col <- 1:nrow(df)
    # names(df)
    
    # nrow(result) # [1] 1650
    # nrow(df) # [1] 1650
    
    # Merge results to original data and search parameters
    result <- merge(df, result, by="key_col")
    
    # Prefix columns to denote query results
    names(result)[24:31] <- paste0("result_",names(result)[24:31])
    
    return(result)
    
} # end of wrapper_func



###################################
# Funtion for scraping docket report
###################################

get_dktrpt <- function(url) {
    
    library(rvest, quietly = TRUE)
    
    # function for trying to read from a url
    function_that_may_fail <- function(i, ...) {
        # read from the url
        doc <- read_html(url)
        # stop if read_html() failed
        stopifnot(is.list(doc), length(doc)==2, class(doc) %in% c("xml_document","xml_node"))
        # return contents read from the url
        return(doc)
    }
    
    # attempt to read from the url, and retry up to 10 times if needed
    for(i in 1:10){
        try({doc <- function_that_may_fail(i, url)
        break #break/exit the for-loop
        }, silent = FALSE)
        Sys.sleep(2)
    }
    
    a_nodes <- html_nodes(doc, "a")
    
    test <- unlist(lapply(1:length(a_nodes), FUN=function(i){
        a_nodes[[i]] %>% html_text()
    }))
    
    ind <- which(grepl("Violations", test))
    
    # a_nodes[[1]] %>% html_text()
    # a_nodes[[2]] %>% html_text()
    # a_nodes[[3]] %>% html_text()
    # a_nodes[[4]] %>% html_text()
    # a_nodes[[5]] %>% html_text() # Judge ID
    # a_nodes[[6]] %>% html_text() # Plaintiff ID
    # a_nodes[[7]] %>% html_text() # Defendnt ID
    # a_nodes[[8]] %>% html_text() # Violations
    # a_nodes[[9]] %>% html_text() # Sentences
    # a_nodes[[10]] %>% html_text()
    # a_nodes[[11]] %>% html_text()
    
    
    # Violations section
    violations <- a_nodes[[ind]] %>% html_text() %>% strsplit("\n") %>% .[[1]] %>% gsub("&nbsp", " ", .) %>% gsub(" +", " ", .) %>% trimws()
    AgeAtViolation <- violations[which(grepl("Age at Violation: ", violations))] %>% gsub("Age at Violation: ","",.,fixed=TRUE) %>% as.integer()
    PleaDate <- violations[which(grepl("Plea: ", violations))] %>% gsub("Plea: ","",.,fixed=TRUE) %>% gsub("NOT GUILTY","NOT_GUILTY",.) %>% gsub("NEGOTIATED GUILTY","NEGOTIATED_GUILTY",.) %>% strsplit(split=" ", .) %>% unlist() %>% .[c(TRUE, FALSE)] %>% as.character() # %>% as.Date(., format="%d-%B-%y") %>% suppressMessages()
    Plea <- violations[which(grepl("Plea: ", violations))] %>% gsub("Plea: ","",.,fixed=TRUE) %>% gsub("NOT GUILTY","NOT_GUILTY",.) %>% gsub("NEGOTIATED GUILTY","NEGOTIATED_GUILTY",.) %>% strsplit(split=" ", .) %>% unlist() %>% .[c(FALSE, TRUE)] %>% as.character()
    ViolationNumber <- violations[which(grepl("Plea: ", violations)) + rep(1,length(Plea))] %>% as.character()
    ViolationDescription <- violations[which(grepl("Plea: ", violations)) + rep(2,length(Plea))] %>% as.character()
    Level <- violations[which(grepl("Level: ", violations))] %>% gsub("Level: ","",.,fixed=TRUE) %>% as.character()
    ViolationDate <- violations[which(grepl("Violation Date: ", violations))] %>% gsub("Violation Date: ","",.,fixed=TRUE) %>% as.character() # %>% as.Date(., format="%d-%B-%y") %>% suppressMessages()
    ViolationTime <- violations[which(grepl("Violation Time: ", violations))] %>% gsub("Violation Time: ","",.,fixed=TRUE) %>% as.character()
    
    maxlen <- max(vapply(list(AgeAtViolation,PleaDate,Plea,ViolationNumber,ViolationDescription,Level,ViolationDate,ViolationTime), length, FUN.VALUE = integer(1)))
    
    violations <- data.frame(AgeAtViolation=NA_integer_,
                             PleaDate=NA_character_,
                             # PleaDate=as.Date(NA_character_),
                             Plea=NA_character_,
                             ViolationNumber=NA_character_,
                             ViolationDescription=NA_character_, 
                             Level=NA_character_,
                             ViolationDate=NA_character_,
                             # ViolationDate=as.Date(NA_character_),
                             ViolationTime=NA_character_, 
                             stringsAsFactors=FALSE)
    
    if(maxlen>0) violations <- data.frame(
        AgeAtViolation=ifelse(rep(identical(AgeAtViolation, character(0)), maxlen), NA_integer_, AgeAtViolation),
        PleaDate=ifelse(rep(identical(PleaDate,character(0)),maxlen), NA_character_, PleaDate),
        Plea=ifelse(rep(identical(Plea, character(0)),maxlen), NA_character_, Plea),
        ViolationNumber=ifelse(rep(identical(ViolationNumber, character(0)),maxlen), NA_character_, ViolationNumber),
        ViolationDescription=ifelse(rep(identical(ViolationDescription, character(0)),maxlen), NA_character_, ViolationDescription),
        Level=ifelse(rep(identical(Level, character(0)),maxlen), NA_character_, Level),
        ViolationDate=ifelse(rep(identical(ViolationDate, character(0)),maxlen), NA_character_, ViolationDate),
        ViolationTime=ifelse(rep(identical(ViolationTime,character(0)),maxlen), NA_character_, ViolationTime),
        stringsAsFactors=FALSE)
    
    # dim(violations) # [1] 3 8
    
    AgeAtViolation <- data.frame(t(as.matrix(violations[,1])), stringsAsFactors = FALSE)
    names(AgeAtViolation) <- paste0("AgeAtViolation_",1:ncol(AgeAtViolation))
    PleaDate <- data.frame(t(as.matrix(violations[,2])))
    names(PleaDate) <- paste0("PleaDate_",1:ncol(PleaDate))
    Plea <- data.frame(t(as.matrix(violations[,3])))
    names(Plea) <- paste0("Plea_",1:ncol(Plea))
    ViolationNumber <- data.frame(t(as.matrix(violations[,4])))
    names(ViolationNumber) <- paste0("ViolationNumber_",1:ncol(ViolationNumber))
    ViolationDescription <- data.frame(t(as.matrix(violations[,5])))
    names(ViolationDescription) <- paste0("ViolationDescription_",1:ncol(ViolationDescription))
    Level <- data.frame(t(as.matrix(violations[,6])))
    names(Level) <- paste0("Level_",1:ncol(Level))
    ViolationDate <- data.frame(t(as.matrix(violations[,7])))
    names(ViolationDate) <- paste0("ViolationDate_",1:ncol(ViolationDate))
    ViolationTime <- data.frame(t(as.matrix(violations[,8])))
    names(ViolationTime) <- paste0("ViolationTime_",1:ncol(ViolationTime))
    
    violations_w <- cbind(AgeAtViolation,
                          PleaDate,
                          Plea,
                          ViolationNumber,
                          ViolationDescription,
                          Level,
                          ViolationDate,
                          ViolationTime,
                          stringsAsFactors=FALSE)
    
    # dim(violations_w) # [1]  1 24
    
    return(violations_w)
    
    
} # end of get_dktrpt function


###################################
# Wrapper function for docket report scraper
###################################

wrapper_fun_2 <- function(search_results_df, nrows=nrow(search_results_df)){
# wrapper_fun_2 <- function(search_results_file, nrows=nrow(read.csv(search_results_file, stringsAsFactors = FALSE))){
    
    ## Get docket report for each case
    l <- lapply(X=1:nrows, FUN=function(i){
        # print progress
        id_txt <- ifelse(is.na(search_results_df$result_case_id[i]), "EMPTY RESULT",search_results_df$result_case_id[i])
        cat(paste0(i,": ", 
                   id_txt," ",search_results_df$query_first_name[i]," ",search_results_df$query_last_name[i], 
                   "... "))
        
        dktrpt <- search_results_df$result_docket_report_url[i]
        if(!is.na(dktrpt)) dktrpt <- get_dktrpt(dktrpt)
        
        # add violations count to progress
        nCols <- ifelse(all(is.na(dktrpt)), 0, ncol(dktrpt))
        cat(paste0(nCols/8," violations","\n"))
        
        return(dktrpt)
        
    })
    
    numViolations <- unlist(lapply(1:length(l), function(i){
        dim(l[[i]])[2]/8
    }))
    
    # Get the largest set of column names
    colNames <- c(paste0("AgeAtViolation_", 1:max(numViolations)), 
                  paste0("Level_", 1:max(numViolations)),
                  paste0("Plea_", 1:max(numViolations)), 
                  paste0("PleaDate_", 1:max(numViolations)),
                  paste0("ViolationDate_", 1:max(numViolations)), 
                  paste0("ViolationDescription_", 1:max(numViolations)),
                  paste0("ViolationNumber_", 1:max(numViolations)),
                  paste0("ViolationTime_", 1:max(numViolations)))
    
    # Replace empty elements with empty dataframe
    ind <- which(unlist(lapply(l, is.character)))
    # length(ind) # [1] 459
    
    l <- lapply(1:length(l), function(i){
        
        if(i %in% ind){
            l[[i]] <- matrix(nrow = 1, ncol = 8*max(numViolations))
            l[[i]] <- as.data.frame(l[[i]], stringsAsFactors = FALSE)
            names(l[[i]]) <- colNames
        }
        
        return(l[[i]])
        
    })
    
    # Combine docket reports into single dataframe
    library(data.table, quietly = TRUE)
    df <- rbindlist(l, fill=TRUE)
    
    # Convert from data.table back to dataframe
    df <- as.data.frame(df, stringsAsFactors=FALSE)
    
    # Put columns in order
    df <- df[, colNames]
    
    # Convert factor columns to character
    indFactors <- which(!grepl("AgeAtViolation_", names(df)))
    df[,indFactors] <- lapply(df[,indFactors], as.character)
    
    # Join docket report data to the search results
    search_results <- search_results_df
    # dim(search_results) # [1] 1650   32
    docket_reports <- df
    # dim(df) # [1] 1650  208
    
    # Return search results and docket reports in seperate dataframes
    # return(list(search_results=search_results, docket_reports=docket_reports))
    
    # Return search results and docket reports in a single dataframe
    result <- cbind(search_results, docket_reports, stringsAsFactors=FALSE)
    
    return(result)
    
} # end of wrapper_fun_2

