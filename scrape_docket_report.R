get_dktrpt <- function(url) {

    # url <- search_2010$docket_report_url[1]
    # url <- search_2010$docket_report_url[563]
    # url <- search_2010$docket_report_url[55]
    # url <- search_2010$docket_report_url[3]
    # url <- search_2010$docket_report_url[27]
    # url <- search_2010$docket_report_url[35]
    
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
    
    # dktrpt <- cbind(Judge_ID = a_nodes[[5]] %>% html_text(), # Judge ID
    #                 Plaintiff_ID = a_nodes[[6]] %>% html_text(), # Plaintiff ID
    #                 Defendnt_ID = a_nodes[[7]] %>% html_text(), # Defendnt ID
    #                 violations_w,
    #                 stringsAsFactors=FALSE)
    # 
    # # dim(dktrpt) # [1]  1 27
    # 
    # return(dktrpt)
    
}


wrapper_fun_2 <- function(search_results_file, nrows=nrow(read.csv(search_results_file, stringsAsFactors = FALSE))){

    ## Get docket report for each case
    search_results_df <- read.csv(search_results_file, stringsAsFactors = FALSE)
    # search_results_df <- read.csv("2010_criminal_search(5).csv", stringsAsFactors = FALSE)
    # nrows=10
    
    ## "https://caseinfo.aoc.arkansas.gov/cconnect/PROD/public/ck_public_qry_doct.cp_dktrpt_frames?backto=P&case_id=16JCR-10-1260"
    ## "https://caseinfo.aoc.arkansas.gov/cconnect/PROD/public/ck_public_qry_doct.cp_dktrpt_docket_report?case_id=16JCR-10-1260"
    
    l <- lapply(X=1:nrows, FUN=function(i){
        cat(paste0(i, "\n"))
        dktrpt <- search_results_df$result_docket_report_url[i]
        if(!is.na(dktrpt)) dktrpt <- get_dktrpt(dktrpt)
        
        return(dktrpt)
        
    })
    
    # saveRDS(l, "dktrpt_2010.RDS")
    
    # l <- readRDS("dktrpt_2010.RDS")
    
    numViolations <- unlist(lapply(1:length(l), function(i){
        dim(l[[i]])[2]/8
    }))
    
    # Look at the distribution of violations count
    # hist(numViolations)
    # range(numViolations)
    
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
    # table(unlist(lapply(l, is.character)))
    # FALSE  TRUE 
    # 1191   459 
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
    
    }


# Scrape docket report from each search result
result <- wrapper_fun_2("2010_criminal_search(5).csv")

# Write results to a stored object
# saveRDS(result, "search_results_docket_reports_2010.RDS")

# Write results to a file
# write.csv(result, "2010_criminal_search.csv", row.names = FALSE)
