
PersonNameOrBusinessNameSearch <- function(last_name, first_name, start_MMDDYYYY, end_MMDDYYYY, case_type, person_type, county_code){
    
    # use parameters to construct query url
    get_query <- function(...){
        
        # format user inputs for the query url
        start_MMDDYYYY <- gsub(pattern="[-. ]", replacement = "/", start_MMDDYYYY)
        end_MMDDYYYY <- gsub(pattern="[-. ]", replacement = "/", end_MMDDYYYY)
        case_type <- gsub(pattern=" ", replacement = "%20", case_type)
        
        baseURL <- "https://caseinfo.aoc.arkansas.gov/cconnect/PROD/public/ck_public_qry_cpty.cp_personcase_srch_details?"
            
        queryURL <- paste0("last_name=", last_name,
                           "&first_name=", first_name,
                           "&begin_date=", start_MMDDYYYY, 
                           "&end_date=", end_MMDDYYYY,
                           "&case_type=", case_type, # "11%20-%20CRIMINAL%20CIRCUIT"
                           "&person_type=", person_type, # "D%20-%20DEFENDANT",
                           "&county_code=", county_code, # "63%20-%20SALINE"
                           "&locn_code=","ALL",
                           "&PageNo=","1")
        
        url <- paste0(baseURL,queryURL)
        
        return(url)
        
    }
 
    query_string <- get_query(last_name, first_name, start_MMDDYYYY, end_MMDDYYYY, case_type, person_type, county_code)
    
    # Get data for specified time period and case type
    library(rvest)

    # Query the url
    doc <- read_html(query_string, options = "HUGE")

    # Get the column names
    html_nodes(doc, "th") %>% html_text()
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
    df <- data.frame(
        id=id,
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



# PersonNameOrBusinessNameSearch(last_name="Williams", first_name="Kevin", start_MMDDYYYY="01/01/2016", end_MMDDYYYY="01/01/2017", case_type="11%20-%20CRIMINAL%20CIRCUIT", person_type="D%20-%20DEFENDANT", county_code="63%20-%20SALINE")

