# CAF-data-scraping

This repoository provides code for querying the Arkansas Administrative Office of the Courts public CourtConnect database to match Civil Asset Forfeture (CAF) claimants with criminal defense cases. The R code is organized into two main scripts.

* helper_functions.R contains web scrapter functions for downloading and tabulating the data from Court Connect. append_parameters(), PersonNameOrBusinessNameSearch() and get_dktrpt() create search parameters for each case in the ASTS data file, scrape data from the Person Name or Business Name Search, and scrape data from associated docket reports, respectively. After append_parameters() has been applied on the ASTS data file, wrapper_fun and wrapper_fun_2 loop over each row of the augmented data file to apply PersonNameOrBusinessNameSearch() and get_dktrpt() respectively.

* run_job.R contains routines for executing the functions in helper_functions.R for each Asset Siezure Tracking System (ASTS) data file.
