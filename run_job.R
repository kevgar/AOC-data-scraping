
# $cd /Users/kjgardner/GitHub/AOC-data-scraping
# $R
# >system.time(source("run_job.R"))

source("helper_functions.R")

# append search parameters to original ASTS dataset
df1 <- append_parameters("2010 Asset Seizures.csv", start_date = "01/01/2010", end_date = "12/31/2011")
# search courtconnect using each set of search parameters
# and output results dataframe to RDS file
saveRDS(wrapper_fun(df1), "df2.RDS")
# read search results dataframe from RDS file, parse each
# docket report for matching violations and write result
# dataframe to RDS file
df2 <- readRDS("df2.RDS")
saveRDS(wrapper_fun_2(df2), "df3.RDS")
# read in results dataframe from RDS file and write
# to a csv file
df3 <- readRDS("df3.RDS")
write.csv(df3, "2010_criminal_search.csv", row.names=FALSE)

# # append search parameters to original ASTS dataset
# df1 <- append_parameters("2011 Asset Seizures.csv", start_date = "01/01/2011", end_date = "12/31/2012")
# # search courtconnect using each set of search parameters
# # and output results dataframe to RDS file
# saveRDS(wrapper_fun(df1), "df2.RDS")
# # read search results dataframe from RDS file, parse each
# # docket report for matching violations and write result
# # dataframe to RDS file
# df2 <- readRDS("df2.RDS") 
# saveRDS(wrapper_fun_2(df2), "df3.RDS")
# # read in results dataframe from RDS file and write
# # to a csv file
# df3 <- readRDS("df3.RDS")
# write.csv(df3, "2011_criminal_search.csv", row.names=FALSE)

# # append search parameters to original ASTS dataset
# df1 <- append_parameters("2012 Asset Seizures.csv", start_date = "01/01/2012", end_date = "12/31/2013")
# # search courtconnect using each set of search parameters
# # and output results dataframe to RDS file
# saveRDS(wrapper_fun(df1), "df2.RDS")
# # read search results dataframe from RDS file, parse each
# # docket report for matching violations and write result
# # dataframe to RDS file
# df2 <- readRDS("df2.RDS")
# saveRDS(wrapper_fun_2(df2), "df3.RDS")
# # read in results dataframe from RDS file and write
# # to a csv file
# df3 <- readRDS("df3.RDS")
# write.csv(df3, "2012_criminal_search.csv", row.names=FALSE)

# append search parameters to original ASTS dataset
# df1 <- append_parameters("2013 Asset Seizures.csv", start_date = "01/01/2013", end_date = "12/31/2014")
# # search courtconnect using each set of search parameters
# # and output results dataframe to RDS file
# saveRDS(wrapper_fun(df1), "df2.RDS")
# # read search results dataframe from RDS file, parse each
# # docket report for matching violations and write result
# # dataframe to RDS file
# df2 <- readRDS("df2.RDS")
# saveRDS(wrapper_fun_2(df2), "df3.RDS")
# # read in results dataframe from RDS file and write
# # to a csv file
# df3 <- readRDS("df3.RDS")
# write.csv(df3, "2013_criminal_search.csv", row.names=FALSE)

# # append search parameters to original ASTS dataset
# df1 <- append_parameters("2014 Asset Seizures.csv", start_date = "01/01/2014", end_date = "12/31/2015")
# start_date = "01/01/2012", end_date = "12/31/2013")
# # search courtconnect using each set of search parameters
# # and output results dataframe to RDS file
# saveRDS(wrapper_fun(df1), "df2.RDS")
# # read search results dataframe from RDS file, parse each
# # docket report for matching violations and write result
# # dataframe to RDS file
# df2 <- readRDS("df2.RDS")
# saveRDS(wrapper_fun_2(df2), "df3.RDS")
# # read in results dataframe from RDS file and write
# # to a csv file
# df3 <- readRDS("df3.RDS")
# write.csv(df3, "2014_criminal_search.csv", row.names=FALSE)

# # append search parameters to original ASTS dataset
# df1 <- append_parameters("2015 Asset Seizures.csv", start_date = "01/01/2015", end_date = "12/31/2016")
# # search courtconnect using each set of search parameters
# # and output results dataframe to RDS file
# saveRDS(wrapper_fun(df1), "df2.RDS")
# # read search results dataframe from RDS file, parse each
# # docket report for matching violations and write result
# # dataframe to RDS file
# df2 <- readRDS("df2.RDS")
# saveRDS(wrapper_fun_2(df2), "df3.RDS")
# # read in results dataframe from RDS file and write
# # to a csv file
# df3 <- readRDS("df3.RDS")
# write.csv(df3, "2015_criminal_search.csv", row.names=FALSE)

# # append search parameters to original ASTS dataset
# df1 <- append_parameters("2016 Asset Seizures.csv", start_date = "01/01/2016", end_date = "12/31/2017")
# # search courtconnect using each set of search parameters
# # and output results dataframe to RDS file
# saveRDS(wrapper_fun(df1), "df2.RDS")
# # read search results dataframe from RDS file, parse each
# # docket report for matching violations and write result
# # dataframe to RDS file
# df2 <- readRDS("df2.RDS")
# saveRDS(wrapper_fun_2(df2), "df3.RDS")
# # read in results dataframe from RDS file and write
# # to a csv file
# df3 <- readRDS("df3.RDS")
# write.csv(df3, "2016_criminal_search.csv", row.names=FALSE)

