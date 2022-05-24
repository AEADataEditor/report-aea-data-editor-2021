# ###########################
# CONFIG: parameters affecting processing
# ###########################

## These control whether the external data is downloaded and processed.
process_anon <- TRUE
download_anon <- TRUE

## These define the start (and end) dates for processing of data
firstday <- "2020-12-01"
lastday  <- "2021-11-30"

# ###########################
# CONFIG: define paths and filenames for later reference
# ###########################

# Change the basepath depending on your system

basepath <- here::here()
setwd(basepath)



# for Jira stuff
jiraanon <- file.path(basepath,"data","jira","anon")
manual   <- file.path(basepath,"data","manual")


# for openICPSR stuff
icpsrbase <- file.path(basepath,"data","icpsr")


# file names
jira.anon.name <- file.path(jiraanon,"jira.anon.RDS")
jira.anon.sha256 <- "73c92a3ff1d4f5ee5cedd2bf05c5940ca900f621c2e9358bf3e3a3149519ae7c"
jira.anon.sha512 <- "c8d828bc34bb965a57aab0e4446e0b06b7e55329f0600a4bb8e6d2d75908f4c14340a9f7ea6fe4e6eae103ffdb1d019aab313956be52c6971bf3c89633ecc314"
jira.anon.urlbase <- "https://raw.githubusercontent.com/AEADataEditor/processing-jira-process-data/698c61395a86df31aa064b843c0e08a67bd64358/data/"
jira.anon.url     <- paste0(jira.anon.urlbase,"anon/jira.anon.RDS")
jira.members.url  <- paste0(jira.anon.urlbase,"replicationlab_members.txt")
jira.members.name <- file.path(jiraanon,"replicationlab_members.txt")

noncompliance.name <- file.path(manual,"noncompliance.xlsx")

# This file is received from the AEA Editorial Office. It may need some minor formatting changes.
scholarone.name <- file.path(basepath,"data","scholarone","dataEditorReport_20201128-20211127Revised.xlsx")
# The file should have one tab per journal. 07_table4.R will rename journal names accordingly.
# Look for the part that says
# 27: Total Number of Rounds Manuscripts Underwent			
# 28: 
# 29: Total Rounds	Manuscripts	Percentage	
# 30:            1	58	0.79	
# 31:            2	15	0.21	
scholarone.skip <- 33  # <---- This number should correspond to the lines above "Total Rounds"

# local
images <- file.path(basepath, "images" )
tables <- file.path(basepath, "tables" )
programs <- file.path(basepath,"programs")
temp   <- file.path(basepath,"data","temp")


# parameters
latexnums.Rda <- file.path(tables,"latexnums.Rda")
latexnums.tex <- file.path(tables,"latexnums.tex")

for ( dir in list(images,tables,programs,temp)){
  if (file.exists(dir)){
  } else {
    dir.create(file.path(dir))
  }
}

set.seed(20201201)


####################################
# global libraries used everywhere #
####################################



## Initialize a file that will be used at the end to write out LaTeX parameters for in-text 
## reference

pkgTest("tibble")
if (file.exists(latexnums.Rda)) {
  print(paste0("File for export to LaTeX found: ",latexnums.Rda))
} else {
  latexnums <- tibble(field="version",value=as.character(date()),updated=date())
  saveRDS(latexnums,latexnums.Rda)
}

update_latexnums <- function(field,value) {
  # should test if latexnums is in memory
  latexnums <- readRDS(latexnums.Rda)
  
  # find out if a field exists
  if ( any(latexnums$field == field) ) {
    message(paste0("Updating existing field ",field))
    latexnums[which(latexnums$field == field), ]$value <- as.character(value)
    latexnums[which(latexnums$field == field), ]$updated <- date()
    #return(latexnums)
  } else {
    message(paste0("Adding new row for field ",field))
    latexnums <- latexnums %>% add_row(field=field,value=as.character(value),updated=date())
    #return(latexnums)
  }
  saveRDS(latexnums,latexnums.Rda)
}
