# Tabulate statistics and make graphs for the AEA data editor report
# Originally written by Harry Son
# 2021-12-30

# Inputs
#   - file.path(jiraanon,"jira.anon.RDS") 
# Outputs


### Load libraries 
### Requirements: have library *here*
source(here::here("global-libraries.R"),echo=TRUE)
source(here::here("programs","config.R"),echo=TRUE)


# Read in data extracted from Jira, anonymized

if ( file.exists(jira.anon.name) ) {
  message("File exists, proceeding.")
} else {
  message("Attempting to download file")
  try(download.file(jira.anon.url,jira.anon.name,mode="wb"))
  if ( file.exists(jira.anon.name) ) {
    message("Download successful, proceeding.")
  } else {
    stop("Download failed. Please investigate")
  }
}

jira.anon <- readRDS(jira.anon.name) 
#
# This should be compared to the published version.
#
jira.test.chksum <- digest(jira.anon,algo="sha256")

if ( jira.test.chksum == jira.anon.sha256) {
  message("SHA256 checksum verified.")
} else {
  stop("SHA256 fails, please verify that you are using the same file. Update config.R if necessary.")
}

# A list of non-issues, typically for information-only
# This code is not robust, and should be changed at some point.
jira.pyear <- jira.anon %>%
  filter(date_created >= firstday, date_created < lastday) %>%
  cSplit("Changed.Fields",",")  %>%
  mutate(status_change = ifelse(Changed.Fields_1=="Status","Yes",ifelse(Changed.Fields_2=="Status","Yes",ifelse(Changed.Fields_3=="Status","Yes",ifelse(Changed.Fields_4=="Status","Yes","No"))))) %>%
  select(-Changed.Fields_1,-Changed.Fields_2,-Changed.Fields_3,-Changed.Fields_4) %>%
  cSplit("Software.used",",")  %>%
  filter(status_change=="Yes"|received=="Yes") %>%
  mutate(subtask_y=ifelse(is.na(subtask),"No",ifelse(subtask!="","Yes",""))) %>%
  filter(subtask_y=="No") %>%
  filter(Journal != "AEA P&P") %>% ## Removing all P&P  
  mutate(reason1=grepl("Discrepancy",reason.failure, ## 8 reasons for failutre to reproduce
                       fixed = "TRUE"),
         reason2=grepl("Bugs",reason.failure,
                       fixed = "TRUE"),
         reason3=grepl("Insufficient",reason.failure,
                       fixed = "TRUE"),
         reason4=grepl("Software",reason.failure,
                       fixed = "TRUE"),
         reason5=grepl("functional",reason.failure,
                       fixed = "TRUE"),
         reason6=grepl("Data not available",reason.failure,
                       fixed = "TRUE"),
         reason6.1=grepl("Data,not,available",reason.failure,
                         fixed = "TRUE"),
         reason7=grepl("Data missing",reason.failure,
                       fixed = "TRUE"),
         reason7.1=grepl("Data,missing",reason.failure,
                         fixed = "TRUE"),
         reason8=grepl("Code missing",reason.failure,
                       fixed = "TRUE"),
         reason8.1=grepl("Code,missing",reason.failure,
                         fixed = "TRUE")) %>% 
  mutate(reason1=case_when(reason1==TRUE ~ "Discrepancy in Output",
                           TRUE ~ ""),
         reason2=case_when(reason2==TRUE ~ "Bugs in code",
                           TRUE ~ ""),
         reason3=case_when(reason3==TRUE ~ "Insufficient time available to replicator",
                           TRUE ~ ""),
         reason4=case_when(reason4==TRUE ~ "Software not available to replicator",
                           TRUE ~ ""),
         reason5=case_when(reason5==TRUE ~ "Code not functional",
                           TRUE ~ ""),
         reason6=case_when(reason6==T ~ "Data not available",
                           reason6.1==T ~ "Data not available",
                           TRUE ~ ""),
         reason7=case_when(reason7==T ~ "Data missing",
                           reason7.1==T ~ "Data missing",
                           TRUE ~ ""),
         reason8=case_when(reason8==T ~ "Code missing",
                           reason8.1==T ~ "Code missing",
                           TRUE ~ "")) %>%
  select(-reason6.1,-reason7.1,-reason8.1) 

#### Break out of the issues
jira.issues.breakout <- jira.pyear %>%
  arrange(desc(row_number())) %>%
  group_by(ticket) %>%
  mutate(status_order =  row_number(), st = "Status") %>%
  mutate(new1 = paste(st, status_order, sep="")) %>%
  select(ticket,Status,new1) %>%
  pivot_wider(names_from = new1, values_from = "Status")

# identifying the list of received tickets
ji <- jira.pyear %>%
  select(ticket,Journal) %>%
  distinct(ticket, .keep_all = TRUE) 

# identifying the list of issues went through alternate workflow
ji_alt <- jira.pyear %>%
  filter(Status == "Alternate"|Status=="Alternate workflow") %>%
  select(ticket) %>%
  distinct() %>%
  mutate(alternate="Yes")

# identifying the list of submitted issues 
jis <- jira.pyear %>%
  filter(Status == "Submitted to MC" & Journal != "AEA P&P") %>%
  select(ticket) %>%
  distinct() %>%
  mutate(submitted="Yes")

# figure out the final status of the issue as of 12/01/
jib <- jira.issues.breakout %>%
  left_join(jis,by="ticket") %>%
  transform(submitted=ifelse(is.na(submitted),"No",as.character(submitted))) %>%
  filter(submitted=="No") %>%
  select(-Status26,-Status27) %>%
  transform(final_status=ifelse(!is.na(Status25),as.character(Status25),
                                ifelse(!is.na(Status24),as.character(Status24),
                                       ifelse(!is.na(Status23),as.character(Status23),
                                              ifelse(!is.na(Status22),as.character(Status22),
                                                     ifelse(!is.na(Status21),as.character(Status21),
                                                            ifelse(!is.na(Status20),as.character(Status20),
                                                                   ifelse(!is.na(Status19),as.character(Status19),
                                                                          ifelse(!is.na(Status18),as.character(Status18),
                                                                                 ifelse(!is.na(Status17),as.character(Status17),
                                                                                        ifelse(!is.na(Status16),as.character(Status16),
                                                                                               ifelse(!is.na(Status15),as.character(Status15),
                                                                                                      ifelse(!is.na(Status14),as.character(Status14),
                                                                                                             ifelse(!is.na(Status13),as.character(Status13),
                                                                                                                    ifelse(!is.na(Status12),as.character(Status12),
                                                                                                                           ifelse(!is.na(Status11),as.character(Status11),
                                                                                                                                  ifelse(!is.na(Status10),as.character(Status10),
                                                                                                                                         ifelse(!is.na(Status9),as.character(Status9),
                                                                                                                                                ifelse(!is.na(Status8),as.character(Status8),
                                                                                                                                                       ifelse(!is.na(Status7),as.character(Status7),
                                                                                                                                                              ifelse(!is.na(Status6),as.character(Status6),
                                                                                                                                                                     ifelse(!is.na(Status5),as.character(Status5),
                                                                                                                                                                            ifelse(!is.na(Status4),as.character(Status4),
                                                                                                                                                                                   ifelse(!is.na(Status3),as.character(Status3),
                                                                                                                                                                                          ifelse(!is.na(Status2),as.character(Status2),
                                                                                                                                                                                                 ifelse(!is.na(Status1),as.character(Status1),"")))))))))))))))))))))))))) %>%
  select(ticket,final_status)

# categorize issues: P&P, Submitted, Not yet submitted, Alternate workflow, Others.
jira.issues.breakout <- jira.issues.breakout %>%  
  left_join(jis,by="ticket") %>%
  left_join(ji,by="ticket") %>%
  left_join(ji_alt,by="ticket") %>%
  transform(submitted=ifelse(is.na(submitted),"No",as.character(submitted))) %>%
  transform(alternate=ifelse(is.na(alternate),"No",as.character(alternate))) %>%
  select(ticket,Journal,submitted,alternate) %>%
  left_join(jib, by="ticket") %>%
  transform(outcome=ifelse(Journal=="AEA P&P","P&P",
                           ifelse(final_status=="Open"|final_status=="Assigned"|final_status=="In Progress"|final_status=="Report Under Review"|final_status=="Write Preliminary Report"|final_status=="Verification"|final_status=="Pre-Approved"|final_status=="Approved"|final_status=="Data"|final_status=="Waiting for info"|final_status=="Waiting for external report","Not yet submitted",
                                  ifelse(alternate=="Yes","Alternate","Others"))))  %>%
  transform(outcome=ifelse(submitted=="Yes","Submitted",as.character(outcome)))

# summarize the breakdown of the cases
summary.breakout <- jira.issues.breakout %>%  
  group_by(outcome) %>%
  summarise(n_outcome = n_distinct(ticket)) 

## separate the "Others" case
jira.others <- jira.issues.breakout %>%
  filter(outcome=="Others"|outcome=="Alternate") %>%
  select(ticket) %>%
  mutate(others="Yes")
# we will need it again

## filter out the "others" case
jira.pyear <- jira.pyear %>%
  left_join(jira.others,by="ticket") %>%
  transform(others=ifelse(is.na(others),"No",as.character(others))) %>%
  filter(others=="No") 

# This filters for the cases **submitted** in the past 12 months
issues_all <- jira.pyear %>%
  select(ticket) %>%
  distinct() %>% nrow()

manuscript_all <- jira.pyear %>%
  select(mc_number_anon) %>% 
  distinct() %>% nrow()

## Total submitted records


jira.filter.submitted <- jira.pyear %>%  
  filter(Status == "Submitted to MC" & Journal != "AEA P&P") 

# we will re-use these 
saveRDS(jira.pyear,file=file.path(temp,"jira.pyear.RDS"))
saveRDS(jira.filter.submitted,file=file.path(temp,"jira.submitted.RDS"))
saveRDS(jira.others,file=file.path(temp,"jira.others.RDS"))


# we used the start and end date here, so write them out
update_latexnums("firstday",firstday)
update_latexnums("lastday",lastday)
update_latexnums("jiraissues",issues_all)
update_latexnums("jiramcs",manuscript_all)

