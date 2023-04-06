# ~2.3k of the rows in CandidateApplications have duplicated candidate_ids
# Add up the "length of service in months" for these and produce only one row for
# each candidate
CandidateApplications$Sum_Service_Months =
  ave(CandidateApplications$`length of service in months`, CandidateApplications$`candidate id`,FUN=sum)
CandidateApplications = CandidateApplications[!duplicated(CandidateApplications$`candidate id`),]

CandidateApplications = CandidateApplications %>%
  select(-one_of(c("length of service in months")))


## Filter out candidates who currently are employed and have less than 6 months
## in the job (removes 1,800 rows)
THRESHOLD_MONTHS_1 = 6
THRESHOLD_MONTHS_2 = 12
THRESHOLD_MONTHS_3 = 24

CandidateApplications = CandidateApplications %>%
  filter(!((CandidateApplications$`Current Employee [1=YES / 0=No]` == 1)
           & (CandidateApplications$`Sum_Service_Months` < THRESHOLD_MONTHS_1)))

## Add a new column to say whether the candidate was successfully retained (1=successful)
## for 6, 12 and 24 months
CandidateApplications = CandidateApplications %>% 
  mutate(Retain_Success_6 = if_else(CandidateApplications$`Sum_Service_Months`
                                    >= THRESHOLD_MONTHS_1, 1, 0))
CandidateApplications = CandidateApplications %>% 
  mutate(Retain_Success_12 = if_else((CandidateApplications$`Sum_Service_Months`
                                      >= THRESHOLD_MONTHS_2) |
                                       (CandidateApplications$`Current Employee [1=YES / 0=No]`
                                        == 1), 1, 0))
CandidateApplications = CandidateApplications %>% 
  mutate(Retain_Success_24 = if_else((CandidateApplications$`Sum_Service_Months`
                                      >= THRESHOLD_MONTHS_3) |
                                       (CandidateApplications$`Current Employee [1=YES / 0=No]`
                                        == 1), 1, 0))