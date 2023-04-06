#prepare the channels dataset for the first EDA

channels <- CandidateApplications %>% 
  select(c('candidate id',"Job Title","Applied Through (Channel Type)","Applied Through (Channel)","Sum_Service_Months","Retain_Success_6","Retain_Success_12","Retain_Success_24") )
#rename the columns
colnames(channels)
names(channels)[names(channels) == "Applied Through (Channel Type)"] <- "channel_type"
names(channels)[names(channels) == "Applied Through (Channel)"] <- "channel"
names(channels)[names(channels) == "Sum_Service_Months"] <- "months_of_service"
names(channels)[names(channels) == "candidate id"] <- "candidate_id"

#remove NA's
channels<-channels[complete.cases(channels),]


#Prepare data for the level of experience EDA
#inner join applications with history.
EmploymentHistory.EmploymentHistory
names(EmploymentHistory.EmploymentHistory)[names(EmploymentHistory.EmploymentHistory) == "candidateid"] <- "candidate id"

EmploymentHistory.EmploymentHistory$`candidate id` = as.character(EmploymentHistory.EmploymentHistory$`candidate id`)
filter_emp <-EmploymentHistory.EmploymentHistory %>% select(`candidate id`,startdate,enddate,orderid)
filter_candidates <- CandidateApplications %>% select(`candidate id`,`Application Date`,`Job Title`)
filter_candidates$`candidate id` = as.character(filter_candidates$`candidate id`)
emp_history <- filter_candidates  %>% left_join(filter_emp, by ="candidate id")
emp_history %>% group_by(`candidate id`) 

emp_history$enddate <- ifelse(grepl("^(c|U|T|P|sti|t|u|S|p|On|on|Da|Cu|No)", emp_history$enddate), emp_history$`Application Date`, emp_history$enddate)
emp_history <- emp_history[grepl("\\d", emp_history$startdate),]
emp_history <- emp_history[grepl("\\d", emp_history$enddate),]
emp_history$enddate <- ifelse(grepl("^(200|201|202|199)", emp_history$enddate),(as.numeric(emp_history$enddate)-1900)*366 , emp_history$enddate)
emp_history$startdate <- ifelse(grepl("^(200|201|202|199)", emp_history$startdate),(as.numeric(emp_history$startdate)-1900)*366 , emp_history$startdate)

emp_history$startdate = convertToDate(emp_history$startdate)
emp_history$enddate = convertToDate(emp_history$enddate)


emp_history<- emp_history %>%
  mutate(months_of_experience = round(time_length(enddate - startdate, unit='months')))

#remove negatives and NA's
emp_history<-emp_history[complete.cases(emp_history) & emp_history$months_of_experience >0,]

# select the months of experience and sum the months of experience for each candidate
emp_experience <- emp_history %>% 
  select(`candidate id`,`Application Date`,months_of_experience) %>% 
  group_by(`candidate id`) %>% 
  dplyr::summarize(total_experience = sum(months_of_experience)) %>% 
  ungroup()

#left join the candidates service months and the experience data
candidates_service <- CandidateApplications %>% select(`candidate id`,Sum_Service_Months,`Job Title`)
emp_service <- candidates_service  %>% left_join(emp_experience, by ="candidate id") 
emp_service<-emp_service[complete.cases(emp_service),]


