# Data-frame after filtering required columns

schools_data = subset(CandidateSchools.CandidateSchools, 
                      select = -c(candidateschoolid, 
                                  postcode, 
                                  startdate, 
                                  enddate, 
                                  description)
)

temp_data = CandidateSchools.CandidateSchools

temp_data <- temp_data %>% arrange(candidateid)
df_grouped <- temp_data %>% group_by(candidateid) %>% slice(1)
df_new <- df_grouped %>% select(candidateid, schoolname, coursetitle, subject)

# Rename column name candidateid to candidate id for merging
colnames(df_new)[1]  <- "candidate id"

# Change datatype of column name candidateid from numeric to character for merging
df_new$'candidate id'<-as.character(df_new$'candidate id')

temp_main = CandidateApplications

df2 <- temp_main %>% inner_join(df_new, by=c('candidate id'='candidate id'))

# Code of Left joint dataframe
df_left <- temp_main %>% left_join(df_new, by=c('candidate id'='candidate id'))

# Code to replace all un-ordered GCSE values with GCSE and add in column Qualification
df_left$Qualification <- ifelse(grepl("gcse|g.c.s.e", df_left$coursetitle, ignore.case = TRUE), "GCSE", df_left$coursetitle)

# Code to replace all un-ordered BSc values with BSc in column Qualification
df_left$Qualification <- ifelse(grepl("Bachelor|Bachelors|Bachelor's|Batchelor|BSc|BSc.|B.sc|B. Sc.|B.S.c|B. Sc", df_left$Qualification, ignore.case = TRUE), "BSc", df_left$Qualification)

# Code to replace all BSc value with BSc - Health and Social Care in qualification column
df_left$Qualification <- ifelse(df_left$Qualification == "BSc" & grepl("nursing|nurse|health|pre-nursing|childcare|child|health|Care and Education|Biological|BIOMEDICAL SCIENCES", df_left$subject, ignore.case = TRUE), "BSc - Health and Social Care or Nursing", df_left$Qualification)

# Code to replace all Diploma value with BSc - Health and Social Care in qualification column where BSc is related to Health care
df_left$Qualification <- ifelse(df_left$Qualification == "BSc" & grepl("nursing|nurse|health|pre-nursing|care|childcare|child|health|Care and Education|Care", df_left$coursetitle, ignore.case = TRUE), "BSc - Health and Social Care or Nursing", df_left$Qualification)

# Code to replace all un-ordered MSc values with MSc in column Qualification
df_left$Qualification <- ifelse(grepl("Master|Masters|Master's|MSc|MSc.|M.sc|M.Sc.|M. Sc.|M.S.c|M. Sc", df_left$Qualification, ignore.case = TRUE), "MSc", df_left$Qualification)

# Code to replace all MSc value with MSc - Health and Social Care in qualification column
df_left$Qualification <- ifelse(df_left$Qualification == "MSc" & grepl("nursing|nurse|health|pre-nursing|childcare|child|health|Care and Education|Biological|BIOMEDICAL SCIENCES", df_left$subject, ignore.case = TRUE), "MSc - Health and Social Care or Nursing", df_left$Qualification)

# Code to replace all Diploma value with MSc - Health and Social Care in qualification column where MSc is related to Health care
df_left$Qualification <- ifelse(df_left$Qualification == "MSc" & grepl("nursing|nurse|health|pre-nursing|care|childcare|child|health|Care and Education|Care", df_left$coursetitle, ignore.case = TRUE), "MSc - Health and Social Care or Nursing", df_left$Qualification)

# Code to replace all un-ordered Diploma values with Diploma in column Qualification
df_left$Qualification <- ifelse(grepl("diploma|hnd", df_left$Qualification, ignore.case = TRUE), "Diploma", df_left$Qualification)

# Code to replace all Diploma value with Diploma - Health and Social Care in qualification column
df_left$Qualification <- ifelse(df_left$Qualification == "Diploma" & grepl("nursing|nurse|health|pre-nursing|care|childcare|child|health|Care and Education|Biological|BIOMEDICAL SCIENCES", df_left$subject, ignore.case = TRUE), "Diploma - Health and Social Care or Nursing", df_left$Qualification)

# Code to replace all Diploma value with Diploma - Health and Social Care in qualification column where Diploma is related to Health care
df_left$Qualification <- ifelse(df_left$Qualification == "Diploma" & grepl("nursing|nurse|health|pre-nursing|care|childcare|child|health|Care and Education|Care", df_left$coursetitle, ignore.case = TRUE), "Diploma - Health and Social Care or Nursing", df_left$Qualification)

# Code to replace all un-ordered Certificate values with Certificate in column Qualification
df_left$Qualification <- ifelse(grepl("certificate|Certification", df_left$Qualification, ignore.case = TRUE), "Certificate", df_left$Qualification)

# Code to replace all Certificate value with Certificate - Health and Social Care in qualification column
df_left$Qualification <- ifelse(df_left$Qualification == "Certificate" & grepl("nursing|nurse|health|pre-nursing|childcare|child|health|Care and Education|Biological|BIOMEDICAL SCIENCES", df_left$subject, ignore.case = TRUE), "Certificate - Health and Social Care or Nursing", df_left$Qualification)

# Code to replace all Certificate value with Certificate - Health and Social Care in qualification column where Certificate is related to Health care
df_left$Qualification <- ifelse(df_left$Qualification == "Certificate" & grepl("nursing|nurse|health|pre-nursing|childcare|child|health|Care and Education|Care", df_left$coursetitle, ignore.case = TRUE), "Certificate - Health and Social Care or Nursing", df_left$Qualification)

# Code to replace all un-ordered NVQ values with NVQ in column Qualification
df_left$Qualification <- ifelse(grepl("nvq|N.V.Q.", df_left$Qualification, ignore.case = TRUE), "NVQ", df_left$Qualification)

# Code to replace all NVQ value with NVQ - Health and Social Care in qualification column
df_left$Qualification <- ifelse(df_left$Qualification == "NVQ" & grepl("nursing|nurse|health|pre-nursing|childcare|child|health|Care and Education|Biological|BIOMEDICAL SCIENCES", df_left$subject, ignore.case = TRUE), "NVQ - Health and Social Care or Nursing", df_left$Qualification)

# Code to replace all NVQ value with NVQ - Health and Social Care in qualification column where NVQ is related to Health care
df_left$Qualification <- ifelse(df_left$Qualification == "NVQ" & grepl("nursing|nurse|health|pre-nursing|care|childcare|child|health|Care and Education|Care|level 3", df_left$coursetitle, ignore.case = TRUE), "NVQ - Health and Social Care or Nursing", df_left$Qualification)

# Code to replace all un-ordered A-Level values with A-Level in column Qualification
df_left$Qualification <- ifelse(grepl("A Level|A Levels|A'Levels|A' Level|'A' Level|A ' Level|A-Level|A - Level", df_left$Qualification, ignore.case = TRUE), "A-Level", df_left$Qualification)

# Code to replace all un-ordered NVQ values with NVQ in column Qualification
df_left$Qualification <- ifelse(grepl("GCSE|Diploma|NVQ|BSc|BSc - Health and Social Care or Nursing|Diploma - Health and Social Care or Nursing|Certificate|MSc|A-Level|NVQ - Health and Social Care or Nursing|MSc - Health and Social Care or Nursing|Certificate - Health and Social Care or Nursing", df_left$Qualification, ignore.case = TRUE), df_left$Qualification, "Other Academics")

# Code to replace all NVQ value with NVQ - Health and Social Care in qualification column
df_left$Qualification <- ifelse(df_left$Qualification == "Other Academics" & grepl("nursing|nurse|health|pre-nursing|childcare|child|health|Care and Education|Biological|Care", paste(df_left$subject, df_left$coursetitle, sep = " "), ignore.case = TRUE), "Other Academics - Health and Social Care or Nursing", df_left$Qualification)

# Plot 4: Trendline code
df_trend = df_left
df_trend$Filtered_Qualification <- ifelse(grepl("Health and Social Care or Nursing", df_left$Qualification, ignore.case = TRUE), "Nursing", df_left$Qualification)
df_trend$Filtered_Qualification <- ifelse(grepl("Nursing", df_trend$Filtered_Qualification, ignore.case = TRUE), "Nursing Qualification", "Other Qualification")

#Filtering 2023 data
tend_graph_df = df_trend
tend_graph_df$'Application Date' <- as.Date(tend_graph_df$'Application Date')
tend_graph_df$Year <- format(tend_graph_df$'Application Date', "%Y")
trend_df_filtered <- tend_graph_df %>% filter(Year != "2023")

success_rates <- aggregate(Retain_Success_6 ~ Year + Filtered_Qualification, data = trend_df_filtered, FUN = function(x) mean(x == 1) * 100)
