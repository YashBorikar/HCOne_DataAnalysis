# Cleaning columns to match their types and converting them to the type 

EDUCATIONAL_COLS = c("Vocational_Certification","Vocational_Licence","Vocational_Other",
                     "Vocational_All","Academic_GCSE","Academic_NVQ","Academic_A-Level",
                     "Academic_HND","Academic_Bachelors","Academic_Masters","Academic_Other",
                     "Academic_All")
CHANNEL_COLS = c("Applied Through (Channel)", "Applied Through (Channel Type)" )

NUMERIC_COLS = flatten(list(EDUCATIONAL_COLS, "length of service in months",
                            "Contracted hours", "Current Employee [1=YES / 0=No]"))

# In the educational columns replace "Not Found" with NA
CandidateApplications = CandidateApplications %>%       
  mutate_at(EDUCATIONAL_COLS, ~ na_if(., "Not Found"))

# In the channel columns replace "NULL" with NA
CandidateApplications = CandidateApplications %>%
  mutate_at(CHANNEL_COLS,  ~ na_if(., "NULL"))

# Convert numeric columns to numeric type
for (col in NUMERIC_COLS)
{
  CandidateApplications[[col]] <- as.numeric(CandidateApplications[[col]])
}

# Convert Excel's internal date representation to proper dates
CandidateApplications$`Application Date` = convertToDate(CandidateApplications$`Application Date`)