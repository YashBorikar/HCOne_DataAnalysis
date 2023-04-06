# Initial cleaning

### Candidate Applications ###

# remove first row (column categories), and rename to something shorter
CandidateApplications = tail(CandidateApplicationsData.CandidateApplications, -1)

# Remove pointer to CandidateApplicationsData.CandidateApplications so it can
# garbage collected
#rm(CandidateApplicationsData.CandidateApplications)

# Make column names the new first row's content and delete this row
names(CandidateApplications) <- CandidateApplications[1,]
CandidateApplications <- CandidateApplications[-1,]

# Rename relevant educational columns to include "Vocational"
colnames(CandidateApplications)[17:20] <- paste("Vocational", colnames(CandidateApplications[,c(17:20)]), sep = "_")

# Rename relevant educational columns to include "Academic"
colnames(CandidateApplications)[21:28] <- paste("Academic", colnames(CandidateApplications[,c(21:28)]), sep = "_")

# Select the columns we need
CandidateApplications = CandidateApplications %>% select(c('vacancy id','candidate id', "length of service in months",
                                   "Current Employee [1=YES / 0=No]", "Contracted hours",
                                   "Application Date", "Home Code", "Job Title",
                                   # Channel applied through EDA
                                   "Applied Through (Channel Type)", "Applied Through (Channel)",
                                   # Acorn Demographic EDA
                                   "Socail Demographic Code", "Group Code", "Type Code",
                                   "Category Code", "Type", "Category",
                                   # Advertised Salary EDA
                                   "Advertised Salary",
                                   # Competitor EDA
                                   "Count of Care homes within 15 min drive",
                                   "Count of Care home Beds  within 15 min drive",
                                   "Average age of the care homes within 15 min drive",
                                   "Count of Care Homes within walking distance (<500 meters) from candidates address",
                                   "Sum of care home beds within walking distance from candidates address",
                                   "Count of bus stops walking distance from candidates address",
                                   # Educational background EDAs
                                   "Vocational_Certification","Vocational_Licence",
                                   "Vocational_Other","Vocational_All",
                                   "Academic_GCSE","Academic_NVQ","Academic_A-Level",
                                   "Academic_HND","Academic_Bachelors","Academic_Masters",
                                   "Academic_Other","Academic_All"))

CandidateApplications$groupCode = CandidateApplications$`Group Code`
CandidateApplications$jobTitle = CandidateApplications$`Job Title`
