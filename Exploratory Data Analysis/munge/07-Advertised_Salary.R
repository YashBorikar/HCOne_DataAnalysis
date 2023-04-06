df <- CandidateApplications

#count(df['Advertised Salary'])

#unique(df['Advertised Salary'])

# Drop NA
df <-  df %>% drop_na(`Advertised Salary`)


# Drop the rows which has 'Competitive, per annum, per year, Negotiable,Rate is dependent,Rate depending,Attractive,PA,band 7,x,Nights' in salaries
df = df[!grepl("Competitive|Competetive|per annum|per year|Negotiable|Rate is dependent|Rate depending|Attractive|PA|band 7|x|Nights", df$`Advertised Salary`, ignore.case = TRUE),]


# replace "per hour", "ph", "Upto" and salaries starting with "Salary" with blank string
for(x in 1:nrow(df)){
  
  if(grepl("per hour|mper hour|ph|per hr", df$`Advertised Salary`[[x]], ignore.case = TRUE)){
    df$`Advertised Salary`[[x]] = (gsub("per hour|mper hour|ph|per hr", "", df$`Advertised Salary`[[x]]))
  }
  if(grepl("upto|up to|Up  to", df$`Advertised Salary`[[x]], ignore.case = TRUE)){
    df$`Advertised Salary`[[x]] = (gsub("upto |up to |Upto |Up to |Up To |Up  to ", "", df$`Advertised Salary`[[x]]))
  }
  if(grepl("Salary: ", df$`Advertised Salary`[[x]])){
    df$`Advertised Salary`[[x]] = str_split(df$`Advertised Salary`[[x]], "")[[1]][2]
  }
}


# Average values for range of salaries (e.g.  £10.20 to £11.50 and £10.20 - £11.50) 

for(x in 1:nrow(df)){
  
  #(e.g. £10.20 - £11.50)
  if(grepl("(£\\d\\d.\\d\\d - £\\d\\d.\\d\\d)|(£\\d.\\d\\d - £\\d\\d.\\d\\d)|(£\\d.\\d\\d - £\\d.\\d\\d)|(£\\d - £\\d\\d)|(£\\d\\d - £\\d\\d)|(£\\d - £\\d\\d.\\d\\d)", df$`Advertised Salary`[[x]])){ 
    first <- as.numeric(gsub('£','',str_split(df$`Advertised Salary`[[x]], " ")[[1]][1]))    
    second <- as.numeric(gsub('£','',str_split(df$`Advertised Salary`[[x]], " ")[[1]][3]))
    df$`Advertised Salary`[[x]] =  round((first+second)/2, 2)
  }
  #(e.g. £10.20-£11.50)
  else if(grepl("(£\\d.\\d\\d-£\\d.\\d\\d)|(£\\d.\\d\\d-£\\d\\d.\\d\\d)|(£\\d\\d.\\d\\d-£\\d\\d.\\d\\d)", df$`Advertised Salary`[[x]])){ 
    
    first <- as.numeric(gsub('£','',str_split(df$`Advertised Salary`[[x]], "-")[[1]][1]))    
    second <- as.numeric(gsub('£','',str_split(df$`Advertised Salary`[[x]], "-")[[1]][2]))
    df$`Advertised Salary`[[x]] =  round((first+second)/2, 2)
  }
  # (e.g. 9.50- 9.80)
  else if(grepl("(£\\d.\\d\\d- £\\d.\\d\\d)|(£\\d.\\d\\d- £\\d\\d.\\d\\d)|(£\\d\\d.\\d\\d- £\\d\\d.\\d\\d)|(£\\d.\\d\\d -£\\d.\\d\\d)|(£\\d.\\d\\d -£\\d\\d.\\d\\d)|(£\\d\\d.\\d\\d -£\\d\\d.\\d\\d)", df$`Advertised Salary`[[x]])){ 
    
    first <- as.numeric(gsub('£','',str_split(df$`Advertised Salary`[[x]], "-")[[1]][1]))    
    second <- as.numeric(gsub('£','',str_split(df$`Advertised Salary`[[x]], "-")[[1]][2]))
    df$`Advertised Salary`[[x]] =  round((first+second)/2, 2)
  }
  # (e.g. From 7.50 to )
  else if(grepl("(From £\\d.\\d\\d to £\\d.\\d\\d)|(From £\\d.\\d\\d to £\\d\\d.\\d\\d)|(From £\\d\\d.\\d\\d-£\\d\\d.\\d\\d)", df$`Advertised Salary`[[x]])){ 
    
    first <- as.numeric(gsub('£','',str_split(df$`Advertised Salary`[[x]], "-")[[1]][1]))    
    second <- as.numeric(gsub('£','',str_split(df$`Advertised Salary`[[x]], "-")[[1]][2]))
    df$`Advertised Salary`[[x]] =  round((first+second)/2, 2)
  }
  #(e.g. £10.20 to £11.50)
  else if(grepl("(£\\d\\d.\\d\\d to £\\d\\d.\\d\\d)|(£\\d.\\d\\d to £\\d\\d.\\d\\d)|(£\\d.\\d\\d to £\\d.\\d\\d)|(£\\d to £\\d)|(£\\d to £\\d.\\d\\d)", df$`Advertised Salary`[[x]])){ 
    
    first <- as.numeric(gsub('£','',str_split(df$`Advertised Salary`[[x]], " ")[[1]][1]))    
    second <- as.numeric(gsub('£','',str_split(df$`Advertised Salary`[[x]], " ")[[1]][3]))
    df$`Advertised Salary`[[x]] =  round((first+second)/2, 2)
  }
  #(e.g. £10.20 -11.50)
  else if(grepl("(£\\d.\\d\\d-\\d.\\d\\d)|(£\\d-\\d)|(£\\d\\d.\\d\\d-\\d\\d.\\d\\d)", df$`Advertised Salary`[[x]])){ 
    
    first <- as.numeric(gsub('£','',str_split(df$`Advertised Salary`[[x]], " ")[[1]][1]))    
    second <- as.numeric(gsub('£','',str_split(df$`Advertised Salary`[[x]], " ")[[1]][3]))
    df$`Advertised Salary`[[x]] =  round((first+second)/2, 2)
  }
}

any(is.na(df$`Advertised Salary`)) 
df <-  df %>% drop_na(`Advertised Salary`)


# Replacing the rows with "plus|Equivalent to" strings with blank string
for(x in 1: nrow(df)){
  if(grepl("plus|+|-", df[x,]$`Advertised Salary`)){
    df[x,]$`Advertised Salary`<- as.numeric(gsub('£','',str_split(df[x,]$`Advertised Salary`, " ")[[1]][1]))    
  }
  else if(grepl("Equivalent to", df[x,]$`Advertised Salary`)){
    df[x,]$`Advertised Salary`<- as.numeric(gsub('£','',str_split(df[x,]$`Advertised Salary`, "")[[1]][3]))    
  }
  
  # if(grepl('£', df[x,]$`Advertised Salary`)){
  #   df[x,]$`Advertised Salary` = as.numeric((gsub("£| £", "", df[x,]$`Advertised Salary`)))
  # }
}

# Drop NA
df <-  df %>% drop_na(`Advertised Salary`)


#Adding a row for ranges for the salaries
df$"Advertised_Salary_Num"<-as.numeric(df$`Advertised Salary`)
df$"Sum_Service_Months_Num"<-as.numeric(df$`Sum_Service_Months`)


specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))
for (x in 1:nrow(df)){
  specify_decimal(df$Advertised_Salary_Num[[x]],2)
}
NoOutliersdf<-subset(df,df$Advertised_Salary_Num<20)
NoOutliersdf<-subset(NoOutliersdf,NoOutliersdf$Sum_Service_Months_Num<100)

df2 <- df %>%
  mutate(Advertised_Salary_Num_Range = case_when(
    Advertised_Salary_Num < 8 ~ "<8",
    Advertised_Salary_Num >= 8 & Advertised_Salary_Num < 9 ~ "08-08.99",
    Advertised_Salary_Num >= 9 & Advertised_Salary_Num < 10 ~ "09-09.99",
    Advertised_Salary_Num >= 10 & Advertised_Salary_Num < 11 ~ "10-10.99",
    Advertised_Salary_Num >= 11 & Advertised_Salary_Num < 12 ~ "11-11.99",
    Advertised_Salary_Num >= 12 & Advertised_Salary_Num < 13 ~ "12-12.99",
    Advertised_Salary_Num >= 13 & Advertised_Salary_Num < 14 ~ "13-13.99",
    Advertised_Salary_Num >= 14 & Advertised_Salary_Num < 15 ~ "14-14.99",
    Advertised_Salary_Num >= 15 & Advertised_Salary_Num < 16 ~ "15-15.99",
    Advertised_Salary_Num >= 16 & Advertised_Salary_Num < 17 ~ "16-16.99",
    Advertised_Salary_Num >= 17 & Advertised_Salary_Num < 18 ~ "17-17.99",
    Advertised_Salary_Num >= 18 & Advertised_Salary_Num < 19 ~ "18-18.99",
    Advertised_Salary_Num >= 19 & Advertised_Salary_Num < 20 ~ "19-19.99",
    Advertised_Salary_Num >= 20 & Advertised_Salary_Num < 21 ~ "20-20.99",
    Advertised_Salary_Num >= 21 ~ "21+",
    TRUE ~ "NA"
  ))


# Adding a row for range of months of service 
df2 <- df2 %>%
  mutate(Sum_Service_Months_Range = case_when(
    Sum_Service_Months < 6  ~ "<06",
    Sum_Service_Months >= 6 & Sum_Service_Months < 12 ~ "06-11",
    Sum_Service_Months >= 12 & Sum_Service_Months < 24 ~ "12-23",
    Sum_Service_Months >= 24  ~ "24+",
    TRUE ~"NA"
  ))

# Create dataframe based on different Job Title
Nurses=subset(df2,df2$`Job Title`=="Nursing")
Carers=subset(df2,df2$`Job Title`=="Carer")
Senior_Carers=subset(df2,df2$`Job Title`=="Senior Carer")

Nurses$"Advertised_Salary_Num"<-as.numeric(Nurses$`Advertised Salary`)
Carers$"Advertised_Salary_Num"<-as.numeric(Carers$`Advertised Salary`)
Senior_Carers$"Advertised_Salary_Num"<-as.numeric(Senior_Carers$`Advertised Salary`)

NoOutliersNurses<-subset(Nurses,Nurses$Advertised_Salary_Num<22)
NoOutliersCarers<-subset(Carers,Carers$Advertised_Salary_Num<22)
NoOutliersSeniorCarer<-subset(Senior_Carers,Senior_Carers$Advertised_Salary_Num<22)

#mean(NoOutliersNurses$Advertised_Salary_Num)
#mean(NoOutliersCarers$Advertised_Salary_Num)
#mean(NoOutliersSeniorCarer$Advertised_Salary_Num)
