#'Count of Care homes within 15 min drive',
#'Count of Care home Beds  within 15 min drive',
#'Average age of the care homes within 15 min drive',
#'Count of Care Homes within walking distance (<500 meters) from candidates address',
#'Sum of care home beds within walking distance from candidates address',
#'Count of bus stops walking distance from candidates address',


cdi = CandidateApplications

cdi_filter <-cdi %>% select('Count of Care homes within 15 min drive',
                            'Count of Care home Beds  within 15 min drive',
                            'Average age of the care homes within 15 min drive',
                            'Count of Care Homes within walking distance (<500 meters) from candidates address',
                            'Sum of care home beds within walking distance from candidates address',
                            'Count of bus stops walking distance from candidates address',
                            'Sum_Service_Months', 'Retain_Success_6', 'Retain_Success_12', 'Retain_Success_24', 'Job Title')


#job values converted to 1, 2, 3
jt <- cdi_filter$`Job Title`
cdi_filter$`Job Title` <- ifelse(jt == "Nursing", 1, 
                          ifelse(jt == "Carer", 2,
                                 ifelse(jt == "Senior Carer", 3, NA)))



#Check if the vector is a numeric variable
is.numeric(cdi_filter)
is.na(cdi_filter)

#Delete elements containing characters
bak_data = cdi_filter
bak_data_num <- data.frame(sapply(bak_data, as.numeric))

#Removing missing values from tables
bak_data_no_na <- na.omit(bak_data_num)

# Modify column name
# 'Count of Care homes within 15 min drive', CH15
# 'Count of Care home Beds  within 15 min drive',CHB15
# 'Average age of the care homes within 15 min drive',AVG
# 'Count of Care Homes within walking distance (<500 meters) from candidates address',CHwalk500dis
# 'Sum of care home beds within walking distance from candidates address',CHBdis
# 'Count of bus stops walking distance from candidates address',BSdis
# 'Sum_Service_Months', 'Retain_Success_6', 'Retain_Success_12', 'Retain_Success_24'
# total,6th,12th,24th
# 'total','six','twelve','twenty-four'

rename_data = bak_data_no_na
# rename_data = bak_data
colnames(rename_data) <- c('CH15','CHB15','AVG','CHwalk500dis','CHBdis','BSdis','total','six','twelve','twenty-four', 'job')
# Plotting a matrix of scatter plots between multiple variables
#ggpairs(rename_data,mapping = aes(color = total),upper = list(continuous = "points", combo = "box"),lower = list(continuous = "smooth", combo = "facetdensity"))


# Standardisation
data_std <- scale(bak_data_no_na)
# pca
data_pca <- prcomp(data_std, scale. = TRUE)
#View summary information for PCA
#summary(data_pca)
# The number of principal components retained was determined by the cumulative contribution of the principal components
# The contribution margin of each principal component indicates the degree of contribution of that principal component to the variance in the original dataset, and the cumulative contribution margin indicates the total contribution of the first n principal components to the variance in the original dataset.
# pca_data <- as.data.frame(pca_result$x[, 1:2])
# plot(data_pca$PC7, data_pca$PC8, main="PCA Plot")

# View cumulative variance contribution
# In the output, the variance contribution and the cumulative variance contribution of each principal component can be seen.
# Normally, it is possible to choose to retain the principal components whose cumulative variance contribution reaches a certain threshold.
# This threshold can be chosen as appropriate, for example retaining principal components with a cumulative variance contribution of 80% or 90%.
#cumsum(data_pca$sdev^2 / sum(data_pca$sdev^2))
# Retain the last 6 principal components
#keep_pca <- predict(data_pca, bak_data_no_na)[,5:10]
# 6 principal components after output
#print(keep_pca)

# Plotting scatter plots for principal component analysis
# ggplot(keep_pca, aes(x = keep_pca$PC5, y = keep_pca$PC6)) + geom_point()
# 
# # Histogram
# barplot(heights, names.arg = names, 
#         main = "Height Comparison", 
#         xlab = "Name", ylab = "Height", 
#         col = "blue")

# Find an interval percentage
range_percen <- function(data,col,month,cx,cy) {
  
  quantiles <- quantile(col, probs = c(0.33, 0.67))
  low1 = min(col)
  low2 = quantiles[1]
  middle1 = quantiles[1]+1
  middle2 = quantiles[2]
  high1 = quantiles[2]+1
  high2 = max(col)

  subset_data1 <- subset(data, col >= low1 & col <= low2, select = c(cx, cy, 'job'))
  percent_ones <- mean(subset_data1[cy] == 1) * 100
  percent_zeros <- mean(subset_data1[cy] == 0) * 100
  cat("percentage of low competitive range of Retain and not Retain：", percent_ones, "% ，", percent_zeros, "%", ' \n')
  
  # Job title does not distinguish between Retain and no Retain
  cat("The number of people in different positions in low competitive range is：", table(subset_data1$job), ' \n')

  subset_data2 <- subset(data, col >= middle1 & col >= middle2, select = c(cx,cy,'job'))
  percent_ones2 <- mean(subset_data2[cy] == 1) * 100
  percent_zeros2 <- mean(subset_data2[cy] == 0) * 100
  cat("percentage of low competitive range of Retain and not Retain：", percent_ones2, "% ，", percent_zeros2, "%", ' \n')
  
  cat("The number of people in different positions in medium competitive range is：", table(subset_data2$job), ' \n')

  subset_data3 <- subset(data, col >= high1 & col <= high2, select = c(cx,cy,'job'))
  percent_ones3 <- mean(subset_data3[cy] == 1) * 100
  percent_zeros3 <- mean(subset_data3[cy] == 0) * 100
  cat("percentage of high competitive range of Retain and not Retain：", percent_ones3, "% ，", percent_zeros3, "%", ' \n')
  cat("The number of people in different positions in high competitive range is：", table(subset_data3$job), ' \n')
 
}

# Retain_Success_6
#range_percen(rename_data, rename_data$CH15, rename_data$six, 'CH15', 'six')
#range_percen(rename_data, rename_data$CHB15, rename_data$six, 'CHB15', 'six')
#range_percen(rename_data, rename_data$AVG, rename_data$six, 'AVG', 'six')
#range_percen(rename_data, rename_data$CHwalk500dis, rename_data$six, 'CHwalk500dis', 'six')
#range_percen(rename_data, rename_data$CHBdis, rename_data$six, 'CHBdis', 'six')
#range_percen(rename_data, rename_data$BSdis, rename_data$six, 'BSdis', 'six')

# Retain_Success_12
#range_percen(rename_data, rename_data$CH15, rename_data$twelve, 'CH15', 'twelve')
#range_percen(rename_data, rename_data$CHB15, rename_data$twelve, 'CHB15', 'twelve')
#range_percen(rename_data, rename_data$AVG, rename_data$twelve, 'AVG', 'twelve')
#range_percen(rename_data, rename_data$CHwalk500dis, rename_data$twelve, 'CHwalk500dis', 'twelve')
#range_percen(rename_data, rename_data$CHBdis, rename_data$twelve, 'CHBdis', 'twelve')
#range_percen(rename_data, rename_data$BSdis, rename_data$twelve, 'BSdis', 'twelve')

# Retain_Success_24
#range_percen(rename_data, rename_data$CH15, rename_data$twenty-four, 'CH15', 'twenty-four')
#range_percen(rename_data, rename_data$CHB15, rename_data$twenty-four, 'CHB15', 'twenty-four')
#range_percen(rename_data, rename_data$AVG, rename_data$twenty-four, 'AVG', 'twenty-four')
#range_percen(rename_data, rename_data$CHwalk500dis, rename_data$twenty-four, 'CHwalk500dis', 'twenty-four')
#range_percen(rename_data, rename_data$CHBdis, rename_data$twenty-four, 'CHBdis', 'twenty-four')
#range_percen(rename_data, rename_data$BSdis, rename_data$twenty-four, 'BSdis', 'twenty-four')


# Plotting 6-24, low-mid-high grouping histograms for different zones
# x for low medium high or 6, 12, 24
# y are scaled values
#"Count of Care homes within 15 min drive"
#retain success
data_CH15_retain <- data.frame(Retain_success = c("6_months","12_months","24_months"),
                               Low_competitive_range = c(70.55695,52.67522,36.98373),
                               Median_competitive_range=c(70.27912,55.3719,39.29518),
                               High_competitive_range = c(70.33938,55.51687,39.41594)) 
#Data remodelling (wide to long)
data_CH15<-melt(data_CH15_retain,id.vars="Retain_success",variable.name="CH15",value.name="percentage")
#Bar chart display order according to month
data_CH15$Retain_success <- factor(data_CH15$Retain_success, levels = c("6_months","12_months","24_months"))
#Histogram of competitive intervals, grouped by month
CH15_1 <- ggplot(data_CH15,aes(Retain_success,percentage,fill=CH15))+
  geom_bar(stat="identity",position="dodge")+
  xlab("Retain month") +
  ylab("percentage of Retain_success(%)")+
  ggtitle("Count of Care homes within 15 min drive vs Retain month")+
  scale_x_discrete(limits = c("6_months","12_months","24_months"))
#Save image
#ggsave("CH15_1.png", CH15_1)
#retain not success
data_CH15_notretain <- data.frame(Not_Retain = c("6_months","12_months","24_months"),
                                  Low_competitive_range = c(29.44305,47.32478,63.01627),
                                  Median_competitive_range=c(29.72088,44.6281,60.70482),
                                  High_competitive_range = c(29.66062,44.48303,60.58406)) 
#Data remodelling (wide to long)
data_CH15<-melt(data_CH15_notretain,id.vars="Not_Retain",variable.name="CH15",value.name="percentage")
#Bar chart display order according to month
data_CH15$Not_Retain <- factor(data_CH15$Not_Retain, levels = c("6_months","12_months","24_months"))
CH15_2 <- ggplot(data_CH15,aes(Not_Retain,percentage,fill=CH15))+
  geom_bar(stat="identity",position="dodge")+
  xlab("Retain month") +
  ylab("percentage of Not_Retain(%)")+
  ggtitle("Count of Care homes within 15 min drive vs Retain month")+
  scale_x_discrete(limits = c("6_months","12_months","24_months"))
#ggsave("CH15-2.png", CH15_2)


###################Count of Care home Beds  within 15 min drive
#retain success
data_CHB15_retain <- data.frame(Retain_success = c("6_months","12_months","24_months"),
                                Low_competitive_range = c(70.56967,52.83257,37.3205),
                                Median_competitive_range=c(70.78209,55.89719,39.70356),
                                High_competitive_range = c(70.78794,55.89786,39.71262)) 

data_CHB15<-melt(data_CHB15_retain,id.vars="Retain_success",variable.name="CHB15",value.name="percentage")
data_CHB15$Retain_success <- factor(data_CHB15$Retain_success, levels = c("6_months","12_months","24_months"))
CHB15_1 <- ggplot(data_CHB15,aes(Retain_success,percentage,fill=CHB15))+
  geom_bar(stat="identity",position="dodge")+
  xlab("Retain month") +
  ylab("percentage of Retain_success(%)")+
  ggtitle("Count of Care home Beds  within 15 min drive vs Retain month")+
  scale_x_discrete(limits = c("6_months","12_months","24_months"))
#ggsave("CHB15_1.png", CHB15_1)
#retain not success
data_CHB15_notretain <- data.frame(Not_Retain = c("6_months","12_months","24_months"),
                                   Low_competitive_range = c(29.43033,47.16743,62.6795),
                                   Median_competitive_range=c(29.21791,44.10281,60.29644),
                                   High_competitive_range = c(29.21206,44.10232,60.28738)) 

data_CHB15<-melt(data_CHB15_notretain,id.vars="Not_Retain",variable.name="CHB15",value.name="percentage")
data_CHB15$Not_Retain <- factor(data_CHB15$Not_Retain, levels = c("6_months","12_months","24_months"))
CHB15_2 <- ggplot(data_CHB15,aes(Not_Retain,percentage,fill=CHB15))+
  geom_bar(stat="identity",position="dodge")+
  xlab("Retain month") +
  ylab("percentage of Not_Retain(%)")+
  ggtitle("Count of Care home Beds  within 15 min drive vs Retain month")+
  scale_x_discrete(limits = c("6_months","12_months","24_months"))
#ggsave("CHB15-2.png", CHB15_2)

############## 'Average age of the care homes within 15 min drive',AVG
#retain success
data_AVG_retain <- data.frame(Retain_success = c("6_months","12_months","24_months"),
                              Low_competitive_range = c(69.60279,52.47387,37.25436),
                              Median_competitive_range=c(70.61147,54.41917,37.50511),
                              High_competitive_range = c(70.77479,54.57648,37.71578)) 

data_AVG<-melt(data_AVG_retain,id.vars="Retain_success",variable.name="AVG",value.name="percentage")
data_AVG$Retain_success <- factor(data_AVG$Retain_success, levels = c("6_months","12_months","24_months"))
AVG_1 <- ggplot(data_AVG,aes(Retain_success,percentage,fill=AVG))+
  geom_bar(stat="identity",position="dodge")+
  xlab("Retain month") +
  ylab("percentage of Retain_success(%)")+
  ggtitle("Average age of the care homes within 15 min drive vs Retain month")+
  scale_x_discrete(limits = c("6_months","12_months","24_months"))
#ggsave("AVG_1.png", AVG_1)
#retain not success
data_AVG_notretain <- data.frame(Not_Retain = c("6_months","12_months","24_months"),
                                 Low_competitive_range = c(30.39721,47.52613,62.74564),
                                 Median_competitive_range=c(29.38853,45.58083,62.49489),
                                 High_competitive_range = c(29.22521,45.42352,62.28422)) 

data_AVG<-melt(data_AVG_notretain,id.vars="Not_Retain",variable.name="AVG",value.name="percentage")
data_AVG$Not_Retain <- factor(data_AVG$Not_Retain, levels = c("6_months","12_months","24_months"))
AVG_2 <- ggplot(data_AVG,aes(Not_Retain,percentage,fill=AVG))+
  geom_bar(stat="identity",position="dodge")+
  xlab("Retain month") +
  ylab("percentage of Not_Retain(%)")+
  ggtitle("Average age of the care homes within 15 min drive vs Retain month")+
  scale_x_discrete(limits = c("6_months","12_months","24_months"))
#ggsave("AVG-2.png", AVG_2)


################## 'Count of Care Homes within walking distance (<500 meters) from candidates address',CHwalk500dis
#retain success
data_CHwalk500dis_retain <- data.frame(Retain_success = c("6_months","12_months","24_months"),
                                       Low_competitive_range = c(69.99422,53.2326,37.07373),
                                       Median_competitive_range=c(69.77302,53.17919,37.1493),
                                       High_competitive_range = c(69.14393,52.68109,36.87676)) 

data_CHwalk500dis<-melt(data_CHwalk500dis_retain,id.vars="Retain_success",variable.name="CHwalk500dis",value.name="percentage")
data_CHwalk500dis$Retain_success <- factor(data_CHwalk500dis$Retain_success, levels = c("6_months","12_months","24_months"))
CHwalk500dis_1 <- ggplot(data_CHwalk500dis,aes(Retain_success,percentage,fill=CHwalk500dis))+
  geom_bar(stat="identity",position="dodge")+
  xlab("Retain month") +
  ylab("percentage of Retain_success(%)")+
  ggtitle("Count of Care Homes within walking distance (<500 meters) from candidates address vs Retain month")+
  scale_x_discrete(limits = c("6_months","12_months","24_months"))
#ggsave("CHwalk500dis_1.png", CHwalk500dis_1)
#retain not success
data_CHwalk500dis_notretain <- data.frame(Not_Retain = c("6_months","12_months","24_months"),
                                          Low_competitive_range = c(30.00578,46.7674,62.92627),
                                          Median_competitive_range=c(30.22698,46.82081,62.8507),
                                          High_competitive_range= c(30.85607,47.31891,63.12324)) 

data_CHwalk500dis<-melt(data_CHwalk500dis_notretain,id.vars="Not_Retain",variable.name="CHwalk500dis",value.name="percentage")
data_CHwalk500dis$Not_Retain <- factor(data_CHwalk500dis$Not_Retain, levels = c("6_months","12_months","24_months"))
CHwalk500dis_2 <- ggplot(data_CHwalk500dis,aes(Not_Retain,percentage,fill=CHwalk500dis))+
  geom_bar(stat="identity",position="dodge")+
  xlab("Retain month") +
  ylab("percentage of Not_Retain(%)")+
  ggtitle("Count of Care Homes within walking distance (<500 meters) from candidates address 
          vs Retain month")+
  scale_x_discrete(limits = c("6_months","12_months","24_months"))
#ggsave("CHwalk500dis-2.png", CHwalk500dis_2)

############# 'Sum of care home beds within walking distance from candidates address',CHBdis
#retain success
data_CHBdis_retain <- data.frame(Retain_success = c("6_months","12_months","24_months"),
                                 Low_competitive_range = c(69.99422,53.2326,37.07373),
                                 Median_competitive_range=c(69.96091,53.49492,37.21658),
                                 High_competitive_range = c(69.95395,53.51755,37.25584)) 

data_CHBdis<-melt(data_CHBdis_retain,id.vars="Retain_success",variable.name="CHBdis",value.name="percentage")
data_CHBdis$Retain_success <- factor(data_CHBdis$Retain_success, levels = c("6_months","12_months","24_months"))
CHBdis_1 <- ggplot(data_CHBdis,aes(Retain_success,percentage,fill=CHBdis))+
  geom_bar(stat="identity",position="dodge")+
  xlab("Retain month") +
  ylab("percentage of Retain_success(%)")+
  ggtitle("Sum of care home beds within walking distance from candidates address vs Retain month")+
  scale_x_discrete(limits = c("6_months","12_months","24_months"))
#ggsave("CHBdis_1.png",CHBdis_1)
#retain not success
data_CHBdis_notretain <- data.frame(Not_Retain = c("6_months","12_months","24_months"),
                                    Low_competitive_range = c(30.00578,46.7674,62.92627),
                                    Median_competitive_range=c(30.03909,46.50508,62.78342),
                                    High_competitive_range = c(30.04605,46.48245,62.74416)) 

data_CHBdis<-melt(data_CHBdis_notretain,id.vars="Not_Retain",variable.name="CHBdis",value.name="percentage")
data_CHBdis$Not_Retain <- factor(data_CHBdis$Not_Retain, levels = c("6_months","12_months","24_months"))
CHBdis_2 <- ggplot(data_CHBdis,aes(Not_Retain,percentage,fill=CHBdis))+
  geom_bar(stat="identity",position="dodge")+
  xlab("Retain month") +
  ylab("percentage of Not_Retain(%)")+
  ggtitle("Sum of care home beds within walking distance from candidates address vs Retain month")+
  scale_x_discrete(limits = c("6_months","12_months","24_months"))
#ggsave("CHBdis-2.png", CHBdis_2)

############ 'Count of bus stops walking distance from candidates address',BSdis
#retain success
data_BSdis_retain <- data.frame(Retain_success = c("6_months","12_months","24_months"),
                                Low_competitive_range = c(69.20094,52.0842,35.79837),
                                Median_competitive_range=c(71.1032,54.50534,38.19217),
                                High_competitive_range = c(71.29001,54.54252,38.23149)) 

data_BSdis<-melt(data_BSdis_retain,id.vars="Retain_success",variable.name="BSdis",value.name="percentage")
data_BSdis$Retain_success <- factor(data_BSdis$Retain_success, levels = c("6_months","12_months","24_months"))
BSdis_1 <- ggplot(data_BSdis,aes(Retain_success,percentage,fill=BSdis))+
  geom_bar(stat="identity",position="dodge")+
  xlab("Retain month") +
  ylab("percentage of Retain_success(%)")+
  ggtitle("Count of bus stops walking distance from candidates address vs Retain month")+
  scale_x_discrete(limits = c("6_months","12_months","24_months"))
#ggsave("BSdis_1.png",BSdis_1)
#retain not success
data_BSdis_notretain <- data.frame(Not_Retain = c("6_months","12_months","24_months"),
                                   Low_competitive_range= c(30.79906,47.9158,64.20163),
                                   Median_competitive_range=c(28.8968,45.49466,61.80783),
                                   High_competitive_range = c(28.70999,45.45748,61.76851)) 

data_BSdis<-melt(data_BSdis_notretain,id.vars="Not_Retain",variable.name="BSdis",value.name="percentage")
data_BSdis$Not_Retain <- factor(data_BSdis$Not_Retain, levels = c("6_months","12_months","24_months"))
BSdis_2 <- ggplot(data_BSdis,aes(Not_Retain,percentage,fill=BSdis))+
  geom_bar(stat="identity",position="dodge")+
  xlab("Retain month") +
  ylab("percentage of Not_Retain(%)")+
  ggtitle("Count of bus stops walking distance from candidates address vs Retain month")+
  scale_x_discrete(limits = c("6_months","12_months","24_months"))
#ggsave("BSdis-2.png", BSdis_2)


##############################################################################
#cycle2
#Define a function that adds the job title as a variable and then calculates the number of retainers in different competition intervals
range_percen_jobtitle <- function(data,col,month,cx,cy,title,type) {
  
  quantiles <- quantile(col, probs = c(0.33, 0.67))
  low1 = min(col)
  low2 = quantiles[1]
  middle1 = quantiles[1]+1
  middle2 = quantiles[2]
  high1 = quantiles[2]+1
  high2 = max(col)
  
  
  subset_data1 <- subset(data, col >= low1 & col <= low2 & title ==type, select = c(cx, cy))
  percent_ones <- mean(subset_data1[cy] == 1) * 100
  percent_zeros <- mean(subset_data1[cy] == 0) * 100
  #cat("percentage of low competitive range of Retain and not Retainin type",type, "job:",percent_ones, "% ，", percent_zeros, "%", ' \n')
  
  # Job title does not distinguish between Retain and no Retain
  #cat("The number of people in different positions in low competitive range is：", table(subset_data1$job), ' \n')
  
  subset_data2 <- subset(data, col >= middle1 & col >= middle2 &title ==type, select = c(cx,cy))
  percent_ones2 <- mean(subset_data2[cy] == 1) * 100
  percent_zeros2 <- mean(subset_data2[cy] == 0) * 100
  #cat("percentage of low competitive range of Retain and not Retainin type",type, "job:",percent_ones2, "% ，", percent_zeros2, "%", ' \n')
  
  #cat("The number of people in different positions in medium competitive range is：", table(subset_data2$job), ' \n')
  
  subset_data3 <- subset(data, col >= high1 & col <= high2 & title == type, select = c(cx,cy))
  percent_ones3 <- mean(subset_data3[cy] == 1) * 100
  percent_zeros3 <- mean(subset_data3[cy] == 0) * 100
  #cat("percentage of high competitive range of Retain and not Retain in type",type, "job:",  percent_ones3, "% ，", percent_zeros3, "%", ' \n')
  #cat("The number of people in different positions in high competitive range is：", table(subset_data3$job), ' \n')
  return(data.frame(percent_ones,percent_ones2,percent_ones3))
}

#####################
#nursing 
#Use function to output the probability of different retain times
# Retain_Success_6
CH15_nursing_6 <-range_percen_jobtitle(rename_data, rename_data$CH15, rename_data$six, 'CH15','six',rename_data$job,'1')
CHB15_nursing_6 <-range_percen_jobtitle(rename_data, rename_data$CHB15, rename_data$six, 'CHB15', 'six',rename_data$job,'1')
AVG_nursing_6 <-range_percen_jobtitle(rename_data, rename_data$AVG, rename_data$six, 'AVG', 'six',rename_data$job,'1')
CHwalk500dis_nursing_6 <- range_percen_jobtitle(rename_data, rename_data$CHwalk500dis, rename_data$six, 'CHwalk500dis', 'six',rename_data$job,'1')
CHBdis_nursing_6<-range_percen_jobtitle(rename_data, rename_data$CHBdis, rename_data$six, 'CHBdis', 'six',rename_data$job,'1')
BSdis_nursing_6<-range_percen_jobtitle(rename_data, rename_data$BSdis, rename_data$six, 'BSdis', 'six',rename_data$job,'1')

# Retain_Success_12
CH15_nursing_12<-range_percen_jobtitle(rename_data, rename_data$CH15, rename_data$twelve, 'CH15', 'twelve',rename_data$job,'1')
CHB15_nursing_12<-range_percen_jobtitle(rename_data, rename_data$CHB15, rename_data$twelve, 'CHB15', 'twelve',rename_data$job,'1')
AVG_nursing_12<- range_percen_jobtitle(rename_data, rename_data$AVG, rename_data$twelve, 'AVG', 'twelve',rename_data$job,'1')
CHwalk500dis_nursing_12<-range_percen_jobtitle(rename_data, rename_data$CHwalk500dis, rename_data$twelve, 'CHwalk500dis', 'twelve',rename_data$job,'1')
CHBdis_nursing_12<-range_percen_jobtitle(rename_data, rename_data$CHBdis, rename_data$twelve, 'CHBdis', 'twelve',rename_data$job,'1')
BSdis_nursing_12<-range_percen_jobtitle(rename_data, rename_data$BSdis, rename_data$twelve, 'BSdis', 'twelve',rename_data$job,'1')

# Retain_Success_24
CH15_nursing_24<-range_percen_jobtitle(rename_data, rename_data$CH15, rename_data$twenty-four, 'CH15', 'twenty-four',rename_data$job,'1')
CHB15_nursing_24<-range_percen_jobtitle(rename_data, rename_data$CHB15, rename_data$twenty-four, 'CHB15', 'twenty-four',rename_data$job,'1')
AVG_nursing_24<-range_percen_jobtitle(rename_data, rename_data$AVG, rename_data$twenty-four, 'AVG', 'twenty-four',rename_data$job,'1')
CHwalk500dis_nursing_24<-range_percen_jobtitle(rename_data, rename_data$CHwalk500dis, rename_data$twenty-four, 'CHwalk500dis', 'twenty-four',rename_data$job,'1')
CHBdis_nursing_24<-range_percen_jobtitle(rename_data, rename_data$CHBdis, rename_data$twenty-four, 'CHBdis', 'twenty-four',rename_data$job,'1')
BSdis_nursing_24<-range_percen_jobtitle(rename_data, rename_data$BSdis, rename_data$twenty-four, 'BSdis', 'twenty-four',rename_data$job,'1')

CH15_data_nursing <- data.frame(Retain_success = c("6_months","12_months","24_months"),
                                Low_competitive_range = c(CH15_nursing_6$percent_ones,CH15_nursing_12$percent_ones,CH15_nursing_24$percent_ones),
                                Median_competitive_range=c(CH15_nursing_6$percent_ones2,CH15_nursing_12$percent_ones2,CH15_nursing_24$percent_ones2),
                                High_competitive_range = c(CH15_nursing_6$percent_ones3,CH15_nursing_12$percent_ones3,CH15_nursing_24$percent_ones3))
CHB15_data_nursing <- data.frame(Retain_success = c("6_months","12_months","24_months"),
                                Low_competitive_range = c(CHB15_nursing_6$percent_ones,CHB15_nursing_12$percent_ones,CHB15_nursing_24$percent_ones),
                                Median_competitive_range=c(CHB15_nursing_6$percent_ones2,CHB15_nursing_12$percent_ones2,CHB15_nursing_24$percent_ones2),
                                High_competitive_range = c(CHB15_nursing_6$percent_ones3,CHB15_nursing_12$percent_ones3,CHB15_nursing_24$percent_ones3))
AVG_data_nursing <- data.frame(Retain_success = c("6_months","12_months","24_months"),
                                Low_competitive_range = c(AVG_nursing_6 $percent_ones,AVG_nursing_12$percent_ones,AVG_nursing_24$percent_ones),
                                Median_competitive_range=c(AVG_nursing_6 $percent_ones2,AVG_nursing_12$percent_ones2,AVG_nursing_24$percent_ones2),
                                High_competitive_range = c(AVG_nursing_6 $percent_ones3,AVG_nursing_12$percent_ones3,AVG_nursing_24$percent_ones3))
CHwalk500dis_data_nursing <- data.frame(Retain_success = c("6_months","12_months","24_months"),
                                Low_competitive_range = c(CHwalk500dis_nursing_6$percent_ones,CHwalk500dis_nursing_12$percent_ones,CHwalk500dis_nursing_24$percent_ones),
                                Median_competitive_range=c(CHwalk500dis_nursing_6$percent_ones2,CHwalk500dis_nursing_12$percent_ones2,CHwalk500dis_nursing_24$percent_ones2),
                                High_competitive_range = c(CHwalk500dis_nursing_6$percent_ones3,CHwalk500dis_nursing_12$percent_ones3,CHwalk500dis_nursing_24$percent_ones3))
CHBdis_data_nursing <- data.frame(Retain_success = c("6_months","12_months","24_months"),
                                Low_competitive_range = c(CHBdis_nursing_6$percent_ones,CHBdis_nursing_12$percent_ones,CHBdis_nursing_24$percent_ones),
                                Median_competitive_range=c(CHBdis_nursing_6$percent_ones2,CHBdis_nursing_12$percent_ones2,CHBdis_nursing_24$percent_ones2),
                                High_competitive_range = c(CHBdis_nursing_6$percent_ones3,CHBdis_nursing_12$percent_ones3,CHBdis_nursing_24$percent_ones3))
BSdis_data_nursing <- data.frame(Retain_success = c("6_months","12_months","24_months"),
                                Low_competitive_range = c(BSdis_nursing_6$percent_ones,BSdis_nursing_12$percent_ones,BSdis_nursing_24$percent_ones),
                                Median_competitive_range=c(BSdis_nursing_6$percent_ones2,BSdis_nursing_12$percent_ones2,BSdis_nursing_24$percent_ones2),
                                High_competitive_range = c(BSdis_nursing_6$percent_ones3,BSdis_nursing_12$percent_ones3,BSdis_nursing_24$percent_ones3))
#"Count of Care homes within 15 min drive" VS Nursing
#retain success
#Data remodelling (wide to long)
data_CH15_job<-melt(CH15_data_nursing,id.vars="Retain_success",variable.name="CH15",value.name="percentage")
#Bar chart display order according to month
data_CH15_job$Retain_success <- factor(data_CH15_job$Retain_success, levels = c("6_months","12_months","24_months"))
#Histogram of competitive intervals, grouped by month
CH15_1_job <- ggplot(data_CH15_job,aes(Retain_success,percentage,fill=CH15))+
  geom_bar(stat="identity",position="dodge")+
  xlab("Retain month") +
  ylab("percentage of Retain_success(%)")+
  ggtitle("Count of Care homes within 15 min drive(Nursing)")+
  scale_x_discrete(limits = c("6_months","12_months","24_months"))
#Save image

#CH15_1_job
###################Count of Care home Beds  within 15 min drive
#retain success
data_CHB15_job<-melt(CHB15_data_nursing,id.vars="Retain_success",variable.name="CHB15",value.name="percentage")
data_CHB15_job$Retain_success <- factor(data_CHB15_job$Retain_success, levels = c("6_months","12_months","24_months"))
CHB15_1_job <- ggplot(data_CHB15_job,aes(Retain_success,percentage,fill=CHB15))+
  geom_bar(stat="identity",position="dodge")+
  xlab("Retain month") +
  ylab("percentage of Retain_success(%)")+
  ggtitle("Count of Care home Beds  within 15 min drive(Nursing)")+
  scale_x_discrete(limits = c("6_months","12_months","24_months"))

#CHB15_1_job

############## 'Average age of the care homes within 15 min drive',AVG
#retain success
data_AVG_job<-melt(AVG_data_nursing,id.vars="Retain_success",variable.name="AVG",value.name="percentage")
data_AVG_job$Retain_success <- factor(data_AVG_job$Retain_success, levels = c("6_months","12_months","24_months"))
AVG_1_job <- ggplot(data_AVG_job,aes(Retain_success,percentage,fill=AVG))+
  geom_bar(stat="identity",position="dodge")+
  xlab("Retain month") +
  ylab("percentage of Retain_success(%)")+
  ggtitle("Average age of the care homes within 15 min drive(Nursing)")+
  scale_x_discrete(limits = c("6_months","12_months","24_months"))



################## 'Count of Care Homes within walking distance (<500 meters) from candidates address',CHwalk500dis
#retain success

data_CHwalk500dis_job<-melt(CHwalk500dis_data_nursing,id.vars="Retain_success",variable.name="CHwalk500dis",value.name="percentage")
data_CHwalk500dis_job$Retain_success <- factor(data_CHwalk500dis_job$Retain_success, levels = c("6_months","12_months","24_months"))
CHwalk500dis_1_job <- ggplot(data_CHwalk500dis_job,aes(Retain_success,percentage,fill=CHwalk500dis))+
  geom_bar(stat="identity",position="dodge")+
  xlab("Retain month") +
  ylab("percentage of Retain_success(%)")+
  ggtitle("Count of Care Homes within walking distance(<500 meters)from candidates address(Nursing)")+
  scale_x_discrete(limits = c("6_months","12_months","24_months"))


############# 'Sum of care home beds within walking distance from candidates address',CHBdis
#retain success

data_CHBdis_job<-melt(CHBdis_data_nursing,id.vars="Retain_success",variable.name="CHBdis",value.name="percentage")
data_CHBdis_job$Retain_success <- factor(data_CHBdis_job$Retain_success, levels = c("6_months","12_months","24_months"))
CHBdis_1_job <- ggplot(data_CHBdis_job,aes(Retain_success,percentage,fill=CHBdis))+
  geom_bar(stat="identity",position="dodge")+
  xlab("Retain month") +
  ylab("percentage of Retain_success(%)")+
  ggtitle("Sum of care home beds within walking distance from candidates address(Nursing)")+
  scale_x_discrete(limits = c("6_months","12_months","24_months"))


############ 'Count of bus stops walking distance from candidates address',BSdis
#retain success

data_BSdis_job<-melt(BSdis_data_nursing,id.vars="Retain_success",variable.name="BSdis",value.name="percentage")
data_BSdis_job$Retain_success <- factor(data_BSdis_job$Retain_success, levels = c("6_months","12_months","24_months"))
BSdis_1_job <- ggplot(data_BSdis_job,aes(Retain_success,percentage,fill=BSdis))+
  geom_bar(stat="identity",position="dodge")+
  xlab("Retain month") +
  ylab("percentage of Retain_success(%)")+
  ggtitle("Count of bus stops walking distance from candidates address(Nursing)")+
  scale_x_discrete(limits = c("6_months","12_months","24_months"))

###Aggregate the images of retain
CH15_j <- CH15_1_job + theme(text = element_text(size = 6))
CHB15_j<- CHB15_1_job + theme(text = element_text(size = 5))
AVG_j<-AVG_1_job +theme(text = element_text(size = 6))
CHwalk500dis_j<-CHwalk500dis_1_job +theme(text = element_text(size = 5))
CHBdis_j<-CHBdis_1_job +theme(text = element_text(size = 5))
BSdis_j<-BSdis_1_job +theme(text = element_text(size = 5))
nursing_retain <- ggarrange(CH15_j,CHB15_j,AVG_j,CHwalk500dis_j,CHBdis_j,BSdis_j, nrow = 3, ncol = 2, widths = c(6,6), heights = c(5,5))
#ggsave("nursing_retain.png",nursing_retain)
########################################################
#Carer
# Retain_Success_6
CH15_carer_6 <-range_percen_jobtitle(rename_data, rename_data$CH15, rename_data$six, 'CH15','six',rename_data$job,'2')
CHB15_carer_6 <-range_percen_jobtitle(rename_data, rename_data$CHB15, rename_data$six, 'CHB15', 'six',rename_data$job,'2')
AVG_carer_6 <-range_percen_jobtitle(rename_data, rename_data$AVG, rename_data$six, 'AVG', 'six',rename_data$job,'2')
CHwalk500dis_carer_6 <- range_percen_jobtitle(rename_data, rename_data$CHwalk500dis, rename_data$six, 'CHwalk500dis', 'six',rename_data$job,'2')
CHBdis_carer_6<-range_percen_jobtitle(rename_data, rename_data$CHBdis, rename_data$six, 'CHBdis', 'six',rename_data$job,'2')
BSdis_carer_6<-range_percen_jobtitle(rename_data, rename_data$BSdis, rename_data$six, 'BSdis', 'six',rename_data$job,'2')

# Retain_Success_12
CH15_carer_12<-range_percen_jobtitle(rename_data, rename_data$CH15, rename_data$twelve, 'CH15', 'twelve',rename_data$job,'2')
CHB15_carer_12<-range_percen_jobtitle(rename_data, rename_data$CHB15, rename_data$twelve, 'CHB15', 'twelve',rename_data$job,'2')
AVG_carer_12<- range_percen_jobtitle(rename_data, rename_data$AVG, rename_data$twelve, 'AVG', 'twelve',rename_data$job,'2')
CHwalk500dis_carer_12<-range_percen_jobtitle(rename_data, rename_data$CHwalk500dis, rename_data$twelve, 'CHwalk500dis', 'twelve',rename_data$job,'2')
CHBdis_carer_12<-range_percen_jobtitle(rename_data, rename_data$CHBdis, rename_data$twelve, 'CHBdis', 'twelve',rename_data$job,'2')
BSdis_carer_12<-range_percen_jobtitle(rename_data, rename_data$BSdis, rename_data$twelve, 'BSdis', 'twelve',rename_data$job,'2')

# Retain_Success_24
CH15_carer_24<-range_percen_jobtitle(rename_data, rename_data$CH15, rename_data$twenty-four, 'CH15', 'twenty-four',rename_data$job,'2')
CHB15_carer_24<-range_percen_jobtitle(rename_data, rename_data$CHB15, rename_data$twenty-four, 'CHB15', 'twenty-four',rename_data$job,'2')
AVG_carer_24<-range_percen_jobtitle(rename_data, rename_data$AVG, rename_data$twenty-four, 'AVG', 'twenty-four',rename_data$job,'2')
CHwalk500dis_carer_24<-range_percen_jobtitle(rename_data, rename_data$CHwalk500dis, rename_data$twenty-four, 'CHwalk500dis', 'twenty-four',rename_data$job,'2')
CHBdis_carer_24<-range_percen_jobtitle(rename_data, rename_data$CHBdis, rename_data$twenty-four, 'CHBdis', 'twenty-four',rename_data$job,'2')
BSdis_carer_24<-range_percen_jobtitle(rename_data, rename_data$BSdis, rename_data$twenty-four, 'BSdis', 'twenty-four',rename_data$job,'2')

CH15_data_carer <- data.frame(Retain_success = c("6_months","12_months","24_months"),
                                Low_competitive_range = c(CH15_carer_6$percent_ones,CH15_carer_12$percent_ones,CH15_carer_24$percent_ones),
                                Median_competitive_range=c(CH15_carer_6$percent_ones2,CH15_carer_12$percent_ones2,CH15_carer_24$percent_ones2),
                                High_competitive_range = c(CH15_carer_6$percent_ones3,CH15_carer_12$percent_ones3,CH15_carer_24$percent_ones3))
CHB15_data_carer <- data.frame(Retain_success = c("6_months","12_months","24_months"),
                                 Low_competitive_range = c(CHB15_carer_6$percent_ones,CHB15_carer_12$percent_ones,CHB15_carer_24$percent_ones),
                                 Median_competitive_range=c(CHB15_carer_6$percent_ones2,CHB15_carer_12$percent_ones2,CHB15_carer_24$percent_ones2),
                                 High_competitive_range = c(CHB15_carer_6$percent_ones3,CHB15_carer_12$percent_ones3,CHB15_carer_24$percent_ones3))
AVG_data_carer <- data.frame(Retain_success = c("6_months","12_months","24_months"),
                               Low_competitive_range = c(AVG_carer_6$percent_ones,AVG_carer_12$percent_ones,AVG_carer_24$percent_ones),
                               Median_competitive_range=c(AVG_carer_6$percent_ones2,AVG_carer_12$percent_ones2,AVG_carer_24$percent_ones2),
                               High_competitive_range = c(AVG_carer_6$percent_ones3,AVG_carer_12$percent_ones3,AVG_carer_24$percent_ones3))
CHwalk500dis_data_carer <- data.frame(Retain_success = c("6_months","12_months","24_months"),
                                      Low_competitive_range = c(CHwalk500dis_carer_6$percent_ones,CHwalk500dis_carer_12$percent_ones,CHwalk500dis_carer_24$percent_ones),
                                      Median_competitive_range=c(CHwalk500dis_carer_6$percent_ones2,CHwalk500dis_carer_12$percent_ones2,CHwalk500dis_carer_24$percent_ones2),
                                      High_competitive_range = c(CHwalk500dis_carer_6$percent_ones3,CHwalk500dis_carer_12$percent_ones3,CHwalk500dis_carer_24$percent_ones3))
CHBdis_data_carer <- data.frame(Retain_success = c("6_months","12_months","24_months"),
                                Low_competitive_range = c(CHBdis_carer_6$percent_ones,CHBdis_carer_12$percent_ones,CHBdis_carer_24$percent_ones),
                                Median_competitive_range=c(CHBdis_carer_6$percent_ones2,CHBdis_carer_12$percent_ones2,CHBdis_carer_24$percent_ones2),
                                High_competitive_range = c(CHBdis_carer_6$percent_ones3,CHBdis_carer_12$percent_ones3,CHBdis_carer_24$percent_ones3))
BSdis_data_carer <- data.frame(Retain_success = c("6_months","12_months","24_months"),
                                Low_competitive_range = c(BSdis_carer_6$percent_ones,BSdis_carer_12$percent_ones,BSdis_carer_24$percent_ones),
                                Median_competitive_range=c(BSdis_carer_6$percent_ones2,BSdis_carer_12$percent_ones2,BSdis_carer_24$percent_ones2),
                                High_competitive_range = c(BSdis_carer_6$percent_ones3,BSdis_carer_12$percent_ones3,BSdis_carer_24$percent_ones3))

#"Count of Care homes within 15 min drive" VS Carer
#retain success
#Data remodelling (wide to long)
data_CH15_job2<-melt(CH15_data_carer ,id.vars="Retain_success",variable.name="CH15",value.name="percentage")
#Bar chart display order according to month
data_CH15_job2$Retain_success <- factor(data_CH15_job2$Retain_success, levels = c("6_months","12_months","24_months"))
#Histogram of competitive intervals, grouped by month
CH15_1_job2 <- ggplot(data_CH15_job2,aes(Retain_success,percentage,fill=CH15))+
  geom_bar(stat="identity",position="dodge")+
  xlab("Retain month") +
  ylab("percentage of Retain_success(%)")+
  ggtitle("Count of Care homes within 15 min drive(Carer)")+
  scale_x_discrete(limits = c("6_months","12_months","24_months"))
#CH15_1_job2
###################Count of Care home Beds  within 15 min drive
#retain success
data_CHB15_job2<-melt(CHB15_data_carer,id.vars="Retain_success",variable.name="CHB15",value.name="percentage")
data_CHB15_job2$Retain_success <- factor(data_CHB15_job2$Retain_success, levels = c("6_months","12_months","24_months"))
CHB15_1_job2 <- ggplot(data_CHB15_job2,aes(Retain_success,percentage,fill=CHB15))+
  geom_bar(stat="identity",position="dodge")+
  xlab("Retain month") +
  ylab("percentage of Retain_success(%)")+
  ggtitle("Count of Care home Beds  within 15 min drive(Carer)")+
  scale_x_discrete(limits = c("6_months","12_months","24_months"))

#CHB15_1_job2

############## 'Average age of the care homes within 15 min drive',AVG
#retain success
data_AVG_job2<-melt(AVG_data_carer,id.vars="Retain_success",variable.name="AVG",value.name="percentage")
data_AVG_job2$Retain_success <- factor(data_AVG_job2$Retain_success, levels = c("6_months","12_months","24_months"))
AVG_1_job2 <- ggplot(data_AVG_job2,aes(Retain_success,percentage,fill=AVG))+
  geom_bar(stat="identity",position="dodge")+
  xlab("Retain month") +
  ylab("percentage of Retain_success(%)")+
  ggtitle("Average age of the care homes within 15 min drive(Carer)")+
  scale_x_discrete(limits = c("6_months","12_months","24_months"))



################## 'Count of Care Homes within walking distance (<500 meters) from candidates address',CHwalk500dis
#retain success

data_CHwalk500dis_job2<-melt(CHwalk500dis_data_carer,id.vars="Retain_success",variable.name="CHwalk500dis",value.name="percentage")
data_CHwalk500dis_job2$Retain_success <- factor(data_CHwalk500dis_job2$Retain_success, levels = c("6_months","12_months","24_months"))
CHwalk500dis_1_job2 <- ggplot(data_CHwalk500dis_job2,aes(Retain_success,percentage,fill=CHwalk500dis))+
  geom_bar(stat="identity",position="dodge")+
  xlab("Retain month") +
  ylab("percentage of Retain_success(%)")+
  ggtitle("Count of Care Homes within walking distance(<500 meters)from candidates address(Carer)")+
  scale_x_discrete(limits = c("6_months","12_months","24_months"))


############# 'Sum of care home beds within walking distance from candidates address',CHBdis
#retain success

data_CHBdis_job2<-melt(CHBdis_data_carer,id.vars="Retain_success",variable.name="CHBdis",value.name="percentage")
data_CHBdis_job2$Retain_success <- factor(data_CHBdis_job2$Retain_success, levels = c("6_months","12_months","24_months"))
CHBdis_1_job2 <- ggplot(data_CHBdis_job2,aes(Retain_success,percentage,fill=CHBdis))+
  geom_bar(stat="identity",position="dodge")+
  xlab("Retain month") +
  ylab("percentage of Retain_success(%)")+
  ggtitle("Sum of care home beds within walking distance from candidates address(Carer)")+
  scale_x_discrete(limits = c("6_months","12_months","24_months"))



############ 'Count of bus stops walking distance from candidates address',BSdis
#retain success

data_BSdis_job2<-melt(BSdis_data_carer,id.vars="Retain_success",variable.name="BSdis",value.name="percentage")
data_BSdis_job2$Retain_success <- factor(data_BSdis_job2$Retain_success, levels = c("6_months","12_months","24_months"))
BSdis_1_job2 <- ggplot(data_BSdis_job2,aes(Retain_success,percentage,fill=BSdis))+
  geom_bar(stat="identity",position="dodge")+
  xlab("Retain month") +
  ylab("percentage of Retain_success(%)")+
  ggtitle("Count of bus stops walking distance from candidates address(Carer)")+
  scale_x_discrete(limits = c("6_months","12_months","24_months"))



###Aggregate the images of retain
CH15_j2 <- CH15_1_job2 + theme(text = element_text(size = 6))
CHB15_j2<- CHB15_1_job2 + theme(text = element_text(size = 5))
AVG_j2<-AVG_1_job2 +theme(text = element_text(size = 6))
CHwalk500dis_j2<-CHwalk500dis_1_job2 +theme(text = element_text(size = 5))
CHBdis_j2<-CHBdis_1_job2 +theme(text = element_text(size = 5))
BSdis_j2<-BSdis_1_job2 +theme(text = element_text(size = 5))
carer_retain <- ggarrange(CH15_j2,CHB15_j2,AVG_j2,CHwalk500dis_j2,CHBdis_j2,BSdis_j2, nrow = 3, ncol = 2, widths = c(6,6), heights = c(5,5))
#ggsave("carer_retain.png",carer_retain)
###########################################################
#Senior Carer
# Retain_Success_6
CH15_seniorcarer_6 <-range_percen_jobtitle(rename_data, rename_data$CH15, rename_data$six, 'CH15','six',rename_data$job,'3')
CHB15_seniorcarer_6 <-range_percen_jobtitle(rename_data, rename_data$CHB15, rename_data$six, 'CHB15', 'six',rename_data$job,'3')
AVG_seniorcarer_6 <-range_percen_jobtitle(rename_data, rename_data$AVG, rename_data$six, 'AVG', 'six',rename_data$job,'3')
CHwalk500dis_seniorcarer_6 <- range_percen_jobtitle(rename_data, rename_data$CHwalk500dis, rename_data$six, 'CHwalk500dis', 'six',rename_data$job,'3')
CHBdis_seniorcarer_6<-range_percen_jobtitle(rename_data, rename_data$CHBdis, rename_data$six, 'CHBdis', 'six',rename_data$job,'3')
BSdis_seniorcarer_6<-range_percen_jobtitle(rename_data, rename_data$BSdis, rename_data$six, 'BSdis', 'six',rename_data$job,'3')

# Retain_Success_12
CH15_seniorcarer_12<-range_percen_jobtitle(rename_data, rename_data$CH15, rename_data$twelve, 'CH15', 'twelve',rename_data$job,'3')
CHB15_seniorcarer_12<-range_percen_jobtitle(rename_data, rename_data$CHB15, rename_data$twelve, 'CHB15', 'twelve',rename_data$job,'3')
AVG_seniorcarer_12<- range_percen_jobtitle(rename_data, rename_data$AVG, rename_data$twelve, 'AVG', 'twelve',rename_data$job,'3')
CHwalk500dis_seniorcarer_12<-range_percen_jobtitle(rename_data, rename_data$CHwalk500dis, rename_data$twelve, 'CHwalk500dis', 'twelve',rename_data$job,'3')
CHBdis_seniorcarer_12<-range_percen_jobtitle(rename_data, rename_data$CHBdis, rename_data$twelve, 'CHBdis', 'twelve',rename_data$job,'3')
BSdis_seniorcarer_12<-range_percen_jobtitle(rename_data, rename_data$BSdis, rename_data$twelve, 'BSdis', 'twelve',rename_data$job,'3')

# Retain_Success_24
CH15_seniorcarer_24<-range_percen_jobtitle(rename_data, rename_data$CH15, rename_data$twenty-four, 'CH15', 'twenty-four',rename_data$job,'3')
CHB15_seniorcarer_24<-range_percen_jobtitle(rename_data, rename_data$CHB15, rename_data$twenty-four, 'CHB15', 'twenty-four',rename_data$job,'3')
AVG_seniorcarer_24<-range_percen_jobtitle(rename_data, rename_data$AVG, rename_data$twenty-four, 'AVG', 'twenty-four',rename_data$job,'3')
CHwalk500dis_seniorcarer_24<-range_percen_jobtitle(rename_data, rename_data$CHwalk500dis, rename_data$twenty-four, 'CHwalk500dis', 'twenty-four',rename_data$job,'3')
CHBdis_seniorcarer_24<-range_percen_jobtitle(rename_data, rename_data$CHBdis, rename_data$twenty-four, 'CHBdis', 'twenty-four',rename_data$job,'3')
BSdis_seniorcarer_24<-range_percen_jobtitle(rename_data, rename_data$BSdis, rename_data$twenty-four, 'BSdis', 'twenty-four',rename_data$job,'3')

CH15_data_seniorcarer <- data.frame(Retain_success = c("6_months","12_months","24_months"),
                              Low_competitive_range = c(CH15_seniorcarer_6$percent_ones,CH15_seniorcarer_12$percent_ones,CH15_seniorcarer_24$percent_ones),
                              Median_competitive_range=c(CH15_seniorcarer_6$percent_ones2,CH15_seniorcarer_12$percent_ones2,CH15_seniorcarer_24$percent_ones2),
                              High_competitive_range = c(CH15_seniorcarer_6$percent_ones3,CH15_seniorcarer_12$percent_ones3,CH15_seniorcarer_24$percent_ones3))
CHB15_data_seniorcarer <- data.frame(Retain_success = c("6_months","12_months","24_months"),
                               Low_competitive_range = c(CHB15_seniorcarer_6$percent_ones,CHB15_seniorcarer_12$percent_ones,CHB15_seniorcarer_24$percent_ones),
                               Median_competitive_range=c(CHB15_seniorcarer_6$percent_ones2,CHB15_seniorcarer_12$percent_ones2,CHB15_seniorcarer_24$percent_ones2),
                               High_competitive_range = c(CHB15_seniorcarer_6$percent_ones3,CHB15_seniorcarer_12$percent_ones3,CHB15_seniorcarer_24$percent_ones3))
AVG_data_seniorcarer <- data.frame(Retain_success = c("6_months","12_months","24_months"),
                             Low_competitive_range = c(AVG_seniorcarer_6$percent_ones,AVG_seniorcarer_12$percent_ones,AVG_seniorcarer_24$percent_ones),
                             Median_competitive_range=c(AVG_seniorcarer_6$percent_ones2,AVG_seniorcarer_12$percent_ones2,AVG_seniorcarer_24$percent_ones2),
                             High_competitive_range = c(AVG_seniorcarer_6$percent_ones3,AVG_seniorcarer_12$percent_ones3,AVG_seniorcarer_24$percent_ones3))
CHwalk500dis_data_seniorcarer <- data.frame(Retain_success = c("6_months","12_months","24_months"),
                                      Low_competitive_range = c(CHwalk500dis_seniorcarer_6$percent_ones,CHwalk500dis_seniorcarer_12$percent_ones,CHwalk500dis_seniorcarer_24$percent_ones),
                                      Median_competitive_range=c(CHwalk500dis_seniorcarer_6$percent_ones2,CHwalk500dis_seniorcarer_12$percent_ones2,CHwalk500dis_seniorcarer_24$percent_ones2),
                                      High_competitive_range = c(CHwalk500dis_seniorcarer_6$percent_ones3,CHwalk500dis_seniorcarer_12$percent_ones3,CHwalk500dis_seniorcarer_24$percent_ones3))
CHBdis_data_seniorcarer <- data.frame(Retain_success = c("6_months","12_months","24_months"),
                                Low_competitive_range = c(CHBdis_seniorcarer_6$percent_ones,CHBdis_seniorcarer_12$percent_ones,CHBdis_seniorcarer_24$percent_ones),
                                Median_competitive_range=c(CHBdis_seniorcarer_6$percent_ones2,CHBdis_seniorcarer_12$percent_ones2,CHBdis_seniorcarer_24$percent_ones2),
                                High_competitive_range = c(CHBdis_seniorcarer_6$percent_ones3,CHBdis_seniorcarer_12$percent_ones3,CHBdis_seniorcarer_24$percent_ones3))
BSdis_data_seniorcarer <- data.frame(Retain_success = c("6_months","12_months","24_months"),
                              Low_competitive_range = c(BSdis_seniorcarer_6$percent_ones,BSdis_seniorcarer_12$percent_ones,BSdis_seniorcarer_24$percent_ones),
                              Median_competitive_range=c(BSdis_seniorcarer_6$percent_ones2,BSdis_seniorcarer_12$percent_ones2,BSdis_seniorcarer_24$percent_ones2),
                              High_competitive_range = c(BSdis_seniorcarer_6$percent_ones3,BSdis_seniorcarer_12$percent_ones3,BSdis_seniorcarer_24$percent_ones3))



#"Count of Care homes within 15 min drive" VS Carer
#retain success
#Data remodelling (wide to long)
data_CH15_job3<-melt(CH15_data_seniorcarer ,id.vars="Retain_success",variable.name="CH15",value.name="percentage")
#Bar chart display order according to month
data_CH15_job3$Retain_success <- factor(data_CH15_job3$Retain_success, levels = c("6_months","12_months","24_months"))
#Histogram of competitive intervals, grouped by month
CH15_1_job3 <- ggplot(data_CH15_job3,aes(Retain_success,percentage,fill=CH15))+
  geom_bar(stat="identity",position="dodge")+
  xlab("Retain month") +
  ylab("percentage of Retain_success(%)")+
  ggtitle("Count of Care homes within 15 min drive(Senior Carer)")+
  scale_x_discrete(limits = c("6_months","12_months","24_months"))
#CH15_1_job2
###################Count of Care home Beds  within 15 min drive
#retain success
data_CHB15_job3<-melt(CHB15_data_seniorcarer,id.vars="Retain_success",variable.name="CHB15",value.name="percentage")
data_CHB15_job3$Retain_success <- factor(data_CHB15_job3$Retain_success, levels = c("6_months","12_months","24_months"))
CHB15_1_job3 <- ggplot(data_CHB15_job3,aes(Retain_success,percentage,fill=CHB15))+
  geom_bar(stat="identity",position="dodge")+
  xlab("Retain month") +
  ylab("percentage of Retain_success(%)")+
  ggtitle("Count of Care home Beds  within 15 min drive(SeniorCarer)")+
  scale_x_discrete(limits = c("6_months","12_months","24_months"))

#CHB15_1_job2

############## 'Average age of the care homes within 15 min drive',AVG
#retain success
data_AVG_job3<-melt(AVG_data_seniorcarer,id.vars="Retain_success",variable.name="AVG",value.name="percentage")
data_AVG_job3$Retain_success <- factor(data_AVG_job3$Retain_success, levels = c("6_months","12_months","24_months"))
AVG_1_job3 <- ggplot(data_AVG_job3,aes(Retain_success,percentage,fill=AVG))+
  geom_bar(stat="identity",position="dodge")+
  xlab("Retain month") +
  ylab("percentage of Retain_success(%)")+
  ggtitle("Average age of the care homes within 15 min drive(Senior Carer)")+
  scale_x_discrete(limits = c("6_months","12_months","24_months"))



################## 'Count of Care Homes within walking distance (<500 meters) from candidates address',CHwalk500dis
#retain success

data_CHwalk500dis_job3<-melt(CHwalk500dis_data_seniorcarer,id.vars="Retain_success",variable.name="CHwalk500dis",value.name="percentage")
data_CHwalk500dis_job3$Retain_success <- factor(data_CHwalk500dis_job3$Retain_success, levels = c("6_months","12_months","24_months"))
CHwalk500dis_1_job3 <- ggplot(data_CHwalk500dis_job3,aes(Retain_success,percentage,fill=CHwalk500dis))+
  geom_bar(stat="identity",position="dodge")+
  xlab("Retain month") +
  ylab("percentage of Retain_success(%)")+
  ggtitle("Count of Care Homes within walking distance(<500 meters)from candidates address(Senior Carer)")+
  scale_x_discrete(limits = c("6_months","12_months","24_months"))


############# 'Sum of care home beds within walking distance from candidates address',CHBdis
#retain success

data_CHBdis_job3<-melt(CHBdis_data_seniorcarer,id.vars="Retain_success",variable.name="CHBdis",value.name="percentage")
data_CHBdis_job3$Retain_success <- factor(data_CHBdis_job3$Retain_success, levels = c("6_months","12_months","24_months"))
CHBdis_1_job3 <- ggplot(data_CHBdis_job3,aes(Retain_success,percentage,fill=CHBdis))+
  geom_bar(stat="identity",position="dodge")+
  xlab("Retain month") +
  ylab("percentage of Retain_success(%)")+
  ggtitle("Sum of care home beds within walking distance from candidates address(Senior Carer)")+
  scale_x_discrete(limits = c("6_months","12_months","24_months"))



############ 'Count of bus stops walking distance from candidates address',BSdis
#retain success

data_BSdis_job3<-melt(BSdis_data_seniorcarer,id.vars="Retain_success",variable.name="BSdis",value.name="percentage")
data_BSdis_job3$Retain_success <- factor(data_BSdis_job3$Retain_success, levels = c("6_months","12_months","24_months"))
BSdis_1_job3 <- ggplot(data_BSdis_job3,aes(Retain_success,percentage,fill=BSdis))+
  geom_bar(stat="identity",position="dodge")+
  xlab("Retain month") +
  ylab("percentage of Retain_success(%)")+
  ggtitle("Count of bus stops walking distance from candidates address(Senior Carer)")+
  scale_x_discrete(limits = c("6_months","12_months","24_months"))


###Aggregate the images of retain
CH15_j3 <- CH15_1_job3 + theme(text = element_text(size = 6))
CHB15_j3<- CHB15_1_job3 + theme(text = element_text(size = 5))
AVG_j3<-AVG_1_job3 +theme(text = element_text(size = 6))
CHwalk500dis_j3<-CHwalk500dis_1_job3 +theme(text = element_text(size = 5))
CHBdis_j3<-CHBdis_1_job3 +theme(text = element_text(size = 5))
BSdis_j3<-BSdis_1_job3 +theme(text = element_text(size = 5))
seniorcarer_retain <- ggarrange(CH15_j3,CHB15_j3,AVG_j3,CHwalk500dis_j3,CHBdis_j3,BSdis_j3, nrow = 3, ncol = 2, widths = c(6,6), heights = c(5,5))
#ggsave("seniorcarer_retain.png",seniorcarer_retain)
#seniorcarer_retain









############################################
#Output the number of different jobs in different competition zones
#LOW 
#plot the number of different jobs in the low competition area
data_job_L <- data.frame(Competitors_type = c('CH15','CHB15','AVG15','CHwalk500dis', 'CHBdis','BSdis'),
                                   Nursing = c(983,968,950,1579,1579,1080),
                                   Carer = c(5032,4986,5759,9853,9853,5696),
                                   Senior_Carer = c(377,383,466,679,679,445))

job_L<-melt(data_job_L,id.vars="Competitors_type",variable.name="Job_title",value.name="Count")
job_L$Competitors_type <- factor(job_L$Competitors_type, levels = c('CH15','CHB15','AVG15','CHwalk500dis', 'CHBdis','BSdis'))
job_L_plot <- ggplot(job_L,aes(Competitors_type,Count,fill=Job_title))+
  geom_bar(stat="identity",position="dodge")+
  xlab("Competitors_type") +
  ylab("numbers by job")+
  ggtitle("Number of people in different competitive categories 
          (low competitive range)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_discrete(limits = c('CH15','CHB15','AVG15','CHwalk500dis', 'CHBdis','BSdis'))


#ggsave("job_L_plot.png",job_L_plot)
#MEDIUM
#plot the number of different jobs in the medium competition area
data_job_M <- data.frame(Competitors_type = c('CH15','CHB15','AVG15','CHwalk500dis', 'CHBdis','BSdis'),
                                 Nursing = c(649,682,804,837,739,797),
                                 Carer = c(5407,5308,6155,5848,5290,5853),
                                 Senior_Carer = c(357,352,384,408,366,375))

job_M<-melt(data_job_M,id.vars="Competitors_type",variable.name="Job_title",value.name="Count")
job_M$Competitors_type <- factor(job_M$Competitors_type, levels = c('CH15','CHB15','AVG15','CHwalk500dis', 'CHBdis','BSdis'))
job_M_plot <- ggplot(job_M,aes(Competitors_type,Count,fill=Job_title))+
  geom_bar(stat="identity",position="dodge")+
  xlab("Competitors_type") +
  ylab("numbers by job")+
  ggtitle("Number of people in different competitive categories
          (medium competitive range)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_discrete(limits = c('CH15','CHB15','AVG15','CHwalk500dis', 'CHBdis','BSdis'))

#ggsave("job_M_plot.png",job_M_plot)

#HIGH
#plot the number of different jobs in the high competition area
data_job_H <- data.frame(Competitors_type = c('CH15','CHB15','AVG15','CHwalk500dis', 'CHBdis','BSdis'),
                                   Nursing = c(638,681,557,276,732,683),
                                   Carer = c(5345,5302,4164,1717,5204,5165),
                                   Senior_Carer = c(352,350,261,133,361,338))

job_H<-melt(data_job_H,id.vars="Competitors_type",variable.name="Job_title",value.name="Count")
job_H$Competitors_type <- factor(job_H$Competitors_type, levels = c('CH15','CHB15','AVG15','CHwalk500dis', 'CHBdis','BSdis'))
job_H_plot <- ggplot(job_H,aes(Competitors_type,Count,fill=Job_title))+
  geom_bar(stat="identity",position="dodge")+
  xlab("Competitors_type") +
  ylab("numbers by job")+
  ggtitle("Number of people in different competitive categories
          (high competitive range)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_discrete(limits = c('CH15','CHB15','AVG15','CHwalk500dis', 'CHBdis','BSdis'))

#ggsave("job_H_plot.png",job_H_plot)


###cycle1
###Aggregate the images of retain
CH15_11 <- CH15_1 + theme(text = element_text(size = 6))
CHB15_11<- CHB15_1 + theme(text = element_text(size = 5))
AVG_11<-AVG_1 +theme(text = element_text(size = 6))
CHwalk500dis_11<-CHwalk500dis_1 +theme(text = element_text(size = 5))
CHBdis_11<-CHBdis_1 +theme(text = element_text(size = 5))
BSdis_11<-BSdis_1 +theme(text = element_text(size = 5))
retain <- ggarrange(CH15_11,CHB15_11,AVG_11,CHwalk500dis_11,CHBdis_11,BSdis_11, nrow = 3, ncol = 2, widths = c(6,6), heights = c(5,5))
#ggsave("Retain.png",retain)
#retain
###Aggregate notretain's images
CH15_21 <- CH15_2 + theme(text = element_text(size = 6))
CHB15_21<- CHB15_2 + theme(text = element_text(size = 5))
AVG_21<-AVG_2 +theme(text = element_text(size = 6))
CHwalk500dis_21<-CHwalk500dis_2 +theme(text = element_text(size = 5))
CHBdis_21<-CHBdis_2 +theme(text = element_text(size = 5))
BSdis_21<-BSdis_2 +theme(text = element_text(size = 5))
notretain <- ggarrange(CH15_21,CHB15_21,AVG_21,CHwalk500dis_21,CHBdis_21,BSdis_21, nrow = 3, ncol = 2, widths = c(6,6), heights = c(5,5))
#ggsave("NotRetain.png",notretain)
#notretain
###Job-title Aggregate
jobtitle_count <- ggarrange(job_L_plot,job_M_plot,job_H_plot, nrow = 3, ncol = 1, widths = c(6, 6), heights = c(5, 5))
#ggsave("jobtitel_count.png",jobtitle_count)










