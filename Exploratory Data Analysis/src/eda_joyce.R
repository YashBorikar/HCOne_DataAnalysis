
#summary of the number of channels for each type of channel
summary <- channels %>% group_by(channel_type)%>% 
  dplyr::summarize(Channel = n()) %>% 
  ungroup()
summary
knitr::kable(summary)

# Conduct Kruskal-Wallis test
kw.test <- kruskal.test(months_of_service ~ channel_type, data = channels)
kw.test

#test for one particular channel
test1 <- channels %>% mutate(is_referral = if_else(channel_type == "Referral",1,0))
kw.test2 <- kruskal.test(months_of_service ~ is_referral, data = test1)
test2 <- channels %>% mutate(is_advertisemnet = if_else(channel_type == "Advertisement",1,0))
kw.test3 <- kruskal.test(months_of_service ~ is_advertisemnet, data = test2)

#first EDA plots cycle 1

#Getting the channels with the highest retention:
success_plot<- channels %>% filter(months_of_service<400) %>% mutate(success = as.factor(Retain_Success_6)) %>% 
  ggplot( aes(fill= success, x=channel_type)) +
  geom_bar(position="dodge") +
  theme(axis.text.x = element_text(angle = 65))+
  scale_fill_viridis(discrete = T, option = "E") +
  geom_text(stat='count', aes(label= base::paste0((round((..count../tapply(..count..,..x..,sum)[..x..])*100)),"%")),position = position_dodge(.9), vjust=-0.5,size = 3)+
  ggtitle("Number of employees that applied through each channel")+
  xlab("Channel Type")+
  ylab("No. of candidates")

#To drill down we can look at the median time served by each employee that applied in each of the channels
channels_plot<-channels %>% filter(months_of_service<250) %>% 
  ggplot(aes(x=as.factor(channel_type), y=months_of_service)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  theme(axis.text.x = element_text(angle = 65))+
  ggtitle("Distribution of length of service per application channel") +
  xlab("Channel Type") +ylab("(log)Months of service")

cor_coeff <- cor(emp_service$Sum_Service_Months,emp_service$total_experience)
cor_coeff
# Box plot of the employees months of experience by 
#create the buckets of candidates in a vector
tags <- c("[0-50)","[50-100)", "[100-150)", "[150-200)", "[200-250)","[250-300)" )
#select the columns we need
cor_test <- emp_service %>% filter(total_experience<400) %>% 
  select(Sum_Service_Months,total_experience)
#assign learners to this buckets using their scores
score_group <- as_tibble(cor_test) %>%
  mutate(tag = case_when(
    Sum_Service_Months < 50 ~ tags[1],
    Sum_Service_Months >= 50 & Sum_Service_Months < 100 ~ tags[2],
    Sum_Service_Months >= 100 & Sum_Service_Months < 150 ~ tags[3],
    Sum_Service_Months >= 150 & Sum_Service_Months < 200 ~ tags[4],
    Sum_Service_Months >= 200 & Sum_Service_Months < 250 ~ tags[5],
    Sum_Service_Months >= 250 & Sum_Service_Months < 300 ~ tags[6]
  ))

score_group$tag <- factor(score_group$tag,
                          levels = tags,
                          ordered = FALSE)

#remove NA's
score_group<-score_group[complete.cases(score_group),]
#Plot of the score groups against hours spent
buckets_box<-
  ggplot(score_group, aes(x=as.factor(tag), y=total_experience)) +
  geom_boxplot(fill="steelblue",alpha=0.4) +
  ggtitle("Total experience for candidates that stayed for x amount of moths") +
  xlab("service months(buckets)") +ylab("Total experience")+scale_y_continuous(trans = 'log10')

kw.test4  <- kruskal.test(total_experience ~ tag , data = score_group)

#2nd cycle
channels_plot2<- channels %>% filter(months_of_service<250) %>% 
  ggplot(aes(x=as.factor(channel_type), y=months_of_service,fill = `Job Title`,color = `Job Title` )) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  theme(axis.text.x = element_text(angle = 90,margin = margin(t = 0, r = 20, b = 0, l = 0)))+
  ggtitle("Distribution of length of service per application channel") +
  xlab("Channel Type") +ylab("Months of service")

kw.test5  <- kruskal.test(months_of_service ~ `Job Title` , data = channels)

#employment history
#scatter plot of months of experience vs months of service
emp_service %>% filter(total_experience<500)  %>% 
  ggplot(aes(x=Sum_Service_Months, y=total_experience,fill = `Job Title`)) + scale_x_continuous(trans = 'log10')+
  facet_wrap(~`Job Title`) +   
  scale_y_continuous(trans = 'log10')+
  geom_point()


# Box plot of the employees months of experience by 
#create the buckets of candidates in a vector
tags <- c("[0-50)","[50-100)", "[100-150)", "[150-200)", "[200-250)","[250-300)" )
#select the columns we need
cor_test <- emp_service %>% filter(total_experience<400) %>% 
  select(Sum_Service_Months,total_experience,`Job Title`)
#assign learners to this buckets using their scores
score_group <- as_tibble(cor_test) %>%
  mutate(tag = case_when(
    Sum_Service_Months < 50 ~ tags[1],
    Sum_Service_Months >= 50 & Sum_Service_Months < 100 ~ tags[2],
    Sum_Service_Months >= 100 & Sum_Service_Months < 150 ~ tags[3],
    Sum_Service_Months >= 150 & Sum_Service_Months < 200 ~ tags[4],
    Sum_Service_Months >= 200 & Sum_Service_Months < 250 ~ tags[5],
    Sum_Service_Months >= 250 & Sum_Service_Months < 300 ~ tags[6]
  ))

score_group$tag <- factor(score_group$tag,
                          levels = tags,
                          ordered = FALSE)

#remove NA's
score_group<-score_group[complete.cases(score_group),]

#Plot of the score groups against hours spent
job_box<-
  ggplot(score_group, aes(x=as.factor(tag), y=total_experience,fill = `Job Title`,color = `Job Title`)) +
  geom_boxplot(fill="steelblue",alpha=0.4) +
  theme(axis.text.x = element_text(angle = 65)) +
  ggtitle("Service Months by total experience") +
  xlab("service months") +ylab("Total experience")+scale_y_continuous(trans = 'log10')





