library('ProjectTemplate')
load.project()

for (dataset in project.info$data)
{
  message(paste('Showing top 5 rows of', dataset))
  print(head(get(dataset)))
}

## Plot1
# Pie chart for all the qualifications except "Other qualifications"
# create a dataframe to exclude the selected value
exclude_value <- "Other Academics"
df_exclude_other <- df_left %>% filter(Qualification != exclude_value)

# Create data with percentage
df_pie <- df_exclude_other %>%
  count(Qualification) %>%
  mutate(percent = n / sum(n))

#Color pallete
my_colors <- viridis(n = length(unique(df_exclude_other$Qualification)))

education_plot <- ggplot(df_pie, aes(x = "", y = percent, fill = Qualification)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(fill = "Qualification") +
  scale_fill_manual(values = my_colors) +
  theme_void() +
  theme(legend.position = "right") +
  geom_label(aes(label = paste0(round(percent * 100), "%")), 
             position = position_stack(vjust = 0.3), 
             label.padding = unit(0.4, "lines"),
             label.size = 0.8,
             show.legend = FALSE)
education_plot

# Plot2
# Pie chart with only health and social care or background
include_values <- c("Other Academics - Health and Social Care or Nursing",
                    "BSc - Health and Social Care or Nursing",
                    "Diploma - Health and Social Care or Nursing",
                    "NVQ - Health and Social Care or Nursing",
                    "MSc - Health and Social Care or Nursing","Certificate - Health and Social Care or Nursing")

df_filtered <- df_left %>% filter(Qualification %in% include_values)

#Numerical Summary
df_pie_hc <- df_filtered %>%
  count(Qualification) %>%
  mutate(percent = n / sum(n))

#Color pallete
my_colors_hc <- viridis(n = length(unique(df_filtered$Qualification)))

education_plot_hc <- ggplot(df_pie_hc, aes(x = "", y = percent, fill = Qualification)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(fill = "Qualification") +
  scale_fill_manual(values = my_colors_hc) +
  theme_void() +
  theme(legend.position = "right") +
  geom_label(aes(label = paste0(round(percent * 100), "%")), 
             position = position_stack(vjust = 0.3), 
             label.padding = unit(0.4, "lines"),
             label.size = 0.8,
             show.legend = FALSE)
education_plot_hc

# Plot3
# Combine the two data frames for the stacked bar plot for All education except others
df_retention_all <- df_left %>%
  group_by(Qualification) %>%
  filter(Qualification != "Other Academics") %>%
  summarize(Retained = mean(`Retain_Success_6`),
            Not_Retained = mean(!`Retain_Success_6`))
df_retention_all

df_melt <- melt(df_retention_all, id.vars = "Qualification")

retention_plot <- ggplot(df_melt, aes(x = Qualification, y = value, fill = variable)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Retention Rates by Qualification",
       x = "Qualification",
       y = "Retention Rate",
       fill = "Retention")+
theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))
retention_plot

#Plot 4
trend_plot <- ggplot(success_rates, aes(x = Year, y = Retain_Success_6, group=`Filtered_Qualification`)) +
  geom_line(aes(color = `Filtered_Qualification`), size=1, linetype="dashed") +
  geom_point(size=3, shape=1) +
  ggtitle("% of Retaining for each Qualification and Years") +
  xlab("Years") +
  ylab("% of Retaining with Total Applications") +
  labs(color = "Qualifications") +
  scale_y_continuous(limits = c(50, 100), breaks = seq(50, 100, by = 10), labels = paste0(seq(50, 100, by = 10), "%"))

##Cycle2
#Create the dataframe for cycle 2
df_c2 = subset(df_left, select = c("Job Title","Qualification", "Retain_Success_6"))
exclude_value <- "Other Academics"
df_c2 <- df_c2 %>% filter(Qualification != exclude_value, df_c2$Retain_Success_6 == 1)

#Add the percentage column to the dataframe for numerical summary
df_c2_percent <- df_c2 %>%
  group_by(Qualification, `Job Title`) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(percentage = round(n/sum(n)*100,2))

#Cycle 2 Plot 5 - 100% bar stacked graph
plot_c2 <- ggplot(df_c2, aes(x = `Job Title`, fill = Qualification)) +
  geom_bar(position = "fill") +
  labs(title = "Job Title and Qualification", x = "Job Title", y = "Proportion") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_bw()

plot_c2
