install.packages("dplyr")
install.packages("ggplot2")
install.packages("treemap")
install.packages("RColorBrewer")
install.packages("plotly")
install.packages("corrplot")

library(dplyr)
library(ggplot2)
library(treemap)
library(RColorBrewer)
library(plotly)
library(corrplot)

url <- "https://raw.githubusercontent.com/ananyadutta03/Data_science_Project/main/country_wise_latest.csv"
mydata <- read.csv(url)
head(mydata)


str(mydata)
summary(mydata)


data<-mydata
data <- data %>%
select(Active)
print(data)
str(data)
summary(data)


data_cleaned <-mydata
data_cleaned$Country.Region[data_cleaned$Country.Region == ""] <- NA
data_cleaned
colSums(is.na(data_cleaned))


missing_values <- colSums(is.na(data_cleaned))
missing_values
if(any(missing_values > 0)) {
  print(missing_values[missing_values > 0])
} else {
  cat("No missing values found in the dataset.\n\n")
}



median_replace<-data_cleaned

median_replace[is.na(median_replace$Deaths), ]

deaths_median <- median(median_replace$Deaths, na.rm = TRUE)
print(deaths_median)

median_replace$Deaths[is.na(median_replace$Deaths)] <- deaths_median

median_replace[is.na(median_replace$Deaths), ]
median_replace





mean_add<-median_replace
mean_add[is.na(mean_add$Confirmed), ]


Confirmed_mean <- mean(mean_add$Confirmed, na.rm = TRUE)
print(Confirmed_mean)

mean_add$Confirmed[is.na(mean_add$Confirmed)] <- Confirmed_mean

mean_add[is.na(mean_add$Confirmed), ]
mean_add




mode_add<-mean_add
mode_add[is.na(mode_add$WHO.Region),]

freq_table <- table(mode_add$WHO.Region)

mode<-names(freq_table)[which.max(freq_table)]

mode_add$WHO.Region[is.na(mode_add$WHO.Region)] <- mode
mode_add[is.na(mydata$WHO.Region),]
mode_add




Remove_missing_row<-mode_add
Remove_missing_row<-na.omit(Remove_missing_row)
Remove_missing_row



duplicated_row <- Remove_missing_row
duplicate_rows <- sum(duplicated(duplicated_row))
if(duplicate_rows > 0) {
  cat("Found", duplicate_rows, "duplicate rows. Removing them.\n")
  Remove_missing_row <- Remove_missing_row[!duplicated(Remove_missing_row), ]
} else {
  cat("No duplicate rows found.\n\n")
}

nrow(duplicated_row)


#data Wrangling
names(duplicated_row)
covid_wrangled <- duplicated_row %>%
  select(Country.Region, Confirmed, Deaths, Recovered, Active, WHO.Region) %>%
  mutate(
    Mortality_Rate = ifelse(Confirmed == 0, 0, (Deaths / Confirmed) * 100),
    Recovery_Rate = ifelse(Confirmed == 0, 0, (Recovered / Confirmed) * 100)
  ) %>%
  
  filter(Confirmed > 0) %>%
  arrange(desc(Confirmed))

head(covid_wrangled)

descriptive_stats <- summary(
  covid_wrangled %>%
    select(Confirmed, Deaths, Recovered, Active, Mortality_Rate, Recovery_Rate)
)
print(descriptive_stats)

names(covid_wrangled)
ncol(covid_wrangled)
head(covid_wrangled)



invalid_data <- covid_wrangled

check <- invalid_data[invalid_data$Deaths > 0 | invalid_data$Deaths < 1000000, ]
print(check)

invalid_data$Deaths[invalid_data$Deaths < 0] <- 0

head(invalid_data)





covid_data <- invalid_data



hist_confirmed <- ggplot(covid_data, aes(x = Confirmed)) +
  geom_histogram(bins = 30, fill = "royalblue", color="white", alpha = 0.8) +
  scale_x_log10(labels = scales::comma) + 
  labs(title = "Distribution of Confirmed COVID-19 Cases",
       subtitle = "Number of countries per case count (log scale)",
       x = "Confirmed Cases (Log10 Scale)", y = "Number of Countries") +
  theme_minimal(base_size = 12)
hist_confirmed



hist_deaths <- ggplot(covid_data, aes(x = Deaths)) +
  geom_histogram(bins = 30, fill = "firebrick", color="white", alpha = 0.8) +
  scale_x_log10(labels = scales::comma) +
  labs(title = "Distribution of COVID-19 Deaths",
       subtitle = "Number of countries per death count (log scale)",
       x = "Deaths (Log10 Scale)", y = "Number of Countries") +
  theme_minimal(base_size = 12)
hist_deaths


top_20_deaths <- covid_data %>%
  arrange(desc(Deaths)) %>%
  top_n(20, Deaths)

bar_chart_deaths <- ggplot(top_20_deaths, aes(x = reorder(Country.Region, Deaths), y = Deaths, fill = Deaths)) +
  geom_bar(stat = "identity") +
  coord_flip() + # Flip coordinates for better readability of country names
  scale_fill_gradient(low = "lightsalmon", high = "darkred") +
  labs(title = "Top 20 Countries by Total Deaths", x = "", y = "Number of Deaths") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")
print(bar_chart_deaths)




scatter_plot_interactive <- plot_ly(
  data = covid_data,
  x = ~Confirmed,
  y = ~Deaths,
  color = ~WHO.Region,
  text = ~paste("Country:", Country.Region, "<br>Confirmed:", Confirmed, "<br>Deaths:", Deaths),
  hoverinfo = 'text',
  type = 'scatter',
  mode = 'markers'
) %>%
  layout(
    title = 'Confirmed Cases vs. Deaths by WHO Region (Log Scale)',
    xaxis = list(title = 'Confirmed Cases', type = 'log'),
    yaxis = list(title = 'Deaths', type = 'log')
  )
print(scatter_plot_interactive)



box_plot_mortality <- ggplot(covid_data, aes(x = WHO.Region, y = Mortality_Rate, fill = WHO.Region)) +
  geom_boxplot(outlier.colour = "red") +
  labs(
    title = "Distribution of COVID-19 Mortality Rate by WHO Region",
    subtitle = "Mortality Rate = (Deaths / Confirmed Cases) * 100",
    x = "WHO Region",
    y = "Mortality Rate (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
print(box_plot_mortality)



region_summary <- covid_data %>%
  group_by(WHO.Region) %>%
  summarise(Total_Confirmed = sum(as.numeric(Confirmed)), .groups = 'drop') 

pie_chart_interactive <- plot_ly(region_summary, labels = ~WHO.Region, values = ~Total_Confirmed, type = 'pie',
                                 textposition = 'inside',
                                 textinfo = 'label+percent',
                                 marker = list(colors = brewer.pal(n = length(region_summary$WHO.Region), name = "Set2"),
                                               line = list(color = '#FFFFFF', width = 1))) %>%
  layout(title = 'Proportion of Global Confirmed Cases by WHO Region')
print(pie_chart_interactive)




top_50_confirmed <- covid_data %>%
  top_n(50, Confirmed)

cat("Displaying Treemap...\n")
treemap(top_50_confirmed,
        index="Country.Region",
        vSize="Confirmed",
        vColor="Mortality_Rate",
        type="value",
        title="Treemap of Confirmed Cases (Top 50 Countries)\n(Size = Confirmed Cases, Color = Mortality Rate)",
        palette="RdYlBu",
        border.col="white",
        fontsize.title = 14)



numeric_data <- covid_data %>%
  select(Confirmed, Deaths, Recovered, Active, Mortality_Rate, Recovery_Rate)

correlation_matrix <- cor(numeric_data, use = "complete.obs") 

corrplot(correlation_matrix,
         method = "color",
         type = "full",
         order = "hclust",
         addCoef.col = "black", 
         diag =TRUE, 
         title = "Correlation Heatmap of COVID-19 Metrics",
         mar=c(0,0,1,0))


bubble_chart_interactive <- plot_ly(
  data = top_50_confirmed,
  x = ~Confirmed,
  y = ~Deaths,
  size = ~Active,
  color = ~WHO.Region,
  text = ~paste("Country:", Country.Region, "<br>Active:", Active),
  hoverinfo = 'text',
  type = 'scatter',
  mode = 'markers',
  marker = list(sizemode = 'diameter', line = list(width = 1, color = '#FFFFFF'))
) %>%
  layout(
    title = 'COVID-19 Overview: Confirmed vs. Deaths (Top 50 Countries)',
    xaxis = list(title = 'Confirmed Cases', type = 'log'),
    yaxis = list(title = 'Deaths', type = 'log'),
    showlegend = TRUE
  )
print(bubble_chart_interactive)
