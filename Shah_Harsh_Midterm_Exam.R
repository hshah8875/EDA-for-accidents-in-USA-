# Name: Harsh Shah
# Midterm Exam
# all necessary libraries are installed
library(data.table)
library(maps)
library(RColorBrewer)
library(mapproj)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(stringr)
library(treemap)
# opening the data set 
data_accidents <- fread("C:/Users/harsh/Desktop/EM 622 Decision R/US_Accidents_Dec21_updated.csv")
# checking for null values
sum(is.na(data_accidents$State))
sum(is.na(data_accidents$County))
length(unique(data_accidents$ID))
sum(is.na(data_accidents$Weather_Condition))
# changing names of columns
colnames(data_accidents)[9] <- "distance"
colnames(data_accidents)[26] <- "visibility"
colnames(data_accidents)[22] <- "temperature"
colnames(data_accidents)[23] <- "wind_chill"
colnames(data_accidents)[24] <- "humidity"
colnames(data_accidents)[25] <- "pressure"
colnames(data_accidents)[28] <- "windspeed"
colnames(data_accidents)[26] <- "visibility"
# data transformation for weather condition column
data_accidents$weather_updated <- ifelse(grepl("Cloudy",data_accidents$Weather_Condition),"Cloudy",ifelse(grepl("Rain",data_accidents$Weather_Condition),"Rain",data_accidents$Weather_Condition))
data_accidents$weather_updated <- ifelse(grepl("Snow",data_accidents$weather_updated),"Snow",data_accidents$weather_updated)
data_accidents$weather_updated <- ifelse(grepl("Fair",data_accidents$weather_updated),"Fair",data_accidents$weather_updated)
data_accidents$weather_updated <- ifelse(grepl("T-Storm",data_accidents$weather_updated),"Thunderstorm",data_accidents$weather_updated)
data_accidents$weather_updated <- ifelse(grepl("Thunder",data_accidents$weather_updated),"Thunderstorm",data_accidents$weather_updated)
data_accidents$weather_updated <- ifelse(grepl("Thunderstorm",data_accidents$weather_updated),"Thunderstorm",data_accidents$weather_updated)
data_accidents$weather_updated <- ifelse(grepl("Fog",data_accidents$weather_updated),"Fog",data_accidents$weather_updated)
View(data_accidents)
# analysing the structure of data frame
str(data_accidents)
data_scatterplot <- filter(data_accidents, weather_updated %in% c("Fair","Cloudy","Rain","Clear","Overcast","Snow","Scattered Clouds","Fog","Haze","Thunderstorm","Smoke"))
data_scatterplot <- data_scatterplot %>% drop_na(Sunrise_Sunset)
data_scatterplot <- data_scatterplot %>% drop_na(visibility)
data_scatterplot <- data_scatterplot %>% drop_na(Side)
data_scatterplot[!data_scatterplot$Sunrise_Sunset=="",]
random_data <- filter(data_scatterplot, data_scatterplot$visibility < 15) %>% sample_n(10000)
View(random_data)

#Scatter plot
myplot <- ggplot(data = random_data, aes(x=weather_updated,y=visibility)) + geom_jitter(mapping = aes(colour=Sunrise_Sunset, size=Severity), alpha = 0.8) + facet_wrap(~Side, ncol = 2) +
  labs(title = "Accident Analysis", caption = "Created by Harsh Shah") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  theme(axis.title = element_text(face = "bold"),strip.background = element_rect(colour = "green", size = 2,fill = "yellow"),legend.text = element_text(color = "turquoise"))+
  geom_rect(data = subset(random_data, Side =="R"),xmax = "Fair", xmin = "Haze", ymin = -Inf, ymax = 2.5, alpha = .02, fill = "grey") +
  geom_rect(data = subset(random_data, Side =="R"),xmax = "Scattered Clouds", xmin = "Overcast", ymin = -Inf, ymax = 3, alpha = .02, fill = "grey") + 
  geom_rect(data = subset(random_data, Side =="R"),xmax = "Smoke", xmin = "Thunderstorm", ymin = -Inf, ymax = 3, alpha = .02, fill = "grey")
myplot


#Tree map
treemap_dataframe <- random_data %>% count(State,City, wt = Severity)
colnames(treemap_dataframe)[3] <- "Traffic affected severity"
treemap(treemap_dataframe,index = c("State","City"),vSize = "Traffic affected severity",vColor = "Traffic affected severity", type = "value",palette = terrain.colors(10), title = "Traffic affected by accident",fontsize.labels = c(15,8),border.col = "white")

#Geographical map
geographical_map_dataframe <- data_accidents %>% count(State,County)
mypaelette <- brewer.pal(6,"Blues")
geographical_map_dataframe$colorbuckets <- as.numeric(cut(geographical_map_dataframe$n,c(0,5,20,70,90,200,5000000)))
leg.txt <- c("<5","5-20","20-70","70-90","90-200",">200")
geographical_map_dataframe$color_code <- mypaelette[geographical_map_dataframe$colorbuckets]
head(geographical_map_dataframe)
maps::map(database="county", col=geographical_map_dataframe$color_code,fill=TRUE,resolution=0,lty=0,projection = "polyconic")
maps::map(database="state", col= "black",fill=FALSE,add = TRUE,lty=1,lwd = 0.2,projection = "polyconic", namefield = "name")
legend("bottomright", legend = leg.txt, horiz = FALSE, fill = mypaelette)
title(main = "Accident distributions across counties in USA",font.main=2,cex.main=2, col.main= "darkblue")

# Table heat maps
data_accidents <- data_accidents %>% drop_na(wind_chill)
data_accidents <- data_accidents %>% drop_na(humidity)
data_accidents <- data_accidents %>% drop_na(pressure)
data_accidents <- data_accidents %>% drop_na(Severity)
data_accidents <- data_accidents %>% drop_na(windspeed)
data_accidents <- data_accidents %>% drop_na(temperature)
data_accidents <- data_accidents %>% drop_na(visibility)
data_accidents <- data_accidents %>% group_by(State) %>% mutate(mean(temperature))
data_accidents <- data_accidents %>% group_by(State) %>% mutate(mean(wind_chill))
data_accidents <- data_accidents %>% group_by(State) %>% mutate(mean(humidity))
data_accidents <- data_accidents %>% group_by(State) %>% mutate(mean(Severity))
data_accidents <- data_accidents %>% group_by(State) %>% mutate(mean(windspeed))
data_accidents <- data_accidents %>% group_by(State) %>% mutate(mean(pressure))
data_accidents <- data_accidents %>% group_by(State) %>% mutate(mean(visibility))
colnames(data_accidents)[49] <- "temperature_mean"
colnames(data_accidents)[50] <- "wind_chill_mean"
colnames(data_accidents)[51] <- "humidity_mean"
colnames(data_accidents)[52] <- "Severity_mean"
colnames(data_accidents)[53] <- "windspeed_mean"
colnames(data_accidents)[54] <- "pressure_mean"
colnames(data_accidents)[55] <- "visibility_mean"
df <- distinct(data_accidents, State, .keep_all = TRUE)
dataframe_table_heat <- df[c("State", "temperature_mean", "wind_chill_mean", "humidity_mean", "Severity_mean", "windspeed_mean", "pressure_mean","visibility_mean")]
my_df <- as.data.frame(dataframe_table_heat)
row.names(my_df) <- my_df$State
new_df_table <- my_df[,2:8]
table_matrix <- data.matrix(new_df_table)
View(table_matrix)
heatmap_accidents <- heatmap(table_matrix, Rowv = TRUE, Colv = NA, margins = c(15,7), col = brewer.pal(9,"Reds"),scale = "column",main = "Heatmap for weather variables according to States ")



