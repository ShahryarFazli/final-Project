library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(viridis)
library(countrycode)
install.packages('highcharter')
library(highcharter)

#some libraries are related to the graphs that I already removed them

data_city <- read.csv("C:/Users/shahr/OneDrive/Desktop/phd/spring2023/data viz/final proj/archive (3)/GlobalLandTemperaturesByCity.csv")
data_country <- read.csv("C:/Users/shahr/OneDrive/Desktop/phd/spring2023/data viz/final proj/archive (3)/GlobalLandTemperaturesByCountry.csv")
data_city$dt <- as.Date(data_city$dt)
data_city$year <- format(data_city$dt,"%Y")
data_city$month <- format(data_city$dt,"%m")
data_country$dt <- as.Date(data_country$dt)
data_country$year <- format(data_country$dt,"%Y")
data_country$month <- format(data_country$dt,"%m")
data_global <- read.csv("C:/Users/shahr/OneDrive/Desktop/phd/spring2023/data viz/final proj/archive (3)/GlobalTemperatures.csv")
data_global$dt <- as.Date(data_global$dt)
data_global$year <- format(data_global$dt,"%Y")




#######
Mea_countries<- c("Bahrain",  "Iran" , "Kuwait","Qatar","Saudi Arabia","United Arab Emirates","India","United States")
mea_v<-data_country %>% filter(Country %in% Mea_countries)%>%filter(!is.na(AverageTemperature))  

ggplot(mea_v, aes(x=Country,y=AverageTemperature,fill=Country,colour=Country))+
  geom_violin()+
  theme(axis.line = element_line(color = "orange",size=1.25))+
  theme(legend.position = "none",axis.title = element_blank(),
        axis.text = element_text(size = 10,angle = 20),
        plot.title = element_text(size=12,face = "bold")) + 
  ggtitle("Average Temperature in Middle East, India and USA") 


####
library(ggplot2)

data_globe <- data_global %>% 
  group_by(year) %>% 
  summarize(max = max(LandAverageTemperature, na.rm = TRUE),
            min = min(LandAverageTemperature, na.rm = TRUE),
            Avg_Temp = mean(LandAverageTemperature, na.rm = TRUE))

data_globe <- gather(data_globe, level, Temp, 2:4)

ggplot(data_globe, aes(x = factor(year), y = Temp, colour = Temp, group = level)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "grey", size = 0.4) +
  scale_color_gradient(low = "green", high = "red") +
  scale_x_discrete(breaks = seq(1750, 2000, by = 50)) +
  scale_y_continuous(limits = c(0, 20),
                     breaks = seq(0, 20, by = 5)) +
  ylab("Temperature (°C)") +
  theme(panel.background = element_blank(),
        legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.text = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"),
        panel.grid = element_blank()) +
  ggtitle("Min, Avg and Max Land Temperature", subtitle = "") +
  theme(axis.line = element_line(color = "black", size = 0.5))
