library(viridis)
library(tidyverse)
library(here)
library(readxl)
library(plotly)


Rawdata <- read_xlsx(here("Data","ExampleAthleteData.xlsx"))

Rawdata <- as.tibble(Rawdata)
Rawdata

colnames(Rawdata) <- c("Date","Athlete","Left", "Right")
Rawdata

Rawdata2 <- Rawdata %>% 
  pivot_longer(c('Left', 'Right'), names_to = "Leg", values_to = "Force")

Rawdata2 %>% 
  ggplot() +
  geom_density(aes(Force, fill = Leg, colour = Athlete), alpha = 0.8) +
  labs("Force (N)", "When (day)")+
  theme(legend.position = "bottom") +
  theme_classic()+
  facet_wrap(~Athlete, scales = "free_x")

#nice interactive boxplot
Rawdata2_plot <- Rawdata2 %>% 
  ggplot(aes(Leg, y = Force))+
  geom_jitter(aes(colour = Athlete))+
  geom_boxplot(alpha = 0.3)+
  theme_classic()

ggplotly(Rawdata2_plot)

#nice interactive crossbar view of individuals over time relative to the group
Summary_byDate_Leg_1 <- Rawdata2 %>% 
  group_by(Date, Leg) %>% 
  summarise(Mean = mean(Force)
            ,SD = sd(Force))

Plot_Date_Leg_1 <- 
  ggplot() +
  geom_crossbar(data = Summary_byDate_Leg_1, aes(Date, y = Mean, ymin = Mean - SD, ymax = Mean + SD)) +
  geom_point(data = Rawdata2, aes(Date, Force, colour = Athlete)) +
  facet_wrap(~Leg, ncol = 1)

ggplotly(Plot_Date_Leg_1)


#To see how one athlete compares to the group
Rawdata2 %>% 
  filter(Athlete == "Gus Smith") %>% 
  ggplot(aes(Date, Force)) +
  geom_point(colour = "red") +
  geom_line(colour = "red") +
  geom_crossbar(data = Summary_byDate_Leg_1, aes(Date, y = Mean, ymin = Mean - SD, ymax = Mean + SD)) +
  facet_wrap(~Leg, ncol = 1)




#Load packages
library(tidyverse)
library(here)
library(gridExtra)
library(scales)

#Load raw data
Rawdata_footy_1 <- read_csv(here("Data","/Session4_ExampleData.csv"))

#Create plot of disposal count each quarter
Disposals_1 <- Rawdata_footy_1 %>% 
  ggplot() +
  geom_bar(aes(x=Quarter), fill = "lightblue") +
  labs(title = "Disposal Count each Quarter") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title = element_blank(), legend.position = "bottom", 
        axis.line = element_blank(), 
        axis.ticks = element_blank(),
        axis.text = element_text(face = "bold")) 

#Create count by disposal type and quarter, also rename column to be clearer
Disposal_mix_1 <-  Rawdata_footy_1 %>% 
  rename(Disposal_Type = DIsposalType) %>% 
  count(Disposal_Type, Quarter)

#Create a function to produce a proportion calculation to be used in next step
prop <- function(x) {x/sum(x)}

#Create a data frame with proportion of disposal type in each quarter
Disposal_mix_2 <- plyr::ddply(Disposal_mix_1, "Quarter", transform, PPN=prop(n))

#Create plot of disposal mix by each quarter displayed as percentage
Disposal_mix_3 <- ggplot(data = Disposal_mix_2, aes(x= Quarter, y = PPN, group = Disposal_Type, colour = Disposal_Type)) +
  geom_line(size = 2) +
  theme_classic() +
  labs(title = "Disposal Mix each Quarter %") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = label_percent (accuracy = 1)) +
  theme(axis.title = element_blank(), legend.position = "bottom", 
        panel.spacing = unit(1, "lines"), axis.line = element_blank(), 
        axis.ticks = element_blank(),
        axis.text = element_text(face = "bold")) 

#Display the two plots in one column
grid.arrange(Disposals_1,Disposal_mix_3, ncol=1)





















