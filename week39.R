setwd('C:/Users/LARRY/Desktop/Tidy_Tuesday/')
getwd()



members <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/members.csv')
expeditions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/expeditions.csv')
peaks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/peaks.csv')


#remove rows with values missing 

members1 <- members[complete.cases(members$highpoint_metres), ]
# Libraries
install.packages('janitor')
library(tidyverse)
library(janitor)

#Pyramid

library(ggplot2)
library(ggthemes)
options(scipen = 999)  # turns of scientific notations like 1e+50


# X Axis Breaks and Labels 
#brks <- seq(0, 20000000, 5000000)
brks1<- seq(0, 85, 10)

# Plot

plot<- members %>%
            filter(!is.na(sex)) %>%
            mutate(sex = case_when(sex=='F'~'Female',
                                   sex== 'M'~ 'Male')) %>% 
  ggplot( aes(age, highpoint_metres)) +   
  geom_bar( aes(fill= sex),stat = "identity", width = 0.6) +   
  
  labs(title="Himalayan Climbers",
       x = 'Age (Yrs)',
       y = 'Frequency',
       subtitle = 'Distribution by Age and Gender') +
  scale_x_continuous(breaks = brks1)+
  
  theme( 
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = 'lightgrey',
                                        color= 'lightgrey'),
      
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +   
  
  scale_fill_brewer(palette = "Dark2") # +  Color palette
  
 # theme_linedraw()

ggsave(
  plot = plot,
  height = 6,
  width = 5,
  dpi = 500,
  device = 'png',
  filename = 'Mountains.png',
  path = "C:/Users/LARRY/Desktop/Tidy_Tuesday"
)


#boxplot of oxygen_used compared to highpoint
plot1<- members %>%
  filter(!is.na(sex)) %>%
  mutate(sex = case_when(sex=='F'~'Female',
                         sex== 'M'~ 'Male')) %>%
  ggplot(aes(oxygen_used, highpoint_metres))+
    geom_boxplot(aes(fill= sex))+
    labs(title = 'Oxygen Used Compared to Highpoint',
         x = 'Status',
         y = 'Height (M)')+
    theme(axis.text.x = element_text(angle = 70, vjust = 0.6))

# save plot
ggsave(
  plot = plot1,
  height = 6,
  width = 5,
  dpi = 500,
  device = 'png',
  filename = 'Mountains1.png',
  path = "C:/Users/LARRY/Desktop/Tidy_Tuesday"
)

getwd()


