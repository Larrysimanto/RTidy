tournament <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-06/tournament.csv')
library(ggplot2)
library(ggExtra)
library(tidyr)
library(dplyr)


Championships<- tournament%>%
                mutate(x1st_game_at_home= case_when(x1st_game_at_home== 'Y'~'Yes',
                                                    x1st_game_at_home== 'N'~ 'No'))%>%
                filter(tourney_finish=='Champ') %>%
                
                mutate(wins =n()) %>%
  ggplot(aes(year, school))+
    geom_point(stat = 'identity', aes(col = x1st_game_at_home), size = 3.5)+
    geom_text(color = 'white', size = 2, label = '')+
    labs(title = 'Championship Wins',
         x = "Year",
         y= "School", 
         col = "First Home Game", size = 3 )+
    scale_x_continuous(breaks = seq(1982, 2018, 3))+

    theme( 
      axis.ticks = element_blank(),
      panel.background = element_rect(fill = 'grey',
                                      color= 'grey', size = .5),
    
      panel.grid.major = element_line(size = .5, 
                                      linetype = 'solid',
                                      color = 'white'),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'white', colour = 'white'), 
      axis.text.x = element_text(angle = 70, vjust = 0.6)
    )+   
    coord_flip()



print(Championships)

ggsave(
  plot = Championships,
  height = 10,
  width = 7,
  dpi = 1000,
  device = 'png',
  filename = 'Bball.png',
  path = "C:/Users/LARRY/Desktop/Tidy_Tuesday/week 41")


