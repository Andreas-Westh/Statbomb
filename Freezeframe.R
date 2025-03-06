library(StatsBombR)
library(dplyr)
library(ggplot2)
library(ggsoccer)
library(stringr)
library(tidyr)

df <- readRDS("Data/dkmshots.rds")

df <- df %>%
    rowwise() %>%
    mutate(location.x=location[1],
           location.y=location[2]) %>% 
  ungroup()
  
df <- df %>%
  rowwise() %>%
  mutate(shot.end_location.x=shot.end_location[1],
         shot.end_location.y=shot.end_location[2],
         shot.end_location.z=shot.end_location[3]) %>% 
  ungroup()

dftest <- df %>% filter(player.name == "Josefine Hasbo")
ggplot(dftest) +
  annotate_pitch(
    dimensions = pitch_statsbomb,
    colour = "white",
    fill = "darkgreen") +
  geom_segment(aes(x = location.x,
                   y = location.y,
                   xend = shot.end_location.x,
                   yend = shot.end_location.y),
               colour = "yellow",
               size = 1) +
  theme_pitch() +
  coord_flip(xlim = c(49,121)) +
  scale_y_reverse() +
  geom_text(aes(x=location.x,y=location.y,label = player.name), size = 2.5,vjust=1)+
  geom_point(aes(x=location.x,y=location.y,color=team.name), size = 2.5)





# Funktion til at generere sektoren
plotSingleShotTri <- function(location) {
  player_x <- location[1]
  player_y <- location[2]
  

  goal_x <- 120
  goal_width <- 40 
  
  goal_left <- c(goal_x, goal_width - 4)  
  goal_right <- c(goal_x, goal_width + 4) 
  
 
  tri_df <- data.frame(
    x = c(player_x, goal_left[1], goal_right[1], player_x), 
    y = c(player_y, goal_left[2], goal_right[2], player_y)
  )
  
  return(tri_df)
}

tridf <- plotSingleShotTri(dftest$location[[1]])
tridf

ggplot(tridf, aes(x = x, y = y)) +
  annotate_pitch(
    dimensions = pitch_statsbomb,
    colour = "white",
    fill = "#3ab54d"
  ) +
  geom_polygon(alpha = 0.4) + 
  theme_pitch() + 
  coord_flip(xlim = c(75,121)) +
    scale_y_reverse() + 
  geom_text(data=dftest[1,], aes(x=location.x,y=location.y,label = player.name), size = 2.5,vjust=1)+
  geom_point(data=dftest[1,], aes(x=location.x,y=location.y,color=team.name), size = 2.5)

testff <- df$shot.freeze_frame[[2]]


  