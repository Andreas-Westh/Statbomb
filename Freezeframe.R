library(StatsBombR)
library(dplyr)
library(ggplot2)
library(ggsoccer)
library(stringr)
library(tidyr)
library(jsonlite)

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
  geom_text(data = df[2,], 
          aes(x = location.x, y = location.y, label = player.name), 
          size = 4.5, vjust = 1)
+
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

tridf <- plotSingleShotTri(df[2,]$location[[1]])
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
  geom_text(data=df[2,], aes(x=location.x,y=location.y,label = player.name), size = 2.5,vjust=1)+
  geom_point(data=df[2,], aes(x=location.x,y=location.y,color=team.name), size = 2.5)

testff <- df$shot.freeze_frame[[2]]
testff <- testff %>% rowwise() %>% mutate(x=location[1])
testff <- testff %>% rowwise() %>% mutate(y=location[2])
testff <- jsonlite::flatten(testff)


ggplot() +
  annotate_pitch(
    dimensions = pitch_statsbomb,
    colour = "white",
    fill = "#3ab54d"
  ) +
  geom_polygon(data = tridf, aes(x = x, y = y), alpha = 0.4, fill = "blue") +
  geom_point(data = testff, aes(x = x, y = y, color = teammate), size = 2) +
  geom_point(data = df[2,], aes(x = location.x, y = location.y), color = "black", size = 4) +
  theme_pitch() +
  direction_label() +
  ggtitle("Simple passmap Taylor",
          "ggsoccer example") +
  coord_flip(xlim = c(75, 121)) +
  scale_y_reverse() +
  geom_text(data = df[2,], 
            aes(x = location.x, y = location.y, label = player.name), 
            size = 4.5, vjust = 1) +
  geom_text(data = testff, aes(x = x, y = y, label = player.name), size = 2, vjust = 1, angle = 90)


# THE MATH
## number of opponent in the tri
### for each opponent test wether or nor in tri
#### compute total area of tri including the x,y of opponent
##### if a0 > a then outside

# PH og modstandere.


# Funktion til at beregne trekantens areal
calculate_triangle_area <- function(x1, y1, x2, y2, x3, y3) {
  return(0.5 * abs(x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2)))
}

# Funktion til at teste, om en modstander er i sektoren
is_opponent_inside_triangle <- function(o_x, o_y, shooter_x, shooter_y) {
  # Målstolper (StatsBomb koordinater)
  x1 <- 120  # Højre målstolpe
  y1 <- 44
  x2 <- 120  # Venstre målstolpe
  y2 <- 36
  x3 <- shooter_x
  y3 <- shooter_y
  
  # Beregn trekantens samlede areal
  total_area <- calculate_triangle_area(x1, y1, x2, y2, x3, y3)
  
  # Beregn arealet af de tre deltrekanter
  area1 <- calculate_triangle_area(o_x, o_y, x2, y2, x3, y3)
  area2 <- calculate_triangle_area(x1, y1, o_x, o_y, x3, y3)
  area3 <- calculate_triangle_area(x1, y1, x2, y2, o_x, o_y)
  
  # Sammenlign arealer (floating-point tolerance på 0.1)
  return(abs((area1 + area2 + area3) - total_area) < 0.1)
}

# Brug df[2,] som skyder
shooter_x <- df[2, "location.x"]
shooter_y <- df[2, "location.y"]

# Tjek modstandere i testff (hvor teammate == FALSE)
opponents <- testff %>% filter(teammate == FALSE)

# Tilføj en ny kolonne der viser, om modstanderen er i sektoren
opponents <- opponents %>%
  rowwise() %>%
  mutate(in_triangle = is_opponent_inside_triangle(x, y, shooter_x, shooter_y))

# Se antal modstandere i sektoren
num_opponents_in_triangle <- sum(opponents$in_triangle)
print(num_opponents_in_triangle)

# plot til at se hvem der er foran den som skyder 
ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "white", fill = "#3ab54d") +
  geom_polygon(data = tridf, aes(x = x, y = y), alpha = 0.4, fill = "blue") +
  geom_point(data = opponents, aes(x = x, y = y, color = in_triangle), size = 2) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +  # Røde modstandere i sektoren
  theme_pitch() +
  coord_flip(xlim = c(75, 121)) +
  scale_y_reverse()
