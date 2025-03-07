library(StatsBombR)
source("util.R")


Matches <- getAllMatches()

# Kvindernes VM 2023, competitionid=107
VMMatches=Matches %>% filter(season.season_id==107 & home_team.home_team_gender=="female")

# Danske kampe
DKVM=VMMatches %>% filter(str_detect(home_team.home_team_name,"Denmark") | str_detect(away_team.away_team_name,"Denmark"))

# find en kamp
DKVM$match_id %>% unique()
dkmatch <- DKVM %>% filter(match_id==3893795)
dkmatchEv <- getAllEventsType(3893795,"f","Shot")
dkmshot <- dkmatchEv %>% filter(type.name=="Shot")
#saveRDS(dkmshot,"dkmshots.rds")

#tt=readRDS("allMShots.rds")

# angle and dist to goal
#tl = unlist(dkmshot[1,'location'])
coln=colnames(dkmshot)
coln
colnn <- coln
#colnn=coln[c(1,4,8,15,16,21,22,23,24,25,26,62:72,90:104)]
write(colnn,"out.txt")

dkmshotsel <- dkmshot %>% select(all_of(colnn))
dkmshotsel <- dkmshotsel %>% rowwise() %>% mutate(angle=mxangle(location))
dkmshotsel <- dkmshotsel %>% rowwise() %>% mutate(dist=disttogoal(location))
dkmshotsel <- dkmshotsel %>% rowwise() %>% mutate(location.x=location[[1]][[1]])
dkmshotsel <- dkmshotsel %>% rowwise() %>% mutate(location.y=location[[2]][[1]])
dkmshotsel <- dkmshotsel %>% rowwise() %>% mutate(shot.end_location.x=shot.end_location[[1]][[1]])
dkmshotsel <- dkmshotsel %>% rowwise() %>% mutate(shot.end_location.y=shot.end_location[[2]][[1]])
dkpshotsel <- dkmshotsel %>% filter(team.id==853)
oppshotsel <- dkmshotsel %>% filter(team.id==1207)

# plot a shot
dftest=dkpshotsel %>% filter(player.name=="Josefine Hasbo")
ggplot(dftest[1,]) +
  annotate_pitch(
    dimensions = pitch_statsbomb,
    colour = "white",
    fill = "#3ab54a")+
  geom_segment(aes(x = location.x, 
                   y = location.y, 
                   xend = shot.end_location.x,
                   yend=shot.end_location.y),
               colour = "yellow",
               size = 1) +
  theme_pitch() +
  coord_flip(xlim = c(49, 121)) +
  scale_y_reverse() +
  geom_text(aes(x=location.x,y=location.y,label=player.name), size=2.5,vjust=1)+
  geom_point(aes(x=location.x,y=location.y,color=team.name), size=2.5)


# another shot with tri
testsh=dftest[1,]
testsh
dfs=plotSingleShotTri(unlist(testsh$location[1]))
ggplot(dfs, aes(x=sx,y=sy))+
  annotate_pitch(
    dimensions = pitch_statsbomb,
    colour = "white",
    fill = "#3ab54a")+
  geom_polygon(alpha=0.4 )+
  theme_pitch() +
  ggtitle("Simple passmap aylor", 
          "ggsoccer example")+
  coord_flip(xlim = c(75, 121)) +
  scale_y_reverse() +
  geom_text(data=testsh,aes(x=location.x,y=location.y,label=player.name), size=2.5,vjust=1)+
  geom_point(data=testsh,aes(x=location.x,y=location.y,color=team.name), size=2.5)


# test fframe
testff <- testsh$shot.freeze_frame[[1]]
testff <- jsonlite::flatten(testff)
testff <- testff %>% rowwise() %>% mutate(x=location[1])
testff <- testff %>% rowwise() %>% mutate(y=location[2])
#testff$team.name <- ifelse(testff$teammate==F,"China",testff$team.name)
#testff <- testff %>% rowwise() %>% mutate(px=location[1])
#testff <- testff %>% rowwise() %>% mutate(py=location[2])

colnames(testff)

# plot tri and players colored by team
ggplot() +
  annotate_pitch(
    dimensions = pitch_statsbomb,
    colour = "white",
    alpha = 0.4,
    fill = "#3ab54a")+
  geom_polygon(data=dfs,aes(x=sx,y=sy),alpha=0.4, fill = "blue" )+
  geom_point(data=testff,aes(x = x, y = y, color=teammate), size = 2) +
  geom_point(data=testsh,aes(x = location.x, y = location.y, color="black"), size = 4) +
  theme_pitch() +
  direction_label() +
  ggtitle("Simple passmap Taylor", 
          "ggsoccer example")+
  coord_flip(xlim = c(75, 121)) +
  scale_y_reverse() +
  geom_text(data=testff,aes(x=x,y=y,label=player.name), angle=10,size=1.5,vjust=-1)+
  geom_text(data=testsh,aes(x=location.x,y=location.y,label=player.name), size=4.5,vjust=1)

# THE MATH
## number of opponent in the tri
### for each opponent test wether on not in tri
#### comput total area of tri including the x,y of opponent
##### if aO > a then outside


# P Harder og Huan Xu
onetry=testff %>% filter(player.name=="Huan Xu") %>% select(location)


###### UN EDITED PART
dkpshotsel8 <- dkpshotsel
is_opponent_inside_triangle(testff[8,'x'],testff[8,'y'],dkpshotsel8$location.x,dkpshotsel8$location.y)

# Rikke M og Huan Xu
is_opponent_inside_triangle(testff[8,'x'],testff[8,'y'],testff[10,'x'],testff[10,'y'])
is_opponent_inside_triangle(testff[7,'x'],testff[7,'y'],testff[10,'x'],testff[10,'y'])
is_opponent_inside_triangle(testff[6,'x'],testff[6,'y'],testff[10,'x'],testff[10,'y'])
is_opponent_inside_triangle(testff[5,'x'],testff[5,'y'],testff[10,'x'],testff[10,'y'])
is_opponent_inside_triangle(testff[4,'x'],testff[4,'y'],testff[10,'x'],testff[10,'y'])
is_opponent_inside_triangle(testff[3,'x'],testff[3,'y'],testff[10,'x'],testff[10,'y'])
is_opponent_inside_triangle(testff[2,'x'],testff[2,'y'],testff[10,'x'],testff[10,'y'])
is_opponent_inside_triangle(testff[1,'x'],testff[1,'y'],testff[10,'x'],testff[10,'y'])

# P Harder og Mengwen Li
is_opponent_inside_triangle(testff[7,'x'],testff[7,'y'],dkpshotsel8$location.x,dkpshotsel8$location.y)

# P Harder og alle opponents
numOfOpps = c()
for (i in (1:nrow(testff))) {
  numOfOpps[i]=is_opponent_inside_triangle(testff[i,'x'],testff[i,'y'],dkpshotsel8$location.x,dkpshotsel8$location.y)
}
shooternum=sum(numOfOpps)

# P Harders teammates og alle opponents
dft=testff %>% filter(teammate==T)
# get Shooter into dataframe
shooterdf=dkpshotsel8 %>% mutate(
  x=location.x,
  y=location.y,
  numops=shooternum,
  position.name="S",
  position.id=222,
  teammate=T
) %>%  select(player.name,player.id,location,x,y,numops,position.name,position.id,teammate) 

dfto=testff %>% filter(teammate==F)
colnames(dft)
dft$numops=0
for (j in (1:nrow(dft))) {
  numOfOpps = c()
  for (i in (1:nrow(dfto))) {
    numOfOpps[i]=is_opponent_inside_triangle(
      testff[i,'x'],
      testff[i,'y'],
      dft[j,'x'],
      dft[j,'y']
    )
  }
  dft[j,'numops']=sum(numOfOpps)
}
dftotal=rbind(dft,shooterdf)
dkpshotsel8$ego=T

# Nu for alle danske skud i kampen

for (k in (1:nrow(dkpshotsel))) {
  #testff <- dkpshotsel8$shot.freeze_frame[[1]]
  testff <- dkpshotsel[k,'shot.freeze_frame']
  testff <- as.data.frame(testff[[1]])
  testff <- testff %>% rowwise() %>% mutate(x=location[1])
  testff <- testff %>% rowwise() %>% mutate(y=location[2])
  
  # split ff according to teammates
  dft=testff %>% filter(teammate==T)
  dfto=testff %>% filter(teammate==F)
  shooter = dkpshotsel[k,]
  # Shooter og alle opponents
  numOfOpps = 0
  for (i in (1:nrow(dfto))) {
    numOfOpps[i]=is_opponent_inside_triangle(dfto[i,'x'],dfto[i,'y'],shooter$location.x,shooter$location.y)
  }
  sum(numOfOpps)
  shooterNumOfOps=sum(numOfOpps)
  
  # Teammates and all opponents
  dft$numops=0
  for (j in (1:nrow(dft))) {
    numOfOpps = c()
    for (i in (1:nrow(dfto))) {
      numOfOpps[i]=is_opponent_inside_triangle(
        testff[i,'x'],
        testff[i,'y'],
        dft[j,'x'],
        dft[j,'y']
      )
    }
    dft[j,'numops']=sum(numOfOpps)
  }
  
  # prepare shooter to enter ff-dataframe
  colnames(shooter)
  shooterdf=shooter %>% mutate(
    x=location.x,
    y=location.y,
    numops=shooterNumOfOps,
    position.name="S",
    position.id=222,
    teammate=T
  ) %>%  select(player.name,player.id,location,x,y,numops,position.name,position.id,teammate) 
  
  #dftotal=rbind(dft,shooterdf)
  tjek = dft$numops < shooterNumOfOps
  sumtjek=sum(tjek)
  
  dkpshotsel[k,'ego']=ifelse(sumtjek > 0,T,F)
  
}
