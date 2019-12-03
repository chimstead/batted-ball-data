################
#scrap work file
###############
setwd("/Users/conorhimstead/Desktop/braves_exercise/Data/app/battedballdata")


devtools::install_github("rstudio/keras")

install.packages('nnet')
install.packages('neuralnet')
devtools::install_github("rstudio/keras")

library(hexbin)
library(RCurl)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(DT)
library(leaflet)
library(stringr)
library(rvest)
library(data.table)
library(shiny)
library(stats)
library(plotly)
library(ggforce)
library(neuralnet)
library(nnet)


data<-read_csv('bbd.csv')

#remove data with null values
data<-data[-3032, ]

##
d2<-data%>%
  filter(!(PLAY_OUTCOME %in% c('FieldersChoice', 'Error', 'Undefined', 'Sacrifice')))%>%
  mutate(numbases = case_when(
    PLAY_OUTCOME == 'Out' ~ 0,
    PLAY_OUTCOME == 'Single' ~ 1,
    PLAY_OUTCOME == 'Double' ~ 2,
    PLAY_OUTCOME == 'Triple' ~ 3,
    PLAY_OUTCOME == 'HomeRun' ~ 4
  ))

d3<-d2%>%
  mutate(xdist = HIT_DISTANCE * cos(pi/4 + (pi/180) * EXIT_DIRECTION),
         ydist = HIT_DISTANCE * sin(pi/4 + (pi/180) * EXIT_DIRECTION))%>%
  filter(xdist>=0, ydist>=0)

d4 <- d3 %>%
  mutate(xbin = cut(xdist, breaks = seq(0, 500, by = 15)),
         ybin = cut(ydist, breaks = seq(0, 500, by = 15)),
         hit = ifelse(numbases>0, 1, 0),
         wOBA_val = case_when(
           PLAY_OUTCOME == 'Out' ~ 0,
           PLAY_OUTCOME == 'Single' ~ 0.870,
           PLAY_OUTCOME == 'Double' ~ 1.217,
           PLAY_OUTCOME == 'Triple' ~ 1.529,
           PLAY_OUTCOME == 'HomeRun' ~ 1.940,
         )) %>%
  group_by(xbin, ybin) %>%
  mutate(bin_count = n(),
         bin_slg = mean(numbases),
         bin_avg = mean(hit),
         bin_wOBA = mean(wOBA_val)
         ) %>%
  ungroup()



circles <- data.frame(
  x0 = rep(0, 5),
  y0 = rep(0, 5),
  r = seq(100, 500, length.out = 5)
)

write.csv(circles, "circles.csv")
write.csv(d4, "d4.csv")

d4%>%
  ggplot()+
  geom_bin2d(aes(x = xdist, y = ydist, group = bin_avg, fill = bin_avg))+
  geom_circle(aes(x0 = x0, y0 = y0, r = r), data = circles)+
  coord_cartesian(xlim = c(0, 500), ylim = c(0, 500))

d4%>%
  ggplot()+
  geom_bin2d(aes(x = xdist, y = ydist, group = bin_slg, fill = bin_slg))+
  geom_circle(aes(x0 = x0, y0 = y0, r = r), data = circles)+
  coord_cartesian(xlim = c(0, 500), ylim = c(0, 500))

d4%>%
  ggplot()+
  geom_bin2d(aes(x = xdist, y = ydist, group = bin_wOBA, fill = bin_wOBA))+
  geom_circle(aes(x0 = x0, y0 = y0, r = r), data = circles)+
  coord_cartesian(xlim = c(0, 500), ylim = c(0, 500))

d4%>%
  ggplot()+
  geom_bin2d(aes(x = EXIT_SPEED, y = LAUNCH_ANGLE, group = bin_wOBA, fill = bin_wOBA))

d4%>%
  ggplot()+
  geom_bin2d(aes(x = EXIT_SPEED, y = LAUNCH_ANGLE, group = bin_slg, fill = bin_slg))

d4%>%
  ggplot()+
  geom_bin2d(aes(x = EXIT_SPEED, y = LAUNCH_ANGLE, group = bin_wOBA, fill = bin_avg))

##
d5<-d3%>%
  mutate(hit = ifelse(numbases>0, 1, 0))

write_csv(d5, 'd5.csv')
g1<-d5%>%
  ggplot(aes(x=HIT_DISTANCE, fill=factor(numbases))) +
  geom_area(stat ="bin", binwidth = 15)

ggplotly(g1)


#########
#data
mldata<-d3%>%
  filter(HIT_SPIN_RATE != "NULL")%>%
  select(LAUNCH_ANGLE, EXIT_SPEED, EXIT_DIRECTION, HIT_DISTANCE, HANG_TIME, HIT_SPIN_RATE, numbases)

mldata$HIT_SPIN_RATE <- as.numeric(as.character(mldata$HIT_SPIN_RATE))


# Encode as a one hot vector multilabel data
train <- cbind(mldata[, 1:5], class.ind(as.factor(mldata$numbases)))
# Set labels name
names(train) <- c(names(mldata)[1:5],"Out","Single","Double", "Triple", "HomeRun")

maxs <- apply(train, 2, max) 
mins <- apply(train, 2, min)

scaled <- as.data.frame(scale(train, center = mins, scale = maxs - mins))


# Set up formula
n <- names(train)
f <- as.formula(paste("Out + Single + Double + Triple + HomeRun ~", paste(n[!n %in% c("Out","Single","Double", "Triple", "HomeRun")], collapse = " + ")))
f

#scale data
s <- sample(nrow(df), 700)
cv <- scaled[s, ]
training <- scaled[-s,]


nn <- neuralnet(f,
                data = training,
                threshold = 0.025,
                hidden = c(5),
                act.fct = "logistic",
                linear.output = FALSE,
                lifesign = "full",
                rep = 1,
                stepmax = 500000)


plot(nn)

cv<-na.omit(cv)

cv_results <- compute(nn, cv[,1:5])

cvr<- cv_results$net.result


original_values <- max.col(cv[, 6:10])
cvr2 <- max.col(cvr)
out <- mean(cvr2 == original_values)
predictions<-data_frame(cv = original_values, pred = cvr2)

p<-predictions%>%
  mutate(correct = ifelse(cv == pred, 1, 0))%>%
  group_by(cv)%>%
  summarize(pct_correct = mean(correct))

p<-predictions%>%
  mutate(hit = ifelse(cv>1, 1, 0),
         hitcorrect = ifelse((cv>1) == (pred>1), 1, 0))%>%
  group_by(hit)%>%
  summarize(pct_correct = mean(hitcorrect))

nn1<-nn


##
#field diagram
#LA slider, EV slider
#filter to HDs it could land at (within x feet of a batted ball with within y degrees and z mph of sliders)
#show only nnet preds for balls in those locations

y<-rep(1:486, 486)

x<-c()
for(i in 1:486){
  x<-c(x, rep(i, 486))
}


points<-data.frame(y = y, x = x)

p2<-points%>%
  filter(sqrt(x*x + y*y)<486)


p3<-p2%>%
  mutate(HIT_DISTANCE = sqrt(x*x + y*y),
         EXIT_DIRECTION = (atan(y/x)*180/pi) - 45)

write_csv(p3, 'p3.csv')
write_csv(d2, 'd2.csv')

LAin <- 50

ESin <- 75



d6<-d2%>%
  filter(LAUNCH_ANGLE - LAin < 3,
         LAUNCH_ANGLE - LAin > -3,
         EXIT_SPEED - ESin < 3,
         EXIT_SPEED - ESin > -3)

ran <- c(min(d6$HIT_DISTANCE), max(d6$HIT_DISTANCE))

p4<-p3%>%
  filter(HIT_DISTANCE<=ran[2],
         HIT_DISTANCE>=ran[1])%>%
  mutate(HANG_TIME = mean(d6$HANG_TIME),
         LAUNCH_ANGLE = LAin,
         EXIT_SPEED = ESin)

p4<-p4[,c(6, 7, 4, 3, 5, 1, 2)]

p_scaled <- as.data.frame(scale(p4[,1:5], center = mins[1:5], scale = maxs[1:5] - mins[1:5]))

p_results <- compute(nn1, p_scaled)

pr<- p_results$net.result

pr2 <- max.col(pr)

p4$result <- factor(pr2)


p4%>%
  mutate(result = case_when(
    result == 1 ~ 'Out',
    result == 2 ~ 'Single',
    result == 3 ~ 'Double',
    result == 4 ~ 'Triple',
    result == 5 ~ 'HomeRun'
  ))%>%
  ggplot()+
  geom_point(aes(x = x, y = y, fill = result, color = result))+
  geom_circle(aes(x0 = x0, y0 = y0, r = r), data = circles)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  coord_cartesian(xlim = c(0, 500), ylim = c(0, 500))


save(nn1, file = 'nnet1.RData')
save(maxs, mins, file = 'maxmin.RData')


nn2<-get(load('nnet.RData'))




