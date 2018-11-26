## Group Project
## Linear mixed model using lme4
## Stats 506, Fall 2018
##
##
## Author: Guanghui Min
## Updated: November 17, 2018

rm(list = ls())

library(tidyverse)
library(lme4)
library(nlme)
library(lmerTest)
library(faraway)
library(stargazer)

# read data------------------------------------------------------------------------------------------
AQ=read.csv("Beijing.csv")


# divide the months into four seasons
seasons = c( 'winter',
             'winter',
             rep('spring',3),
             rep('summer',3),
             rep('fall',3),
             'winter'
)

# data preprocessing---------------------------------------------------------------------------------
AQ = AQ %>%
  filter(!is.na(pm2.5) & pm2.5 > 0) %>%
  mutate(season=factor(month, 1:12, seasons)) 

# the data is seriously left-skewed, we need to do some transfomation-------------------------------
qplot(cbwd, pm2.5, facets = . ~ cbwd, 
      colour = cbwd, geom = "boxplot", data = AQ)


# use box-cox method to find a suitable transformation
g =lm(pm2.5~., data = AQ)
boxcox(g,plotit=T)

# according to box-cox, we try y->log(y)-------------------------------------------------------------
AQ=
  AQ%>%
  mutate(pm2.5_log=log(pm2.5)) %>%
  filter(is.finite(pm2.5_log))

qplot(cbwd, pm2.5_log, facets = . ~ cbwd, 
      colour = cbwd, geom = "boxplot", data = AQ)

# Then we drop the outliers-------------------------------------------------------------------------
AQ_cv=
  AQ %>%
  filter(cbwd=="cv") %>%
  filter(pm2.5_log>2.2 & pm2.5_log<6.8)

AQ_NE=
  AQ %>%
  filter(cbwd=="NE") %>%
  filter(pm2.5_log>0.5)

AQ_NW=
  AQ %>%
  filter(cbwd=="NW") %>%
  filter(pm2.5_log>0.5)

AQ_SE=
  AQ %>%
  filter(cbwd=="SE") %>%
  filter(pm2.5_log>2.60 & pm2.5_log<sort(pm2.5_log,decreasing = T)[6])

AQ_de=rbind(AQ_cv,AQ_NE,AQ_NW,AQ_SE)

qplot(cbwd, pm2.5_log, facets = . ~ cbwd, 
      colour = cbwd, geom = "boxplot", data = AQ_de)

# Multiple plot function----------------------------------------------------------------------------
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


# draw the plots of pm2.5_log_mean vs. predictors in cbwd groups----------------------------------
p1 = AQ_de %>%
  group_by(cbwd, year) %>%
  summarise(mean_pm2.5_log = mean(pm2.5_log)) %>%
  ggplot(aes(x=year, y=mean_pm2.5_log, 
                     colour=cbwd, group=cbwd)) +
  geom_line(size=2) 

p2 = AQ_de %>%
  group_by(cbwd, month) %>%
  summarise(mean_pm2.5_log = mean(pm2.5_log)) %>%
  ggplot(aes(x=month, y=mean_pm2.5_log, 
             colour=cbwd, group=cbwd)) +
  geom_line(size=2) 

p3 = AQ_de %>%
  group_by(cbwd, day) %>%
  summarise(mean_pm2.5_log = mean(pm2.5_log)) %>%
  ggplot(aes(x=day, y=mean_pm2.5_log, 
             colour=cbwd, group=cbwd)) +
  geom_line(size=2) 

p4 = AQ_de %>%
  group_by(cbwd, hour) %>%
  summarise(mean_pm2.5_log = mean(pm2.5_log)) %>%
  ggplot(aes(x=hour, y=mean_pm2.5_log, 
             colour=cbwd, group=cbwd)) +
  geom_line(size=2) 

p5 = AQ_de %>%
  group_by(cbwd, DEWP) %>%
  summarise(mean_pm2.5_log = mean(pm2.5_log)) %>%
  ggplot(aes(x=DEWP, y=mean_pm2.5_log, 
             colour=cbwd, group=cbwd)) +
  geom_line(size=2) 

p6 = AQ_de %>%
  group_by(cbwd, TEMP) %>%
  summarise(mean_pm2.5_log = mean(pm2.5_log)) %>%
  ggplot(aes(x=TEMP, y=mean_pm2.5_log, 
             colour=cbwd, group=cbwd)) +
  geom_line(size=2) 

p7 = AQ_de %>%
  group_by(cbwd, PRES) %>%
  summarise(mean_pm2.5_log = mean(pm2.5_log)) %>%
  ggplot(aes(x=PRES, y=mean_pm2.5_log, 
             colour=cbwd, group=cbwd)) +
  geom_line(size=2) 

p8 = AQ_de %>%
  group_by(cbwd, Is) %>%
  summarise(mean_pm2.5_log = mean(pm2.5_log)) %>%
  ggplot(aes(x=Is, y=mean_pm2.5_log, 
             colour=cbwd, group=cbwd)) +
  geom_line(size=2) 

p9 = AQ_de %>%
  group_by(cbwd, Ir) %>%
  summarise(mean_pm2.5_log = mean(pm2.5_log)) %>%
  ggplot(aes(x=Ir, y=mean_pm2.5_log, 
             colour=cbwd, group=cbwd)) +
  geom_line(size=2) 

# plot the figures----------------------------------------------------------------------------------- 
multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9,cols=3)

# the final model(random intercepts and slopes)------------------------------------------------------
res = lmer(pm2.5_log ~ year+month+day+hour+DEWP+TEMP+PRES+Is+Ir+(1+hour+PRES|cbwd), data = AQ_de)
summary(res)
coef(res)

resb = lm(pm2.5_log ~ .-pm2.5-cbwd-Iws-season, data = AQ_de)

dev1 = -2*logLik(res);dev0 = -2*logLik(resb)
devdiff = as.numeric(dev0-dev1); devdiff
dfdiff <- attr(dev1,"df")-attr(dev0,"df"); dfdiff
cat('Chi-square =', devdiff, '(df=', dfdiff,'), p =', 
    pchisq(devdiff,dfdiff,lower.tail=FALSE))

BIC(resb, res) # compare the BIC of simple linear regression and linear mixed model
plot(res)
qqnorm(resid(res))
qqline(resid(res))

resc=lme(pm2.5_log ~ year+month+day+hour+DEWP+TEMP+PRES+Is+Ir, random=~1+hour+PRES|cbwd,  
            method = 'ML', data = AQ_de)

BIC(res, resc)

stargazer(resc, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")
