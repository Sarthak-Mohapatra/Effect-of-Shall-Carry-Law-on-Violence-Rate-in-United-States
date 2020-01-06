##
## EFFECT OF SHALL-CARRY LAW ON VIOLENCE IN UNITED STATES
##

##
## Using pacman, loading and installing required packages.
##
pacman::p_load(data.table, forecast, leaps, tidyverse, caret, corrplot, glmnet, mlbench, ggplot2, 
               gplots, ggpubr, MASS, knitr, rmarkdowns, AER, plm, usmap, hmisc, writexl, foreign)
options(scipen = 999)
## 
## Read the data file from the working directory.
##
gun.data=read.dta("guns.dta")
##
## Checking for NULL values in the data.
##
table(is.na(gun.data))
##
## Displaying the correlation matrix. Explains the correlation of each variable with other variables. A negative value implies,      
## negative correlation. It means, with increase in one unit of the independent variable, the dependant variable will go down/ reduce  ## by Beta unit.
## Similarly, a positive value signifies positive correlation and the value of dependent variable would increase.
##
##
cor(gun.data, gun.data)
##
## Below plot is the graphical representation of the correlation among variables. 
##
heatmap.2(cor(gun.data), dendrogram = "none", cellnote = round(cor(gun.data),2), notecol = "navy", key = FALSE, trace = "none", symm=T, scale="left")
corrplot(cor(gun.data), method = "color", type = "lower", order = "hclust", tl.srt = 25)
##
## From the correlation plot, we have the below observations:
## 1 - Incarceration rate is having no correlation/0 correlation with Shall Law signifying it as an Instrumental variable.
##
##
## Plots to understand each column of the data and it's variation with respect to other columns.
##
hist(gun.data$vio, col='red3', border='black', main='Violence rate Distribution.', xlab = 'Frequency', ylab = 'Violence Rate')
hist(log(gun.data$vio), col='blue4', border='black', main='Violence rate Distribution.', xlab = 'Frequency', ylab = 'Violence Rate - Log Transformed.')

hist(gun.data$mur, col='red3', border='black', main='Murder rate Distribution.', xlab = 'Frequency', ylab = 'Murder Rate')
hist(log(gun.data$mur), col='blue4', border='black', main='Murder rate Distribution.', xlab = 'Frequency', ylab = 'Murder Rate - Log Transformed.')

hist(gun.data$rob, col='red3', border='black', main='Robbery rate Distribution.', xlab = 'Frequency', ylab = 'Robbery Rate')
hist(log(gun.data$rob), col='blue4', border='black', main='Robbery rate Distribution.', xlab = 'Frequency', ylab = 'Robbery Rate - Log Transformed.')

hist(gun.data$incarc_rate, col='red3', border='black', main='Incarceration rate Distribution.', xlab = 'Frequency', ylab = 'Incarceration Rate') 
hist(log(gun.data$incarc_rate), col='blue4', border='black', main='Incarceration rate Distribution.', xlab = 'Frequency', ylab = 'Incarceration Rate - Log Transformed.')

hist(gun.data$pb1064, col='green4', border='black', main='Distribution on percentage of state population that are Black.', xlab = 'Frequency', 
     ylab = 'Percentage of state population that are Black.') 
hist(log(gun.data$pb1064), col='green4', border='black', main='Distribution on percentage of state population that are Black.', xlab = 'Frequency', 
     ylab = 'Percentage of state population that are Black - Log Transformed.')

hist(gun.data$pw1064, col='pink3', border='black', main='Distribution on percentage of state population that are Women.', xlab = 'Frequency', ylab = '% of Women in state.') 
hist(log(gun.data$pw1064), col='pink3', border='black', main='Distribution on percentage of state population that are Women.', xlab = 'Frequency', 
     ylab = '% of women in state - Log Transformed.')

hist(gun.data$pm1029, col='gold3', border='black', main='Distribution on percentage of state population that are Men.', xlab = 'Frequency', ylab = '% of Men in state.') 
hist(log(gun.data$pm1029), col='gold3', border='black', main='Distribution on percentage of state population that are Men.', xlab = 'Frequency', 
     ylab = '% of Men in state - Log Transformed.')

hist(gun.data$pop, col='tomato3', border='black', main='Distribution on population in state.', xlab = 'Frequency', ylab = 'Population of state.') 
hist(log(gun.data$pop), col='tomato3', border='black', main='Distribution on population in state.', xlab = 'Frequency', ylab = 'Population of state - Log Transformed.')

hist(gun.data$pop, col='yellow3', border='black', main='Distribution on Average Income in state.', xlab = 'Frequency', ylab = 'Average income of state.') 
hist(log(gun.data$pop), col='yellow3', border='black', main='Distribution on Average Income in state.', xlab = 'Frequency', ylab = 'Average income of state - Log Transformed.')

hist(gun.data$density, col='brown4', border='black', main='Distribution on population density in state.', xlab = 'Frequency', ylab = 'Population density in state.') 
hist(log(gun.data$density), col='brown4', border='black', main='Distribution on population density in state.', xlab = 'Frequency', ylab = 'Population density of state - Log Transformed.')

##
## The first plot is the relationship between Year and Stateid factored by Shall column. A red plot indicates that year in which the Shall law  
## was passed and a black plot signifies that the Shall Law was not yet passsed in that state for the observational year.  
##
plot(gun.data$stateid, gun.data$year, pch=19, col=factor(gun.data$shall), xlab='Stateid - Id for each state.', 
     ylab='Year - Year of observation.', main='Scatter plot showing the existance of Shall Law in each state over years.', 
     sub='Red - Shall law in effect   Black - No Shall law')

ggplot(gun.data) + geom_point(aes(x=stateid, y=year, color=factor(shall)), alpha=1) + ylab('Year - Year of observation.') + 
  xlab('Stateid - Id for each state.') + ggtitle("Scatter plot showing the existance of Shall Law in each state over years.")

##
## Now let's check the violence rate in each state over years.
##
ggplot(gun.data) + geom_bar(aes(x=stateid, y=log(vio), fill=factor(year)), stat='identity', width=0.8) + ylab('Violence Rate') + 
  xlab('Stateid - Numerical representation of each state') + ggtitle('Violence rate in each state')

ggplot(gun.data) + geom_bar(aes(x=stateid, y=log(vio), fill=factor(shall)), stat='identity', width=0.8) + ylab('Violence Rate') + 
  xlab('Stateid - Numerical representation of each state') + ggtitle('Violence rate in each state')

ggplot(gun.data) + geom_bar(aes(x=year, y=log(vio), fill=factor(shall)), stat='identity', width=0.8) + ylab('Percentage change in Violence Rate') + 
  xlab('Year in which data is observed.') + ggtitle('Change in total Violence rate in year.')
plot(gun.data$stateid, gun.data$vio, pch=19, col=factor(gun.data$shall), xlab='Stateid - Numerical representation for each state.', 
     ylab='Violence rate.', main='Violence rate in each state over years.')
plot(gun.data$shall, gun.data$vio, pch=19, col=factor(gun.data$shall), xlab='Shall.', ylab='Violence rate.', main='Violence rate in relation with presence and absence of Shall Law.',
     sub='Black - Shall Law not in effect       Red - Shall Law in effect')

ggplot(gun.data) + geom_bar(aes(x=stateid, y=log(vio), fill=factor(shall)), stat='identity', width=0.8) + ylab('Percentage change in Violence Rate') + 
  xlab('State for which data is observed.') + ggtitle('Change in total Violence rate in year.')

plot(gun.data$stateid, gun.data$vio, pch=19, col=factor(gun.data$year), xlab='Stateid - Numerical representation for each state.', 
     ylab='Violence rate.', main='Violence rate in each state over years.', 
     sub='Absence of observations for a particular stateid means there are no data observed for that state.')

plot(gun.data$year, gun.data$vio, pch=19, col=factor(gun.data$stateid), xlab='Year of observation.', ylab='Violence rate.', main='Violence rate observed in each year for different states.')

ggplot(gun.data) + geom_point(aes(x=rob, y=log(vio), color=factor(shall)), alpha=1) + ylab('Violence Rate') + xlab('Robbery Rate.') + 
  ggtitle("Relationship between Violence rate and Robbery rate.")
plot(gun.data$rob, gun.data$vio, pch=19, col=factor(gun.data$shall), xlab='Robbery Rate.', ylab='Violence rate.', main='Relationship between Violence rate and Robbery rate.',
     sub='Black - Shall Law not in effect       Red - Shall Law in effect')
abline(lm(gun.data$vio~gun.data$rob))

plot(gun.data$incarc_rate, gun.data$vio, pch=19, col=factor(gun.data$shall), xlab='Incarceration Rate.', ylab='Violence rate.', 
     main='Relationship between Violence rate and Incarceration rate.',sub='Black - Shall Law not in effect       Red - Shall Law in effect')
abline(lm(gun.data$vio~gun.data$incarc_rate))

ggplot(gun.data) + geom_point(aes(x=mur, y=log(vio), color=factor(shall)), alpha=1) + ylab('Violence Rate') + xlab('Murder Rate.') + 
  ggtitle("Relationship between Violence rate and Murder rate.")
plot(gun.data$mur, gun.data$vio, pch=19, col=factor(gun.data$shall), xlab='Murder Rate.', ylab='Violence rate.', 
     main='Relationship between Violence rate and Murder rate.', sub='Black - Shall Law not in effect       Red - Shall Law in effect')
abline(lm(gun.data$vio~gun.data$mur))

ggplot(gun.data) + geom_point(aes(x=pb1064, y=log(vio), color=factor(shall)), alpha=1) + ylab('Violence Rate') + 
  xlab('Percentage of state population that is Black.') + ggtitle("Relationship between Violence rate and Population of Black in the state.")
plot(gun.data$pb1064, gun.data$vio, pch=19, col=factor(gun.data$shall), xlab='Percentage of state population that is Black.', 
     ylab='Violence rate.', main='Relationship between Violence rate and Population of Black in the state.', sub='Black - Shall Law not in effect       Red - Shall Law in effect')
abline(lm(gun.data$vio~gun.data$pb1064))

ggplot(gun.data) + geom_point(aes(x=pw1064, y=log(vio), color=factor(shall)), alpha=1) + ylab('Violence Rate') + 
  xlab('Percentage of state population that are Women.') + ggtitle("Relationship between Violence rate and Population of Women in the state.")
plot(gun.data$pw1064, gun.data$vio, pch=19, col=factor(gun.data$shall), xlab='Percentage of state population that are White.', 
     ylab='Violence rate.', main='Relationship between Violence rate and Population of White in the state.', sub='Black - Shall Law not in effect       Red - Shall Law in effect')
abline(lm(gun.data$vio~gun.data$pw1064))

ggplot(gun.data) + geom_point(aes(x=pm1029, y=log(vio), color=factor(shall)), alpha=1) + ylab('Violence Rate') + 
  xlab('Percentage of state population that are Men.') + ggtitle("Relationship between Violence rate and Population of Men in the state.")
plot(gun.data$pm1029, gun.data$vio, pch=19, col=factor(gun.data$shall), xlab='Percentage of state population that are Men.', 
     ylab='Violence rate.', main='Relationship between Violence rate and Population of Men in the state.', sub='Black - Shall Law not in effect       Red - Shall Law in effect')
abline(lm(gun.data$vio~gun.data$pm1029)) 

ggplot(gun.data) + geom_point(aes(x=pm1029, y=log(vio), color=factor(shall)), alpha=1) + ylab('Violence Rate') + 
  xlab('State population in millions.') + ggtitle("Relationship between Violence rate and State population.")


ggplot(gun.data) + geom_point(aes(x=avginc, y=log(vio), color=factor(shall)), alpha=1) + ylab('Violence Rate') + 
  xlab('Real per capita personal income in the state, in thousands of dollars.') + ggtitle("Relationship between Violence rate and Average Income.")
plot(gun.data$avginc, gun.data$vio, pch=19, col=factor(gun.data$shall), xlab='Real per capita personal income in the state, in thousands of dollars.', 
     ylab='Violence rate.', main='Relationship between Violence rate and Average Income.', sub='Black - Shall Law not in effect       Red - Shall Law in effect')
abline(lm(gun.data$vio~gun.data$avginc))

ggplot(gun.data) + geom_point(aes(x=density, y=log(vio), color=factor(shall)), alpha=1) + ylab('Violence Rate') + xlab('Population per square mile of land area.') + 
  ggtitle("Relationship between Violence rate and Population Density.")
plot(log(gun.data$density), log(gun.data$vio), pch=19, col=factor(gun.data$shall), xlab='Population per square mile of land area.', ylab='Violence rate.', 
     main='Relationship between Violence rate and Population Density.', sub='Black - Shall Law not in effect       Red - Shall Law in effect')
abline(lm(log(gun.data$vio)~log(gun.data$density)))

plot(gun.data$pop, gun.data$vio, pch=19, col=factor(gun.data$shall), xlab='Population of state (millions of people).', ylab='Violence rate.', 
     main='Relationship between Violence rate and Population of state.', sub='Black - Shall Law not in effect       Red - Shall Law in effect')
abline(lm(gun.data$vio~gun.data$pop))

typeof(gun.data)
gun.data1=data.frame(gun.data)
gun.data2=data.frame(gun.data)
gun.data1 <- gun.data1[gun.data1$shall == 1,]
gun.data2 <- gun.data2[gun.data2$shall == 0, ]

plot(gun.data1$year, gun.data1$vio, pch=18, col=factor(gun.data1$stateid), xlab='Year of observation.', ylab='Violence rate.', 
     main='Violence rate observed in each year for different states.', sub='Black - Shall Law not in effect       Red - Shall Law in effect')

ggplot(gun.data1) + geom_point(aes(x=year, y=vio, color=factor(stateid)), alpha=1) + ylab('Violence rate.') + xlab('Year of observation.') + 
  ggtitle("Violence rate observed in each year for different states.")


avgvio.year <- aggregate(gun.data1$vio, list(gun.data1$year), mean)
avgvio.year
avgvio.year.0 <- aggregate(gun.data2$vio, list(gun.data2$year), mean) 
plot(avgvio.year$Group.1, avgvio.year$x, pch=19, col='blue', xlab='Year of Observation', ylab='Average Violence Rate', 
     main ='Mean Violance Rate of all states where Shall Law was introduced.')

ggplot()+ geom_line(data = avgvio.year, aes(x = Group.1, y = x, color = "Shall = 1"), size =1.0) + geom_line(data = avgvio.year.0, aes(x =  Group.1, y = x, color = "Shall = 0"),
          size =1.0)  + xlab('Year') + ylab('Violence Rate') + ggtitle('Change in Violence rate with respect to the inclusion of Shall Law') + labs(color="Shall Law Status")

