##
## EFFECT OF SHALL-CARRY LAW ON VIOLENCE IN UNITED STATES
##
##
## Using pacman, loading and installing required packages.
##
pacman::p_load(data.table, forecast, leaps, tidyverse, caret, corrplot, glmnet, mlbench, ggplot2, 
               gplots, ggpubr, MASS, AER, plm, usmap, writexl, foreign, ggthemes, devtools, sqldf, plm, lmtest, AER, stargazer, systemfit, panelr)
options(scipen = 999)
## 
## Read the data file from the working directory.
##
df=read.dta("guns.dta")
head(df)
df2=read.csv("stateid.csv")
df3=merge(df,df2, by="stateid")
head(df3)
write.xlsx(df3, data.xlsx,sheetName="Sheet1")
unique(df3[c("stateid","State")])
col_names=colnames(df)
length(col_names)
ch=filter(df3,df3$stateid==13)
##
## Building the multiple regression model.
##
m1= lm(vio~mur+rob+incarc_rate+pb1064+pw1064+pm1029+pop+avginc+density+stateid+shall,df)
summary(m1)
m2=lm(vio~mur+rob+incarc_rate+pm1029+density+stateid+shall,df)
summary(m2)
m3=lm(vio~mur+rob+incarc_rate+pm1029+density+stateid+shall,df)
summary(m3)
m4=lm(vio~mur+rob+incarc_rate+pm1029+pop+avginc+density+stateid+shall,df)
summary(m4)
m5=lm(vio~mur+rob+incarc_rate+pm1029+pop+density+stateid+shall,df)
summary(m5)
m6=lm(vio~mur+rob+incarc_rate+pm1029+avginc+density+stateid+shall,df)
summary(m6)
m7=lm(vio~mur+rob+incarc_rate+pm1029+pb1064+avginc+density+stateid+shall,df)
summary(m7) ## good 
m8=lm(vio~mur+rob+incarc_rate+pm1029+pb1064+pw1064+avginc+density+stateid+shall,df)
summary(m8)

## t-test for incarc
mod1=lm(vio~incarc_rate,df)
summary(mod1)

## log-log models

m9=lm(log(vio)~mur+rob+log(incarc_rate)+pm1029+pb1064+avginc+density+stateid+shall,df)
summary(m9)
m10=lm(log(vio)~rob+log(incarc_rate)+pm1029+pb1064+avginc+density+stateid+shall,df)
summary(m10)
m11=lm(log(vio)~log(rob)+log(incarc_rate)+pm1029+pb1064+log(density)+stateid+shall,df)
summary(m11)
m12=lm(log(vio)~log(mur)+log(rob)+log(incarc_rate)+pm1029+pb1064+log(density)+stateid+shall,df)
summary(m12) ## good
m13=lm(log(vio)~log(mur)+log(rob)+log(incarc_rate)+I(log(incarc_rate)^2)+pm1029+pb1064+log(density)+stateid+shall,df)
summary(m13)
m12_v1=lm(log(vio)~mur+log(rob)+log(incarc_rate)+pm1029+pb1064+log(density)+shall,df)
summary(m12_v1) ## good
m12_v2=lm(log(vio)~log(incarc_rate)+pm1029+pb1064+log(density)+shall,df)
summary(m12_v2) ## good



## Pooled Models


class(df_pool$vio)
df_pool=pdata.frame(df,index=c("stateid","year"))
df_pool$vio=as.numeric(df$vio)
ff=plm(shall~rob,data=df,model='pooling')
summary(ff)
m14=plm(log(as.numeric(vio))~log(mur)+log(rob)+log(incarc_rate)+pm1029+pb1064+log(density)+stateid+shall,data=df_pool,model="pooling")
summary(m14)
m15=plm(log(as.numeric(vio))~log(mur)+log(rob)+log(incarc_rate)+pm1029+log(density)+shall+stateid,data=df_pool,model="pooling")
summary(m15)
m15_v1=plm(log(as.numeric(vio))~log(incarc_rate)+pm1029+log(density)+shall+stateid,data=df_pool,model="pooling")
summary(m15_v1)
m15_v2=plm(log(as.numeric(vio))~mur+log(rob)+log(incarc_rate)+pm1029+log(density)+shall+stateid,data=df_pool,model="pooling")
summary(m15_v2) ## good
m15_v3=plm(log(as.numeric(vio))~log(incarc_rate)+pm1029+log(density)+shall+stateid,data=df_pool,model="pooling")
summary(m15_v3)

m15_v4=plm(log(vio)~log(incarc_rate)+pb1064+pw1064+pm1029+pop+avginc+log(density)+shall,data=df_pool,model="pooling")
summary(m15_v4)
m15_v5=plm(log(vio)~log(incarc_rate)+pm1029+pop+avginc+log(density)+shall,data=df_pool,model="pooling")
summary(m15_v5)  ## pooled model finalised


## Heteroskedasticity check

vio_predicted=predict(m12)
residual=resid(m12)
plot(vio_predicted,residual,main = "Test for heteroskedasticity",col="blue",pch=19)
abline(h=0,colour="blue")


bptest(m15_v5)
bptest(plm(log(vio)~log(incarc_rate)+pm1029+pop+avginc+log(density)+shall,
           data=df_pool,model="pooling"))
coeftest(m12, vcov = vcovHC(m12))
bptest(m15_v5)
coeftest(m15_v5, vcov = vcovHC(m15_v5))

## Heteroskedasticity exists but the magnitude is of not much concern
## We correct it by using robust standard errors where SE is consistent


## Fixed

m16=plm(log(as.numeric(vio))~log(mur)+log(rob)+log(incarc_rate)+pm1029+pb1064+log(density)+stateid+shall,data=df_pool,model="within")
summary(m16)
m17=plm(log(vio)~log(mur)+log(rob)+log(incarc_rate)+pm1029+log(density)+shall+stateid,data=df_pool,model="within")
summary(m17)
m18=plm(log(vio)~log(mur)+log(rob)+log(incarc_rate)+pm1029+log(density)+shall+stateid,data=df_pool,model="within")
summary(m18)
m19=plm(log(vio)~log(incarc_rate)+pm1029+log(density)+shall+stateid,data=df_pool,model="within")
summary(m19)
m20=plm(log(as.numeric(vio))~mur+log(rob)+pm1029+log(density)+shall+stateid+as.factor(year),data=df_pool,model="within")
summary(m20) ## good model till now
m21=plm(log(as.numeric(vio))~log(incarc_rate)+pm1029+log(density)+shall+stateid,data=df_pool,model="within")
summary(m21)
m22=plm(log(vio)~log(incarc_rate)+pb1064+pm1029+pop+avginc+log(density)+shall+pw1064+as.factor(year),data=df_pool,model="within")
summary(m22)
m23=plm(log(as.numeric(vio))~avginc+log(incarc_rate)+pm1029+pb1064+log(density)+stateid+shall,data=df_pool,model="within")
summary(m23)
m24=plm(log(vio)~log(incarc_rate)+pb1064+pm1029+pw1064+pop+avginc+log(density)+shall,data=df_pool,model="within")
summary(m24)
m25=plm(log(vio)~log(incarc_rate)+pb1064+pm1029+pw1064+pop+log(density)+shall,data=df_pool,model="within")
summary(m25)  ## model to be included
m26=plm(log(mur)~log(incarc_rate)+pb1064+pw1064+pm1029+pop+log(density)+shall,data=df_pool,model="within")
summary(m26)  ## FE model to be included for mur
m27=plm(log(rob)~log(incarc_rate)+pb1064+pw1064+pm1029+pop+log(density)+shall,data=df_pool,model="within")
summary(m27)  ## FE model to be included for mur




## Fixed time

m31=plm(log(as.numeric(vio))~mur+log(rob)+log(incarc_rate)+pm1029+log(density)+shall+stateid,data=df_pool,model="within",effect = "time")
summary(m31) 
m32=plm(log(as.numeric(vio))~mur+log(rob)+log(incarc_rate)+pm1029+log(density)+shall+stateid+as.factor(year),data=df_pool,model="within")
summary(m32)
m33=plm(log(vio)~log(incarc_rate)+pb1064+pw1064+pm1029+pop+log(density)+shall+as.factor(year),data=df_pool,model="within")
summary(m33)  ## model to be included for vio
m34=plm(log(mur)~log(incarc_rate)+pb1064+pw1064+pm1029+pop+log(density)+shall+as.factor(year),data=df_pool,model="within")
summary(m34)  ## model to be included for mur
m35=plm(log(rob)~log(incarc_rate)+pb1064+pw1064+pm1029+pop+log(density)+shall+as.factor(year),data=df_pool,model="within")
summary(m35)  ## model to be included for mur

## Ftest for time effect

pFtest(m25,m33)


## RANDOM EFFECTS 
m41=plm(log(rob)~log(incarc_rate)+pop+log(density)+shall,data=df_pool,model="random")
summary(m41)

## Test for endogenity
phtest(m41,m33)
phtest(plm(log(rob)~log(incarc_rate)+pop+log(density)+shall,data=df_pool,
           model="random"),
       plm(log(vio)~log(incarc_rate)+pb1064+pw1064+pm1029+pop+log(density)+
             shall,data=df_pool, model="within"))





c(AIC(m16,m17,m18,m19,m20,m21))
c(BIC(m16,m17,m18,m19,m20,m21))

c(AIC(m1,m2,m3,m4,m5,m6,m7,m8))
c(BIC(m1,m2,m3,m4,m5,m6,m7,m8))
c(AIC(m9,m10,m11,m12,m13))
c(BIC(m9,m10,m11,m12,m13))



## Endogenuity check
hausman.systemfit(summary(m12))

summary(m12,dependencies=TRUE)


#colnames(df)
#[1] "year"        "vio"         "mur"         "rob"        
#[5] "incarc_rate" "pb1064"      "pw1064"      "pm1029"     
#[9] "pop"         "avginc"      "density"     "stateid"    
#[13] "shall"




a=sqldf("Select df.stateid,avg(df.vio),sum(df.mur),df2.state from df join df2
      on df.stateid=df2.stateid group by df.stateid order by sum(df.vio) desc, sum(df.mur) desc ")

b=sqldf("Select stateid, state, year from df3 where shall=1 group by stateid order by stateid")
c=sqldf("Select count(stateid),year from b group by year")
histogram(c$year,ylim=c(0,7),xlim=c(76,98),breaks=20,main="With breaks=20")

treemap(state, 
        index=c("state"),  
        vSize = "avg.vio", 
        type="index" ,
        title="Avg. vio rate across different state", ## Customize your title
        fontsize.title = 14 ## Change the font size of the title
)
 

df4=data.frame(df3)
names(df4)[14]= "state"
plot_usmap(data = df4, values = "vio", lines = "black" ,
           labels = TRUE, label_color = "black",size=0.1) + 
  scale_fill_continuous(
    low = "white", high = "blueviolet", name = "Scale")


treemap(df3, 
        index=c("state"),  
        vSize = "avg.vio", 
        type="index" ,
        title="Average vio rate by state", #Customize your title
        fontsize.title = 14 #Change the font size of the title
)


form= vio~mur+rob+incarc_rate+pm1029+avginc+density+stateid+shall
wi=plm(form,data=df3,model="random")