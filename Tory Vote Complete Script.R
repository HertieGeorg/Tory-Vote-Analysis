#-----------------------------------------------------------------------------
#Data Analysis - Tory Vote 

install.packages("foreign")
install.packages("stargazer")   
install.packages("ggplot2")    
install.packages("corrplot")    
install.packages("tidyverse") 
install.packages("dplyr")
install.packages("car") 

library(foreign)
library(stargazer)
library(ggplot2)
library(corrplot) 
library(dplyr)       
library(tidyverse)
library(car) 

rm(list=ls())             #delete all objects from environment
getwd()                   #get working directories if needed
#setwd()                  #set working directories if needed
options(scipen=999)       #remove scientific notation in printing e.g. 'e-01'


data_org <- read.dta("ToryVote_final_da.dta") 

reg1 <- lm(con_like ~ rile + age + income + religion, data = data_org) 
summary(reg1)

stargazer(reg1, 
          title="Table (2a): Likelihood of voting Conservative party", 
          type = "text", style = "default", out="reg(2a).html") 


#Visualizing how R-squared changes based on the i.v. included
reg1.1  <- lm(con_like ~ rile, data = data_org)
reg1.2  <- lm(con_like ~ rile + age, data = data_org)
reg1.3  <- lm(con_like ~ rile + age + income, data = data_org)
reg1.4  <- lm(con_like ~ rile + age + income + religion ,
              data = data_org)

stargazer(reg1.1, reg1.2, reg1.3, reg1.4, 
          title="Table (2d): Analysis of the composition of R-squared", 
          type = "text", style = "default",out="reg_(2d).html")


avPlot(reg1, "rile" )

#identified outliers: 1835, 1253, 833, 2397

#identification numbers
data_org$idno[833]   #=  44340
data_org$idno[1253]  #=  47017
data_org$idno[1835]  #=  51004
data_org$idno[2397]  #=  54714

dfbetasPlots(reg1,  terms=~ rile) #influence of outliers
#values are not extraordinary high therefore we don't have to omit them

summary(reg1) 
vif(reg1)
sqrt(vif(reg1))
1/vif(reg1)  #tolerance 

reg_EU <- lm(con_like ~ rile + age + income + religion + att_eu, 
             data = data_org)

stargazer(reg1, reg_EU, 
          title="Table (4a): Likelihood of ever voting Conservative party(2)",
          type = "text", style = "default", out="reg(4a).html")

data_org$att_eu_int <- NA

data_org$att_eu_int[data_org$att_eu == "strongly disapprove"] <- 1
data_org$att_eu_int[data_org$att_eu == "disapprove"] <- 2
data_org$att_eu_int[data_org$att_eu == "neither nor"] <- 3
data_org$att_eu_int[data_org$att_eu == "approve"] <- 4
data_org$att_eu_int[data_org$att_eu == "strongly approve"] <- 5

data_org$att_eu_int <- data_org$att_eu_int %>% as.character() %>% as.numeric()

reg_EUinter <- lm(con_like ~ rile + age + income + religion + att_eu_int,
                  data = data_org)

stargazer( reg1, reg_EU, reg_EUinter,
           title="Table (4b): Likelihood of voting Conservative party (2.1)", 
           type = "text", style = "default", out="reg(4b).html")


data_org$scot_character <- ifelse(data_org$scot == 1, 
                                  "Scotland", "England/Wales") 

#converting into a categorical variable
data_org$scot_character<- as.factor(data_org$scot_character)

#for boxplots: NAs are allowed in the data

plot(data_org$scot_character, data_org$con_like,  
     main="Likelihood of voting for Conservative Party: 
     Scotland compared to England/Wales",
     xlab="Likelihood of ever voting Conservative party",
     ylab="",
     col = c("orange", "light blue"),
     horizontal=T)

data_org$con_like_Scot <- ifelse(data_org$scot == 1, data_org$con_like, NA) 
data_org$con_like_NoScot <- ifelse(data_org$scot == 0, data_org$con_like, NA) 

data_subset_Scot <- select(data_org, con_like_Scot, scot)
data_subset_NoScot <- select(data_org, con_like_NoScot, scot)

data_subset_Scot <- na.omit(data_subset_Scot)
data_subset_NoScot <- na.omit(data_subset_NoScot)

diff.test <- t.test(data_subset_Scot$con_like_Scot, 
                    data_subset_NoScot$con_like_NoScot, na.action=TRUE)

t_test_results <- capture.output(print(diff.test))
writeLines(t_test_results, con = file("t.test.txt"))

#calculating standard errors

sd_Scot <- sd(data_subset_Scot$con_like_Scot)  #= 3.587346
sd_NoScot <- sd(data_subset_NoScot$con_like_NoScot)  #= 3.773669

n_Scot <- length(data_subset_Scot$con_like_Scot)  #=339
n_NoScot <- length(data_subset_NoScot$con_like_NoScot)  #=1460

se_Scot <- sd_Scot/sqrt(n_Scot)  #= 0.1948379
se_NoScot <- sd_NoScot/sqrt(n_NoScot)  #= 0.09876142





reg_interaction <- lm(con_like ~ rile + scot + rile*scot , data = data_org)

stargazer(reg_interaction, 
          title="Table (6a): Interaction rile*scot", 
          type = "text", style = "default", out="reg(6a).html")




reg_all <- lm(con_like ~ rile + age + income + religion + att_eu
              + scot + rile*scot, data = data_org)

stargazer(reg1, reg_EU, reg_EUinter, reg_all,
          title="Table (4b): Likelihood of voting Conservative party (2.2)",
          type = "text", style = "default", out="reg(6c).html")




stargazer(reg1, reg_EU, reg_EUinter, reg_interaction, reg_all,
          title="Table (1): Likelihood of ever voting Conservative party",
          type = "text", style = "default", out="reg(complete).html")



# teseting for skeweded variables to ensure the R-squared is not biased
hist(data_org$rile)
hist(data_org$age)
hist(data_org$religion)
hist(data_org$income) #is not normally distributet therefore we log it 

data_org$ln_income <- log(data_org$income) #takes the natural log
hist(data_org$ln_income) #Histogram of the new variable

reg1_ln <- lm(con_like ~ rile + age + ln_income + religion, 
              data = data_org) # including new income variable

stargazer(reg1, reg1_ln, 
          title="Table (2d.2) Income_ln: Likelihood of voting Conservative
          party", type = "text", style = "default", out="reg(2d.2).html")

# visually checking whether homoscedasticity assumption is fullfilled

ggplot(data_org, aes(income, con_like)) +
  geom_point(alpha = 0.3, position = position_jitter())+
  scale_colour_discrete()+
  geom_smooth(method = "lm")+
  theme_classic()




