library(dplyr)
library(ggplot2)
library(tidyverse)
install.packages("tidyverse")
###Data Input
Data<- read.csv("winemag-data-130k-v2.csv")
Data<-filter(Data, country == "US")
colSums(is.na(Data))
Data <- na.omit(Data)
glimpse(Data)


Data <- Data%>%extract(title,'year',"(20\\d\\d)",convert = T,remove = F)%>%mutate(year=ifelse(year<1900,NA,year))
Data$wordcount <- sapply(gregexpr("\\S+", Data$description), length)
summary(Data$wordcount)
Data$description[which(Data$wordcount == 135)]
Data$description[which(Data$wordcount == 3)]


# ggplot(data = Data, aes(x= points))+
#   geom_histogram(binwidth = 1)+
#   labs(x = "points", y= "Frequency", title = "Distribution of points over data")
# cor(Data$points, Data$wordcount)

###Removing columns that are not used
wine_dataset_US<-select (Data,-c(country, taster_twitter_handle, designation, description, title, X))

######encoding

encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  #x
}

wine_dataset_US$province <- encode_ordinal(wine_dataset_US[["province"]])
wine_dataset_US$region_1<-encode_ordinal(wine_dataset_US[["region_1"]])
wine_dataset_US$region_2<-encode_ordinal(wine_dataset_US[["region_2"]])
wine_dataset_US$variety<-encode_ordinal(wine_dataset_US[["variety"]])
wine_dataset_US$winery<-encode_ordinal(wine_dataset_US[["winery"]])
wine_dataset_US$taster_name<-encode_ordinal(wine_dataset_US[["taster_name"]])
wine_dataset_US$year<-encode_ordinal(wine_dataset_US[["year"]])
wine_dataset_US$points<-as.numeric(wine_dataset_US[["points"]])
wine_dataset_US$wordcount<-as.numeric(wine_dataset_US[["wordcount"]])
wine_dataset_US$price<-as.numeric(wine_dataset_US[["price"]])
wine_dataset_US$reviewcount<-as.numeric(wine_dataset_US[["reviewcount"]])


glimpse(wine_dataset_US)
sapply(wine_dataset_US, class)

cor(subset(wine_dataset_US, select=c(points,price,province,region_1,region_2,taster_name,variety,winery,reviewcount,year,wordcount)))
cor(subset(wine_dataset_US, select=c(points,wordcount,year)))



######
ggplot(wine_dataset_US, aes(x = points,
                      y = wordcount)) +
  geom_point(position = "jitter", alpha = 0.1) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Number of Words in Description Against Wine Rating",
       x = "Rating",
       y = "Words in description",
       caption = "Points score is from 80 - 100; noise has been added to reduce plotting overlap")


# ggplot(wine_dataset_US, aes(x = points,
#                             y = wor)) +
#   geom_point(position = "jitter", alpha = 0.1) +
#   geom_smooth(method = "lm", se = FALSE) +
#   labs(title = "Variety Vs Wine Rating",
#        x = "Rating",
#        y = "Number of province per rating",
#        caption = "Points score is from 80 - 100; noise has been added to reduce plotting overlap")

# ggplot(data = wine_dataset_US, aes(x= points, colour = I('black'), fill = I('#099DD9')))+
#   geom_histogram(binwidth = 1)+
#   labs(x = "Points", y= "Frequency", title = "Distribution of points")
# 
# ggplot(wine_dataset_US, aes(x=points, fill="points")) + geom_histogram()

ggplot(data = wine_dataset_US, aes(x= points, y= wordcount, fill = wordcount))+
  geom_density_ridges ()+
  labs(x = "Word Count", title = "Distribution of word count of description")+
  scale_fill_cyclical(values = c("#CC3300", "#FFCC00"))

plot(points~count(points), data = wine_dataset_US)

##
v_vec<- Data %>% count(variety, sort = T) %>% head(20) %>% pull(variety)

Data %>%
  filter(variety %in% v_vec) %>%
  ggplot(aes(x = reorder(variety, points), y = points))+
  geom_boxplot(aes(fill = points), show.legend = F, alpha = .7)+
  coord_flip()+
  theme_bw()+
  labs(x = '',
       title = 'Wine Ratings by Varietal')+
  theme(plot.title = element_text(hjust = 0.5, vjust = 2.5))




#wine_dataset_US[, .N, by=.("Points", Bin)]
install.packages('sqldf') 
library(sqldf)
countdata<-sqldf("SELECT  Points, COUNT(*) AS Count FROM wine_dataset_US GROUP BY Points")
wine_dataset_US$revcount<-countdata$Count(where())
#install.packages("PerformanceAnalytics", dependencies = TRUE)
library("PerformanceAnalytics")
wine_dataset_US1<-select (wine_dataset_US,c(points,price,province,taster_name,variety,winery,wordcount,year))
chart.Correlation(wine_dataset_US1, histogram=TRUE, pch=19)

##linear regression model 1
lm_model1 <- lm(points ~ price+province+region_1+region_2+taster_name+year+variety+winery+wordcount, data = wine_dataset_US)
summary(lm_model1)
plot(lm_model1)
AIC(lm_model1)
step_AIC_backward <- step(lm_model1)

##linear regression model 2
lm_model2 <- lm(points ~ log(price)+province+taster_name+year+variety+winery+log(wordcount), data = wine_dataset_US)
summary(lm_model2)

##linear regression model 3
lm_model3 <- lm(points ~ (log(wordcount)+log(price))^2+variety+taster_name*province+winery+year, data = wine_dataset_US)
summary(lm_model3)
plot(lm_model3)
boxplot(anova(lm_model1.,lm_model2,lm_model3,test="Chisq"))
ggPredict(lm_model1,se=TRUE,interactive=TRUE)

yhat <-predict(lm_model2)
plot(yhat, wine_dataset_US$points)
line(yhat)
abline(lm_model2)

install.packages("ggiraphExtra")
library(ggiraphExtra)
ggPredict(lm_model2,se=TRUE,interactive=TRUE)

install.packages("BAS")
library(BAS)
AIC(lm_model2)
##Applying stepwise linear regression (AIC forward)
step_AIC_forward <-step(lm(points~1, data = wine_dataset_US), direction = "forward", scope =list(upper = lm_model1))
summary(step_AIC_forward)
step_AIC_forward

step_AIC_backward <- step(lm_model1)
summary(step_AIC_backward)

lm_model2 <- lm(points ~ log(price)+province+taster_name+year+variety+winery+log(wordcount), data = wine_dataset_US)
summary(lm_model2)

cor(subset(wine_dataset_US, select=c(points,log(price),province,region_1,region_2,taster_name,year,variety,winery,wordcount)))

lm3 <- lm(points ~ log(price), data = wine_dataset_US)
summary(lm3)
lm4 <- lm(points ~ province, data = Data)
summary(lm4)
lm5 <- lm(points ~ region_1, data = wine_dataset_US)
summary(lm5)
lm6 <- lm(points ~ region_2, data = wine_dataset_US)
summary(lm6)
lm7 <- lm(points ~ I(taster_name^2), data = wine_dataset_US)
summary(lm7)
lm8 <- lm(points ~ year, data = wine_dataset_US)
summary(lm8)
lm9 <- lm(points ~ variety, data = wine_dataset_US)
summary(lm9)
lm10 <- lm(points ~ I(winery^2), data = wine_dataset_US)
summary(lm10)
lm11 <- lm(points ~ log(wordcount), data = wine_dataset_US)
summary(lm11)



lm2 <- lm(points ~ log(wordcount)+log(price)+variety+taster_name+province+winery+year, data = wine_dataset_US)

summary(lm2)

# 
# Call:
#   lm(formula = points ~ log(I(wordcount^2)) + log(price) + variety + 
#        taster_name + province + winery + year, data = wine_dataset_US)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -9.5753 -1.4653  0.0698  1.5421  7.9966 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          6.638e+01  1.186e-01 559.670  < 2e-16 ***
#   log(I(wordcount^2))  2.292e+00  1.603e-02 143.029  < 2e-16 ***
#   log(price)           1.892e+00  1.804e-02 104.877  < 2e-16 ***
#   variety             -3.720e-03  4.467e-04  -8.326  < 2e-16 ***
#   taster_name         -6.151e-02  4.806e-03 -12.797  < 2e-16 ***
#   province            -4.295e-02  6.509e-03  -6.599 4.18e-11 ***
#   winery              -1.838e-04  7.831e-06 -23.472  < 2e-16 ***
#   year                -6.288e-02  2.269e-03 -27.706  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 2.187 on 54257 degrees of freedom
# Multiple R-squared:  0.5076,	Adjusted R-squared:  0.5075 
# F-statistic:  7989 on 7 and 54257 DF,  p-value: < 2.2e-16



