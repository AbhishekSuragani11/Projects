library(tidyverse)
############## Loading Abhishek's Dataset#########
Hesi<-read.csv("Regions and Households.csv") %>% 
  dummy_cols(select_columns = "Region") 
Hesi
head(Hesi)
summary(Hesi)
str(Hesi)

############## Loading Unemployment rate Dataset#########
unemployment_rate <- read.csv("Unemployment.csv")

##cleaning unemployment rate data
unemployment_rate <- unemployment_rate %>% 
  filter(Attribute=="Unemployment_rate_2020") %>% 
  rename( unemployment_rate = "Value") %>% 
  select(FIPS, unemployment_rate)


########Loading Obesity Dataset #########

diabetes_obesity<-read.csv("Diabetes_Obesity.csv")
diabetes_obesity
head(diabetes_obesity)
summary(diabetes_obesity)
str(diabetes_obesity)

##############Loading Politics Dataset #########

politics<-read.csv("CountyPolitics.csv")
politics
head(politics)
summary(politics)
str(politics)

##cleaning politics dataset, creating binary variables of party, renaming fips code
library(dplyr)
library(fastDummies)

politics <- politics %>% 
  group_by(county_fips) %>% 
  slice_max(n = 1, candidatevotes) %>% 
  dummy_cols(select_columns = "party") %>% 
  select(county_fips, party_DEMOCRAT, party_REPUBLICAN) %>% 
  rename(FIPS= "county_fips")

#######Merging Abhishek's dataset + Obesity##########
df1 <- merge(Hesi,diabetes_obesity, by.x="FIPS", by.y="FIPS")
df1

#######Merging Abhishek's dataset + Unemployment rate dataset##########
df2 <- merge(df1,unemployment_rate, by.x="FIPS", by.y="FIPS")
df2

###### Merging healthy with politics##########
df3<- merge(df2, politics, by.x="FIPS", by.y="FIPS")
df3
colnames(df3)

############## Viewing the final dataset#############

df3
head(df3)
tail(df3)
str(df3)
summary(df3)
colnames(df3)

## missing values
sum(is.na(df3))
missing <- df3[rowSums(is.na(df3)) > 0, ]
missing
count(missing)
#the only variable with missing values is the percent of fully vaccinated people so removed that variable
#one county Rio Arriba did not have an SVI values, so exclueded it from the data 
#After dealing with  all missing values, we have a total of 3112 observations in our data set

######### Running the methods###############
###### Correlation #######
typeof(df3$Estimated.hesitant)
typeof(df3$SVI) 

cor(df3$Estimated.hesitant,df3$SVI)
cor(df3$Estimated.hesitant,df3$CVAC.level.of.concern.for.vaccination.rollout)
cor(df3$Estimated.hesitant,df3$Diabetes_Percent)
cor(df3$Estimated.hesitant,df3$Obesity_Percent)
cor(df3$Estimated.hesitant,df3$Percent.Hispanic)
cor(df3$Estimated.hesitant,df3$Percent.non.Hispanic.American.Indian.Alaska.Native)
cor(df3$Estimated.hesitant,df3$Percent.non.Hispanic.Asian)
cor(df3$Estimated.hesitant,df3$Percent.non.Hispanic.Black)
cor(df3$Estimated.hesitant,df3$Percent.non.Hispanic.White)
cor(df3$Estimated.hesitant,df3$Percent.non.Hispanic.Native.Hawaiian.Pacific.Islander)
cor(df3$Estimated.hesitant,df3$Median_Household_Income_2019)
cor(df3$Estimated.hesitant,df3$unemployment_rate)


##### cor not working with SVI and % adult fully vaccinated
#this is issue is fixed, both had missing values


##### cor for strongly hesitant ####
cor(df3$Estimated.strongly.hesitant,df3$SVI)
cor(df3$Estimated.strongly.hesitant,df3$CVAC.level.of.concern.for.vaccination.rollout)
cor(df3$Estimated.strongly.hesitant,df3$Diabetes_Percent)
cor(df3$Estimated.strongly.hesitant,df3$Obesity_Percent)
cor(df3$Estimated.strongly.hesitant,df3$Percent.Hispanic)
cor(df3$Estimated.strongly.hesitant,df3$Percent.non.Hispanic.American.Indian.Alaska.Native)
cor(df3$Estimated.strongly.hesitant,df3$Percent.non.Hispanic.Asian)
cor(df3$Estimated.strongly.hesitant,df3$Percent.non.Hispanic.Black)
cor(df3$Estimated.strongly.hesitant,df3$Percent.non.Hispanic.White)
cor(df3$Estimated.strongly.hesitant,df3$Percent.non.Hispanic.Native.Hawaiian.Pacific.Islander)
cor(df3$Estimated.strongly.hesitant,df3$Median_Household_Income_2019)
cor(df3$Estimated.strongly.hesitant,df3$unemployment_rate)


#########################################################################################################
###### k-means clustering######
####Creating Dataframes for all the numerical columns####

numeric_df <- select(df3,
                     Estimated.hesitant,
                     SVI,
                     Percent.non.Hispanic.American.Indian.Alaska.Native,
                     Percent.non.Hispanic.Asian,
                     Percent.non.Hispanic.Black,
                     Percent.non.Hispanic.Native.Hawaiian.Pacific.Islander,
                     Percent.non.Hispanic.White,
                     Diabetes_Percent,
                     Obesity_Percent,
                     unemployment_rate,
                     Median_Household_Income_2019,
                     CVAC.level.of.concern.for.vaccination.rollout)
glimpse(numeric_df)
km <- data.frame(numeric_df)
km.norm <- scale(km)
library(cluster)
set.seed(12345)
gaps <- clusGap(km.norm,kmeans,10,d.power=2) 
maxSE(gaps$Tab[,"gap"],gaps$Tab[,"SE.sim"],"Tibs2001SEmax")
plot(gaps$Tab[,"gap"])

#6 clusters make sense looking at the plot

set.seed(12345)
km.cluster <- kmeans(km.norm, 6 , nstart=10)
unscale <- function (vals, norm.data, col.ids)
{
  cols <- if (missing(col.ids))
    1:NCOL(vals)
  else col.ids
  if (length(cols) != NCOL(vals))
    stop("Incorrect dimension of data to unscale.")
  centers <- attr(norm.data, "scaled:center")[cols]
  scales <- attr(norm.data, "scaled:scale")[cols]
  unvals <- scale(vals, center = (-centers/scales), scale = 1/scales)
  attr(unvals, "scaled:center") <- attr(unvals, "scaled:scale") <- NULL
  unvals
}
unscale(km.cluster$centers, km.norm)
####i'm not sure how to interpret these centers. will discuss in group

###partitioning data####
df3 <- as.data.frame(df3)
set.seed(12345) 
training <- sample(1:nrow(df3), 0.6*nrow(df3))
ycol <- match('Estimated.hesitancy',colnames(df3))
df3.training <- df3[training,-ycol]
df3.training.results <- df3[training,ycol] 
df3.test <- df3[-training,-ycol]
df3.test.results <- df3[-training,ycol] 

######### Multiple Regression#########
MLRmodel <- lm(Estimated.hesitant ~ 
                 SVI+
                 CVAC.level.of.concern.for.vaccination.rollout+
                 Percent.Hispanic +
                 Percent.non.Hispanic.American.Indian.Alaska.Native +
                 Percent.non.Hispanic.Asian +
                 Percent.non.Hispanic.Black +
                 Percent.non.Hispanic.Native.Hawaiian.Pacific.Islander +
                 Percent.non.Hispanic.White +
                 Median_Household_Income_2019 +
                 Region_Midwest +
                 Region_Northeast +
                 Region_South +
                 Diabetes_Percent +
                 Obesity_Percent +
                 unemployment_rate +
                 party_DEMOCRAT,
               data=df3)

summary(MLRmodel)

# ########## Adding Dummy Variables ##########
# str(df3)
# #####Making a df with dummy variables######
# kmHealthpol.dummy <- data.frame(df3$Estimated.hesitant,df3$Social.Vulnerability.Index..SVI.,
#                           df3$Percent.Hispanic,df3$Percent.non.Hispanic.American.Indian.Alaska.Native,
#                           df3$Percent.non.Hispanic.Asian,df3$Percent.non.Hispanic.Black,
#                           df3$Percent.non.Hispanic.Native.Hawaiian.Pacific.Islander,df3$Percent.non.Hispanic.White,
#                           df3$Diabetes_Total,df3$Obesity_Total, 
#                           df3$CVAC.level.of.concern.for.vaccination.rollout,
#                           party=df3$party)
# #Applying Dummy Variable
# kmHealthpol.dummy.create <- dummy_cols(kmHealthpol.dummy, select_columns='party')
# kmHealthpol.dummy.create
# 
# DummyRegression <-lm(df3$Estimated.hesitant ~ df3$SVI+
#                     df3$Percent.Hispanic+df3$Percent.non.Hispanic.American.Indian.Alaska.Native+
#                     df3$Percent.non.Hispanic.Asian+df3$Percent.non.Hispanic.Black+
#                      df3$Percent.non.Hispanic.Native.Hawaiian.Pacific.Islander+df3$Percent.non.Hispanic.White+
#                     df3$Diabetes_Total+df3$Obesity_Total+
#                     df3$CVAC.level.of.concern.for.vaccination.rollout+
#                     df3$party - party - party_DEMOCRAT, data=kmHealthpol.dummy.create)
# 
# summary(DummyRegression)

########Visualizations GGPLOT2##############


