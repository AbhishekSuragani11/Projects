library(tidyverse)
library(dplyr)
library(fastDummies)


############## Loading Abhishek's Dataset#########
Hesi <- read.csv("Regions and Households.csv") %>%
  dummy_cols(select_columns = "Region")
Hesi
head(Hesi)
summary(Hesi)
str(Hesi)

############## Loading Unemployment rate Dataset#########
unemployment_rate <- read.csv("Unemployment.csv")

## cleaning unemployment rate data
unemployment_rate <- unemployment_rate %>%
  filter(Attribute == "Unemployment_rate_2020") %>%
  rename(unemployment_rate = "Value") %>%
  select(FIPS, unemployment_rate)


######## Loading Obesity Dataset #########

diabetes_obesity <- read.csv("Diabetes_Obesity.csv")
diabetes_obesity
head(diabetes_obesity)
summary(diabetes_obesity)
str(diabetes_obesity)

############## Loading Politics Dataset #########

politics <- read.csv("CountyPolitics.csv")
politics
head(politics)
summary(politics)
str(politics)

## cleaning politics dataset, creating binary variables of party, renaming fips code

politics <- politics %>%
  group_by(county_fips) %>%
  slice_max(n = 1, candidatevotes) %>%
  dummy_cols(select_columns = "party") %>%
  select(county_fips, party_DEMOCRAT, party_REPUBLICAN) %>%
  rename(FIPS = "county_fips")

####### Merging Abhishek's dataset + Obesity##########
df1 <- merge(Hesi, diabetes_obesity, by.x = "FIPS", by.y = "FIPS")
df1

####### Merging Abhishek's dataset + Unemployment rate dataset##########
df2 <- merge(df1, unemployment_rate, by.x = "FIPS", by.y = "FIPS")
df2

###### Merging healthy with politics##########
df3 <- merge(df2, politics, by.x = "FIPS", by.y = "FIPS")
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
# the only variable with missing values is the percent of fully vaccinated people so removed that variable
# one county Rio Arriba did not have an SVI values, so exclueded it from the data, this is also why
# cor of SVI was not working
# After dealing with  all missing values, we have a total of 3112 observations in our data set
# our primary data set is called df3

######### Running the methods###############
###### Correlation Analysis and sactter plots #######
typeof(df3$Estimated.hesitant)
typeof(df3$SVI)

cor(df3$Estimated.hesitant, df3$SVI)
scatterplot(Estimated.hesitant ~ SVI, data=df3)

cor(df3$Estimated.hesitant, df3$CVAC.level.of.concern.for.vaccination.rollout)
scatterplot(Estimated.hesitant ~ CVAC.level.of.concern.for.vaccination.rollout, data=df3)

cor(df3$Estimated.hesitant, df3$Diabetes_Percent)
scatterplot(Estimated.hesitant ~ Diabetes_Percent, data=df3)

cor(df3$Estimated.hesitant, df3$Obesity_Percent)
scatterplot(Estimated.hesitant ~ Obesity_Percent, data=df3)

cor(df3$Estimated.hesitant, df3$Percent.Hispanic)
scatterplot(Estimated.hesitant ~ Percent.Hispanic, data=df3)

cor(df3$Estimated.hesitant, df3$Percent.non.Hispanic.American.Indian.Alaska.Native)
scatterplot(Estimated.hesitant ~ Percent.non.Hispanic.American.Indian.Alaska.Native, data=df3)

cor(df3$Estimated.hesitant, df3$Percent.non.Hispanic.Asian)
scatterplot(Estimated.hesitant ~ Percent.non.Hispanic.Asian, data=df3)

cor(df3$Estimated.hesitant, df3$Percent.non.Hispanic.Black)
scatterplot(Estimated.hesitant ~ Percent.non.Hispanic.Black, data=df3)

cor(df3$Estimated.hesitant, df3$Percent.non.Hispanic.White)
scatterplot(Estimated.hesitant ~ Percent.non.Hispanic.White, data=df3)

cor(df3$Estimated.hesitant, df3$Percent.non.Hispanic.Native.Hawaiian.Pacific.Islander)
scatterplot(Estimated.hesitant ~ Percent.non.Hispanic.Native.Hawaiian.Pacific.Islander, data=df3)

cor(df3$Estimated.hesitant, df3$Median_Household_Income_2019)
scatterplot(Estimated.hesitant ~ Median_Household_Income_2019, data=df3)

cor(df3$Estimated.hesitant, df3$unemployment_rate)
scatterplot(Estimated.hesitant ~ unemployment_rate, data=df3)

cor(df3$Estimated.hesitant, df3$Region_Midwest)
scatterplot(Estimated.hesitant ~ SVI, data=df3)

cor(df3$Estimated.hesitant, df3$Region_Northeast)
scatterplot(Estimated.hesitant ~ Region_Northeast, data=df3)

cor(df3$Estimated.hesitant, df3$Region_South)
scatterplot(Estimated.hesitant ~ Region_South, data=df3)

cor(df3$Estimated.hesitant, df3$Region_West)
scatterplot(Estimated.hesitant ~ Region_West, data=df3)

cor(df3$Estimated.hesitant, df3$party_DEMOCRAT)
scatterplot(Estimated.hesitant ~ party_DEMOCRAT, data=df3)

cor(df3$Estimated.hesitant, df3$party_REPUBLICAN)
scatterplot(Estimated.hesitant ~ party_REPUBLICAN, data=df3)

##### cor not working with SVI and % adult fully vaccinated
# this is issue is fixed, both had missing values


##### cor for strongly hesitant ####
cor(df3$Estimated.strongly.hesitant, df3$SVI)
cor(df3$Estimated.strongly.hesitant, df3$CVAC.level.of.concern.for.vaccination.rollout)
cor(df3$Estimated.strongly.hesitant, df3$Diabetes_Percent)
cor(df3$Estimated.strongly.hesitant, df3$Obesity_Percent)
cor(df3$Estimated.strongly.hesitant, df3$Percent.Hispanic)
cor(df3$Estimated.strongly.hesitant, df3$Percent.non.Hispanic.American.Indian.Alaska.Native)
cor(df3$Estimated.strongly.hesitant, df3$Percent.non.Hispanic.Asian)
cor(df3$Estimated.strongly.hesitant, df3$Percent.non.Hispanic.Black)
cor(df3$Estimated.strongly.hesitant, df3$Percent.non.Hispanic.White)
cor(df3$Estimated.strongly.hesitant, df3$Percent.non.Hispanic.Native.Hawaiian.Pacific.Islander)
cor(df3$Estimated.strongly.hesitant, df3$Median_Household_Income_2019)
cor(df3$Estimated.strongly.hesitant, df3$unemployment_rate)




#########################################################################################################
###### k-means clustering######
#### Creating Dataframes for all the numerical columns####

numeric_df <- select(
  df3,
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
  CVAC.level.of.concern.for.vaccination.rollout
)
glimpse(numeric_df)
km <- data.frame(numeric_df)
km.norm <- scale(km)
# the following code takes time, so we dont need to run it everytime
# library(cluster)
# set.seed(12345)
# gaps <- clusGap(km.norm,kmeans,10,d.power=2)
# maxSE(gaps$Tab[,"gap"],gaps$Tab[,"SE.sim"],"Tibs2001SEmax")
# plot(gaps$Tab[,"gap"])

# 6 clusters make sense looking at the clusgap plot

set.seed(12345)
km.cluster <- kmeans(km.norm, 6, nstart = 10)
unscale <- function(vals, norm.data, col.ids) {
  cols <- if (missing(col.ids)) {
    1:NCOL(vals)
  } else {
    col.ids
  }
  if (length(cols) != NCOL(vals)) {
    stop("Incorrect dimension of data to unscale.")
  }
  centers <- attr(norm.data, "scaled:center")[cols]
  scales <- attr(norm.data, "scaled:scale")[cols]
  unvals <- scale(vals, center = (-centers / scales), scale = 1 / scales)
  attr(unvals, "scaled:center") <- attr(unvals, "scaled:scale") <- NULL
  unvals
}
unscale(km.cluster$centers, km.norm)
#### i'm not sure how to interpret these centers. will discuss in group

# smaller cluster for ease of interpretation:
km.cluster <- kmeans(km.norm, 3, nstart = 10)
unscale(km.cluster$centers, km.norm)

### partitioning data####
# to avoid overfitting
# to calculate RMSE which will help us compare our models.
df3 <- as.data.frame(df3)
set.seed(12345)
training <- sample(1:nrow(df3), 0.6 * nrow(df3))
ycol <- match("Estimated.hesitant", colnames(df3))
df3.training <- df3[training, -ycol]
df3.training.results <- df3[training, ycol]
df3.test <- df3[-training, -ycol]
df3.test.results <- df3[-training, ycol]

######### Multiple Regression#########
# MODEL A:
MLRmodel <- lm(Estimated.hesitant ~
SVI +
  CVAC.level.of.concern.for.vaccination.rollout +
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
data = df3[training, ]
)

summary(MLRmodel)

## using this model we generate results for the test set and calculate RMSE:
MLRmodel.predictions <- predict(MLRmodel, df3[-training, ])
(mean((df3.test.results - MLRmodel.predictions)^2))^0.5
# our root mean squared error is 0.0344 which is veryy small. That's a good thing!'

### #MODEL B:regression w/out unemployment rate, income and race
MLRmodel2 <- lm(Estimated.hesitant ~
SVI +
  CVAC.level.of.concern.for.vaccination.rollout +
  Region_Midwest +
  Region_Northeast +
  Region_South +
  Diabetes_Percent +
  Obesity_Percent +
  party_DEMOCRAT,
data = df3[training, ]
)

summary(MLRmodel2) 
## using this model we generate results for the test set and calculate RMSE:
MLRmodel2.predictions <- predict(MLRmodel2, df3[-training, ])
(mean((df3.test.results - MLRmodel2.predictions)^2))^0.5
# RMSE is 0.0364 almost the same as Model A
#R squared is 0.3501

### #MODEL C:regression w/out SVI
MLRmodel3 <- lm(Estimated.hesitant ~
                 CVAC.level.of.concern.for.vaccination.rollout +
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
               data = df3[training, ]
)

summary(MLRmodel3)
## using this model we generate results for the test set and calculate RMSE:
MLRmodel3.predictions <- predict(MLRmodel3, df3[-training, ])
(mean((df3.test.results - MLRmodel3.predictions)^2))^0.5
#rmse is [1] 0.03441295
#R square 0.4121

#### Regression Trees
library(tree)

df3.tree <- tree(Estimated.hesitant ~
SVI +
  CVAC.level.of.concern.for.vaccination.rollout +
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
data = df3[training, ],
mindev = 0.005
)

# The next two lines plot the best tree
plot(df3.tree)
text(df3.tree, cex = 0.6)
summary(df3.tree)
# residual mean deviance is 0.001099

# The following command generates predictions for the test set and calculates RMSE
df3.tree.predictions <- predict(df3.tree, df3[-training, ])
(mean((df3.test.results - df3.tree.predictions)^2))^0.5

# determining the best mindev by comparing many different tree to come up with a tree with lowest possible RMSE
best.mindev <- -1
RMSE <- -1
best.RMSE <- 99999999
for (i in seq(from = 0.0005, to = 0.05, by = 0.0005)) {
  df3.tree <- tree(Estimated.hesitant ~
                     SVI +
                     CVAC.level.of.concern.for.vaccination.rollout +
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
                   data = df3[training, ], mindev = i)
  df3.tree.predictions <- predict(df3.tree, df3)[-training]
  RMSE <- (mean((df3.test.results - df3.tree.predictions)^2))^0.5
  if (RMSE < best.RMSE) {
    best.mindev <- i
    best.RMSE <- RMSE
  }
}
print(paste("The optimal value of mindev is", best.mindev, "with a RMSE of", best.RMSE))
#"The optimal value of mindev is 0.0045 with a RMSE of 0.0345403185339632"

df3.best.tree <- tree(Estimated.hesitant ~
                        SVI +
                        CVAC.level.of.concern.for.vaccination.rollout +
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
                      data = df3[training, ],
                      mindev=best.mindev)
plot(df3.best.tree)
text(df3.best.tree, cex=0.5)

#All three models have very similar rmse

######## Visualizations GGPLOT2##############


df3 <- as.data.frame(df3)

df3%>%
  group_by(Region)%>%
  summarise(Average_Estimated.hesitant = mean(Estimated.hesitant))%>%
  ggplot(aes(x = Region, y = Average_Estimated.hesitant)) + 
  geom_col(aes(fill = Region)) + theme_bw() +
  labs( x = "Region", y = "Average Vaccine Hesitancy Rate", 
        title = "Region vs Average Vaccine Hesitancy Rate") +
  theme(plot.title = element_text(hjust = 0.5)) 


ggplot(df3, aes(x = SVI.Category, y = Estimated.hesitant, fill = SVI.Category)) + 
  geom_boxplot() + theme_bw() +
  labs( x = "SVI Category", y = "Vaccine Hesitancy", 
        title = "SVI Category vs Vaccine Hesitancy") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom")

ggplot(df3, aes( x = Diabetes_Percent, 
                 y = Estimated.hesitant, color = Diabetes_Percent > 10)) + geom_point() + geom_smooth(fill = NA, method = lm) + expand_limits(y=0) +
  labs( x = "Diabetes(%)", y = "Vaccine Hesitancy", 
        title = "Diabetes(%) vs Vaccine Hesitancy") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(df7,aes(x = Percent, y = Estimated.hesitant)) +
  geom_point(aes(color = Race)) + facet_wrap(~ Race)
theme_bw() + labs( x = "Race", 
                   y = "Vaccine Hesitancy Rate", 
                   title = "Race vs Vaccine Hesitancy") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom") 


ggplot(df9, aes(x= Party, y = Majority, color = Party)) + 
  geom_col() + facet_wrap(~ Estimated.hesitant) +
  theme_bw() +
  labs( x = "Party", y = "Majority", 
        title = "Vaccine Hesitancy vs party") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom")

