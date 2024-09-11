############## Loading Abhishek's Dataset#########
Hesi<-read.csv("Regions and Households.csv")
Hesi
head(Hesi)
summary(Hesi)
str(Hesi)

########Loading Obesity Dataset #########

Fatsugar<-read.csv("Diabetes&Obesity.csv")
Fatsugar
head(Fatsugar)
summary(Fatsugar)
str(Fatsugar)

##############Loading Politics Dataset #########

politics<-read.csv("CountyPolitics.csv")
politics
head(politics)
summary(politics)
str(politics)

#######Merging Abhishek's dataset + Obesity##########
Healthy <- merge(Hesi,Fatsugar, by.x="FIPS.Code", by.y="FIPS_Codes")
Healthy

###### Merging healthy with politics##########
Healthpol<- merge(Healthy, politics, by.x="FIPS.Code", by.y="county_fips")
Healthpol
colnames(Healthpol)

############# Removing unwanted columns################
Healthpol$OBJECTID <-NULL
Healthpol$NAME <- NULL
Healthpol$ ST_ABBREV <-NULL
Healthpol$year <-NULL
Healthpol$state <-NULL
Healthpol$state_po <-NULL
Healthpol$county_name <-NULL
Healthpol$office <-NULL
Healthpol$candidate <-NULL
Healthpol$candidatevotes <-NULL
Healthpol$totalvotes <-NULL
Healthpol$version <- NULL
Healthpol$mode <-NULL
Healthpol$State <-NULL
############## Viewing the final dataset#############

Healthpol
head(Healthpol)
tail(Healthpol)
str(Healthpol)
summary(Healthpol)
colnames(Healthpol)
######### Running the methods###############
###### Correlation #######
typeof(Healthpol$Estimated.hesitant)
typeof(Healthpol$Obesity_Total)
typeof(Healthpol$Social.Vulnerability.Index..SVI.) #not working
as.integer(Healthpol$Social.Vulnerability.Index..SVI.) #not working
typeof(Healthpol$Social.Vulnerability.Index..SVI.)# not working
cor(Healthpol$Estimated.hesitant,Healthpol$Social.Vulnerability.Index..SVI.)
cor(Healthpol$Estimated.hesitant,
    Healthpol$Percent.adults.fully.vaccinated.against.COVID.19)
cor(Healthpol$Estimated.hesitant,Healthpol$Diabetes_Total)
cor(Healthpol$Estimated.hesitant,Healthpol$Diabetes_Percent)
cor(Healthpol$Estimated.hesitant,Healthpol$Obesity_Total)
cor(Healthpol$Estimated.hesitant,Healthpol$Obesity_Percent)
cor(Healthpol$Estimated.hesitant,Healthpol$Percent.Hispanic)
cor(Healthpol$Estimated.hesitant,Healthpol$Percent.non.Hispanic.American.Indian.Alaska.Native)
cor(Healthpol$Estimated.hesitant,Healthpol$Percent.non.Hispanic.Asian)
cor(Healthpol$Estimated.hesitant,Healthpol$Percent.non.Hispanic.Black)
cor(Healthpol$Estimated.hesitant,Healthpol$Percent.non.Hispanic.White)
cor(Healthpol$Estimated.hesitant,Healthpol$Percent.non.Hispanic.Native.Hawaiian.Pacific.Islander)
cor(Healthpol$Estimated.hesitant,Healthpol$CVAC.level.of.concern.for.vaccination.rollout)
##### cor not working with SVI and % adult fully vaccinated


##### cor for strongly hesitant ####
cor(Healthpol$Estimated.strongly.hesitant,Healthpol$Social.Vulnerability.Index..SVI.)
cor(Healthpol$Estimated.strongly.hesitant,
    Healthpol$Percent.adults.fully.vaccinated.against.COVID.19)
cor(Healthpol$Estimated.strongly.hesitant,Healthpol$Diabetes_Total)
cor(Healthpol$Estimated.strongly.hesitant,Healthpol$Diabetes_Percent)
cor(Healthpol$Estimated.strongly.hesitant,Healthpol$Obesity_Total)
cor(Healthpol$Estimated.strongly.hesitant,Healthpol$Obesity_Percent)
cor(Healthpol$Estimated.strongly.hesitant,Healthpol$Percent.Hispanic)
cor(Healthpol$Estimated.strongly.hesitant,Healthpol$Percent.non.Hispanic.American.Indian.Alaska.Native)
cor(Healthpol$Estimated.strongly.hesitant,Healthpol$Percent.non.Hispanic.Asian)
cor(Healthpol$Estimated.strongly.hesitant,Healthpol$Percent.non.Hispanic.Black)
cor(Healthpol$Estimated.strongly.hesitant,Healthpol$Percent.non.Hispanic.White)
cor(Healthpol$Estimated.strongly.hesitant,Healthpol$Percent.non.Hispanic.Native.Hawaiian.Pacific.Islander)
cor(Healthpol$Estimated.strongly.hesitant,Healthpol$CVAC.level.of.concern.for.vaccination.rollout)

###### k-means clustering######
####Creating Dataframes for all the numerical columns####
kmHealthpol <- data.frame(Healthpol$Estimated.hesitant,Healthpol$Social.Vulnerability.Index..SVI.,
                          Healthpol$Percent.Hispanic,Healthpol$Percent.non.Hispanic.American.Indian.Alaska.Native,
                          Healthpol$Percent.non.Hispanic.Asian,Healthpol$Percent.non.Hispanic.Black,
                          Healthpol$Percent.non.Hispanic.Native.Hawaiian.Pacific.Islander,Healthpol$Percent.non.Hispanic.White,
                          Healthpol$Diabetes_Total,Healthpol$Obesity_Total)

kmHealthpol.norm <- scale(kmHealthpol)
set.seed(12345)
kmHealth.cluster <- kmeans(kmHealthpol.norm, 10, nstart=10)
KmHealth.clusters$centers
str(kmHealthpol)

######### Multiple Regression#########
kmHealthpol.MLRmodel <- lm(Healthpol$Estimated.hesitant ~ Healthpol$Social.Vulnerability.Index..SVI.+
                             Healthpol$Percent.Hispanic+Healthpol$Percent.non.Hispanic.American.Indian.Alaska.Native+
                             Healthpol$Percent.non.Hispanic.Asian+Healthpol$Percent.non.Hispanic.Black+
                             Healthpol$Percent.non.Hispanic.Native.Hawaiian.Pacific.Islander+
                             Healthpol$Percent.non.Hispanic.White+
                           Healthpol$Diabetes_Total+Healthpol$Obesity_Total+
                             Healthpol$CVAC.level.of.concern.for.vaccination.rollout,
                           data=kmHealthpol)
summary(kmHealthpol.MLRmodel)

########## Adding Dummy Variables ##########
str(Healthpol)
#####Making a df with dummy variables######
kmHealthpol.dummy <- data.frame(Healthpol$Estimated.hesitant,Healthpol$Social.Vulnerability.Index..SVI.,
                          Healthpol$Percent.Hispanic,Healthpol$Percent.non.Hispanic.American.Indian.Alaska.Native,
                          Healthpol$Percent.non.Hispanic.Asian,Healthpol$Percent.non.Hispanic.Black,
                          Healthpol$Percent.non.Hispanic.Native.Hawaiian.Pacific.Islander,Healthpol$Percent.non.Hispanic.White,
                          Healthpol$Diabetes_Total,Healthpol$Obesity_Total, 
                          Healthpol$CVAC.level.of.concern.for.vaccination.rollout,
                          party=Healthpol$party)
#Applying Dummy Variable
kmHealthpol.dummy.create <- dummy_cols(kmHealthpol.dummy, select_columns='party')
kmHealthpol.dummy.create

DummyRegression <-lm(Healthpol$Estimated.hesitant ~ Healthpol$Social.Vulnerability.Index..SVI.+
                    Healthpol$Percent.Hispanic+Healthpol$Percent.non.Hispanic.American.Indian.Alaska.Native+
                    Healthpol$Percent.non.Hispanic.Asian+Healthpol$Percent.non.Hispanic.Black+
                     Healthpol$Percent.non.Hispanic.Native.Hawaiian.Pacific.Islander+Healthpol$Percent.non.Hispanic.White+
                    Healthpol$Diabetes_Total+Healthpol$Obesity_Total+
                    Healthpol$CVAC.level.of.concern.for.vaccination.rollout+
                    Healthpol$party - party - party_DEMOCRAT, data=kmHealthpol.dummy.create)

summary(DummyRegression)

########Visualizations GGPLOT2##############


