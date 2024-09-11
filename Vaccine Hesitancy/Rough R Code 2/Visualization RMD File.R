
#Quantitative: Use a histogram

# Categorical: Use a bar chart. Or just a table of *proportions* (`table()` then `prop.table()`).


# Quantitative vs Quantitative: Use a scatterplot

# Categorical vs Quantitative: Use a boxplot

# Categorical vs Categorical: Use a mosaic plot or a count plot

#1. #Based on regions what is the proportion of people that are vaccine hesitant ?




df3 <- read.csv("df3.csv")

df3%>%
  group_by(Region)%>%
  summarise(Average_Estimated.hesitant = mean(Estimated.hesitant + Estimated.strongly.hesitant))%>%
  ggplot(aes(x = Region, y = Average_Estimated.hesitant)) + 
  geom_col(aes(fill = Region)) + theme_bw() +
  ggtitle("Mean Estimated Hesitant Rate as per Region") +
  theme(plot.title = element_text(hjust = 0.5)) 


#2. SVI Category vs estimated Hesitant


#boxplot -

#df3 %>%mutate(SVI.Category = as.factor(SVI.Category, levels = c("Very Low Concern", "Low Concern", "Moderate Concern", "High Concern", "Very High Concern")) %>%

ggplot(df3, aes(x = SVI.Category, y = Estimated.hesitant, fill = SVI.Category)) + 
  geom_boxplot() + theme_bw() +
  ggtitle("SVI Category vs Estimated.hesitant") +
  theme(plot.title = element_text(hjust = 0.5)) 

# we can add region but it seems hard to explain 


#3) Estimated Hesitancy vs Diabetes(Quant vs Quant)


ggplot(df3, aes( x = Diabetes_Percent, 
                 y = Estimated.hesitant, color = Diabetes_Percent > 10)) + geom_point() + geom_smooth(fill = NA) + expand_limits(y=0) +
  theme_bw() +
  ggtitle("Diabetes Percent vs Estimated.hesitant") +
  theme(plot.title = element_text(hjust = 0.5)) 
#Given a condition as it will help while explaining project that Diabetes Percent is directly proportiinal to Estimated Hesitancy 


#4) Estimated Hesitant based on Party


df3 %>%
  rename(Republican = "party_REPUBLICAN", 
         Democratic = "party_DEMOCRAT" ) %>%
  pivot_longer(c(Democratic, Republican), names_to = "Party", values_to = "Majority") -> df9

ggplot(df9, aes(x= Party, y = Majority, color = Party)) + 
  geom_col() + facet_wrap(~ Estimated.hesitant) +
  theme_bw() +
  ggtitle("Hesitancy rate wrt Party in Rule") +
  theme(plot.title = element_text(hjust = 0.5)) 




# 5) Estimated hesitancy based on Race


colnames(df3) 
df3 %>% 
  rename(Hispanic = "Percent.Hispanic",
         American_Indian_Alaska_Native = "Percent.non.Hispanic.American.Indian.Alaska.Native",
         Asian = "Percent.non.Hispanic.Asian",
         Black = "Percent.non.Hispanic.Black",
         Hawaiian_Pacific_Islander = "Percent.non.Hispanic.Native.Hawaiian.Pacific.Islander", 
         White = "Percent.non.Hispanic.White") %>%
  pivot_longer(c(Hispanic,American_Indian_Alaska_Native,
                 Asian, Black, Hawaiian_Pacific_Islander, White), 
               names_to = "Race",values_to = "Percent") -> df7


ggplot(df7, aes(x = Percent, y = Estimated.hesitant)) +
  geom_point(aes(color = Race)) + facet_wrap(~ Race)
theme_bw() + ggtitle("Hesitancy rate wrt Percent of people in the Race") +
  theme(plot.title = element_text(hjust = 0.5)) 

#tried geom col not getting good visualization  


