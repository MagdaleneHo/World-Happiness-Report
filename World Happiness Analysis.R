#importing library 
library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(data.table)
library(corrgram)       
library(corrplot)
library(formattable)
library(ggpubr)
library(plotly)

# Merging the three datasets through rbind
total_data = rbind(HR_2015,HR_2016)
happiness = rbind(total_data, HR_2017)

#Exporting to csv file to be used later on
write.table(happiness, "happiness.csv", sep="\t")

#Data Exploration
hap <- fread("~/Desktop/happiness1.csv", data.table = FALSE)
str(hap)
summary(hap)

# Question 1 
#(Will Asia top the score for the family factor compared to other continents?) 

#Correlation plot using corrplot for the numeric variables.
pdf(file = "corrplot.pdf", width = 8.5, height = 11)
corrplot(cor(hap %>% 
               select(Score:Dystopia)), #Selecting the numeric variables
         method="color",  #the output of plot
         sig.level = 0.01, insig = "blank",
         addCoef.col = "black", 
         tl.srt=45, 
         type="upper" 
)
dev.off()

#Scatter plot using plot_ly between family and continent 
plot_ly(data = hap, #inputing the dataset hap
        x=~Family, y=~Score, color=~Continent, type = "scatter", #choosing scatter for the plot
        text = ~paste("Country:", Country)) %>% 
  layout(title = "Happiness, Family and Continent", #labelling the graph
         xaxis = list(title = "Family"),
         yaxis = list(title = "Score"))

#Bar chart using plot_ly aggregate between family and continent
p <- plot_ly(
  type = 'bar', #choosing bar for the plot
  y = hap$Family,
  x = hap$Continent,
  transforms = list( #transform the variable for further condition
    list(
      type = 'aggregate', #aggregating the variable to get the average
      groups = hap$Continent,
      aggregations = list(
        list(
          target = 'y', func = 'avg', enabled = T #average option is choose
        )
      )
      )
    )
) %>%
layout(title = "Family and Continent", #labelling the graph
       xaxis = list(title = "Continent"),
       yaxis = list(title = "Family"))


#Question 2 
#(Are there a significant change in the ranking of the top 6 countries throughout the year 2015-2017?)

#Line graph using ggplot between top country, year and score.
pdf(file = "Country 2015-2017", width = 9, height = 8)

c1 <- hap %>% filter(Rank < 6) %>% select(Country) #filter top country and select country
c2 <- as.list(c1) #placing them in a list
viz1 <- hap %>% filter(hap$Country %in% c2$Country == 1) 

#using ggplot to plot the line graph
ggplot(viz1,aes(Year,Score,color = Country)) + 
  geom_line(aes(group = Country)) + geom_text(aes(label = Country),size = 3) + 
  ggtitle("Top 6 Countries Score (2015-2017)") +
  geom_point() + theme_bw() + 
  theme(panel.grid.major = element_blank(),plot.title = element_text(hjust = 0.5),
                       panel.grid.minor = element_blank())
dev.off()


#Question 3 
#(What are the major factors that causes the changes in the ranking of Malaysia?)
#All the below graphs are almost the same just changing the variable and layout.


#Bar graph using ggplot between Malaysia and Happiness Score
pdf(file = "Malaysia 2015-2017", width = 5, height = 6)
ggplot(hap %>% filter(Country == "Malaysia"),aes(Year,Score)) + 
  geom_bar(stat="identity", fill="pink") + geom_text(aes(label = Score),size = 5) + 
  ggtitle("Malaysia Happiness Score 2015-2017") +
  xlab("Year") + ylab("Score") + theme(panel.grid.major = element_blank(),plot.title = element_text(hjust = 0.5),
                                       panel.grid.minor = element_blank())
dev.off()

#Bar graph using ggplot between Malaysia and Economy
pdf(file = "Malaysia Economy (2015-2017)", width = 9, height = 8)
ggplot(hap %>% filter(Country == "Malaysia"),aes(Year,Economy,color=Year,fill=Year)) + 
  geom_bar(stat="identity") + geom_text(aes(label = Economy),size = 5,color="Red") + 
  ggtitle("Malaysia Economy Score 2015-2017") + 
  xlab("Year") + ylab("Economy") + theme_bw() +
  theme(panel.grid.major = element_blank(),plot.title = element_text(hjust = 0.5),
                                       panel.grid.minor = element_blank())
dev.off()

#Bar graph using ggplot between Malaysia and Family
pdf(file = "Malaysia Family (2015-2017)", width = 9, height = 8)
ggplot(hap %>% filter(Country == "Malaysia"),aes(Year,Economy,color=Year,fill=Year)) + 
  geom_bar(stat="identity") + geom_text(aes(label = Family),size = 5,color="Red") + 
  ggtitle("Malaysia Family Score 2015-2017") + theme_bw() +
  xlab("Year") + ylab("Family") + theme(panel.grid.major = element_blank(),plot.title = element_text(hjust = 0.5),
                                         panel.grid.minor = element_blank())
dev.off()

#Bar graph using ggplot between Malaysia and Health
pdf(file = "Malaysia Health (2015-2017)", width = 9, height = 8)
ggplot(hap %>% filter(Country == "Malaysia"),aes(Year,Health,color=Year,fill=Year)) + 
  geom_bar(stat="identity") + geom_text(aes(label = Health),size = 5,color="Red") + 
  ggtitle("Malaysia Health Score 2015-2017") + theme_bw() +
  xlab("Year") + ylab("Health") + theme(panel.grid.major = element_blank(),plot.title = element_text(hjust = 0.5),
                                         panel.grid.minor = element_blank())
dev.off()

#Bar graph using ggplot between Malaysia and Freedom
pdf(file = "Malaysia Freedom (2015-2017)", width = 9, height = 8)
ggplot(hap %>% filter(Country == "Malaysia"),aes(Year,Freedom,color=Year,fill=Year)) + 
  geom_bar(stat="identity") + geom_text(aes(label = Freedom),size = 5,color="Red") + 
  ggtitle("Malaysia Freedom Score 2015-2017") + theme_bw() +
  xlab("Year") + ylab("Freedom") + theme(panel.grid.major = element_blank(),plot.title = element_text(hjust = 0.5),
                                         panel.grid.minor = element_blank())
dev.off()

#Bar graph using ggplot between Malaysia and Trust in Government
pdf(file = "Malaysia Trust in Government (2015-2017)", width = 9, height = 8)
ggplot(hap %>% filter(Country == "Malaysia"),aes(Year,Trust,color=Year,fill=Year)) + 
  geom_bar(stat="identity") + geom_text(aes(label = Trust),size = 5,color="Red") + 
  ggtitle("Malaysia Trust in Government Score 2015-2017") + theme_bw() +
  xlab("Year") + ylab("Trust in Government") + theme(panel.grid.major = element_blank(),plot.title = element_text(hjust = 0.5),
                                         panel.grid.minor = element_blank())
dev.off()

#Bar graph using ggplot between Malaysia and Genorisity 
pdf(file = "Malaysia Generosity (2015-2017)", width = 9, height = 8)
ggplot(hap %>% filter(Country == "Malaysia"),aes(Year,Generosity,color=Year,fill=Year)) + 
  geom_bar(stat="identity") + geom_text(aes(label = Generosity),size = 5,color="Red") + 
  ggtitle("Malaysia Generosity Score 2015-2017") + theme_bw() +
  xlab("Year") + ylab("Generosity") + theme(panel.grid.major = element_blank(),plot.title = element_text(hjust = 0.5),
                                         panel.grid.minor = element_blank())
dev.off()



