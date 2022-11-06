#We load the libraries we want----

library(readr) #in order to be able to download the data from Github
library(dplyr) #for data manipulation
library(ggplot2) #for graphics
library(plotly) #for interactive graphics
library(scales) #for graphics
library(corrplot) #in order to present more aesthetically the correlations
library(tidyverse) #for data manipulation
library(psych) #for Factor Analysis
library(GPArotation) #for the quartimax rotation
library(Gmedian) #for the kmedians method
library(Rclusterpp) #for the hierarchical clustering
library(dendextend) #for the hierarchical clustering
library(randomcoloR) #in order to have the most discrete colors
library("dgof") #for normality check
library(mctest) #for multicollinearity check
library(DFA.CANCOR) #for the homogeneity check
library(qgraph) #for the graph of the means's distances

#First of all we will create the heatmaps plots for all the continents----

#Load the data
data = read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")

#We assign the dataset as dataframe in a variable
data1 = as.data.frame(data)

#We convert the date to character because it is more convenient to handle
data1$date = as.character(data1$date)

#We create a variable in order to hold the set of countries

list_of_countries = unique(data1$location)

#We focus only on the variables: continent,location,date,total_cases,total_deaths

data1 = data1[,c("continent","location","date","total_cases","total_deaths")]

data1 = subset(data1, data1$location!="Western Sahara") #we don't want Sahara

list_of_countries = unique(data1$location) #we have to because we removed Western Sahara

#We want to find when the data of every country began to be registered

starting_date = rep(0,length(list_of_countries))

#We want to find when the data of every country finished

ending_date = rep(0,length(list_of_countries))

for (i in 1:length(list_of_countries)){
  
  k = data1$date[data1$location == list_of_countries[i]];
  
  starting_date[i] = k[1]
  
  ending_date[i] = k[length(k)]
  
}

#We don't want countries which their data begin or end in 2021.So we have to find them

#We create the list of countries which data start or end in 2021
countries_2021 = list()

#We create a dataframe witch will have the starting date and ending date of every country
df = data.frame("location" = unique(data1$location),"start_date" = starting_date
                ,"end_date" = ending_date)

for (i in 1:length(list_of_countries)) {
  
  #for every country of the dataset
  
  if ((grepl("2021",df$start_date[i],fixed=TRUE) == TRUE) |
      
      (grepl("2021",df$end_date[i],fixed=TRUE) == TRUE)){
    
    #If the starting date or the ending date contatins 2021
    #the put this country in countries_2021
    
    countries_2021 = append(countries_2021,df$location[i])
    
  }
  
}

#We want to remove the countries which belong to countries_2021

for (country in countries_2021) {
  
  data1 = subset(data1, data1$location != country)
  
}

for (country in countries_2021) {
  
  list_of_countries = list_of_countries[list_of_countries != country]
  
}

#We don't want the below countries

drop_countries = c("Africa","Asia","Europe","European Union","International",
                   "North America","Oceania","South America","High income",
                   "Low income","Lower middle income","Upper middle income",
                   "World")

#After we saw that we don't want the below countries

drop_countries = c(drop_countries,"Saint Helena","Samoa","Solomon Islands","Vanuatu","Wallis and Futuna")

for (country in drop_countries) {
  
  data1 = subset(data1, data1$location != country)
  
}


for (country in drop_countries) {
  
  list_of_countries = list_of_countries[list_of_countries != country]
  
}

#We find the starting and ending dates again

starting_date = rep(0,length(list_of_countries))


ending_date = rep(0,length(list_of_countries))

for (i in 1:length(list_of_countries)){
  
  k = data1$date[data1$location == list_of_countries[i]];
  
  starting_date[i] = k[1]
  
  ending_date[i] = k[length(k)]
  
}

#We will find the "oldest" and the "newest" date

max_starting_date = max(as.Date(starting_date))

min_ending_date = min(as.Date(ending_date))

#We want to trim the dataset so that every country has the same stating date

starting_date = as.data.frame(starting_date)

rownames(starting_date) = list_of_countries

for (country in list_of_countries) {
  
  if(starting_date[country,]!= max_starting_date){
    
    count1 = 1
    
    while (data1$location[count1]!=country) {
      
      count1 = count1 + 1
      
    }
    
    count2 = count1
    
    while (data1$date[count2]!=max_starting_date) {
      
      count2 = count2 + 1
      
    }
    
    count2 = count2 - 1
    
    rows_to_delete = count1:count2
    
    data1 = data1[-rows_to_delete,]
    
  }
  
}

#We want to trim the data so every country has the same ending date

i = 1

while (i <= length(list_of_countries)){
  
  country = list_of_countries[i]
  
  data_c = data1[data1$location == country,]
  
  if(data_c$date[length(data_c$date)] != min_ending_date){
    
    start1 = 1
    
    while ((data1$location[start1]!=country) == TRUE) {
      
      start1 = start1 + 1
      
    }
    
    #Θα βρούμε την σειρά του dataframe από την οποία τελειώνει η καταγραφή για
    #την χώρα country στο αρχικό dataset
    
    data_c = data1[data1$location == country,]
    
    ind = 1
    
    steps = 0
    
    while (as.Date(data_c$date[ind])<=as.Date(min_ending_date)) {
      
      steps = steps + 1
      
      ind = ind + 1
      
    }
    
    start1 = start1 + steps
    
    finish1  = start1
    
    while((data1$location[finish1] == country) & (is.na(data1$location[finish1]) == FALSE)){
      
      finish1 = finish1 + 1
      
    }
    
    finish1 = finish1 - 1
    
    rows_to_delete = start1:finish1
    
    data1 = data1[-rows_to_delete,]
    
  }
  
  i = i + 1
  
}

#We find the starting and ending date of every country just for check

starting_date = rep(0,length(list_of_countries))

ending_date = rep(0,length(list_of_countries))

for (i in 1:length(list_of_countries)){
  
  k = data1$date[data1$location == list_of_countries[i]];
  
  starting_date[i] = k[1]
  
  ending_date[i] = k[length(k)]
  
}

cbind(unique(data1$location),starting_date,ending_date)

#Now, we are ready for the heatmap plots

#Heatmap for Europe----

data.europe = subset(data1, data1$continent == "Europe") #with missing values

data.europe.not.na = na.omit(data.europe) #without missing values

brk = seq(from = min(data.europe.not.na$total_cases),
          to = max(data.europe.not.na$total_cases),
          length.out = 4)

p = as.data.frame(data.europe.not.na) %>%
  ggplot(aes(x = date, y = location, fill = total_cases)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Progress of total cases in European countries")

ggplotly(p)

brk = seq(from = min(data.europe.not.na$total_deaths),
          to = max(data.europe.not.na$total_deaths),
          length.out = 4)

p = as.data.frame(data.europe.not.na) %>%
  ggplot(aes(x = date, y = location, fill = total_deaths)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Progress of total deaths in European countries")

ggplotly(p)

#Heatmap for Asia----

data.asia = subset(data1, data1$continent == "Asia") #with missing values

data.asia.not.na = na.omit(data.asia) #without missing values

brk = seq(from = min(data.asia.not.na$total_cases),
          to = max(data.asia.not.na$total_cases),
          length.out = 4)

p = as.data.frame(data.asia.not.na) %>%
  ggplot(aes(x = date, y = location, fill = total_cases)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Progress of total cases in Asian countries")

ggplotly(p)

brk = seq(from = min(data.asia.not.na$total_deaths),
          to = max(data.asia.not.na$total_deaths),
          length.out = 4)

p = as.data.frame(data.asia.not.na) %>%
  ggplot(aes(x = date, y = location, fill = total_deaths)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Progress of total deaths in Asian countries")

ggplotly(p)

#Heatmap for South America

data.south.america = subset(data1, data1$continent == "South America") #with missing values

data.south.america.not.na = na.omit(data.south.america) #without missing values

brk = seq(from = min(data.south.america.not.na$total_cases),
          to = max(data.south.america.not.na$total_cases),
          length.out = 4)

p = as.data.frame(data.south.america.not.na) %>%
  ggplot(aes(x = date, y = location, fill = total_cases)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Progress of total cases in countries of South America")

ggplotly(p)

brk = seq(from = min(data.south.america.not.na$total_deaths),
          to = max(data.south.america.not.na$total_deaths),
          length.out = 4)

p = as.data.frame(data.south.america.not.na) %>%
  ggplot(aes(x = date, y = location, fill = total_deaths)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Progress of total deaths in countries of South America")

ggplotly(p)

#Heatmap for North America

data.north.america = subset(data1, data1$continent == "North America") #with missing values

data.north.america.not.na = na.omit(data.north.america) #without missing values

brk = seq(from = min(data.north.america.not.na$total_cases),
          to = max(data.north.america.not.na$total_cases),
          length.out = 4)

p = as.data.frame(data.north.america.not.na) %>%
  ggplot(aes(x = date, y = location, fill = total_cases)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Progress of total cases in countries of North America")

ggplotly(p)

brk = seq(from = min(data.north.america.not.na$total_deaths),
          to = max(data.north.america.not.na$total_deaths),
          length.out = 4)

p = as.data.frame(data.north.america.not.na) %>%
  ggplot(aes(x = date, y = location, fill = total_deaths)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Progress of total deaths in countries of North America")

ggplotly(p)

#Heatmap for Oceania

data.oceania = subset(data1, data1$continent == "Oceania") #with missing values

data.oceania.not.na = na.omit(data.oceania) #without missing values

brk = seq(from = min(data.oceania.not.na$total_cases),
          to = max(data.oceania.not.na$total_cases),
          length.out = 4)

p = as.data.frame(data.oceania.not.na) %>%
  ggplot(aes(x = date, y = location, fill = total_cases)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Progress of total cases in countries of Oceania")

ggplotly(p)

brk = seq(from = min(data.oceania.not.na$total_deaths),
          to = max(data.oceania.not.na$total_deaths),
          length.out = 4)

p = as.data.frame(data.oceania.not.na) %>%
  ggplot(aes(x = date, y = location, fill = total_deaths)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Progress of total deaths in countries of Oceania")

ggplotly(p)

#Heatmap for Africa----

data.africa = subset(data1, data1$continent == "Africa") #with missing values

data.africa.not.na = na.omit(data.africa) #without missing values

brk = seq(from = min(data.africa.not.na$total_cases),
          to = max(data.africa.not.na$total_cases),
          length.out = 4)

p = as.data.frame(data.africa.not.na) %>%
  ggplot(aes(x = date, y = location, fill = total_cases)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Progress of total cases in African countries")

ggplotly(p)

brk = seq(from = min(data.africa.not.na$total_deaths),
          to = max(data.africa.not.na$total_deaths),
          length.out = 4)

p = as.data.frame(data.africa.not.na) %>%
  ggplot(aes(x = date, y = location, fill = total_deaths)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Progress of total deaths in African countries")

ggplotly(p)

#Now we will do the same but for continents

#Heatmaps with all the continents together

data2 = as.data.frame(data)

data2$date = as.character(data2$date)

data2 = data2[,c("location","date","total_cases","total_deaths")]

data.europe = data2[data2$location == "Europe",]
data.europe = na.omit(data.europe)

data.asia = data2[data2$location == "Asia",]
data.asia = na.omit(data.asia)

data.south.america = data2[data2$location == "South America",]
data.south.america = na.omit(data.south.america)

data.north.america = data2[data2$location == "North America",]
data.north.america = na.omit(data.north.america)

data.oceania = data2[data2$location == "Oceania",]
data.oceania = na.omit(data.oceania)

data.africa = data2[data2$location == "Africa",]
data.africa = na.omit(data.africa)

data.all = rbind.data.frame(data.europe,data.asia,data.south.america,data.north.america,data.oceania,data.africa)

brk = seq(from = min(data.all$total_cases),
          to = max(data.all$total_cases),
          length.out = 4)

p = as.data.frame(data.all) %>%
  ggplot(aes(x = date, y = location, fill = total_cases)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Progress of total cases in all continents")

ggplotly(p)

brk = seq(from = min(data.all$total_deaths),
          to = max(data.all$total_deaths),
          length.out = 4)

p = as.data.frame(data.all) %>%
  ggplot(aes(x = date, y = location, fill = total_deaths)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Progress of total deaths in all continents")

ggplotly(p)

#Delete what we have done
rm(list = ls())

#We do the same preprocessing

data1 = read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")

data1 = as.data.frame(data1)

data1$date = as.character(data1$date)

list_of_countries = unique(data1$location)

starting_date = rep(0,length(list_of_countries))

ending_date = rep(0,length(list_of_countries))

for (i in 1:length(list_of_countries)){
  
  k = data1$date[data1$location == list_of_countries[i]];
  
  starting_date[i] = k[1]
  
  ending_date[i] = k[length(k)]
  
}

cbind(unique(data1$location),starting_date,ending_date)

data1 = subset(data1, data1$location!="Western Sahara")

starting_date = starting_date[-240]

ending_date = ending_date[-240]

list_of_countries = list_of_countries[list_of_countries != "Western Sahara"]

countries_2021 = list()

df = data.frame("location" = unique(data1$location),"start_date" = starting_date
                ,"end_date" = ending_date)

for (i in 1:length(list_of_countries)) {
  
  if ((grepl("2021",df$start_date[i],fixed=TRUE) == TRUE) |
      
      (grepl("2021",df$end_date[i],fixed=TRUE) == TRUE)){
    
    countries_2021 = append(countries_2021,df$location[i])
    
  }
  
}

for (country in countries_2021) {
  
  data1 = subset(data1, data1$location != country)
  
}

for (country in countries_2021) {
  
  list_of_countries = list_of_countries[list_of_countries != country]
  
}

drop_countries = c("Africa","Asia","Europe","European Union","International",
                   "North America","Oceania","South America","High income",
                   "Low income","Lower middle income","Upper middle income",
                   "World")

drop_countries = c(drop_countries,"Saint Helena","Samoa","Solomon Islands","Vanuatu","Wallis and Futuna")

for (country in drop_countries) {
  
  data1 = subset(data1, data1$location != country)
  
}

for (country in drop_countries) {
  
  list_of_countries = list_of_countries[list_of_countries != country]
  
}

starting_date = rep(0,length(list_of_countries))

ending_date = rep(0,length(list_of_countries))

for (i in 1:length(list_of_countries)){
  
  k = data1$date[data1$location == list_of_countries[i]];
  
  starting_date[i] = k[1]
  
  ending_date[i] = k[length(k)]
  
}

cbind(unique(data1$location),starting_date,ending_date)

max_starting_date = max(as.Date(starting_date))

min_ending_date = min(as.Date(ending_date))

starting_date = as.data.frame(starting_date)

rownames(starting_date) = list_of_countries

for (country in list_of_countries) {
  
  if(starting_date[country,]!= max_starting_date){
    
    count1 = 1
    
    while (data1$location[count1]!=country) {
      
      count1 = count1 + 1
      
    }
    
    count2 = count1
    
    while (data1$date[count2]!=max_starting_date) {
      
      count2 = count2 + 1
      
    }
    
    count2 = count2 - 1
    
    rows_to_delete = count1:count2
    
    data1 = data1[-rows_to_delete,]
    
  }
  
}

i = 1

while (i <= length(list_of_countries)){
  
  country = list_of_countries[i]
  
  data_c = data1[data1$location == country,]
  
  if(data_c$date[length(data_c$date)] != min_ending_date){
    
    start1 = 1
    
    while ((data1$location[start1]!=country) == TRUE) {
      
      start1 = start1 + 1
      
    }
    
    data_c = data1[data1$location == country,]
    
    ind = 1
    
    steps = 0
    
    while (as.Date(data_c$date[ind])<=as.Date(min_ending_date)) {
      
      steps = steps + 1
      
      ind = ind + 1
      
    }
    
    start1 = start1 + steps
    
    finish1  = start1
    
    while((data1$location[finish1] == country) & (is.na(data1$location[finish1]) == FALSE)){
      
      finish1 = finish1 + 1
      
    }
    
    finish1 = finish1 - 1
    
    rows_to_delete = start1:finish1
    
    data1 = data1[-rows_to_delete,]
    
  }
  
  i = i + 1
  
}

starting_date = rep(0,length(list_of_countries))

ending_date = rep(0,length(list_of_countries))

for (i in 1:length(list_of_countries)){
  
  k = data1$date[data1$location == list_of_countries[i]];
  
  starting_date[i] = k[1]
  
  ending_date[i] = k[length(k)]
  
}

cbind(unique(data1$location),starting_date,ending_date)

#Calculation of percentage of missing value in every variable----

sort((colMeans(is.na(data1)))*100)

#So we will keep the below variables

final_dataset = data1[,c("iso_code","continent","location","date","population"
                         ,"life_expectancy","total_cases","new_cases",
                         "population_density","diabetes_prevalence","total_deaths",
                         "new_deaths","median_age","aged_70_older",
                         "cardiovasc_death_rate","gdp_per_capita",
                         "human_development_index","reproduction_rate",
                         "stringency_index","hospital_beds_per_thousand")]

#Clean our data from missing data----

final_dataset = na.omit(final_dataset)

list_of_countries = unique(final_dataset$location)

#Correlations----

M = cor(final_dataset[,-(1:4)])

X11(width=55, height=35) #in order to be printed more nicely, after that line it is as it was without it

corrplot(M, method = 'number',number.cex = 0.7,type = "lower",addCoef.col = 1) # colorful number

#PCA----

n = length(final_dataset[,1]) #number of observations

p = length(final_dataset[1,]) - 4 #we don't consider iso_code,continent,location,date as variables

data.pca = prcomp(final_dataset[,-(1:4)], scale. = TRUE) #we apply pca to the scaled data

#Note that eigenvectors in R point in the negative direction by default, so we’ll 
#multiply by -1 to reverse the signs.

data.pca$rotation = -1*data.pca$rotation

#Note that the principal components scores for each state are stored in results$x.
#We will also multiply these scores by -1 to reverse the signs:

data.pca$x = -1*data.pca$x

#We want to calculate the percentage of variance which every component explains

percentages_of_var = ((data.pca$sdev^2) / sum(data.pca$sdev^2))*100

cum_percentages_of_var = ((cumsum(data.pca$sdev^2)/ sum(data.pca$sdev^2)))*100

qplot(c(1:p),data.pca$sdev^2) + geom_line() + xlab("Component Number") +
  ylab("Eigenvalue") + ggtitle("Scree Plot") + ylim(0,max(data.pca$sdev^2)+1)+
  geom_hline(yintercept = 1,linetype = "dashed",col = "red")

qplot(c(1:p), percentages_of_var) + geom_line() + xlab("Component Number") + 
  ylab("Variance Explained(%)") + ggtitle("Percentage of Variance explained by every component") +
  ylim(0, 35)

qplot(c(1:p),cum_percentages_of_var) + geom_line() + xlab ("Principal Component")+
  ylab("Cumulative Variance Explained(%)") + ggtitle("Cumulative percentage of Variance explained by components") + ylim(0,100)

#Correlations between the pcs and the variables
cor.variables.pcs = cor(final_dataset[,-(1:4)],data.pca$x,method = c("pearson"))

#Quality of Representation

k = 5

f = matrix(0, nrow = n, ncol = k)

for (i in 1:k) {
  
  f[,i] = (scale(final_dataset[,-(1:4)],scale = TRUE)) %*% (data.pca$rotation[,i])
  
}

f_star = rowSums(f^2)

magnitude_of_vectors_sq = rowSums((scale(final_dataset[,-(1:4)],scale = TRUE))^2)

q.x_i = (f_star/magnitude_of_vectors_sq)*100

#Histogram o qualities of representation

ggplot() + geom_histogram(aes(q.x_i), bins = 50, col = "black",fill = "cadetblue1") + xlab("Quality of Representation(%)") + 
  ylab("Counts") + ggtitle("Histogram of Quality of Representation")

#Scores in PCA----

scores.all.countries = cbind(final_dataset[,c(2,3,4)],data.pca$x)

#Heatmaps in the first component----

#Europe

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "Europe",])$PC1),
          to = max((scores.all.countries[scores.all.countries$continent == "Europe",])$PC1),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'Europe') %>%
  ggplot(aes(x = date, y = location, fill = PC1)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of European countries in the first PC")

ggplotly(p)

#Asia

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "Asia",])$PC1),
          to = max((scores.all.countries[scores.all.countries$continent == "Asia",])$PC1),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'Asia') %>%
  ggplot(aes(x = date, y = location, fill = PC1)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of Asian countries in the first PC")

ggplotly(p)

#South America

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "South America",])$PC1),
          to = max((scores.all.countries[scores.all.countries$continent == "South America",])$PC1),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'South America') %>%
  ggplot(aes(x = date, y = location, fill = PC1)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries of South America in the first PC")

ggplotly(p)

#North America

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "North America",])$PC1),
          to = max((scores.all.countries[scores.all.countries$continent == "North America",])$PC1),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'North America') %>%
  ggplot(aes(x = date, y = location, fill = PC1)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries of North America in the first PC")

ggplotly(p)

#Oceania

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "Oceania",])$PC1),
          to = max((scores.all.countries[scores.all.countries$continent == "Oceania",])$PC1),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'Oceania') %>%
  ggplot(aes(x = date, y = location, fill = PC1)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries of Oceania in the first PC")

ggplotly(p)

#Africa

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "Africa",])$PC1),
          to = max((scores.all.countries[scores.all.countries$continent == "Africa",])$PC1),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'Africa') %>%
  ggplot(aes(x = date, y = location, fill = PC1)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of African countries in the first PC")

ggplotly(p)

#Heatmaps in the second component----

#Europe

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "Europe",])$PC2),
          to = max((scores.all.countries[scores.all.countries$continent == "Europe",])$PC2),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'Europe') %>%
  ggplot(aes(x = date, y = location, fill = PC2)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of European countries in the second PC")

ggplotly(p)

#Asia

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "Asia",])$PC2),
          to = max((scores.all.countries[scores.all.countries$continent == "Asia",])$PC2),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'Asia') %>%
  ggplot(aes(x = date, y = location, fill = PC2)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of Asian countries in the second PC")

ggplotly(p)

#South America

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "South America",])$PC2),
          to = max((scores.all.countries[scores.all.countries$continent == "South America",])$PC2),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'South America') %>%
  ggplot(aes(x = date, y = location, fill = PC2)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in South America in the second PC")

ggplotly(p)

#North America

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "North America",])$PC2),
          to = max((scores.all.countries[scores.all.countries$continent == "North America",])$PC2),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'North America') %>%
  ggplot(aes(x = date, y = location, fill = PC2)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in North America in the second PC")

ggplotly(p)

#Oceania

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "Oceania",])$PC2),
          to = max((scores.all.countries[scores.all.countries$continent == "Oceania",])$PC2),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'Oceania') %>%
  ggplot(aes(x = date, y = location, fill = PC2)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in Oceania in the second PC")

ggplotly(p)

#Africa

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "Africa",])$PC2),
          to = max((scores.all.countries[scores.all.countries$continent == "Africa",])$PC2),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'Africa') %>%
  ggplot(aes(x = date, y = location, fill = PC2)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of African countries in the second PC")

ggplotly(p)

#Heatmaps in the third component----

#Europe

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "Europe",])$PC3),
          to = max((scores.all.countries[scores.all.countries$continent == "Europe",])$PC3),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'Europe') %>%
  ggplot(aes(x = date, y = location, fill = PC3)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of European countries in the third PC")

ggplotly(p)

#Asia

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "Asia",])$PC3),
          to = max((scores.all.countries[scores.all.countries$continent == "Asia",])$PC3),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'Asia') %>%
  ggplot(aes(x = date, y = location, fill = PC3)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of Asian countries in the third PC")

ggplotly(p)

#South America

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "South America",])$PC3),
          to = max((scores.all.countries[scores.all.countries$continent == "South America",])$PC3),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'South America') %>%
  ggplot(aes(x = date, y = location, fill = PC3)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in South America in the third PC")

ggplotly(p)

#North America

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "North America",])$PC3),
          to = max((scores.all.countries[scores.all.countries$continent == "North America",])$PC3),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'North America') %>%
  ggplot(aes(x = date, y = location, fill = PC3)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in North America in the third PC")

ggplotly(p)

#Oceania

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "Oceania",])$PC3),
          to = max((scores.all.countries[scores.all.countries$continent == "Oceania",])$PC3),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'Oceania') %>%
  ggplot(aes(x = date, y = location, fill = PC3)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in Oceania in the third PC")

ggplotly(p)

#Africa

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "Africa",])$PC3),
          to = max((scores.all.countries[scores.all.countries$continent == "Africa",])$PC3),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'Africa') %>%
  ggplot(aes(x = date, y = location, fill = PC3)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of African countries in the third PC")

ggplotly(p)

#Heatmaps in the fourth component----

#Europe

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "Europe",])$PC4),
          to = max((scores.all.countries[scores.all.countries$continent == "Europe",])$PC4),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'Europe') %>%
  ggplot(aes(x = date, y = location, fill = PC4)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of European countries in the fourth PC")

ggplotly(p)

#Asia

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "Asia",])$PC4),
          to = max((scores.all.countries[scores.all.countries$continent == "Asia",])$PC4),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'Asia') %>%
  ggplot(aes(x = date, y = location, fill = PC4)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of Asian countries in the fourth PC")

ggplotly(p)

#South America

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "South America",])$PC4),
          to = max((scores.all.countries[scores.all.countries$continent == "South America",])$PC4),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'South America') %>%
  ggplot(aes(x = date, y = location, fill = PC4)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in South America in the fourth PC")

ggplotly(p)

#North America

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "North America",])$PC4),
          to = max((scores.all.countries[scores.all.countries$continent == "North America",])$PC4),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'North America') %>%
  ggplot(aes(x = date, y = location, fill = PC4)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in North America in the fourth PC")

ggplotly(p)

#Oceania

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "Oceania",])$PC4),
          to = max((scores.all.countries[scores.all.countries$continent == "Oceania",])$PC4),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'Oceania') %>%
  ggplot(aes(x = date, y = location, fill = PC4)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in Oceania in the fourth PC")

ggplotly(p)

#Africa

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "Africa",])$PC4),
          to = max((scores.all.countries[scores.all.countries$continent == "Africa",])$PC4),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'Africa') %>%
  ggplot(aes(x = date, y = location, fill = PC4)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of African countries in the fourth PC")

ggplotly(p)

#Heatmaps in the fifth component----

#Europe

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "Europe",])$PC5),
          to = max((scores.all.countries[scores.all.countries$continent == "Europe",])$PC5),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'Europe') %>%
  ggplot(aes(x = date, y = location, fill = PC5)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of European countries in the fifth PC")

ggplotly(p)

#Asia

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "Asia",])$PC5),
          to = max((scores.all.countries[scores.all.countries$continent == "Asia",])$PC5),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'Asia') %>%
  ggplot(aes(x = date, y = location, fill = PC5)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of Asian countries in the fifth PC")

ggplotly(p)

#South America

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "South America",])$PC5),
          to = max((scores.all.countries[scores.all.countries$continent == "South America",])$PC5),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'South America') %>%
  ggplot(aes(x = date, y = location, fill = PC5)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in South America in the fifth PC")

ggplotly(p)

#North America

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "North America",])$PC5),
          to = max((scores.all.countries[scores.all.countries$continent == "North America",])$PC5),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'North America') %>%
  ggplot(aes(x = date, y = location, fill = PC5)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in North America in the fifth PC")

ggplotly(p)

#Oceania

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "Oceania",])$PC5),
          to = max((scores.all.countries[scores.all.countries$continent == "Oceania",])$PC5),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'Oceania') %>%
  ggplot(aes(x = date, y = location, fill = PC5)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in Oceania in the fifth PC")

ggplotly(p)

#Africa

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "Africa",])$PC5),
          to = max((scores.all.countries[scores.all.countries$continent == "Africa",])$PC5),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'Africa') %>%
  ggplot(aes(x = date, y = location, fill = PC5)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of African countries in the fifth PC")

ggplotly(p)

#Factor Analysis----

#Calculation of the KMO

KMO(final_dataset[,-(1:4)])

#Factor Analysis using PCA for the estimation of L----

data.fa.pca = fa(r = cor(final_dataset[,-(1:4)]), nfactors = 5, rotate = "none",fm = "pa")

#Factor Analysis using MLE for the estimation of L----

data.fa.mle = fa(r = cor(final_dataset[,-(1:4)]), nfactors = 5, rotate = "none",fm = "ml")

#PCA vs MLE----

sum(data.fa.pca$residual<=0.05)

sum(data.fa.mle$residual<=0.05)

#Varimax rotation----

data.fa.pca.varimax = fa(r = cor(final_dataset[,-(1:4)]), nfactors = 5, rotate = "varimax",fm = "pa")

#Quartimax rotation----

data.fa.pca.quartimax = fa(r = cor(final_dataset[,-(1:4)]), nfactors = 5, rotate = "quartimax",fm = "pa")

#Equamax rotation

data.fa.pca.equamax = fa(r = cor(final_dataset[,-(1:4)]), nfactors = 5, rotate = "equamax",fm = "pa")

#Barlett scores in Factor Analysis using quartimax rotation----

barlett.scores = factor.scores(x = final_dataset[,-(1:4)], f=data.fa.pca.quartimax,method = "Bartlett")$scores #στο x=Dataframe or matrix of raw data (needed to get factor scores) or matrix with correlations.

bar.all.scores = cbind(final_dataset[,c(2,3,4)],barlett.scores)

#Heatmaps in the first factor----

#Europe

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "Europe",])$PA1),
          to = max((bar.all.scores[bar.all.scores$continent == "Europe",])$PA1),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'Europe') %>%
  ggplot(aes(x = date, y = location, fill = PA1)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of European countries in the first factor")

ggplotly(p)

#Asia

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "Asia",])$PA1),
          to = max((bar.all.scores[bar.all.scores$continent == "Asia",])$PA1),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'Asia') %>%
  ggplot(aes(x = date, y = location, fill = PA1)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of Asian countries in the first factor")

ggplotly(p)

#South America

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "South America",])$PA1),
          to = max((bar.all.scores[bar.all.scores$continent == "South America",])$PA1),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'South America') %>%
  ggplot(aes(x = date, y = location, fill = PA1)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in South America in the first factor")

ggplotly(p)

#North America

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "North America",])$PA1),
          to = max((bar.all.scores[bar.all.scores$continent == "North America",])$PA1),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'North America') %>%
  ggplot(aes(x = date, y = location, fill = PA1)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in North America in the first factor")

ggplotly(p)

#Oceania

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "Oceania",])$PA1),
          to = max((bar.all.scores[bar.all.scores$continent == "Oceania",])$PA1),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'Oceania') %>%
  ggplot(aes(x = date, y = location, fill = PA1)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in Oceania in the first factor")

ggplotly(p)

#Africa

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "Africa",])$PA1),
          to = max((bar.all.scores[bar.all.scores$continent == "Africa",])$PA1),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'Africa') %>%
  ggplot(aes(x = date, y = location, fill = PA1)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of African countries in the first factor")

ggplotly(p)

#Heatmaps in the second factor----

#Europe

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "Europe",])$PA2),
          to = max((bar.all.scores[bar.all.scores$continent == "Europe",])$PA2),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'Europe') %>%
  ggplot(aes(x = date, y = location, fill = PA2)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of European countries in the second factor")

ggplotly(p)

#Asia

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "Asia",])$PA2),
          to = max((bar.all.scores[bar.all.scores$continent == "Asia",])$PA2),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'Asia') %>%
  ggplot(aes(x = date, y = location, fill = PA2)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of Asian countries in the second factor")

ggplotly(p)

#South America

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "South America",])$PA2),
          to = max((bar.all.scores[bar.all.scores$continent == "South America",])$PA2),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'South America') %>%
  ggplot(aes(x = date, y = location, fill = PA2)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in South America in the second factor")

ggplotly(p)

#North America

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "North America",])$PA2),
          to = max((bar.all.scores[bar.all.scores$continent == "North America",])$PA2),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'North America') %>%
  ggplot(aes(x = date, y = location, fill = PA2)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in North America in the second factor")

ggplotly(p)

#Oceania

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "Oceania",])$PA2),
          to = max((bar.all.scores[bar.all.scores$continent == "Oceania",])$PA2),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'Oceania') %>%
  ggplot(aes(x = date, y = location, fill = PA2)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in Oceania in the second factor")

ggplotly(p)

#Africa

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "Africa",])$PA2),
          to = max((bar.all.scores[bar.all.scores$continent == "Africa",])$PA2),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'Africa') %>%
  ggplot(aes(x = date, y = location, fill = PA2)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of African countries in the second factor")

ggplotly(p)

#Heatmaps in the third factor----

#Europe

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "Europe",])$PA3),
          to = max((bar.all.scores[bar.all.scores$continent == "Europe",])$PA3),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'Europe') %>%
  ggplot(aes(x = date, y = location, fill = PA3)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of European countries in the third factor")

ggplotly(p)

#Asia

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "Asia",])$PA3),
          to = max((bar.all.scores[bar.all.scores$continent == "Asia",])$PA3),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'Asia') %>%
  ggplot(aes(x = date, y = location, fill = PA3)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of Asian countries in the third factor")

ggplotly(p)

#South America

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "South America",])$PA3),
          to = max((bar.all.scores[bar.all.scores$continent == "South America",])$PA3),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'South America') %>%
  ggplot(aes(x = date, y = location, fill = PA3)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in South America in the third factor")

ggplotly(p)

#North America

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "North America",])$PA3),
          to = max((bar.all.scores[bar.all.scores$continent == "North America",])$PA3),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'North America') %>%
  ggplot(aes(x = date, y = location, fill = PA3)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in North America in the third factor")

ggplotly(p)

#Oceania

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "Oceania",])$PA3),
          to = max((bar.all.scores[bar.all.scores$continent == "Oceania",])$PA3),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'Oceania') %>%
  ggplot(aes(x = date, y = location, fill = PA3)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in Oceania in the third factor")

ggplotly(p)

#Africa

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "Africa",])$PA3),
          to = max((bar.all.scores[bar.all.scores$continent == "Africa",])$PA3),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'Africa') %>%
  ggplot(aes(x = date, y = location, fill = PA3)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of African countries in the third factor")

ggplotly(p)

#Heatmaps in the fourth factor----

#Europe

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "Europe",])$PA4),
          to = max((bar.all.scores[bar.all.scores$continent == "Europe",])$PA4),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'Europe') %>%
  ggplot(aes(x = date, y = location, fill = PA4)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of European countries in the fourth factor")

ggplotly(p)

#Asia

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "Asia",])$PA4),
          to = max((bar.all.scores[bar.all.scores$continent == "Asia",])$PA4),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'Asia') %>%
  ggplot(aes(x = date, y = location, fill = PA4)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of Asian countries in the fourth factor")

ggplotly(p)

#South America

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "South America",])$PA4),
          to = max((bar.all.scores[bar.all.scores$continent == "South America",])$PA4),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'South America') %>%
  ggplot(aes(x = date, y = location, fill = PA4)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in South America in the fourth factor")

ggplotly(p)

#North America

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "North America",])$PA4),
          to = max((bar.all.scores[bar.all.scores$continent == "North America",])$PA4),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'North America') %>%
  ggplot(aes(x = date, y = location, fill = PA4)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in North America in the fourth factor")

ggplotly(p)

#Oceania

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "Oceania",])$PA4),
          to = max((bar.all.scores[bar.all.scores$continent == "Oceania",])$PA4),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'Oceania') %>%
  ggplot(aes(x = date, y = location, fill = PA4)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in Oceania in the fourth factor")

ggplotly(p)

#Africa

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "Africa",])$PA4),
          to = max((bar.all.scores[bar.all.scores$continent == "Africa",])$PA4),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'Africa') %>%
  ggplot(aes(x = date, y = location, fill = PA4)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of African countries in the fourth factor")

ggplotly(p)


#Heatmaps in the fifth factor----

#Europe

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "Europe",])$PA5),
          to = max((bar.all.scores[bar.all.scores$continent == "Europe",])$PA5),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'Europe') %>%
  ggplot(aes(x = date, y = location, fill = PA5)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of European countries in the fifth factor")

ggplotly(p)

#Asia

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "Asia",])$PA5),
          to = max((bar.all.scores[bar.all.scores$continent == "Asia",])$PA5),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'Asia') %>%
  ggplot(aes(x = date, y = location, fill = PA5)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of Asian countries in the fifth factor")

ggplotly(p)

#South America

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "South America",])$PA5),
          to = max((bar.all.scores[bar.all.scores$continent == "South America",])$PA5),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'South America') %>%
  ggplot(aes(x = date, y = location, fill = PA5)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in South America in the fifth factor")

ggplotly(p)

#North America

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "North America",])$PA5),
          to = max((bar.all.scores[bar.all.scores$continent == "North America",])$PA5),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'North America') %>%
  ggplot(aes(x = date, y = location, fill = PA5)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in North America in the fifth factor")

ggplotly(p)

#Oceania

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "Oceania",])$PA5),
          to = max((bar.all.scores[bar.all.scores$continent == "Oceania",])$PA5),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'Oceania') %>%
  ggplot(aes(x = date, y = location, fill = PA5)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in Oceania in the fifth factor")

ggplotly(p)

#Africa

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "Africa",])$PA5),
          to = max((bar.all.scores[bar.all.scores$continent == "Africa",])$PA5),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'Africa') %>%
  ggplot(aes(x = date, y = location, fill = PA5)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of African countries in the fifth factor")

ggplotly(p)

#Cluster Analysis----

data.scaled = scale(final_dataset[,-(1:4)], scale = TRUE)

data.scaled = as.data.frame(data.scaled)

#k-medians----

no.clusters = 145

data.nonh = kmeans(data.scaled, centers = no.clusters, algorithm = "MacQueen",iter.max = 100)

#We want to calculate the accuracy of this method.So we will create a vector which indicates
#in which country every observation truly belongs to

group_1 = rep(0,nrow(final_dataset))

for(i in 1:length(list_of_countries)){
  
  country = list_of_countries[i]
  
  start = 1
  
  while (final_dataset$location[start]!= country) {
    
    start = start + 1
  }
  
  while (final_dataset$location[start] == country & start<=nrow(final_dataset)) {
    
    group_1[start] = i
    
    start = start + 1
    
  }
}

#So the accuracy is:

((sum(group_1 == data.nonh$cluster))/(nrow(final_dataset)))*100

data.scaled$cluster = as.character(data.nonh$cluster)

X11(width=55, height=35)

ggplot() + geom_point(data.scaled, mapping = aes(x = population, y = life_expectancy, 
                                                 colour = cluster)) + 
  geom_point(mapping = aes_string(x = data.nonh$centers[, "population"], 
                                  y = data.nonh$centers[, "life_expectancy"]), 
             color = "red", size = 4) +
  ggtitle("Graph of population and life_expectancy") +
  geom_text(mapping = aes_string(x = data.nonh$centers[, "population"],
                                 y = data.nonh$centers[, "life_expectancy"],
                                 label = 1:no.clusters),color = "black",
            size = 4) +
  theme_light()

locations.kmeans = cbind.data.frame(final_dataset$location,data.scaled)

colnames(locations.kmeans)[1] = "location"

unique(locations.kmeans[locations.kmeans$cluster == "97",]$location)

sum(locations.kmeans[locations.kmeans$cluster == "97",]$location == "China")

sum(locations.kmeans[locations.kmeans$cluster == "97",]$location == "India")

length(final_dataset[final_dataset$location == "China",1])

length(final_dataset[final_dataset$location == "India",1])

X11(width=55, height=35)

ggplot() + geom_point(data.scaled, mapping = aes(x = new_cases, y = new_deaths, 
                                                 colour = cluster)) + 
  geom_point(mapping = aes_string(x = data.nonh$centers[, "new_cases"], 
                                  y = data.nonh$centers[, "new_deaths"]), 
             color = "red", size = 4) +
  ggtitle("Graph of new cases and new deaths") +
  geom_text(mapping = aes_string(x = data.nonh$centers[, "new_cases"],
                                 y = data.nonh$centers[, "new_deaths"],
                                 label = 1:no.clusters),color = "black",
            size = 4) +
  theme_light()

unique(locations.kmeans[locations.kmeans$cluster == "38",]$location)

unique(locations.kmeans[locations.kmeans$cluster == "140",]$location)

X11(width=55, height=35)

ggplot() + geom_point(data.scaled, mapping = aes(x = total_cases, y = total_deaths, 
                                                 colour = cluster)) + 
  geom_point(mapping = aes_string(x = data.nonh$centers[, "total_cases"], 
                                  y = data.nonh$centers[, "total_deaths"]), 
             color = "red", size = 4) +
  ggtitle("Graph of total cases and total deaths") +
  geom_text(mapping = aes_string(x = data.nonh$centers[, "total_cases"],
                                 y = data.nonh$centers[, "total_deaths"],
                                 label = 1:no.clusters),color = "black",
            size = 4) +
  theme_light()

unique(locations.kmeans[locations.kmeans$cluster == "139",]$location)

unique(locations.kmeans[locations.kmeans$cluster == "140",]$location)

unique(locations.kmeans[locations.kmeans$cluster == "90",]$location)

X11(width=55, height=35)

ggplot() + geom_point(data.scaled, mapping = aes(x = stringency_index, y = new_cases, 
                                                 colour = cluster)) + 
  geom_point(mapping = aes_string(x = data.nonh$centers[, "stringency_index"], 
                                  y = data.nonh$centers[, "new_cases"]), 
             color = "red", size = 4) +
  ggtitle("Graph of stringency index and new cases") +
  geom_text(mapping = aes_string(x = data.nonh$centers[, "stringency_index"],
                                 y = data.nonh$centers[, "new_cases"],
                                 label = 1:no.clusters),color = "black",
            size = 4) +
  theme_light()

unique(locations.kmeans[locations.kmeans$cluster == "95",]$location)

unique(locations.kmeans[locations.kmeans$cluster == "116",]$location)

#Problem with outliers----

g = ggplot(data = final_dataset,aes(x = final_dataset$location
                                    ,y = final_dataset$total_cases)) +
  geom_boxplot()

g + labs(x = "Country",y = "Total Cases")

#Boxplot Location~New cases
g = ggplot(data = final_dataset,aes(x = final_dataset$location
                                    ,y = final_dataset$new_cases)) +
  geom_boxplot()

g + labs(x = "Country",y = "New Cases")

#Boxplot Location~Total deaths
g = ggplot(data = final_dataset,aes(x = final_dataset$location
                                    ,y = final_dataset$total_deaths)) +
  geom_boxplot()

g + labs(x = "Country",y = "Total Deaths")

#Boxplot Location~New deaths
g = ggplot(data = final_dataset,aes(x = final_dataset$location
                                    ,y = final_dataset$new_deaths)) +
  geom_boxplot()

g + labs(x = "Country",y = "New Deaths")

#Boxplot Location~Reproduction rate
g = ggplot(data = final_dataset,aes(x = final_dataset$location
                                    ,y = final_dataset$reproduction_rate)) +
  geom_boxplot()

g + labs(x = "Country",y = "Reproduction Rate")

#Boxplot Location~Stringency Index
g = ggplot(data = final_dataset,aes(x = final_dataset$location
                                    ,y = final_dataset$stringency_index)) +
  geom_boxplot()

g + labs(x = "Country",y = "Stringency Index")

#k-medians----

data.scaled = scale(final_dataset[,-(1:4)], scale = TRUE)
data.scaled = as.data.frame(data.scaled)

data.kmedians = kGmedian(X = data.scaled,ncenters=145,iter.max = 100)

#We calculate the accuracy of the method

((sum(group_1 == data.kmedians$cluster))/(nrow(final_dataset)))*100

data.scaled$cluster = as.character(data.kmedians$cluster)

locations_and_data.scaled = cbind(final_dataset$continent,final_dataset$location,data.scaled)
colnames(locations_and_data.scaled)[1] = "Continent"
colnames(locations_and_data.scaled)[2] = "Location"

# new_cases~new_deaths

ggplot() + geom_point(data.scaled, mapping = aes(x = new_cases, y = new_deaths, 
                                                 colour = cluster)) + 
  geom_point(mapping = aes_string(x = data.kmedians$centers[, "new_cases"], 
                                  y = data.kmedians$centers[, "new_deaths"]), 
             color = "red", size = 4) +
  geom_text(mapping = aes_string(x = data.kmedians$centers[, "new_cases"],
                                 y = data.kmedians$centers[, "new_deaths"],
                                 label = 1:no.clusters),color = "black",
            size = 4) +
  
  ggtitle("Graph for new cases and new deaths") +
  
  theme_light()

unique(locations_and_data.scaled[locations_and_data.scaled$cluster == "11","Location"])

unique(locations_and_data.scaled[locations_and_data.scaled$cluster == "71","Location"])

unique(locations_and_data.scaled[locations_and_data.scaled$cluster == "27","Location"])

#stringency_index~new_cases

ggplot() + geom_point(data.scaled, mapping = aes(x = stringency_index, y = new_cases, 
                                                 colour = cluster)) + 
  geom_point(mapping = aes_string(x = data.kmedians$centers[, "stringency_index"], 
                                  y = data.kmedians$centers[, "new_cases"]), 
             color = "red", size = 4) +
  geom_text(mapping = aes_string(x = data.kmedians$centers[, "stringency_index"],
                                 y = data.kmedians$centers[, "new_cases"],
                                 label = 1:no.clusters),color = "black",
            size = 4) +
  
  ggtitle("Graph for stringency index and new cases") +
  
  theme_light()

unique(locations_and_data.scaled[locations_and_data.scaled$cluster == "2","Location"])

unique(locations_and_data.scaled[locations_and_data.scaled$cluster == "71","Location"])

unique(locations_and_data.scaled[locations_and_data.scaled$cluster == "11","Location"])

#hospital_beds_per_thousand~new_deaths

ggplot() + geom_point(data.scaled, mapping = aes(x = hospital_beds_per_thousand, y = new_deaths, 
                                                 colour = cluster)) + 
  geom_point(mapping = aes_string(x = data.kmedians$centers[, "hospital_beds_per_thousand"], 
                                  y = data.kmedians$centers[, "new_deaths"]), 
             color = "red", size = 4) +
  geom_text(mapping = aes_string(x = data.kmedians$centers[, "hospital_beds_per_thousand"],
                                 y = data.kmedians$centers[, "new_deaths"],
                                 label = 1:no.clusters),color = "black",
            size = 4) +
  
  ggtitle("Graph for hospital beds per thousand and new deaths") +
  
  theme_light()

#reproduction_rate~new_cases

ggplot() + geom_point(data.scaled, mapping = aes(x = reproduction_rate, y = new_cases, 
                                                 colour = cluster)) + 
  geom_point(mapping = aes_string(x = data.kmedians$centers[, "reproduction_rate"], 
                                  y = data.kmedians$centers[, "new_cases"]), 
             color = "red", size = 4) +
  geom_text(mapping = aes_string(x = data.kmedians$centers[, "reproduction_rate"],
                                 y = data.kmedians$centers[, "new_cases"],
                                 label = 1:no.clusters),color = "black",
            size = 4) +
  
  ggtitle("Graph for reproduction rate and new cases") +
  
  theme_light()


#Hierarchical Clustering----

data.scaled = scale(final_dataset[,-(1:4)], scale = TRUE)
data.scaled = as.data.frame(data.scaled)

#Single Linkage

data.clust.single = Rclusterpp.hclust(x = data.scaled,method = "single",distance = "euclidean")

#Dendrogram of Single Linkage

plot(data.clust.single, labels = FALSE)

#We believe that we should cut the dendrogram in 3.3

abline(h = 3.3,col = "red")

#Let's see how many groups exist in this height

length(unique(cutree(as.dendrogram(data.clust.single),h = 3.3)))

# load code of A2R function
source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")

op = par(bg = "#EFEFEF")

A2Rplot(data.clust.single, k = 30, boxes = FALSE, col.up = "gray50", main = "Dendrogram for method Single Linkage",
        show.labels = FALSE,col.down = distinctColorPalette(30))

par(op)

#Let's find in which group every observation belongs to

families.single = cutree(data.clust.single, k = 30)[data.clust.single$order]

#Let's see how many observations exist in every group

table(families.single)

#Complete Linkage

data.clust.complete = Rclusterpp.hclust(x = data.scaled,method = "complete",distance = "euclidean")

#Dendrogram of Complete Linkage

plot(data.clust.complete,labels = FALSE)

#We believe that we should cut the dendrogram in 23

abline(h = 23,col = "red")

#Let's see how many groups exist in this height

length(unique(cutree(as.dendrogram(data.clust.complete),h = 23)))

# load code of A2R function
source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")

op = par(bg = "#EFEFEF")

A2Rplot(data.clust.complete, k = 9, boxes = FALSE, col.up = "gray50", main = "Dendrogram for method Complete Linkage",
        show.labels = FALSE, col.down = distinctColorPalette(9))

par(op)

#Let's find in which group every observation belongs to

families.complete = cutree(data.clust.complete, k = 9)[data.clust.complete$order]

#Let's see how many observations exist in every group

table(families.complete)

#Ward

data.clust.ward = Rclusterpp.hclust(x=data.scaled,method="ward",distance="euclidean")

#Dendrogram of Ward

plot(data.clust.ward,labels = FALSE)

#We believe that we should cut the dendrogram in 96000

abline(h = 96000,col = "red")

#Let's see how many groups exist in this height

length(unique(cutree(as.dendrogram(data.clust.ward),h = 96000)))

# load code of A2R function
source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")

op = par(bg = "#EFEFEF")

A2Rplot(data.clust.ward, k = 5, boxes = FALSE, col.up = "gray50", main = "Dendrogram for method Ward",
        show.labels = FALSE, col.down = distinctColorPalette(5))

par(op)

families.ward = cutree(data.clust.ward, k = 5)[data.clust.ward$order]

#Let's find in which group every observation belongs to

table(families.ward)

#Weighted Average Linkage

data.clust.weighted.average.linkage = Rclusterpp.hclust(x=data.scaled,method="average",distance="euclidean")

#Dendrogram of Weighted Average Linkage

plot(data.clust.weighted.average.linkage,labels = FALSE)

#We believe that we should cut the dendrogram in 13

abline(h = 13,col = "red")

#Let's see how many groups exist in this height

length(unique(cutree(as.dendrogram(data.clust.weighted.average.linkage),h = 13)))

# load code of A2R function
source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")

op = par(bg = "#EFEFEF")

A2Rplot(data.clust.weighted.average.linkage, k = 10, boxes = FALSE, col.up = "gray50", main = "Dendrogram for method Weighted Average Linkage",
        show.labels = FALSE,col.down = distinctColorPalette(10))

par(op)

families.weighted.average.linkage = cutree(data.clust.weighted.average.linkage, k = 10)[data.clust.weighted.average.linkage$order]

#Let's find in which group every observation belongs to

table(families.weighted.average.linkage)

final_dataset.star=cbind.data.frame(final_dataset,families.weighted.average.linkage)

first.group.of.countries.ave = unique(final_dataset.star[final_dataset.star$families.weighted.average.linkage == 1,"location"])

locations.and.percentages.group1.ave = data.frame(location = first.group.of.countries.ave, percentages = NA)

for (i in 1:nrow(locations.and.percentages.group1.ave)) {
  
  country = locations.and.percentages.group1.ave$location[i]
  
  data_sub = final_dataset.star[final_dataset.star$location == country,]
  
  locations.and.percentages.group1.ave$percentages[i] = (sum(data_sub$families.weighted.average.linkage == 1)/nrow(data_sub) * 100) #για να βγει σε μορφή ποσοστού
  
}

second.group.of.countries.ave = unique(final_dataset.star[final_dataset.star$families.weighted.average.linkage == 2,"location"])

locations.and.percentages.group2.ave = data.frame(location = second.group.of.countries.ave, percentages = NA)

for (i in 1:nrow(locations.and.percentages.group2.ave)) {
  
  country = locations.and.percentages.group2.ave$location[i]
  
  data_sub = final_dataset.star[final_dataset.star$location == country,]
  
  locations.and.percentages.group2.ave$percentages[i] = (sum(data_sub$families.weighted.average.linkage == 2)/nrow(data_sub) * 100) #για να βγει σε μορφή ποσοστού
  
}

third.group.of.countries.ave = unique(final_dataset.star[final_dataset.star$families.weighted.average.linkage == 3,"location"])

locations.and.percentages.group3.ave = data.frame(location = third.group.of.countries.ave, percentages = NA)

for (i in 1:nrow(locations.and.percentages.group3.ave)) {
  
  country = locations.and.percentages.group3.ave$location[i]
  
  data_sub = final_dataset.star[final_dataset.star$location == country,]
  
  locations.and.percentages.group3.ave$percentages[i] = (sum(data_sub$families.weighted.average.linkage == 3)/nrow(data_sub) * 100) #για να βγει σε μορφή ποσοστού
  
}

forth.group.of.countries.ave = unique(final_dataset.star[final_dataset.star$families.weighted.average.linkage == 4,"location"])

locations.and.percentages.group4.ave = data.frame(location = forth.group.of.countries.ave, percentages = NA)

for (i in 1:nrow(locations.and.percentages.group4.ave)) {
  
  country = locations.and.percentages.group4.ave$location[i]
  
  data_sub = final_dataset.star[final_dataset.star$location == country,]
  
  locations.and.percentages.group4.ave$percentages[i] = (sum(data_sub$families.weighted.average.linkage == 4)/nrow(data_sub) * 100) #για να βγει σε μορφή ποσοστού
  
}

fifth.group.of.countries.ave = unique(final_dataset.star[final_dataset.star$families.weighted.average.linkage == 5,"location"])

locations.and.percentages.group5.ave = data.frame(location = fifth.group.of.countries.ave, percentages = NA)

for (i in 1:nrow(locations.and.percentages.group5.ave)) {
  
  country = locations.and.percentages.group5.ave$location[i]
  
  data_sub = final_dataset.star[final_dataset.star$location == country,]
  
  locations.and.percentages.group5.ave$percentages[i] = (sum(data_sub$families.weighted.average.linkage == 5)/nrow(data_sub) * 100) #για να βγει σε μορφή ποσοστού
  
}

sixth.group.of.countries.ave = unique(final_dataset.star[final_dataset.star$families.weighted.average.linkage == 6,"location"])

locations.and.percentages.group6.ave = data.frame(location = sixth.group.of.countries.ave, percentages = NA)

for (i in 1:nrow(locations.and.percentages.group6.ave)) {
  
  country = locations.and.percentages.group6.ave$location[i]
  
  data_sub = final_dataset.star[final_dataset.star$location == country,]
  
  locations.and.percentages.group6.ave$percentages[i] = (sum(data_sub$families.weighted.average.linkage == 6)/nrow(data_sub) * 100) #για να βγει σε μορφή ποσοστού
  
}

seventh.group.of.countries.ave = unique(final_dataset.star[final_dataset.star$families.weighted.average.linkage == 7,"location"])

locations.and.percentages.group7.ave = data.frame(location = seventh.group.of.countries.ave, percentages = NA)

for (i in 1:nrow(locations.and.percentages.group7.ave)) {
  
  country = locations.and.percentages.group7.ave$location[i]
  
  data_sub = final_dataset.star[final_dataset.star$location == country,]
  
  locations.and.percentages.group7.ave$percentages[i] = (sum(data_sub$families.weighted.average.linkage == 7)/nrow(data_sub) * 100) #για να βγει σε μορφή ποσοστού
  
}

eighth.group.of.countries.ave = unique(final_dataset.star[final_dataset.star$families.weighted.average.linkage == 8,"location"])

locations.and.percentages.group8.ave = data.frame(location = eighth.group.of.countries.ave, percentages = NA)

for (i in 1:nrow(locations.and.percentages.group8.ave)) {
  
  country = locations.and.percentages.group8.ave$location[i]
  
  data_sub = final_dataset.star[final_dataset.star$location == country,]
  
  locations.and.percentages.group8.ave$percentages[i] = (sum(data_sub$families.weighted.average.linkage == 8)/nrow(data_sub) * 100) #για να βγει σε μορφή ποσοστού
  
}

ninth.group.of.countries.ave = unique(final_dataset.star[final_dataset.star$families.weighted.average.linkage == 9,"location"])

locations.and.percentages.group9.ave = data.frame(location = ninth.group.of.countries.ave, percentages = NA)

for (i in 1:nrow(locations.and.percentages.group9.ave)) {
  
  country = locations.and.percentages.group9.ave$location[i]
  
  data_sub = final_dataset.star[final_dataset.star$location == country,]
  
  locations.and.percentages.group9.ave$percentages[i] = (sum(data_sub$families.weighted.average.linkage == 9)/nrow(data_sub) * 100) #για να βγει σε μορφή ποσοστού
  
}

tenth.group.of.countries.ave = unique(final_dataset.star[final_dataset.star$families.weighted.average.linkage == 10,"location"])

locations.and.percentages.group10.ave = data.frame(location = tenth.group.of.countries.ave, percentages = NA)

for (i in 1:nrow(locations.and.percentages.group10.ave)) {
  
  country = locations.and.percentages.group10.ave$location[i]
  
  data_sub = final_dataset.star[final_dataset.star$location == country,]
  
  locations.and.percentages.group10.ave$percentages[i] = (sum(data_sub$families.weighted.average.linkage == 10)/nrow(data_sub) * 100) #για να βγει σε μορφή ποσοστού
  
}

final_dataset.star=cbind.data.frame(final_dataset,families.ward)

first.group.of.countries.ward = unique(final_dataset.star[final_dataset.star$families.ward == 1,"location"])

locations.and.percentages.group1.ward = data.frame(location = first.group.of.countries.ward, percentages = NA)

for (i in 1:nrow(locations.and.percentages.group1.ward)) {
  
  country = locations.and.percentages.group1.ward$location[i]
  
  data_sub = final_dataset.star[final_dataset.star$location == country,]
  
  locations.and.percentages.group1.ward$percentages[i] = (sum(data_sub$families.ward == 1)/nrow(data_sub) * 100) #για να βγει σε μορφή ποσοστού
  
}

second.group.of.countries.ward = unique(final_dataset.star[final_dataset.star$families.ward == 2,"location"])

locations.and.percentages.group2.ward = data.frame(location = second.group.of.countries.ward, percentages = NA)

for (i in 1:nrow(locations.and.percentages.group2.ward)) {
  
  country = locations.and.percentages.group2.ward$location[i]
  
  data_sub = final_dataset.star[final_dataset.star$location == country,]
  
  locations.and.percentages.group2.ward$percentages[i] = (sum(data_sub$families.ward == 2)/nrow(data_sub) * 100) #για να βγει σε μορφή ποσοστού
  
}

third.group.of.countries.ward = unique(final_dataset.star[final_dataset.star$families.ward == 3,"location"])

locations.and.percentages.group3.ward = data.frame(location = third.group.of.countries.ward, percentages = NA)

for (i in 1:nrow(locations.and.percentages.group3.ward)) {
  
  country = locations.and.percentages.group3.ward$location[i]
  
  data_sub = final_dataset.star[final_dataset.star$location == country,]
  
  locations.and.percentages.group3.ward$percentages[i] = (sum(data_sub$families.ward == 3)/nrow(data_sub) * 100) #για να βγει σε μορφή ποσοστού
  
}

forth.group.of.countries.ward = unique(final_dataset.star[final_dataset.star$families.ward == 4,"location"])

locations.and.percentages.group4.ward = data.frame(location = forth.group.of.countries.ward, percentages = NA)

for (i in 1:nrow(locations.and.percentages.group4.ward)) {
  
  country = locations.and.percentages.group4.ward$location[i]
  
  data_sub = final_dataset.star[final_dataset.star$location == country,]
  
  locations.and.percentages.group4.ward$percentages[i] = (sum(data_sub$families.ward == 4)/nrow(data_sub) * 100) #για να βγει σε μορφή ποσοστού
  
}

fifth.group.of.countries.ward = unique(final_dataset.star[final_dataset.star$families.ward == 5,"location"])

locations.and.percentages.group5.ward = data.frame(location = fifth.group.of.countries.ward, percentages = NA)

for (i in 1:nrow(locations.and.percentages.group5.ward)) {
  
  country = locations.and.percentages.group5.ward$location[i]
  
  data_sub = final_dataset.star[final_dataset.star$location == country,]
  
  locations.and.percentages.group5.ward$percentages[i] = (sum(data_sub$families.ward == 5)/nrow(data_sub) * 100) #για να βγει σε μορφή ποσοστού
  
}

#Discriminant Analysis----

final_dataset.star = final_dataset

final_dataset.star$group_star = NA #we create this column in order to calculate the accuracy of the method

for(i in 1:length(list_of_countries)){
  
  country = list_of_countries[i]
  
  start = 1
  
  while (final_dataset.star$location[start]!= country) {
    
    start = start + 1
  }
  
  while (final_dataset.star$location[start] == country & start<=nrow(final_dataset.star)) {
    
    final_dataset.star$group_star[start] = i
    
    start = start + 1
    
  }
}

#Normality check

lst.ks = lapply(1:ncol(final_dataset.star[,-c(1:4,21)]), function(i)
  ks.test(final_dataset.star[,-(1:4)][, i], "pnorm"))

#Homogeneity check

HOMOGENEITY(data = final_dataset.star, groups = "group_star",variables = colnames(final_dataset.star[,5:20]))

list_of_means = list()

for (i in 1:length(list_of_countries)) {
  
  data_sub = final_dataset[final_dataset$location == list_of_countries[i],]
  
  list_of_means[[i]] = colMeans(data_sub[,5:20])
  
}

list_of_cov_matrices = list()

for (i in 1:length(list_of_countries)) {
  
  data_sub = final_dataset[final_dataset$location == list_of_countries[i],]
  
  list_of_cov_matrices[[i]] = cov(data_sub[,5:20])
  
}

solve(matrix(unlist(list_of_cov_matrices[1]), ncol = 16, byrow = TRUE))

solve(matrix(unlist(list_of_cov_matrices[2]), ncol = 16, byrow = TRUE))

solve(matrix(unlist(list_of_cov_matrices[3]), ncol = 16, byrow = TRUE))

#As we saw there are covariance matrices which are singular (determinant equals to zero)
#So, there is collinearity. Let's find which variables cause this problem

model_all = lm(group_star ~ ., data = final_dataset.star[,5:21])

omcdiag(x = final_dataset.star[,5:20],y = final_dataset.star$group_star,mod = model_all)

imcdiag(x = final_dataset.star[,5:20],y = final_dataset.star$group_star,mod = model_all)

k = ncol(final_dataset.star[,5:20])

qf(1-0.05,k-1,n - k)

var_to_remove = c("life_expectancy","total_cases","total_deaths","median_age",
                  "aged_70_older","gdp_per_capita","human_development_index",
                  "hospital_beds_per_thousand","population","population_density",
                  "diabetes_prevalence","cardiovasc_death_rate")

final_dataset.star = final_dataset.star[,!names(final_dataset.star) %in% var_to_remove]

#We will calculate the list of means and the list of covariance matrices again

list_of_cov_matrices = list()

for (i in 1:length(list_of_countries)) {
  
  data_sub = final_dataset.star[final_dataset.star$location == list_of_countries[i],]
  
  list_of_cov_matrices[[i]] = cov(data_sub[,5:8])
  
}

#Just a check that we all the covariance matrices are not singular

for (i in 1:length(list_of_cov_matrices)) {
  
  solve(matrix(unlist(list_of_cov_matrices[i]), ncol = 4, byrow = TRUE))
  
}

list_of_means = list()

for (i in 1:length(list_of_countries)) {
  
  data_sub = final_dataset.star[final_dataset.star$location == list_of_countries[i],]
  
  list_of_means[[i]] = colMeans(data_sub[,5:8])
  
}

final_dataset.star$discr_group = NA #this column has the predicted group via discriminant
#analysis

for (i in 1:nrow(final_dataset.star)) {
  
  w = as.numeric(final_dataset.star[i,5:8])
  
  g = 1
  
  min1 = t((w - unlist(list_of_means[1])))%*%solve(matrix(unlist(list_of_cov_matrices[1]), ncol = 4, byrow = TRUE))%*%(w - unlist(list_of_means[1]))
  
  for (j in 2:length(list_of_countries)) {
    
    if (t((w - unlist(list_of_means[j])))%*%solve(matrix(unlist(list_of_cov_matrices[j]), ncol = 4, byrow = TRUE))%*%(w - unlist(list_of_means[j]))< min1){
      
      g = j
      
      min1 = t((w - unlist(list_of_means[j])))%*%solve(matrix(unlist(list_of_cov_matrices[j]), ncol = 4, byrow = TRUE))%*%(w - unlist(list_of_means[j]))
      
    }
    
  }
  
  final_dataset.star$discr_group[i] = g
  
}

#Accuracy of the method

((sum(final_dataset.star$group_star == final_dataset.star$discr_group))/(nrow(final_dataset.star)))*100

table(final_dataset.star$discr_group)

#We want to check if every population has at least 10 * 4 observation

h = 0

for (i in 1:length(list_of_countries)) {
  
  data_sub = final_dataset.star[final_dataset.star$location == list_of_countries[i],]
  
  if (nrow(data_sub) > 10 * 4){
    
    h = h + 1
  }
  
}

#We want to make a graph with the distances of means of population

df.means = as.data.frame(do.call(rbind, list_of_means))

dist.mat = dist(as.matrix(df.means),diag = TRUE)

dist_mi = 1/dist.mat # one over, as qgraph takes similarity matrices as input
jpeg('example_forcedraw.jpg', width=1000, height=1000, unit='px')
qgraph(dist_mi, layout='spring', vsize=3)
dev.off()