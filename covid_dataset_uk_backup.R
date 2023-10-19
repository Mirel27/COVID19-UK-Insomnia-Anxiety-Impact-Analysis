rm(list = ls()) #remove all variable
library(tidyverse)
library(corrplot)
library(reshape2)
install.packages("gridExtra")
library(gridExtra)
covid_dataset <- read.csv("owid-covid-data.csv",header = TRUE)
install.packages("corrplot")


View(covid_dataset)
covid_unitedkingdom <- filter(covid_dataset,location == "United Kingdom" )

#------------Creating new Columns united Kingdom -----------
str(covid_unitedkingdom$date) # check the data type of date column
covid_unitedkingdom$date<-as.Date(covid_unitedkingdom$date,"%d-%m-%Y") #convert character datatype to date datatype and chenge the format from DD-MM-YYYY to YYYY-MM-DD
covid_unitedkingdom$months <- months(covid_unitedkingdom$date) #Exact month of the date and add it to the new column

covid_unitedkingdom$year <- format(covid_unitedkingdom$date,"%Y") # Extract year of the date and create a new column year in the data frame
covid_unitedkingdom <- filter(covid_unitedkingdom,year=="2020")
covid_unitedkingdom$week_number <- format(covid_unitedkingdom$date, "%W") 
#---------Selecting necessary columns-------------------------
View(covid_unitedkingdom)

#Replace all NA with 0 
covid_unitedkingdom[is.na(covid_unitedkingdom)] <- 0


View(covid_unitedkingdom)

#---new_cases--
covid_unitedkingdom_new_cases <- covid_unitedkingdom %>% select(new_cases,week_number,months,year)%>%
  filter(months %in% c('January','February','March','April','May'))%>% 
  group_by(year,week_number) %>% 
  summarise(weekly_cases=sum(new_cases,na.rm = TRUE) )


View(covid_unitedkingdom_new_cases)

#monthly deaths
covid_unitedkingdom_death <- covid_unitedkingdom %>% select(new_deaths,week_number,months,year)%>%
  filter(months %in% c('January','February','March','April','May'))%>% 
  group_by(year,week_number) %>% 
  summarise(weekly_deaths=sum(new_deaths,na.rm = TRUE) )

View(covid_unitedkingdom_death)

# mean weekly icu patients


covid_unitedkingdom_icu <- covid_unitedkingdom %>% select(icu_patients,week_number,months,year)%>%
  filter(months %in% c('January','February','March','April','May'))%>% 
  group_by(year,week_number) %>% 
  summarise(weekly_icu=mean(icu_patients,na.rm = TRUE) )

View(covid_unitedkingdom_icu)

covid_unitedkingdom_stringency_index <- covid_unitedkingdom %>% select(stringency_index,week_number,months,year)%>%
  filter(months %in% c('January','February','March','April','May'))%>% 
  group_by(year,week_number) %>% 
  summarise(weekly_stringency_index=mean(stringency_index,na.rm = TRUE) )

View(covid_unitedkingdom_stringency_index)

#Join all covid dataset aggregated

covid_unitedkingdom_clean <-inner_join(covid_unitedkingdom_new_cases,covid_unitedkingdom_death,by="week_number")
covid_unitedkingdom_clean <-inner_join(covid_unitedkingdom_clean,covid_unitedkingdom_icu,by="week_number")
covid_unitedkingdom_clean <-inner_join(covid_unitedkingdom_clean,covid_unitedkingdom_stringency_index,by="week_number")


View(covid_unitedkingdom_clean)

nrow(covid_unitedkingdom_clean)
write.csv(covid_unitedkingdom_clean,file = "covid_unitedkingdom_clean.csv")
#---------------------------Google search---------------

search_terms_dataset <- read.csv("search_term_uk.csv",header = TRUE)
View(search_terms_dataset)
search_terms_dataset$Week<-as.Date(search_terms_dataset$Week,"%d-%m-%Y")
search_terms_dataset$Year <-format(search_terms_dataset$Week, "%Y") 

#---------Filter out records for 2020-------------
search_terms_dataset <- search_terms_dataset %>% filter(Year=="2020")

search_terms_dataset$week_number <- format(search_terms_dataset$Week, "%W") 

#----------- Join---------------

covid_search_dataset <-inner_join(covid_unitedkingdom_clean,search_terms_dataset,by="week_number")

#check which series it belongs to

covid_search_dataset<-covid_search_dataset %>% select(Year,week_number,weekly_cases,weekly_icu,weekly_deaths,weekly_stringency_index,anxiety,insomnia)


#Convert week_number from character to integer 

covid_search_dataset$week_number <- as.integer(covid_search_dataset$week_number)

#-------- Multivariate Modeling-------------

train_Size=0.7
covid_search_dataset_train <- covid_search_dataset[1:(train_Size*nrow(covid_search_dataset)),]
covid_search_dataset_test <- covid_search_dataset[(nrow(covid_search_dataset_train)+1):nrow(covid_search_dataset),]


#---------------Check correlation between all variables------------------

covid_search_dataset_corr <-covid_search_dataset %>% select(weekly_cases,weekly_icu,weekly_deaths,weekly_stringency_index,anxiety,insomnia)
cor_matrix <- cor(covid_search_dataset_corr)
cor_matrix <- round(cor_matrix,2)
View(cor_matrix)

??round

#multivariate modeling  Anxiety
mod_anxiety <-covid_search_dataset_train %>% lm(
  formula = anxiety~weekly_cases+weekly_icu+weekly_deaths+weekly_stringency_index
)

coefs <- coef(mod_anxiety)

#Equation that we obtain

#anxiety = 93.102 -0.00141(Weekly_cases)-0.0064(weekly_icu)+(0.0068)weekly_deaths+0.1608(weekly_stringency_index)

#to test the model
summary(mod_anxiety)
predict(mod_anxiety,
        newdata=covid_search_dataset_test,
        interval='confidence')


covid_search_dataset_test_mv <- covid_search_dataset_test
covid_search_dataset_test_mv$predicted <-predict(mod_anxiety,
                                       newdata=covid_search_dataset_test)
covid_search_dataset_test_mv$residuals <- covid_search_dataset_test_mv$predicted -covid_search_dataset_test_mv$anxiety
covid_search_dataset_test_mv$errorperc <- round(abs(covid_search_dataset_test_mv$residuals/covid_search_dataset_test_mv$anxiety)*100,2)


sse_dist <- sum(covid_search_dataset_test_mv$residuals**2)
View(covid_search_dataset_test_mv)

plot(mod_anxiety, which=1) #plot between residuals and fitted values shows the outliers)


#multivariate modeling  insomnia
mod_insomnia <-covid_search_dataset_train %>% lm(
  formula = insomnia~weekly_cases+weekly_icu+weekly_deaths+weekly_stringency_index
)

coefs <- coef(mod_insomnia)

#Equation that we obtain

#Insonia = 57.43 -0.0038(Weekly_cases)-0.031(weekly_icu)+ 0.0309(weekly_deaths)+ 0.614(weekly_stringency_index)

#to test the model
summary(mod_insomnia)

#----------Visualization--------------

#----------------------------Line and Bar Chart-------------------------------------


scale=300
plot_insomia_cases <-ggplot(covid_search_dataset, aes(x=week_number)) +
  # Bar plot on the left side
  geom_bar(aes(y = weekly_cases), fill = "lightblue", stat = "identity") +
  # Line plot on the right side
  # Highlight the column with the maximum value    
  geom_bar(data=subset(covid_search_dataset, weekly_cases==max(weekly_cases)), aes(week_number, weekly_cases),fill="darkblue", stat="identity")+
  geom_line(aes(y = insomnia*scale, color = "red")) +
  geom_line(aes(y = anxiety*scale, color = "orange"))+
  # Set the axis on the left and right side
  scale_y_continuous(sec.axis = sec_axis(~./scale, name = "Insomnia/Anxiety"))+
  scale_colour_manual(name = "Colour", labels = c("Insomia", "Anxiety"), values = c("red", "orange"))+
  theme( axis.title.y.right = element_text( angle = 90))+
  labs(x="Week",y="New Cases",
  title="Impact on Insomia and Anxiety v/s Rise in Cases during strict lockdown")
  



scale1=60
plot_insomia_death<-ggplot(covid_search_dataset, aes(x=week_number)) +
  # Bar plot on the left side
  geom_bar(aes(y = weekly_deaths), fill = "lightblue", stat = "identity") +
  # Highlight the column with the maximum value    
  geom_bar(data=subset(covid_search_dataset, weekly_deaths==max(weekly_deaths)), aes(week_number, weekly_deaths),fill="darkblue", stat="identity")+
  # Line plot on the right side
  geom_line(aes(y = insomnia*scale1, color = "red")) +
  geom_line(aes(y = anxiety*scale1, color = "orange"))+
  # Set the axis on the left and right side
  scale_y_continuous(sec.axis = sec_axis(~./scale1, name = "Insomnia/Anxiety"))+
  scale_colour_manual(name = "Colour", labels = c("Insomia", "Anxiety"), values = c("red", "orange"))+
  theme( axis.title.y.right = element_text( angle = 90))+
  labs(x="Week",y="Deaths",
       title="Impact on Insomia and Anxiety v/s Covid Death during strict lockdown")



plot_insomia_stringency<-ggplot(covid_search_dataset, aes(x=week_number)) +
  # Bar plot on the left side
  geom_bar(aes(y = weekly_stringency_index), fill = "lightblue", stat = "identity") +
  # Highlight the column with the maximum value    
  geom_bar(data=subset(covid_search_dataset, weekly_stringency_index==max(weekly_stringency_index)), aes(week_number, weekly_stringency_index),fill="darkblue", stat="identity")+
  # Line plot on the right side
  geom_line(aes(y = insomnia, color = "red")) +
  geom_line(aes(y = anxiety, color = "orange"))+
  # Set the axis on the left and right side
  scale_y_continuous(sec.axis = sec_axis(~., name = "Insomnia/Anxiety"))+
  scale_colour_manual(name = "Colour", labels = c("Insomia", "Anxiety"), values = c("red", "orange"))+
  theme( axis.title.y.right = element_text( angle = 90))+
  labs(x="Week",y="Stringency Index",
       title="Impact on Insomia and Anxiety v/s Stringency Index during strict lockdown")
  

scale3=30
plot_insomia_icu<-ggplot(covid_search_dataset, aes(x=week_number)) +
  # Bar plot on the left side
  geom_bar(aes(y = weekly_icu), fill = "lightblue", stat = "identity") +
  # Highlight the column with the maximum value    
  geom_bar(data=subset(covid_search_dataset, weekly_icu==max(weekly_icu)), aes(week_number, weekly_icu),fill="darkblue", stat="identity")+
  # Line plot on the right side
  geom_line(aes(y = insomnia*scale3,color = "red") )+
  geom_line(aes(y = anxiety*scale3, color = "orange"))+
  # Set the axis on the left and right side
  scale_y_continuous(sec.axis = sec_axis(~./scale3, name = "Insomnia/Anxiety"))+
  scale_colour_manual(name = "Colour", labels = c("Insomia", "Anxiety"), values = c("red", "orange"))+
  theme( axis.title.y.right = element_text( angle = 90))+
  labs(x="Week",y="ICU patients",
       title="Impact on Insomia and Anxiety v/s ICU admissions during strict lockdown")
 



grid.arrange(plot_insomia_cases, plot_insomia_death, plot_insomia_stringency, plot_insomia_icu, nrow=2)
#------------------------------------------------------------------------------


#------stacked area plot-----------------
# Plot the data
ggplot(covid_search_dataset, aes(x=week_number)) +
  geom_area(aes(y=anxiety, fill="anxiety")) +
  geom_area(aes(y=insomnia, fill="insomnia"))+
  geom_line(aes(y=anxiety),color = "black" )+
  geom_line(aes(y=insomnia),color="black")

#Plot correlation heatmap
corrplot(cor_matrix, type="upper", order="hclust", tl.col="black", tl.srt=45)

#---------------------

melted_cormat <- melt(cor_matrix)

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Reorder the correlation matrix
cormat <- reorder_cormat(cor_matrix)
upper_tri <- get_upper_tri(cor_matrix)

lower_tri <- get_lower_tri(cor_matrix)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
melted_cormat <- melt(lower_tri,na.rm = TRUE)
# Create a ggheatmap

cord.labels <- c("New Cases","ICU Admission","Deaths","Stringency Index","Anxiety","Insomia")

ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "#FFB6C1", high = "#800080", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 10, hjust = 1))+
  coord_fixed() +#Used to set aspect ratio of the plot
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5, face="bold", lineheight=0.8, size=14,margin=margin(0,0,30,0) ),
    plot.title.position = "plot",
    axis.text.x = element_text(hjust = 0),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(0, 1),
    legend.position = c(0.7, 0.4),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))+
  scale_x_discrete(position = "top" ,labels = cord.labels)+
  scale_y_discrete(labels = cord.labels)+
  ggtitle("Correlation Between Variable")
# Print the heatmap
print(ggheatmap)


