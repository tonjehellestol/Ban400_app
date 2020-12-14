#----------------------------------------------------------------------
#BAN400 - Introduction to R
#December 2020
#Candidates: 71, XX, XX and XX
#----------------------------------------------------------------------
#References:
#Covid19.analytics package:
#https://www.rdocumentation.org/packages/covid19.analytics/versions/1.0
#Shiny:
#https://shiny.rstudio.com/tutorial/
#Covid19 package:
#https://www.rdocumentation.org/packages/COVID19/versions/2.3.1




#Installing missing packages (if any) that are needed for the application
#Retrieved from 
#https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them

list.of.packages <- c("covid19.analytics", "magrittr", "tidyr", "ggplot2",
                      "shiny", "shinyWidgets", "data.table", "scales", "wpp2019",
                      "RColorBrewer", "rworldmap", "dplyr", "plotly","COVID19","ggthemes",
                      "gganimate", "forecast", "zoo", "stringr", "lubridate", "tidyquant", "docstring")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)



library("COVID19")
library(covid19.analytics)
library(magrittr)
library(tidyr)
library(ggplot2)
library(shiny)
library(shinyWidgets)
library(data.table)
library(scales)
library(wpp2019)
library(RColorBrewer)
library(rworldmap)
library(dplyr)
library(plotly)
library(gganimate)
library(ggthemes)
library(forecast)
library(zoo)
library(stringr)
library(lubridate)
library(tidyquant)
library(docstring)







#---------------------------------- Retriving datasets ----------------------------------#

### Retriving data for global statistics ###

#Column names wanted for the data
column_names = c("ID", "date", "confirmed_cases", "confirmed_deaths", "country_name", "population")

na.locf2 <- function(x) na.locf(x, na.rm = FALSE) #replace NA by previous value


Crossgovsources_df <- covid19() %>% 
  select("id","date","tests","confirmed","deaths","administrative_area_level_1","population") %>% 
  rename_at(vars(c("id","date","confirmed","deaths","administrative_area_level_1","population")), ~ column_names) %>%
  filter(country_name != "Costa Atlantica",country_name != "Diamond Princess") %>% #not countries
  group_by(country_name) %>% 
  do(na.locf2(.)) %>% #replace NA by previous cumulative value
  replace(is.na(.),0) %>% #replace NAs with not previous values by 0 
  mutate(daily_cases = c(0,diff(confirmed_cases)), daily_deaths = c(0,diff(confirmed_deaths))) %>% #calculate daily increase in number of deaths and confirmed cases
  ungroup() 

#Adding column "negative_daily_cases" and "negative_daily_deaths", holds the value 1 if daily_cases/daily_deaths are negative, 0 otherwise
#Columns are used in the accumulative_test function
Crossgovsources_df <- Crossgovsources_df %>% group_by(country_name) %>%
  mutate(negative_daily_cases = (ifelse( daily_cases < 0, 1, 0)), negative_daily_deaths = (ifelse( daily_deaths < 0, 1, 0))) %>% 
  ungroup()

#Correction: changing negative daily_deaths and negative daily_cases to 0
Crossgovsources_df <- Crossgovsources_df %>% 
  mutate( daily_deaths = replace(daily_deaths , daily_deaths < 0, 0), daily_cases = replace(daily_cases , daily_cases < 0, 0))



#options for user - countries in the dataset
countries <- Crossgovsources_df  %>% select(country_name)


### Retriving data for Norwegian statistics ###


### Retrieved from https://github.com/thohan88/covid19-nor-data
norwaydata <- read.csv("https://raw.githubusercontent.com/thohan88/covid19-nor-data/master/data/01_infected/msis/municipality_and_district.csv", na.strings = "", fileEncoding = "UTF-8-BOM")
norwaydata$date <- as.Date(norwaydata$date)
norwaydata$kommune_name

norway <- norwaydata %>% 
  rename("country_name" = kommune_name) %>% 
  rename("confirmed_cases" = cases ) %>% 
  select("date","country_name","fylke_name","confirmed_cases") %>% 
  group_by(country_name, date, fylke_name) %>% 
  summarise_at(vars(confirmed_cases), #Sums cases for Municipalities that are divided into sub-areas           
               list(confirmed_cases = sum)) %>% 
  ungroup() %>% 
  group_by(country_name) %>% 
  mutate(daily_cases= c(0,diff(confirmed_cases))) %>% #Calculates daily difference for each Municipality
  ungroup()

#Adding column "negative_daily_cases" and "negative_daily_deaths", holds the value 1 if daily_cases/daily_deaths are negative, 0 otherwise
#Columns are used in the accumulative_test function
norway <- norway %>% group_by(country_name) %>%
  mutate(negative_daily_cases = (ifelse( daily_cases < 0, 1, 0))) %>%
  ungroup()

#Correction: changing negative daily_deaths and negative daily_cases to 0
#This is done such that the graphs created by the app does not display negative number of cases
norway <- norway %>% 
  mutate(daily_cases = replace(daily_cases , daily_cases < 0, 0))


#Creating a dataset for the statistics and one for the municipalities 
kommune <- norway %>% select("country_name")




#Retrieving datasets from covid19.analytics package
#The source of the data is John Hopkins University and is to be compared to the 
#data provided by the COVID19 package
JHD_df_cleaning <- function(df, case_type){
  #'Covid-19 John Hopkins Data cleaning
  #'
  #'Cleans the date.frame as produced by covid19.analytics package
  #'
  #'@param df the dataset to clean
  #'@param case_type the type of John Hopkins data to clean (confirmed cases/deaths)
  
  
  
  df %>% 
    group_by(Country.Region) %>% 
    select(-"Province.State", - "Lat", - "Long") %>% 
    mutate_at(vars(-group_cols()), sum) %>% 
    distinct() %>% 
    pivot_longer(cols = colnames(.)[-(1:1)], 
                 names_to = "date",  
                 values_to = case_type)
}

JHD_df_confirmed <- JHD_df_cleaning(covid19.data("TS-confirmed"), "confirmed_cases")

JHD_df_deaths <- JHD_df_cleaning(covid19.data("TS-deaths"), "confirmed_deaths")

JHD_df_full <- JHD_df_confirmed %>% 
  full_join(JHD_df_deaths) %>% 
  rename(country_name = Country.Region) %>% 
  mutate(date=as.Date(date, format = "%Y-%m-%d"), country_name = as.character(country_name)) %>% 
  mutate(daily_cases = c(0,diff(confirmed_cases)), daily_deaths = c(0,diff(confirmed_deaths)))


# Gone through all the country names and numbers to manually change
# the names between them, fuzzy matching did not yield an appropriate result 

#American Samoa,Bermuda, Costa Atlantica, Grand Princess, Guam, Northern Mariana Islands, 
#Puerto Rico, Virgin Islands, U.S. not in JHD-dataset. So no difference in these countries. 

JHD_df_full$country_name[which(JHD_df_full$country_name  == "US")] <- "United States"
JHD_df_full$country_name[which(JHD_df_full$country_name  == "Cabo Verde")] <- "Cape Verde"
JHD_df_full$country_name[which(JHD_df_full$country_name  == "Czechia")] <- "Czech Republic"
JHD_df_full$country_name[which(JHD_df_full$country_name  == "Burma")] <- "Myanmar"
JHD_df_full$country_name[which(JHD_df_full$country_name  == "Taiwan*")] <- "Taiwan"
JHD_df_full$country_name[which(JHD_df_full$country_name  == "West Bank and Gaza")] <- "Palestine"
JHD_df_full$country_name[which(JHD_df_full$country_name  == "Eswatini")] <- "Swaziland"
JHD_df_full$country_name[which(JHD_df_full$country_name  == "Congo (Brazzaville)")] <- "Congo"
JHD_df_full$country_name[which(JHD_df_full$country_name  == "Congo (Kinshasa)")] <- "Congo, the Democratic Republic of the"


#Changed the names of the following to get correct entry;
#crossgov - Swaziland = Eswatini in JHD
#crossgov - Palestine = West Bank and Gaza in JHD
#crossgov - Taiwan = 	Taiwan* in JHD
#crossgov - Myanmar = Burma in JHD 
#crossgov - Czech Republic = Czechia in JHD
#crossgov - Cape Verde = 	Cabo Verde in JHD
#crossgov  - Congo = Congo (Brazzaville) in JHD
#crossgov - Congo, the Democratic Republic of the = Congo (Kinshasa) in JHD


JHD_df_full$confirmed_cases_JHD     <-JHD_df_full$confirmed_cases  
JHD_df_full$confirmed_deaths_JHD    <-JHD_df_full$confirmed_deaths
JHD_df_full$daily_cases_JHD         <-JHD_df_full$daily_cases
JHD_df_full$daily_deaths_JHD        <-JHD_df_full$daily_deaths
Crossgovsources_df$confirmed_cases_CG  <-Crossgovsources_df$confirmed_cases  
Crossgovsources_df$confirmed_deaths_CG <-Crossgovsources_df$confirmed_deaths
Crossgovsources_df$daily_cases_CG      <-Crossgovsources_df$daily_cases
Crossgovsources_df$daily_deaths_CG     <-Crossgovsources_df$daily_deaths

difference_jhd_cgov <- merge(JHD_df_full, Crossgovsources_df, by=c("date","country_name"), all.x = TRUE, all.y = TRUE)



#---------------------------------- Functions ----------------------------------#

##########Tests#########



####Testing if there exist a decrease in accumulative cases for a given country/municipality
####The function returns a string with the test results
accumulative_test <- function(df, group, column="cases", short = FALSE){ #parameter short defines if a short testresult should be returned
  #'Accumulative test for covid19 datasets
  #'
  #'Tests if accumulative values for confirmed cases or deaths are ever increasing and not decreasing
  #'Output comes in form of a string, due to integration with shiny linebreaks are written with HTML encoding (<br/>)
  #'
  #'@param df the dataset to test
  #'@param group Which country or municipality to test
  #'@param column Type of data to test (cases or deaths)
  #'@param short Boolean, set to True if a short test result is desired, False for detailed result
  
  
  
  temp_df <- df %>% filter(country_name == group, df[paste0("negative_daily_",as.character(column))] == 1) #creates a temporary dataset with municipalities and binary column for cases/deaths
  n <- nrow(temp_df) #counts number of rows equal to 1 (i.e 1 if accumulated value has decreased)
  if(n == 0){ #Short test result if tess is passed (There is no need for a detailed result if passed)
    result <- paste("***Accumulative Test PASSED***<br/>There have been 0 instances where accumulative values have decreased in ", group)
  } else if (n!=0 & short == TRUE){ #Short test result if test is not passed
    result <- paste("***Accumulative Test FAILED**<br/>There have been ", n, " instances where accumulative values have decreased in ", group,
                    "<br/>To view full diagnostic, go to the diagnostic page and chose current input", sep="")
    
  }else { #Detailed test result if test is not passed
    result <- paste("***Accumulative Test FAILED***<br/>There have been", n, "instances where accumulative values have decreased in", group,
                    "<br/>This might be due to correction of quantity registered, although this is not certain.",
                    "<br/>The dates this happened are postet below:<br/>",sep = " ")
    for(row in 1:nrow(temp_df)){ #Iterates through data frame adding date and values to string for output
      temp_df2 <- df %>% #Creates data.frame to retrieve value for cases prior to decrease
        filter(date == temp_df$date[row] - 1, country_name == group)
      result <- paste(result, "<br/>Date: ", temp_df$date[row], " ",str_to_title(column),": ", temp_df$confirmed_cases[row], 
                      "  ---- ",str_to_title(column) ," previous day: ",temp_df2$confirmed_cases[1], " ---- Difference: ", 
                      (temp_df[row, paste0("confirmed_",as.character(column))] - temp_df2[1, paste0("confirmed_", as.character(column))]), sep="")
    }
    
  }
  return(result)
}



###Testing if there exist differences between crossgov and JHU datasets, both containing global data 
getDifference_datasets<- function(data,type,country_name_input){
  
  data <- difference_jhd_cgov
  df_2 <- data
  difference_jhd_cgov_ <- df_2 %>% filter(country_name == country_name_input)
  difference_jhd_cgov_$date <- as.Date(difference_jhd_cgov_$date)
  
  # Sorting first by country, then date <- ascending 
  difference_jhd_cgov_ <- difference_jhd_cgov_[order(difference_jhd_cgov_[,2], difference_jhd_cgov_[,1] ),]
  difference_jhd_cgov_$confirmed_cases_JHD[is.na(difference_jhd_cgov_$confirmed_cases_JHD)] <- 0
  difference_jhd_cgov_$confirmed_deaths_JHD[is.na(difference_jhd_cgov_$confirmed_deaths_JHD)] <- 0
  if(type == "Cases") {
    
    difference_jhd_cgov_1 <- difference_jhd_cgov_ %>% 
      mutate(sum_confirmed_cases_JHD = (confirmed_cases_JHD),
             sum_confirmed_cases_CG = (confirmed_cases_CG))
    difference_jhd_cgov_1$difference_from_Cgov <- ifelse(is.na(difference_jhd_cgov_1$sum_confirmed_cases_JHD),1,ifelse(is.na(difference_jhd_cgov_1$sum_confirmed_cases_CG),1,ifelse(difference_jhd_cgov_1$sum_confirmed_cases_JHD == 0,1,ifelse(difference_jhd_cgov_1$sum_confirmed_cases_CG==0,1,difference_jhd_cgov_1$sum_confirmed_cases_CG/difference_jhd_cgov_1$sum_confirmed_cases_JHD))))
    print("Finding number of confirmed cases similarity in datasets!")
    
  }
  if(type == "Deaths") {
    
    difference_jhd_cgov_1 <- difference_jhd_cgov_ %>% 
      mutate(sum_confirmed_deaths_JHD = (confirmed_deaths_JHD),
             sum_confirmed_deaths_CG = (confirmed_deaths_CG)) 
    difference_jhd_cgov_1$difference_from_Cgov <- ifelse(is.na(difference_jhd_cgov_1$sum_confirmed_deaths_JHD),1,ifelse(is.na(difference_jhd_cgov_1$sum_confirmed_deaths_CG),1,ifelse(difference_jhd_cgov_1$sum_confirmed_deaths_JHD == 0,1,ifelse(difference_jhd_cgov_1$sum_confirmed_deaths_CG==0,1,difference_jhd_cgov_1$sum_confirmed_deaths_CG/difference_jhd_cgov_1$sum_confirmed_deaths_JHD))))
    print("Finding number of confirmed deaths similarity in datasets!")
    
  }
  
  
  weekly_diff <- difference_jhd_cgov_1 %>%
    tq_transmute(select     = difference_from_Cgov,
                 mutate_fun = apply.weekly,
                 FUN        = sum)
  #unique(diff_datasets$difference_from_Cgov)
  weekly_diff$aggregate_weekly_diff <- 100*weekly_diff$difference_from_Cgov/7
  
  aa <- weekly_diff %>% group_by(date) %>% summarise(Weekly_difference_between_datasets = mean(aggregate_weekly_diff))
  #The data on individual level will be by-weekly, but the different countries started testing and logging 
  # information at different times,. i.e. the week start date is different each chosen country
  
  aa$Interval <- cut(aa$Weekly_difference_between_datasets, c(0,95,100,105,Inf))
  
  aaa <- ggplot(data=aa, aes(x=date, y=Weekly_difference_between_datasets)) + geom_smooth() + geom_point(aes(colour = Interval)) + ggtitle("% Similiarity in datasets")
  return (aaa + xlab("Date") +ylab("% Similiarity between COVID19-package and John Hopkins dataset"))
  
  
  
}




##########Graphs##########


graph_dailyConfirmed <- function(df, country){
  #'Graph Daily Confirmed Cases
  #'
  #'Graphs daily confirmed cases for a given country/municipality
  #'
  #'@param df The dataset to graph from
  #'@param country Which country or municipality to be displayed
  
  
  temp_df <- df 
  temp_df$average <- ma(temp_df$daily_cases, 7)
  
  
  #Creating graph  
  graph_cases <- temp_df %>%
    ggplot(aes(x=date)) +
    geom_bar(aes(y=daily_cases, text = paste0("New Cases: ", daily_cases, "\nDate: ", date)), stat ="identity", colour = "#DD8888", fill = "#e74c4c", alpha = 0.4) +
    geom_line(aes(y = average), size = 1.5, colour = "red", alpha = 0.6) +
    labs (title = paste("Daily Confirmed Cases of Covid19 in", country, sep = " "),
          x = "Date\n",
          y = "Confirmed Cases\n") +
    theme_hc() +
    theme(axis.title.x =element_blank(),
          axis.title.y = element_text(size = 16, face = "bold"),
          plot.title =element_text(hjust = 0.5, face = "bold", size = 16)) +
    scale_x_date(date_breaks = "months" , date_labels = "%b-%y")
  
  
  ## Creating font for the interactive tooltip
  interactive_font = list(
    family = "DM Sans",
    size = 16,
    color = "white"
  )
  
  ## Creating the layout for the interactive tooltip
  interactive_label = list(
    bgcolor = "#595959",
    bordercolor = "transparent",
    font = interactive_font
  )
  
  
  ## Turning graph_cases to an interactive graph
  interactive_plot <- ggplotly(graph_cases, tooltip = c("text"), layerData = 1) %>% 
    style(hoverlabel = interactive_label) %>% #sets layout for tooltip labels
    layout(font = interactive_font, #sets font for plot
           yaxis = list(fixedrange = TRUE)) %>% 
    config(displayModeBar = FALSE) #Removes the tooltip bar at top left of graphing window
  
  
  
  return(interactive_plot)
}


graph_dailyDeaths <- function(df, country){
  
  #'Graph Daily Confirmed Deaths
  #'
  #'Graphs daily confirmed deaths for a given country/municipality
  #'
  #'@param df The dataset to graph from
  #'@param country Which country or municipality to be displayed
  
  
  temp_df <- df # Creates a temporary dataframe
  temp_df$average <- ma(temp_df$daily_deaths, 7) #Calculates moving averages by interval of 7 days (1 week)
  
  
  #Creating graph  
  graph_cases <- temp_df %>%
    ggplot(aes(x=date)) +
    geom_bar(aes(y=daily_deaths, text = paste0("New Cases: ", daily_cases, "\nDate: ", date)), stat ="identity", colour = "#DD8888", fill = "#e74c4c", alpha = 0.4) +
    geom_line(aes(y = average), size = 1.5, colour = "red", alpha = 0.6) +
    labs (title = paste("Daily Confirmed Deaths of Covid19 in", country, sep = " "),
          x = "Date\n",
          y = "Confirmed deaths\n") +
    theme_hc() +
    theme(axis.title.x =element_blank(),
          axis.title.y = element_text(size = 16, face = "bold"),
          plot.title =element_text(hjust = 0.5, face = "bold", size = 16)) +
    scale_x_date(date_breaks = "months" , date_labels = "%b-%y")
  
  
  ## Creating font for the interactive tooltip
  interactive_font = list(
    family = "DM Sans",
    size = 16,
    color = "white"
  )
  
  ## Creating the layout for the interactive tooltip
  interactive_label = list(
    bgcolor = "#595959",
    bordercolor = "transparent",
    font = interactive_font
  )
  
  ##
  
  ## Turning graph_cases to an interactive graph
  interactive_plot <- ggplotly(graph_cases, tooltip = c("text"), layerData = 1) %>% 
    style(hoverlabel = interactive_label) %>% 
    layout(font = interactive_font,
           yaxis = list(fixedrange = TRUE)) %>% 
    config(displayModeBar = FALSE)
  
  
  
  
  return(interactive_plot)
}


##Top3


plotTop3dailyCases <- function(df, title){
  #'Graph top 3 municipalities in Norway
  #'
  #'Graphs the top 3 municipalities that has had the higher number of confirmed cases in the last week
  #'
  #'@param df The dataset of covid19 data for Norway
  #'@param title Title of the plot
  
  
  tempdaily_df <- df %>% 
    filter(date >= Sys.Date()-8 & date <= Sys.Date()-1)%>% #Filters the dataset for the last week
    arrange(desc(daily_cases)) #Sorts the data
  
  #Aggregating the data
  aggdaily <- aggregate(x = tempdaily_df$daily_cases, #aggregated daily cases 
                        by = list(tempdaily_df$country_name), #sorting by municipality
                        FUN = sum)
  
  
  #Creating a vector for the aggregated daily cases by municipality 
  top_3 <- as.vector(aggdaily[["Group.1"]]) #Group.1 is default when using the function aggregate. It is the columname for the municipalities
  
  #Creating a dataset for aggregated daily cases for each municipality
  top3 <- aggdaily %>%
    filter(Group.1 %in% top_3)%>% #filtering the data
    rename(country_name= "Group.1")%>% #renaming to the original name
    arrange(desc(x))%>% #sorts the data
    head(3) #includes only the 3 municipalities with highest aggregated daily cases
  
  #Creating a vector for the 3 municipalities with highest aggregated daily cases the last seven days
  top <- as.vector(top3[["country_name"]])
  
  #Creating a dataset for the 3 municipalities with highest aggregated daily cases the last seven days
  top3 <- tempdaily_df %>% 
    filter(country_name %in% top)
  
  
  #Plots the data
  graph2 = top3 %>% 
    ggplot(aes(x = date, y = daily_cases, color = country_name)) +
    geom_line( size = 2, alpha = 0.9) +
    theme_hc() +
    labs (title = title,
          y = "Number of cases",
          color = "Location:   ") +
    theme(axis.title.x =element_blank(),
          plot.title =element_text(hjust = 0.5))
  
  return (graph2)
}






##########MAP#############


#Retrieved total number of confirmed cases globally
getGlobalConfirmed<- function(df){
  #'Calculate global confirmed cases
  #'
  #'calculates global confirmed cases per 100 000 capita
  #'
  #'@param df The dataset to calculate from
  #'
  
  
  
  map_data <- df %>% 
    filter(date == Sys.Date()-2 ) %>% 
    mutate(casesPer100k = round(confirmed_cases/population*100000,0),)%>% 
    select(ID, date, country_name, ID, casesPer100k) %>% 
    mutate(hover = paste0(country_name, "\n", casesPer100k))
  
  return (map_data)
  
}

#Retrieves total number of deaths globally
getGlobalDeaths<- function(df){
  #'Calculate global confirmed deaths
  #'
  #'calculates global confirmed deaths per 100 000 capita
  #'
  #'@param df The dataset to calculate from
  #'
  
  
  map_data <- df %>% 
    filter(date == Sys.Date()-2) %>% 
    mutate(casesPer100k = round(confirmed_deaths/population*100000,0),)%>% #calculate and store cases per 100k
    select(ID, date, country_name, ID, casesPer100k) %>% 
    mutate(hover = paste0(country_name, "\n", casesPer100k))#hover for the map 
  
  return (map_data)
  
}


#Function to draw the interactive map 
drawMap <- function(map_data, main_title, colorbar_title){
  #'Color "heat-map" for covid19 data globally
  #'
  #'Creates an interactive heat map displaying covid19 cases and death
  #'
  #'@param map_data The dataset to plot from
  #'@param main_title Title for the plot
  #'@param colorbar_title Title of the colorbar
  
  map <- plot_ly(map_data, 
                 type='choropleth', #map
                 locations=map_data$ID, #country letters
                 z=map_data$casesPer100k, 
                 zmin=0,
                 zmax = max(map_data$casesPer100k),
                 colorscale = list(c(0, 0.1, 0.2,0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), 
                                   c(brewer.pal(11,'Spectral'))), #11 color groups to display the differences between countries
                 color = map_data$casesPer100k,
                 text = map_data$hover,
                 hoverinfo = 'text') %>% #info about which country and the number of cases
    colorbar(title = colorbar_title, len = 0.75) %>% 
    add_annotations(#title adjustments
      y=1.05, 
      x=0.5, 
      text= main_title, 
      showarrow=F,
      font=list(size=15)
    ) %>% 
    config(displayModeBar = FALSE) #hide displaybar
  
  
  
  return(map)
  
}


###Stats for boxes 



#Returns total number of deaths in the dataset
totalDeaths <-function(df){
  #'Total Covid Deaths
  #'
  #'Calculates the total number of deaths from covid19 dataset
  #'for all countries or municipalities
  #'
  #'@param df The dataset to calculate from
  
  
  df <- df %>% filter(date == Sys.Date() -2) 
  number(sum(df$confirmed_deaths),
         big.mark = " ")
}

#Returns total number of confirmed cases in the dataset
totalConfirmed <- function(df){
  #'Total Covid Confirmed Cases
  #'
  #'Calculates the total number of confirmed cases from covid19 dataset
  #'for all countries or municipalities
  #'
  #'@param df The dataset to calculate from
  
  
  df <- df %>% filter(date == Sys.Date() -2) 
  number(sum(df$confirmed_cases), big.mark = " ")
}

#Return total number of tests in the dataset
totalTested <- function(df){
  #'Total Covid Tests Conducted
  #'
  #'Calculates the total number of covid 19 tests conducted accross all countries
  #'
  #'@param df The dataset to calculate from
  
  
  df <- df %>% filter(date == Sys.Date() -2) 
  number(sum(df$tests), big.mark = " ")
}


casesmonth <- function(df){
  #'Total Number of Cases Last Month in Norway
  #'
  #'Calculates the total number of covid 19 cases confirmed in Norway in the past 30 days (month)
  #'for a given municipality or total (depending on selected input in the app)
  #'
  #'@param df The dataset to calculate from
  
  
  numb <- df$daily_cases[df$date >= Sys.Date()-31 & df$date <= Sys.Date()-1]
  return(number(sum(numb), big.mark = " "))
}

casesweek <- function(df){
  #'Total Number of Cases Last week in Norway
  #'
  #'Calculates the total number of covid 19 cases confirmed in Norway in the past 7 days (week)
  #'for a given municipality or total (depending on selected input in the app)
  #'
  #'@param df The dataset to calculate from
  
  
  
  numb <- df$daily_cases[df$date >= Sys.Date()-8 & df$date <= Sys.Date()-1]
  return(number(sum(numb), big.mark = " "))
}



#---------------------------------- UI ----------------------------------#
#User interface - the users choices
ui <- fluidPage( 
  titlePanel(
    img(src = "https://www.siv.no/PublishingImages/Nyheter/Praksisnytt/Koronavirus.png?RenditionID=3", height = "200px", width = "99.9%")
  ), #collects image from specified webpage
  #Source: https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/
  navbarPage("COVID-19 Statistics", #Titletab
             tabPanel("Global", icon=icon("home"), #First tab
                      sidebarPanel(
                        helpText("Select", em("Cases"), "or", em("Deaths"), "and a country to examine.
               
               The data is continously updating."), #Help text with italic text for the options
                        
                        selectInput('Stat', 'Data:', c('Cases', 'Deaths')), #Select data type 
                        selectInput('PlotType', 'Data Visualization:', c('Map', 'Graph')), #Select the data visualization
                        
                        #This panel will only show if the data visualization chosen is "Graph"
                        conditionalPanel(
                          condition = "input.PlotType == 'Graph'", 
                          selectInput('Country', 'Country', countries, selected = countries[1]), #Select country 
                          dateRangeInput("dates",
                                         "Date range",
                                         start = "2020-01-22", #start date of the dataset
                                         end = as.character(Sys.Date())) #Ends at todays date by default
                        )
                      ),
                      #Creates the space and size of the output 
                      mainPanel(strong(
                        plotlyOutput('plot',height = '500px'),
                        verbatimTextOutput('text'),
                        width = 5)
                      ),
                      
                      hr(),
                      
                      #Rows below the mainpanel
                      #Inspiration from https://github.com/RiveraDaniel/Regression/blob/master/ui.R
                      fluidRow(column(width=3),
                               column(br(),
                                      strong(p("Total confirmed number of deaths:")), #Bold text
                                      textOutput("death"), #Prints number of deaths globally/ for a chosen country
                                      br(), #Space
                                      width = 3,style="background-color:	lightgray;border-left:6px solid gray;border-top: 1px solid black;border-right:1px solid black;border-bottom: 1px solid black"),
                               column(widt=4),
                               column(br(),
                                      strong(p("Total confirmed cases:")), 
                                      textOutput("TCC"), #Prints total confirmed cases globally/ for a chosen country
                                      br(),
                                      width = 3,style="background-color:	lightgray;border-left:6px solid gray;border-top: 1px solid black;border-right:1px solid black;border-bottom: 1px solid black"),
                               br(),
                               column(widt=2),
                               
                               column(br(),
                                      strong(p("Total reported number of tests:")), 
                                      textOutput("ntests"), #Prints total reported number of tests globally/ for a chosen country
                                      br(),
                                      width = 3,style="background-color:	lightgray;border-left:6px solid gray;border-top: 1px solid black;border-right:1px solid black;border-bottom: 1px solid black"),
                      ),
                      
                      br(),
                      br(),
                      
                      #A column for teststatus
                      fluidRow(column(width=3),
                               column(br(),
                                      htmlOutput("tsglobal"), #Prints the appropriate teststatus 
                                      width=9,style="background-color:lightyellow;border-radius: 10px",
                               ),
                               br(),
                      ),
                      
                      br(),
                      br(),
                      
             ),
             
             #Additional panel for Norway
             tabPanel("Norway", icon=icon("bar-chart-o"),
                      sidebarPanel(
                        helpText("Select either Top 3 or Graph to view data. Top 3 displays number of cases for the municipalities with the highest number of cases in the last week."),
                        selectInput('PlotTypeNorway', 'Data Visualization:', c('Top 3', 'Graph')), #Select the data vizualisation. "Top 3" is default
                        
                        #will only show this panel if the data visualization chosen is "Graph"
                        conditionalPanel(
                          condition = "input.PlotTypeNorway == 'Graph'", 
                          selectInput('Municipality', 'Municipality',unique( kommune), selected = 'Bergen'), #Select municipality. Bergen is default
                          dateRangeInput("datesNor",
                                         "Date range",
                                         start = "2020-01-22", #start date of the dataset
                                         end = as.character(Sys.Date())) #Ends at todays date by default
                          
                          
                        )
                      ),
                      #Creates the space and size of the output
                      mainPanel(strong(
                        plotlyOutput('PlotNor',height = '500px'),
                        verbatimTextOutput('text2'),
                        width = 5)
                      ),
                      
                      
                      hr(), #fluidRow below mainpanel
                      
                      #Columns/textboxes next to each other
                      fluidRow(column(width=3),
                               column(br(),
                                      strong(p("Total confirmed cases:")), 
                                      textOutput("casesnorway"), #Prints total confirmed cases in Norway
                                      br(),
                                      width = 3,style="background-color:	lightgray;border-left:6px solid gray;border-top: 1px solid black;border-right:1px solid black;border-bottom: 1px solid black"),
                               column(widt=4),
                               column(br(),
                                      strong(p("Total confirmed cases the last 30 days:")), 
                                      textOutput("ccmonth"), #Prints total confirmed cases the last 30 days
                                      br(),
                                      width = 3,style="background-color:	lightgray;border-left:6px solid gray;border-top: 1px solid black;border-right:1px solid black;border-bottom: 1px solid black"),
                               br(),
                               column(widt=2),
                               
                               column(br(),
                                      strong(p("Total confirmed cases the last 7 days:")), 
                                      textOutput("ccweek"), #Prints total confirmed cases the last 7 days
                                      br(),
                                      width = 3,style="background-color:	lightgray;border-left:6px solid gray;border-top: 1px solid black;border-right:1px solid black;border-bottom: 1px solid black"),
                      ),
                      br(),
                      br(),
                      
                      #columns for test status
                      fluidRow(column(width=3),
                               column(br(),
                                      htmlOutput("tsnorway"), #Prints the appropriate test status 
                                      width=9,style="background-color:lightyellow;border-radius: 10px",
                               ),
                               
                               br(),
                      ),
                      
                      
                      br(),
                      br(),
                      
             ),
             #Additional panel for diagnostics
             tabPanel("Diagnostics", icon=icon("chart-line"),
                      sidebarPanel(
                        helpText("Choose the testdata you want to examine."),
                        selectInput('dataset', 'Data:', c('Global', 'Norway')),  #Select data 
                        
                        
                        #will only show if the data "Global" is chosen
                        conditionalPanel(
                          condition = "input.dataset == 'Global'", 
                          selectInput('countryDiagnostic', 'Country', countries, selected = countries[1]), #Select country 
                          selectInput('statDiagnostic', 'Statistics', c("Cases","Deaths"), selected = countries[1]) #Select statistics
                          
                        ),
                        #will only show if the data "Norway" is chosen
                        conditionalPanel(
                          condition = "input.dataset == 'Norway'", 
                          selectInput('MunicipalityDiagnostic', 'Municipality', kommune, selected = kommune[1]) #select municipality
                          
                        )),
                      
                      #Creates the space and size of the output and a title
                      mainPanel(
                        h3(p(strong('Diagnostics',style="color:salmon")),align="center"),
                        column(htmlOutput("TextDiagnostic"),width = 12,style="border:1px solid black"),
                        column(br(), plotOutput("plotDiagnostic"),width = 12),
                      ),
                      hr(),
                      #Column below the output 
                      fluidRow(column(width=3),
                               column(br(),
                                      htmlOutput("testInfo"), #explanation of the test
                                      width=9,style="background-color:lightyellow;border-radius: 10px",
                               ),
                               
                               br(),
                      ),
                      
                      
                      br(),
                      br(),
                      
             ),
             
             #Additional panel for further information
             tabPanel("About this site", icon=icon("arrow-right"),
                      tags$div(
                        tags$h4("Last update"), #Title
                        h6(paste0("2020-12-14")), #Pastes todays date 
                        p("This site uses data for Norway retrived from", a(href="https://github.com/thohan88/covid19-nor-data", "https://github.com/thohan88/covid19-nor-data", target="_blank"), 
                          "and the built in data packages in R, covid19() and covid19.analytics(). The data is updated once daily, and numbers till yesterday is included in the data visualization. 
                          As of 2020-12-14, all the datasets are still updated daily and are expected to do in the foreseeable future. The data used for a global visualization of the covid19 situation is retrieved from the covid19() package. 
                          Credit goes to the work put out by Guidotti and Ardia (2020). Some comparing of datasets have been conducted, specifically between the dataset developed in the covid19 package and that of John Hopkins (covid19.analytics package). 
                          Since the dataset developed by Guidotti and Ardia (2020) uses cross governmental sources at national, regional and city level, we thought it interesting to compare this with a more global dataset that many international media outlets use (i.e John Hopkins).")
                        , #Pastes text and link to dataset
                        
                        
                      )
             )))



#####Server######
server <- function(input, output) {
  
  
  ######GLOBAL########
  
  #retrives data depending on which country and date range the user choose
  data_graph = reactive({
    Crossgovsources_df %>% mutate(date = as.Date(date)) %>% 
      filter( country_name == input$Country, date >= input$dates[1] & date <=input$dates[2] ) 
  })
  
  
  #plots the users requested plot(graph or map) and case-type(confirmed cases or deaths)
  output$plot <- renderPlotly({
    if(input$PlotType == 'Graph'){ #draw a graph
      data <- data_graph()#retrive relevant data
      if (input$Stat == 'Deaths'){ #plot confirmed deaths of wanted country
        graph_dailyDeaths(data, input$Country)
      }else{ #plot confirmed cases of wanted country
        graph_dailyConfirmed(data, input$Country)
      }}else{# draw map
        if (input$Stat == 'Deaths'){ #map confirmed deaths
          Crossgovsources_df %>% getGlobalDeaths() %>%
            drawMap( paste0("Confirmed number of deaths per 100.000 as of ", as.character(Sys.Date()-2)), "Number of deaths" )
        }else{ #map confirmed cases
          Crossgovsources_df %>% getGlobalConfirmed() %>%
            drawMap( paste0("Confirmed cases per 100.000 as of ", as.character(Sys.Date()-2)), "Confirmed cases")
        }
      }
  })
  
  ##Output for the boxes below the graph/map
  
  #number of deaths
  output$death <- renderText({
    if (input$PlotType == 'Graph'){ #number of deaths for the given country
      totalDeaths(data_graph())
      
    }else{ #total of deaths related to covid in the world
      totalDeaths(Crossgovsources_df)
    }
    
  }) 
  
  #display number of confirmed cases
  output$TCC <- renderText({
    if (input$PlotType == 'Graph'){ #number of cases for the given country
      totalConfirmed(data_graph())
      
    }else{#total cases of covid19 in the world
      totalConfirmed(Crossgovsources_df) 
    }
  })
  
  #display the number of tested reported
  output$ntests <- renderText({
    if (input$PlotType == 'Graph'){ #number of test reported for the given country
      totalTested(data_graph())
      
    }else{ #total number of tests reported in the world
      totalTested(Crossgovsources_df)
    }
    
  }) 
  
  
  
  ######Norway########
  
  
  #Retriving data depending on the date range and the municipality choosen by the user
  data_graphNorway = reactive({
    norway %>% mutate(date = as.Date(date)) %>% 
      filter(country_name == input$Municipality , date >= input$datesNor[1] & date <=input$datesNor[2] )
  })
  
  
  #Plot confirmed cases
  #Graph of a municipality or top 3 dpending on the users input
  output$PlotNor <- renderPlotly({
    
    if(input$PlotTypeNorway == 'Graph'){
      data <- data_graphNorway()
      graph_dailyConfirmed(data,input$Municipality) #plot daily confirmed of the given municipality
    }else{#plot top 3 municipalities with the highest number of cases the last week
      plotTop3dailyCases(norway, "Three municipalities with the highest number of cases in the last week") 
    }
    
  })
  
  ##Output for boxes below the graph
  
  #display number of cases
  output$casesnorway <- renderText({
    if (input$PlotTypeNorway == 'Graph'){ #number of cases for the given municipality
      totalConfirmed(data_graphNorway()) 
      
    }else{
      totalConfirmed(norway)#total confirmed cases in norway
    }})
  
  #display number of cases the last month
  output$ccmonth <- renderText({
    if (input$PlotTypeNorway == 'Graph'){
      casesmonth(data_graphNorway()) #for the given municipality
    }else{ 
      casesmonth(norway)  #for Norway in total
    }})
  
  #display number of cases the last week
  output$ccweek <- renderText({
    if(input$PlotTypeNorway == 'Graph'){ 
      casesweek(data_graphNorway())#the given municipality
    }else{
      casesweek(norway)#for Norway in total
      
    }})
  
  
  
  #Short output for diagnostics
  
  
  output$tsnorway <- renderText({
    if(input$PlotTypeNorway == 'Graph'){
      HTML(paste(accumulative_test(norway, input$Municipality, column = "cases", short = TRUE))) #short test result of the accumulative test
    }else{#top3
      HTML(paste("Chose  'graph' and a municipality to examine the data in more details."))#info
    }
  }) 
  
  
  output$tsglobal <- renderText({
    if(input$PlotType == 'Graph'){
      HTML(paste(accumulative_test(Crossgovsources_df, input$Country, tolower(input$Stat), short = TRUE))) #short test result of the accumulative test
    }else{ #map
      HTML(paste("Chose 'graph' and a country to examine the data in more details."))
    }
  })
  
  
  ##Output for the diagnostics tab
  
  #output for the accumulative test
  output$TextDiagnostic <- renderText({
    if(input$dataset == "Global"){ 
      HTML(paste(accumulative_test(Crossgovsources_df,input$countryDiagnostic, tolower(input$statDiagnostic))))#test result for wanted country
    }
    else if(input$dataset == "Norway"){ 
      HTML(paste(accumulative_test(norway, input$MunicipalityDiagnostic)))#test result for wanted municipality
    }
  })
  
  #Output for the difference test
  #Displays a plot showing in what extent the two global datasets are reporting the same results
  output$plotDiagnostic <- renderPlot({
    if(input$dataset == "Global"){
      getDifference_datasets(difference_jhd_cgov,input$statDiagnostic, input$countryDiagnostic)
      
    }})
  
  output$testInfo <- renderText({
    if((input$dataset == "Global")){
      HTML(paste("* Difference in datasets * <br/> <br/> 
                The Covid19-dataset is compared to JHD- John Hopkins, in a weekly average cumulative sum of deaths/cases. <br/> 
              *100% score indicates that the two datasets are reporting the same numbers biweekly. <br/> 
              *50% score indicates that Covid19-dataset reports  100% more cases/deaths than John Hopkins have reported. <br/> 
              *150% score indicates that John Hopkins reports 50% more cases/deaths than what the Covid19-dataset reports. <br/> <br/> <br/> 
                 
              *Data cleaning* <br/> <br/> 
              All missing confirmed values in the dataset has been replace by the closest previous value if any exist, and 
              given the value 0 otherwise. 
              "
                 
                 
      ))
    }else{
      HTML(paste("*Data cleaning* <br/> <br/> 
              All missing confirmed values in the dataset has been replace by the closest previous value if any exist, and 
              given the value 0 otherwise.<br/><br/> "
      ))
    }
    
  })
  
  
  
  
}



shinyApp(ui = ui, server = server)

