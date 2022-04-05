library(RMySQL)
library(pacman)
library(plyr)
library(car)
library(alr4)
library(DT)
library(mosaic)
library(pander)
library(readr)
library(ResourceSelection) 
library(plotly)
library(gridExtra)
library(scico)
library(grid)
library(devtools)
library(rvest)
library(directlabels)
library(lubridate)
library(berryFunctions)
library(googlesheets4)
library(xlsx)
pacman::p_load(dygraphs, scales)
p_load(R.utils, ggplot2, tidyverse, trelliscopejs, gapminder, formattable)

subset_data <- read.csv("~/R/Investment Tool/sheldon_trelliscope_WI22/data/homelessness/fsr_input.tsv.gz", sep = ',', header = TRUE) %>% 
  select(id_award, amount_award, year_budget,  name_department_funding, city_company, state_company, zipcode_company) %>% 
  filter(state_company == "IL")

names(subset_data)[2] <- "Amount"
names(subset_data)[3] <- "Year"
names(subset_data)[4] <- "Funding Department"
names(subset_data)[5] <- "City"
names(subset_data)[6] <- "State"
names(subset_data)[7] <- "Zipcode"

Departments <- subset_data %>% 
  distinct(`Funding Department`)

Years <- subset_data %>% 
  distinct(`Year`)

Zipcodes <- subset_data %>% 
  distinct(`Zipcode`)

trell.data <- data.frame()


for(i in 1:length(Zipcodes$Zipcode)){
  for(j in 1:length(Departments$`Funding Department`)){
    for(k in 1:length(Years$Year)){
      temp.data <- subset_data %>% 
        filter(Year == Years$Year[k]) %>% 
        filter(`Funding Department` == Departments$`Funding Department`[j]) %>% 
        filter(Zipcode == Zipcodes$Zipcode[i])
      
      temp.data2 <- subset_data %>% 
        filter(Zipcode == Zipcodes$Zipcode[i])
      
      Award.Amount <- sum(temp.data$Amount)
      St <- temp.data2$State[1]
      Cit <- temp.data2$City[1]
      Zip <- Zipcodes$Zipcode[i]
      Dep <-  Departments$`Funding Department`[j]
      Year <- Years$Year[k]
      
      trell.data <- data.frame(rbind(trell.data, cbind(St, Cit, Zip, Dep, Year, Award.Amount)))
      
    }
    print(paste("J:", j))
  }
  print(paste("I:", i))
}






names(trell.data)[6] <- "Amount"
names(trell.data)[5] <- "Year"
names(trell.data)[4] <- "Funding Department"
names(trell.data)[2] <- "City"
names(trell.data)[1] <- "State"
names(trell.data)[3] <- "Zipcode"


Chase.Trelliscope <- trell.data %>% 
  mutate(Amount = as.numeric(Amount)) %>% 
  mutate("Funding Department" = case_when(`Funding Department` == "DEPARTMENT OF HEALTH AND HUMAN SERVICES" ~ "Department of Health and Human Services",
                                          `Funding Department` == "DEPARTMENT OF VETERANS AFFAIRS" ~ "Department of Veterans Affairs",
                                          `Funding Department` == "DEPARTMENT OF HOUSING AND URBAN DEVELOPMENT" ~ "Department of Housing and Urban Development",
                                          `Funding Department` == "DEPARTMENT OF 3600) VETERANS AFFAIRS" ~ "Department of 3600 Veterans Affairs",
                                          `Funding Department` == "CORPORATION FOR NATIONAL AND COMMUNITY SERVICE" ~ "Department of National and Community Service",
                                          `Funding Department` == "DEPARTMENT OF LABOR" ~ "Department of Labor",
                                          `Funding Department` == "DEPARTMENT OF THE TREASURY" ~ "Department of the Treasury",
                                          `Funding Department` == "DEPARTMENT OF EDUCATION" ~ "Department of Education",
                                          `Funding Department` == "DEPARTMENT OF DEFENSE" ~ "Department of Defense",
                                          `Funding Department` == "DEPARTMENT OF JUSTICE" ~ "Department of Justice",
                                          `Funding Department` == "DEPARTMENT OF HOMELAND SECURITY" ~ "Department of Homeland Security",
                                          `Funding Department` == "DEPARTMENT OF STATE" ~ "Department of State")) %>% 
  nest(data = !c(State, City, Zipcode)) %>% 
  drop_na() %>% 
  mutate(State = "Illinois")



CH.Trell.Graph.Func <- function(x){
  if((max(as.numeric(x$Amount)) >= -min(as.numeric(x$Amount))) == TRUE ){
    max <- max(as.numeric(x$Amount))
  } else {
    max <- -min(as.numeric(x$Amount))
  }
  
  
gg <- ggplot()+
  geom_tile(data = x, mapping = aes(x = as.numeric(Year), y = `Funding Department`, fill = as.numeric(Amount), text1 = `Funding Department`, text = paste("Year:", Year, "- Award Amount: $", Amount, sep = " ")))+
  scale_x_continuous(name = 'Year', limits = c(2000, NA)) +
  labs(x = "Year", y = "Agency", fill = "Award Amount", caption = "*Denomitations in thousands of dollars") +
  scale_fill_gradient2(labels=scales::dollar_format(), limits = c(-max, max), breaks = seq(-max, max, by = (max/2)), low = "darkred", mid = "white", high = "navy")+
  theme_bw()+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggplotly(gg, tooltip = c("text1", "text"))

}

Chase.Trelliscope <- Chase.Trelliscope %>% 
  mutate("Illinois State Homelessness Funding" = map_plot(data, CH.Trell.Graph.Func))

Chase.Trelliscope %>% 
  trelliscope(name = "Illinois State Homelessness Funding",
              path = "C:/Users/Chatc/OneDrive/Documents/Winter 2022/Trell")


















