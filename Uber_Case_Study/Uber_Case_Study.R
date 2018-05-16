library(lubridate)
library(ggplot2)
library(dplyr)

################################################################################
# DATA LOADING
################################################################################

#Reading the 'Uber Request Data.csv' file
#Ensuring all the empty values are read as "NA"

uber <- read.csv("Uber Request Data.csv" , stringsAsFactors = FALSE , na.strings = c("" , " " , "NA"))

################################################################################
# DATA CLEANUP
################################################################################

# Converting the Request.timestamp and Drop timestamp to a common Date/Time format 

uber$Request.timestamp <- parse_date_time(uber$Request.timestamp , c("dmY" , "dmY HMS" , "dmY HM"))
uber$Drop.timestamp <- parse_date_time(uber$Drop.timestamp , c("dmY" , "dmY HMS" , "dmY HM"))


################################################################################
# DERIVED METRIC
################################################################################

#Creating a new column request hour

uber$Request.hour <- hour(uber$Request.timestamp)

#Filter the request that have the status as "Cancelled" or "No Cars"

uber1 <- subset(uber , Status == "Cancelled" | Status == "No Cars Available")

ggplot(uber1 , aes(x = factor(Request.hour) , fill = factor(uber1$Pickup.point))) + 
geom_bar(position = "dodge") + 
labs(title = "Hourly Unfulfilled Demand for Cabs" , x = "Hour" , y = "Num Of Requests : Cancelled/No Cars" , fill = "Pickup Point") 

############################################
#Dividing the hours into the following slots
############################################

#05 - 10 -- Early Morning
#11 - 16 -- Day Time
#17 - 22 -- Late Evening
#23 - 04 -- Night

##################################
#Creating a new column "Slot"
##################################

uber$slot <- ifelse(uber$Request.hour >= 5 & uber$Request.hour <= 10 , "Early Morning" ,
                    ifelse(uber$Request.hour >= 11 & uber$Request.hour <= 16 , "Day Time" ,
                           ifelse(uber$Request.hour >= 17 & uber$Request.hour <= 22 , "Late Evening" , "Night")))

###################################################################################
#Stacked bar chart : 
#   X axis - "Slot"
#   Y axis - "No of Requests"
#   Breakdown of requests as "Cancelled" , "Trips completed" , "No Cars available"
###################################################################################

ggplot(uber , aes(x=reorder(slot,slot,function(x)-length(x))  , fill = factor(uber$Status))) + 
geom_bar(stat = 'count') +  
scale_fill_manual("Status", values = c("Cancelled" = "red", "No Cars Available" = "orange", "Trip Completed" = "green")) +
geom_text(stat='count',aes(label=..count..), size = 3.3, position = position_stack(vjust = 0.6)) +  
labs(title = "Trips Status in different time slots" , x = "Slot" , y = "Requests" , fill = "Status") 

## It is clearly visible that in the 
# Early morning most of the trips get "Cancelled"
# In evening most of the requests are rejected with "No Cars Available:

###################################################
#Subsetting four data frames on the basis of slots
###################################################

uber_morning_slot <- subset(uber , slot == "Early Morning")
uber_day_slot <- subset(uber , slot == "Day Time")
uber_evening_slot <- subset(uber , slot == "Late Evening")
uber_night_slot <- subset(uber , slot == "Night")

####################################################################
#Plot to show the number of requests against the Status of Requests 
#made in the "Morning Slot" on the basis of Pickup point
####################################################################

ggplot(uber_morning_slot , aes(x = Status , fill = factor(uber_morning_slot$Pickup.point))) + 
geom_bar() +
geom_text(stat='count',aes(label=..count..), size = 3.3, position = position_stack(vjust = 0.6)) +
labs(title = "Cab Status in Morning slot" , x = "Status" , y = "Num Of Requests" , fill = "Pickup Point") 

###############################################################################
#Stacked Bar Plot to show the number of requests against the Status of Requests 
#made in the "Evening Slot" on the basis of Pickup point
###############################################################################

ggplot(uber_evening_slot , aes(x = Status , fill = factor(uber_evening_slot$Pickup.point))) + 
geom_bar(stat = 'count') +
geom_text(stat='count',aes(label=..count..), size = 3.3, position = position_stack(vjust = 0.6)) +
labs(title = "Cab Status in Evening slot" , x = "Status" , y = "Num Of Requests" , fill = "Pickup Point")

###############################################################################
#Stacked Bar Plot to show the number of requests against the Status of Requests 
#made in the "Night Slot" on the basis of Pickup point
###############################################################################

ggplot(uber_night_slot , aes(x = Status , fill = factor(uber_night_slot$Pickup.point))) + 
geom_bar(stat = 'count') +
geom_text(stat='count',aes(label=..count..), size = 2, position = position_stack(vjust = 0.6)) +
labs(title = "Cab Status in Night slot" , x = "Status" , y = "Num Of Requests" , fill = "Pickup Point")

########################################################################
#DEMAND AND SUPPLY GAP
########################################################################

#########################################################################################
# Grouping "uber" dataframe by "Status" and calculating the "# of requests" as percentage
#########################################################################################

d11 <- uber %>% group_by(Status) %>% summarise(count = n()) %>% mutate(perc = (count/sum(count))*100)

#############################################################################################
# Grouping "uber" dataframe by  "Pickup point and Status" and calculating the "# of requests" as percentage
#############################################################################################

d12 <- uber %>% group_by(Pickup.point , Status) %>% summarise(count = n()) %>% mutate(perc = (count/sum(count))*100)

#################################################################################
# Grouping by "Slot and Status" and calculating the "# of requests" as percentage
#################################################################################

d2 <- uber %>% group_by(slot , Status) %>% summarise(count = n()) %>% mutate(perc = (count/sum(count))*100)

####################################################################################
# Grouping the dataframes "uber_morning_slot" , "uber_evening_slot" , "uber_day_slot"
# and uber "uber_night_slot 
# By "Slot and Status" and calculating the "# of requests" as percentage
####################################################################################

d3 <- uber_morning_slot %>% group_by(Pickup.point , Status) %>% summarise(count = n()) %>% mutate(perc = (count/sum(count))*100)
d4 <- uber_evening_slot %>% group_by(Pickup.point , Status) %>% summarise(count = n()) %>% mutate(perc = (count/sum(count))*100)
d5 <- uber_day_slot %>% group_by(Pickup.point , Status) %>% summarise(count = n()) %>% mutate(perc = (count/sum(count))*100)
d6 <- uber_night_slot %>% group_by(Pickup.point , Status) %>% summarise(count = n()) %>% mutate(perc = (count/sum(count))*100)

####################################################################
#Demand = Total number of requests/Trips Incomplete
#Supply = Total number of requests - that have status "Completed"
####################################################################

###################################################################
#Stacked bar chart showing the all the request status as percentage
###################################################################

ggplot(d11 , aes(x = factor(""), y = perc , fill = Status)) + 
scale_fill_manual("Status", values = c("Cancelled" = "red", "No Cars Available" = "orange", "Trip Completed" = "green")) +
geom_col(position = "stack" , width = 0.2) +
geom_text(aes(label=scales::percent(perc/100)), size = 3, position = position_stack(vjust = 0.6)) +
labs(title = "Breakdown of Requests" , x = "Slot" , y = "Num Of Requests" , fill = "Status") +  
scale_y_continuous(labels = function(x){ paste0(x, "%") }) +
annotate("text", x=0,y=100,label="Overall Gap b/w Demand & Supply = 58%", hjust=-0.01,vjust=1)

########################################################################
# Stacked bar chart showing the all the request status (as Percentage) 
#   VS
# Pickup.point status 
########################################################################

ggplot(d12 , aes(x = Pickup.point, y = perc , fill = Status)) + 
scale_fill_manual("Status", values = c("Cancelled" = "red", "No Cars Available" = "orange", "Trip Completed" = "green")) +
geom_col(position = "stack" , width = 0.3) +
geom_text(aes(label=scales::percent(perc/100)), size = 3, position = position_stack(vjust = 0.6)) +
labs(title = "Breakdown of Requests from Pickup point perspective" , x = "Pickup point" , y = "Num Of Requests" , fill = "Status") +  
scale_y_continuous(labels = function(x){ paste0(x, "%") }) 

##############################################################################################################
#Stacked bar chart :  (Represented as percentage)
#   X axis - "Slot"
#   Y axis - "No of Requests" (Represented as percentage)
#   Breakdown of requests as "Cancelled" , "Trips completed" , "No Cars available" (Represented as percentage)
##############################################################################################################

ggplot(d2 , aes(x=slot, y = perc , fill = factor(d2$Status))) + 
geom_col(width = 0.5) +
scale_fill_manual("Status", values = c("Cancelled" = "red", "No Cars Available" = "orange", "Trip Completed" = "green")) +
geom_text(aes(label=scales::percent(perc/100)), size = 3, position = position_stack(vjust = 0.6)) +  
labs(title = "Breakdown of #requests as % in time slots" , x = "Slot" , y = "Number Of Requests in %" , fill = "Status") +
scale_y_continuous(labels = function(x){ paste0(x, "%") })

##############################################################################################################
#Stacked bar chart :  (Represented as percentage)
#   X axis - "Pickup Point"
#   Y axis - "No of Requests" (Represented as percentage) made in the "Morning Slot"
#   Breakdown of requests as "Cancelled" , "Trips completed" , "No Cars available" (Represented as percentage)
##############################################################################################################

ggplot(d3 , aes(x=Pickup.point, y = perc , fill = factor(d3$Status))) + 
scale_fill_manual("Status", values = c("Cancelled" = "red", "No Cars Available" = "orange", "Trip Completed" = "green")) + 
geom_col() +
geom_text(aes(label=scales::percent(perc/100)), size = 3, position = position_stack(vjust = 0.6)) +  
labs(title = "Cab Status in Morning slot" , x = "Pickup Point" , y = "Num Of Requests in %" , fill = "Status") +
scale_y_continuous(labels = function(x){ paste0(x, "%") })

##############################################################################################################
#Stacked bar chart :  (Represented as percentage)
#   X axis - "Slot"
#   Y axis - "No of Requests" (Represented as percentage) made in the "Evening Slot"
#   Breakdown of requests as "Cancelled" , "Trips completed" , "No Cars available" (Represented as percentage)
##############################################################################################################

ggplot(d4 , aes(x=Pickup.point, y = perc , fill = factor(d4$Status))) + 
scale_fill_manual("Status", values = c("Cancelled" = "red", "No Cars Available" = "orange", "Trip Completed" = "green")) +
geom_col() +
geom_text(aes(label=scales::percent(perc/100)), size = 3, position = position_stack(vjust = 0.6)) +  
labs(title = "Demand Supply Gap in Evening slot" , x = "Pickup Point" , y = "Num Of Requests in %" , fill = "Status") +
scale_y_continuous(labels = function(x){ paste0(x, "%") })


##################################################################
#Demand & Supply Gap in "Morning Slot
##################################################################

ggplot(d3 , aes(x=Pickup.point, y = count , fill = factor(d3$Status))) + 
scale_fill_manual("Status", values = c("Cancelled" = "red", "No Cars Available" = "orange", "Trip Completed" = "green")) + 
geom_col() +
geom_text(aes(label=count), size = 3, position = position_stack(vjust = 0.6)) +  
labs(title = "Demand Supply Gap in Morning slot" , x = "Pickup Point" , y = "Num Of Requests" , fill = "Status") +
annotate("text" , x = 0 , y = 1990 , label = "Demand / Supply Gap at City in Morning = 1310" ,hjust=-.03,vjust=1) +
annotate("text" , x = 0 , y = 1930 , label = "Demand = 1845" ,hjust=-.1,vjust=1) + 
annotate("text" , x = 0 , y = 1870 , label = "Supply = 535" ,hjust=-.1,vjust=1)

####################################################################
#Demand & Supply Gap in "Late Evening Slot"
####################################################################

ggplot(d4 , aes(x=Pickup.point, y = count , fill = factor(d4$Status))) + 
scale_fill_manual("Status", values = c("Cancelled" = "red", "No Cars Available" = "orange", "Trip Completed" = "green")) +
geom_col() +
geom_text(aes(label=count), size = 3, position = position_stack(vjust = 0.6)) +  
labs(title = "Demand Supply Gap in Evening slot" , x = "Pickup Point" , y = "Num Of Requests" , fill = "Status") +
annotate("text" , x = 0 , y = 2100 , label = "Demand / Supply Gap at Airport in Evening = 1530" ,hjust=-.03,vjust=1) +
annotate("text" , x = 0 , y = 2030 , label = "Demand = 1983" ,hjust=-.1,vjust=1) + 
annotate("text" , x = 0 , y = 1970 , label = "Supply = 453" ,hjust=-.1,vjust=1)