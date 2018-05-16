library(dplyr)
library(tidyr)

################################################################################
# DATA LOADING
################################################################################
# From the pdf <URL?> mentioned in the assignment, below is the list
# of countries (in thier ISO codes) which has english as their official language
english_speaking_countries <- c('BWA', 'CMR', 'ETH', 'ERI', 'GMB', 'GHA', 'KEN',
                                'LSO', 'LBR', 'MWI', 'MUS', 'NAM', 'NGA', 'RWA',
                                'SYC', 'SLE', 'ZAF', 'SSD', 'SDN', 'SWZ', 'TZA',
                                'UGA', 'ZMB', 'ZWE', 
                                'ATG', 'BHS', 'BRB', 'BLZ', 'CAN', 'DMA', 'GRD',
                                'GUY', 'JAM', 'KNA', 'LCA', 'VCT', 'TTO', 'USA',
                                'IND', 'PAK', 'PHL', 'SGP',
                                'AUS', 'FJI', 'KIR', 'MHL', 'FSM', 'NRU', 'NZL',
                                'PLW', 'PNG', 'WSM', 'SLB', 'TON', 'TUV', 'VUT',
                                'IRL', 'MLT', 'GBR')

# Reading the companies.txt file in companies data frame
# a. Using the separator as "\t".
# b. Ensuring all the empty values are read as "NA".
companies <- read.delim("companies.txt" , sep = "\t" , na.strings = c("", " ", "NA"),
                        stringsAsFactors = FALSE)

# Reading the rounds2.csv in rounds2 data frame.
# Ensuring all the empty values are read as "NA".
rounds2 <- read.csv("rounds2.csv" , na.strings = c("", " ", "NA") , stringsAsFactors = FALSE)

################################################################################
# DATA CLEANUP
################################################################################
# 1. convert the 'permalink' column of 'companies' to lower case.
companies$permalink <- tolower(companies$permalink)

# 2. Updating the value of country_code to "Missing", if it is NA.
companies$country_code <- toupper(companies$country_code)
companies$country_code[is.na(companies$country_code)] <- "Missing"

# 3. Convert the 'company_permalink' column of the 'rounds2' to lower case.
rounds2$company_permalink <- tolower(rounds2$company_permalink)

# 4. Convert the 'funding_round_type' column of the 'rounds2' to lower case.
rounds2$funding_round_type <- tolower(rounds2$funding_round_type)

################################################################################
# Checkpoint 1
################################################################################
# 1. No of unique companies in rounds2 dataframe.
no_of_uniq_companies_round2 <- length(unique(rounds2$company_permalink))

# 2. No of unique companies in companies dataframe.
# NOTE: We do not need 'unique' here since we are claiming that permalink is 
#          supposed to be unique.
no_of_uniq_companies <- length(companies$permalink)

# 4. To find if there are any companies in the rounds2 file which are not present in companies
diff <- anti_join(rounds2 , companies , by = c("company_permalink" = "permalink" ))

# NOTE: Alternate way to find without doing a join 
# additional_companies <- sum(! rounds2$company_permalink %in% companies$permalink) >= 1

# 5. Merge the round2 and companies to find the number of observations in the master_frame
master_frame <- full_join(rounds2 , companies ,  by = c("company_permalink" = "permalink" ))
no_of_obs_in_master_frame <- nrow(master_frame)

# Writing the data frame to a csv file for analysis TABLEAU:
master_frame <- subset(master_frame , !is.na(raised_amount_usd))
write.csv(master_frame , file = "master_frame_DDA1730346.csv")

################################################################################
# Checkpoint 2: Funding type Analysis
################################################################################
## Calculating the most suitable funding type
## Filtering the required funding types : "seed , angel , venture , private_equity"
## grouping them by funding_round_type and summarising as per their "Average amount of Investment"
funding_types <- c('venture', 'seed', 'angel', 'private_equity')
average_amount_funding_types <- filter(master_frame , funding_round_type %in% funding_types) %>%
                group_by(funding_round_type) %>%
                summarise(Average_Investment = mean(raised_amount_usd , na.rm = TRUE)) %>%
                arrange(desc(Average_Investment)) 

## filtering the funding type that lies in the range of 5M - 15M
most_suitable_funding_type <- filter(average_amount_funding_types , 
                                     between(Average_Investment, 5000000, 15000000)) %>%
                              select(funding_round_type)

# Now we have creaed a new data frame "master_frame_venture"
# filtering the records from master_frame where investment type is as found above (which is 'venture')
master_frame_venture <- filter(master_frame , funding_round_type %in% most_suitable_funding_type)

################################################################################
# Checkpoint 3: Country Analysis
################################################################################

# 1. Spark Funds wants to see the top nine countries which have received the highest total funding 
#    (across ALL sectors for the chosen investment type)
# a. Grouping by the country code
# b. summarize the money invested
# c. Get the top 9 based on 'Total_Investment' column.
top9 <- group_by(master_frame_venture , country_code) %>% 
        summarise(Total_Investment = sum(raised_amount_usd , na.rm = TRUE)) %>% 
        arrange(desc(Total_Investment)) %>%
        head(n=9)
        

# 2. Use the 'english_speaking_countries' map to find which countries are english speaking
top9$eng_official_lang <- top9$country_code %in% english_speaking_countries

# 3. top 3 English-speaking countries in the data frame top9
top3 <- subset(top9 , top9$eng_official_lang == TRUE)[1:3,]

################################################################################
# Checkpoint 4: Sector Analysis 1
################################################################################

# 1. Reading the mapping.csv file in mapping data frame.
#    Ensuring all the empty values are read as "NA"
mapping <- read.csv("mapping.csv" , na.strings = c("", " ", "NA") , check.names = FALSE)

# 2. Cleanup of mapping data frame.
#    a. deleting the row that has empty category list
#    b. Converting the mapping data frame from wide format to long format
#    c. Deleting the asymmetric values
#    d. Remove the 'value' column as it only has 1 values
#    e. Cleaning the data as at most places "0"(zero) is there instead of "na"
#       So updating the right value.
#    f. Convert the category_list to lower case in 'master_frame_venture' data frame
#    g. Convert the category_list to lower case in 'mapping_newdata' data frame

mapping <-subset(mapping , !is.na(category_list))
mapping_newdata <- gather(mapping , category , value , 
                          "Automotive & Sports":"Social, Finance, Analytics, Advertising")
mapping_newdata <- mapping_newdata[!(mapping_newdata$value == 0),]
mapping_newdata <- mapping_newdata[,-3]
mapping_newdata$category_list <- gsub("0" , "na" ,mapping_newdata$category_list)
master_frame_venture$category_list <- tolower(master_frame_venture$category_list)
mapping_newdata$category_list <- tolower(mapping_newdata$category_list)

# 1.Extract the primary sector of each category list from the category_list column
master_frame_venture <- separate(master_frame_venture , category_list , c("primary_sector"),
                                 sep="\\|" , remove = FALSE , extra = "drop")

# 2.Use the mapping file 'mapping.csv' to map each primary sector to one of the eight main sectors
#   (Note that 'Others' is also considered one of the main sectors)
# Merging the master_frame_venture and mapping_new_data

master_frame_venture <- inner_join(master_frame_venture , mapping_newdata , 
                                   by = c("primary_sector" = "category_list"))
colnames(master_frame_venture)[17] <- "main_sector"

#Writing this data frame to a csv file for analysis in TABLEAU:
write.csv(master_frame_venture , file = "master_frame_venture_mapping_DDA1730346.csv")

################################################################################
# Checkpoint 5: Sector Analysis 2
################################################################################

#----------------
# Functions
#----------------
#################################################################################
# Returning the company with highest investment in a specific "main sector"
# Parameters:
#    df     - The data frame which need to be operated upon
#    sector - The sector which need to be sliced upon
#
#################################################################################

# This function returns the name of the company
# once we get the required permalink from the below function.
# This function is called internally from the below function.
# Since companies are characterized by permalink (uniquely).
# Parameters:
  # df - data frame:
  # permalink : uniue permalink for the company that has rasied max inv.

get_company_name <- function(df , permalink) {
  filter(df, company_permalink == permalink) %>%
  select(name) %>% 
  head(n=1)
}

get_company <- function(df , sector) {
  permalink <- filter(df , main_sector == sector) %>% 
    group_by(company_permalink) %>% 
    summarise(Tot_amount_raised = sum(raised_amount_usd)) %>% 
    arrange(desc(Tot_amount_raised)) %>% 
    head(n=1) %>% 
    select(company_permalink) %>% 
    as.character()
    return(get_company_name(df , permalink))
}

##################################################################################
# summary_for_country :
# This is a method to perform analysis on the data frame based on given
# country code.
# 
# 1. filters the data set by given country code and for investments made 
#    within 5M to 15M USD
# 2. Find the sector wise number of investments.
# 3. Calculate the total amount of investment made per sector
# 4. Merges the 'sector wise investment count' and 'sector wise total investment' 
#    with the main data frame.
# 5. Calculate the total investments made
# 6. Calculate the total number of investments made.
# 7. Find the top 3 sectors and their total investments.
# 8. Find the company highest invested into among the top sector
# 9. Find the company most invested into among the top#2 sector
#
# Parameters:
#    master_data_frame - The data frame on which the analysis is to be made.
#    cntry_code        - The country code against which slicing is to be performed
####################################################################################
summary_for_country <- function(master_data_frame, cntry_code) {
  country_data_frame <- subset(master_data_frame , country_code == cntry_code & 
                                 between(raised_amount_usd, 5000000, 15000000)
                                )
  
  no_of_inv <- group_by(country_data_frame , main_sector) %>% 
    summarise(no_of_investments = n()) %>% 
    arrange(desc(no_of_investments)) 
  sum_of_inv <- group_by(country_data_frame , main_sector) %>% 
    summarise(Sum_of_total_Investments = sum(raised_amount_usd , na.rm = T)) %>% 
    arrange(desc(Sum_of_total_Investments))
  
  country_data_frame <- inner_join(country_data_frame , no_of_inv , by = "main_sector" )
  country_data_frame <- inner_join(country_data_frame , sum_of_inv , by = "main_sector"  )
  
  # Total number of investment in country 1
  total_no_of_inv <- sum(no_of_inv$no_of_investments)
  
  # Total amount of investment in country 1
  total_amount_of_inv <- sum(sum_of_inv$Sum_of_total_Investments)
  
  
  # Top 3 investment sectors (count-wise)
  top3_no_of_inv <- no_of_inv %>% top_n(3, no_of_investments)
  
  
  country_summary <- c(total_no_of_inv, total_amount_of_inv, 
                       top3_no_of_inv[1,1], top3_no_of_inv[2,1], top3_no_of_inv[3,1],
                       top3_no_of_inv[1,2], top3_no_of_inv[2,2], top3_no_of_inv[3,2],
                       as.character(get_company(country_data_frame , as.character(top3_no_of_inv[1,1]))),
                       as.character(get_company(country_data_frame , as.character(top3_no_of_inv[2,1]))))
  return(country_summary)
}

# Create an empty data frame to which we will add records for each country analyzed.
summary_frame <- data.frame(no_of_investments = numeric(),
                            total_investment = double(),
                            top_sector_1 = character(),
                            top_sector_2 = character(),
                            top_sector_3 = character(),
                            top_sector_1_inv = numeric(),
                            top_sector_2_inv = numeric(),
                            top_sector_3_inv = numeric(),
                            highest_1_inv_comp = character(),
                            highest_2_inv_comp = character(),
                            stringsAsFactors = FALSE)
for(country_code in top3$country_code) {
  summary_frame[nrow(summary_frame) + 1, ] <- summary_for_country(master_frame_venture, country_code)
}
 