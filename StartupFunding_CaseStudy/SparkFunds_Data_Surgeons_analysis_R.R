#----------------- SETUP --------------#
## Package installation check ##
### Doing check for required packages ##
required_packages <- c("dplyr","countrycode","tidyr","pryr","stringr")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(new_packages)) install.packages(new_packages)

library(dplyr)
library(countrycode)
library(tidyr)
library(stringr)

#-------------- SETUP - END --------------#

#-------------- SETTING UP WD ------------#
## Set working directory where all files have been kept on your system ##
setwd("C:/Upgrad")
getwd()
#----------------------------------------#

#------------ IMPORTING SOURCE FILES --------- #
## Note: Import the modified companies_csv.csv file, as the original file is tab separated"
companies <- read.delim("companies.txt",sep = "\t", stringsAsFactors = FALSE)

## Reading rounds2 csv file ##
rounds2 <- read.csv("rounds2.csv", stringsAsFactors = FALSE)

## Reading mapping.csv file ##
mapping <- read.csv("mapping.csv",stringsAsFactors = FALSE,check.names = FALSE)

## Reading English speaking base file ##
eng_country_base <- read.delim("english_speaking_countries.txt",sep="\n")
#---------------- ALL DONE -------------------#
  
  cat("--------- DATA CLEANING ---------\n")
  ## Data Cleaning Section ##
  ## Note : It seems companies$permalink and rounds2$company_permalink has mixed values ##
  ##      : i.e. some values in caps and some in small letters. It is better to convert ##
  ##      : everythink to small letters                                                 ##
  companies$permalink <- tolower(companies$permalink)
  rounds2$company_permalink <- tolower(rounds2$company_permalink)
  cat("\n\n--------- DATA CLEANING - END ---------\n")
  
  cat("\n\n--------- Checkpoint 1 ---------\n")
  ## Checkpoint 1 ##
  ## Table 1.1 for excel sheel named = "Investment" ##
  ## Question : How many unique companies are present in rounds2? ##
  cat("Q) Number of unique companies present in rounds2 :\n", length(unique(rounds2$company_permalink)), "\n")
  
  ## Question : How many unique companies are present in companies? ##
  cat("Q) Number of unique companies present in companies :\n", length(unique(companies$permalink)), "\n")
  
  ## Question : In the companies data frame, which column can be used as the 
  ##           unique key for each company? Write the name of the column.
  cat("Q) In the companies data frame, which column can be used as the unique key for each company? :\npermalink\n")
  
  ## Question : Are there any companies in the rounds2 file which are not 
  ##            present in companies? Answer yes or no: Y/N
  diff_df <- dplyr::setdiff(unique(rounds2$company_permalink),unique(companies$permalink))
  if (length(diff_df) == 0) {
    cat("Q) Are there any companies in the rounds2 file which are not present in companies? Answer yes or no: Y/N :\nN\n")
  } else {
    cat("Q) Are there any companies in the rounds2 file which are not present in companies? Answer yes or no: Y/N :\nY\n")
  }
  
  ## Question : Merge the two data frames so that all variables (columns) in the 
  ##          : companies frame are added to the rounds2 data frame. Name the merged 
  ##          : frame master_frame. How many observations are present in master_frame?
  master_frame <- dplyr::full_join(rounds2,companies,by=c("company_permalink"="permalink"))
  cat("Q) How many observations are present in master_frame? :\n", nrow(master_frame), "\n")
  cat("\n\n--------- Checkpoint 1 - END ---------\n")
  
  cat("\n\n--------- Checkpoint 2 ---------\n")
  ## Checkpoint 2 ##
  ## Table 2.1 : NA Values Treatment  ##
  
   ## Question : How many NA values are present in the column raised_amount_usd?
   cat("Q) How many NA values are present in the column raised_amount_usd? :\n", length(which(is.na(master_frame$raised_amount_usd))), "\n")
   
   ## Question : What do you replace NA values of raised_amount_usd with? Enter a numeric value.
   cat("Q) What do you replace NA values of raised_amount_usd with? :\n-1\n")
   master_frame[which(is.na(master_frame$raised_amount_usd)),'raised_amount_usd'] = -1
   
   ## Question : Average funding amount of venture type ##
   mean_venture <- (master_frame %>% 
                      filter(funding_round_type=="venture",
                             raised_amount_usd != -1,
                             !is.na(raised_amount_usd)) %>% 
                      summarize(avg = mean(raised_amount_usd)))[,'avg']
   cat("Q) Average funding amount of venture type :\n", mean_venture,"\n")
   
   ## Question : Average funding amount of angel type
   mean_angel <- (master_frame %>% 
                    filter(funding_round_type=="angel",
                           raised_amount_usd != -1,
                           !is.na(raised_amount_usd)) %>% 
                    summarize(avg = mean(raised_amount_usd)))[,'avg']
   cat("Q) Average funding amount of angel type :\n", mean_angel,"\n")
   
   ## Question : Average funding amount of seed type
   mean_seed <- (master_frame %>% 
                   filter(funding_round_type=="seed",
                          raised_amount_usd != -1,
                          !is.na(raised_amount_usd)) %>% 
                   summarize(avg = mean(raised_amount_usd)))[,'avg']
   cat("Q) Average funding amount of seed type :\n", mean_seed,"\n")
   
   ## Question : Average funding amount of private equity type
   mean_private_equity <- (master_frame %>% 
                             filter(funding_round_type=="private_equity",
                                    raised_amount_usd != -1,
                                    !is.na(raised_amount_usd)) %>% 
                             summarize(avg = mean(raised_amount_usd)))[,'avg']
   cat("Q) Average funding amount of private_equity type :\n", mean_private_equity,"\n")
   cat("\n\n--------- Checkpoint 2 - END ---------\n")
   
   cat("\n\n--------- Checkpoint 3 ---------\n")
   ## Checkpoint 3: Funding Type Analysis
   ## Question : Considering that Spark Funds wants to invest between 5 to 15 million USD 
   ##          : per investment round, which investment type is the most suitable for it?
   
   ## Note : Building a mean data frame which will have mean values associated with each
   ##       type of investment
   mean.df <- data.frame(mean_type=c("venture","angel","seed","private_equity"),
                         avg=c(mean_venture,mean_angel,mean_seed,mean_private_equity),
                         stringsAsFactors = FALSE)
   preferred_investment <- (mean.df %>% filter(avg > 5000000 & avg < 15000000))$mean_type
   cat("Q) Which investment type is the most suitable for it :\n", preferred_investment,"\n")
   
   cat("\n\n--------- Checkpoint 3 - END ---------\n")
   
   cat("\n\n--------- Checkpoint 4 ---------\n")
   ## Checkpoint 4 : Country Analysis ##
   
   ## Note : Conditions for top9
   ## Condition 1 : funding_route_type = perferred_investment selected above
   ## Condition 2 : country_code shoud not be ""
   ## Condition 3 : raised_amount_usd != -1 & != NA (though we have changed NAs above, but just for sanity)
   ## Grouping by country_code
   ## Summarizing by sum(raised_amount_usd)
   ## arranging in descending order
   ## taking first 9 observations
   top9 <- master_frame %>% 
     filter(funding_round_type == preferred_investment,
            country_code != "",raised_amount_usd != -1,
            !is.na(raised_amount_usd)) %>% 
     group_by(country_code) %>% 
     summarize(total_investment = sum(raised_amount_usd)) %>% 
     arrange(desc(total_investment)) %>% slice(1:9)
   
   #### BELOW CODE IS EXTREMELY SLOW, and it is helpful when plotting graphs in tableau.
   #### Since it is not required as per questions, hence omitting for code performance
   #master_frame$english_speaking <- sapply(master_frame$country_code,
   #                                          function(x) ifelse((!is.na(x) & x != ""),
   #                                                             countrycode(x,'iso3c','country.name') %in% eng_country_base$Country,
   #                                                             x))
   top9$country_name <- countrycode(top9$country_code,'iso3c', 'country.name')
   
   top_english_all <- top9 %>% filter(country_name %in% eng_country_base$Country)
   top_english_1 <- data.frame(top9 %>% filter(country_name %in% eng_country_base$Country) %>% slice(1))
   top_english_2 <- data.frame(top9 %>% filter(country_name %in% eng_country_base$Country) %>% slice(2))
   top_english_3 <- data.frame(top9 %>% filter(country_name %in% eng_country_base$Country) %>% slice(3))
   
   cat("Q) Top English-speaking country :\n",top_english_1$country_name,"\n")
   cat("Q) Second English-speaking country :\n",top_english_2$country_name,"\n")
   cat("Q) Third English-speaking country :\n",top_english_3$country_name,"\n")
   cat("\n\n--------- Checkpoint 4 - END ---------\n")

   cat("\n\n--------- Checkpoint 5 ---------\n")
   ## Checkpoint 5: Sector Analysis 1 ##

   ## There are lot of 'na' being replaced as 0 in the data, so doing a gsub
   mapping$category_list <- gsub("0","na",mapping$category_list)
   
   ## converting 0 -> na would trouble Enterprise 2.0 as well, just reverting just this ##
   mapping[244,1] = "Enterprise 2.0"
   
   ## Below code is extremely slow, moving to dplyr::gather functionality
   #mapping$main_sector <- sapply(mapping$category_list,
   #                                function(x) colnames(mapping)[which(mapping[which(mapping$category_list == x),] == 1)])
   mapping <- gather(mapping, key=main_sector,values=c(2:ncol(mapping))) %>% filter(main_sector != "category_list",value != 0)
   
  
   ## Below sapply seems to be extremely slow, moving to tidyr's separate functionality ## 
   #master_frame$primary_sector <- sapply(master_frame$category_list,function(x) ifelse(!is.na(x),strsplit(x,'|',fixed=TRUE)[[1]][1],x))
   #master_frame <- master_frame %>% separate(category_list,c("primary_sector","newcol2"),sep="\\|") %>% select(-newcol2)
   
   #### IMPORTANT NOTE : Above separate funtionality is good to use, as the performance is quite high
   ####                  However, above execution is giving warnings because of lengths of filtered objects
   ####                  Hence resorting to stringr::str_split function
   master_frame$primary_sector <- sapply(str_split(master_frame$category_list,pattern="\\|"),function(x) x[1])
   ## Below is not needed any longer ##
   #master_frame$primary_sector[which(is.na(master_frame$primary_sector))] <- ""
   
   mf_temp <- master_frame %>% select(primary_sector)
   mf_temp$primary_sector <- tolower(mf_temp$primary_sector)
   mapping$category_list <- tolower(mapping$category_list)
   master_frame$main_sector <- full_join(mf_temp,mapping,by=c("primary_sector"="category_list"))$main_sector
   master_frame[which(is.na(master_frame$main_sector)),'main_sector'] <- "Blanks"
   cat("\n\n--------- Checkpoint 5 - END ---------\n")
   
   cat("\n\n--------- Checkpoint 6 ---------\n")
   ## Checkpoint 6: Sector Analysis 2 ##
   # Create three separate data frames D1, D2 and D3 for each of the three countries containing the 
   # observations of funding type FT falling within the 5-15 million USD range. The three data frames 
   # should contain:
   #   All the columns of the master_frame along with the primary sector and the main sector
   #   The total number (or count) of investments for each main sector in a separate column
   #   The total amount invested in each main sector in a separate column
   
   top_sector_company <- function(mydf,slice_value,mode) {
     ## mode = 1 : Find the main_sector according to slice_value given, whether 1st/2nd/3rd etc.
     ## mode = 2 : Find the sector_invest_cnt based on slice_value given, whether 1st/2nd/3rd etc.
     ## mode = 3 : Find the top company name in the respective df given according to sector given by 
     ##            slice value, getting the highest investment.
     if (mode == 1) {
       return((unique(mydf %>% 
                        select(main_sector,sector_invest_cnt)) %>% 
                 arrange(desc(sector_invest_cnt)) %>% 
                 slice(slice_value))$main_sector)
     } else if (mode == 2) {
       return((unique(mydf %>% 
                        select(main_sector,sector_invest_cnt)) %>% 
                 arrange(desc(sector_invest_cnt)) %>% 
                 slice(slice_value))$sector_invest_cnt)  
     } else {
       c_permalink <- (mydf %>% 
                         filter(main_sector == top_sector_company(mydf,slice_value,1)) %>% 
                         group_by(company_permalink) %>% 
                         summarize(total_invest = sum(raised_amount_usd)) %>% 
                         arrange(desc(total_invest)) %>% 
                         slice(1))$company_permalink
       return((mydf %>% filter(company_permalink == c_permalink) %>% select(name) %>% slice(1))$name)
     }
   }
   
   country_1 <- top_english_1$country_code
   country_2 <- top_english_2$country_code
   country_3 <- top_english_3$country_code
   country_1_nm <- top_english_1$country_name
   country_2_nm <- top_english_2$country_name
   country_3_nm <- top_english_3$country_name
   FT <- preferred_investment
   
   D1 <- master_frame %>% filter(country_code == country_1,funding_round_type == FT,(raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000),main_sector != "Blanks")
   D2 <- master_frame %>% filter(country_code == country_2,funding_round_type == FT,(raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000),main_sector != "Blanks")
   D3 <- master_frame %>% filter(country_code == country_3,funding_round_type == FT,(raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000),main_sector != "Blanks")
   
   D1_sector_wise_investment <- D1 %>% group_by(main_sector) %>% summarize(sector_invest_cnt = length(company_permalink),sector_invest_amt = sum(raised_amount_usd))
   D1 <- full_join(D1,D1_sector_wise_investment,by="main_sector")
   
   D2_sector_wise_investment <- D2 %>% group_by(main_sector) %>% summarize(sector_invest_cnt = length(company_permalink),sector_invest_amt = sum(raised_amount_usd))
   D2 <- full_join(D2,D2_sector_wise_investment,by="main_sector")
   
   D3_sector_wise_investment <- D3 %>% group_by(main_sector) %>% summarize(sector_invest_cnt = length(company_permalink),sector_invest_amt = sum(raised_amount_usd))
   D3 <- full_join(D3,D3_sector_wise_investment,by="main_sector")
   
   ##Question 1
   ## Total number of investments (count)
   cat("Q) Total number of investment in\ncountry = ",country_1_nm, "\nis -> ", nrow(D1),"\n")
   cat("Q) Total number of investment in\ncountry = ",country_2_nm, "\nis -> ", nrow(D2),"\n")
   cat("Q) Total number of investment in\ncountry = ",country_3_nm, "\nis -> ", nrow(D3),"\n")
   
   ##Question 2
   ## Total amount of investment (USD)
   cat("Q) Total amount of investment in\ncountry = ",country_1_nm, "\nis -> $", sum(D1$raised_amount_usd),"\n")
   cat("Q) Total amount of investment in\ncountry = ",country_2_nm, "\nis -> $", sum(D2$raised_amount_usd),"\n")
   cat("Q) Total amount of investment in\ncountry = ",country_3_nm, "\nis -> $", sum(D3$raised_amount_usd),"\n")
   
   ##Question 3
   ## Top sector (based on count of investments)
   cat("Q) Top sector based on count of investment for\ncountry = ",country_1_nm,"\nsector ->", top_sector_company(D1,1,1),"\n")
   cat("Q) Top sector based on count of investment for\ncountry = ",country_2_nm,"\nsector ->", top_sector_company(D2,1,1),"\n")
   cat("Q) Top sector based on count of investment for\ncountry = ",country_3_nm,"\nsector ->", top_sector_company(D3,1,1),"\n")
   
   ## Question 4
   ##  Second-best sector (based on count of investments)
   cat("Q) Second-best sector based on count of investment for\ncountry = ",country_1_nm,"\nsector ->", top_sector_company(D1,2,1),"\n")
   cat("Q) Second-best sector based on count of investment for\ncountry = ",country_2_nm,"\nsector ->", top_sector_company(D2,2,1),"\n")
   cat("Q) Second-best sector based on count of investment for\ncountry = ",country_3_nm,"\nsector ->", top_sector_company(D3,2,1),"\n")   
   
   ## Question 5 
   ##  5. Third-best sector (based on count of investments
   cat("Q) Third-best sector based on count of investment for\ncountry = ",country_1_nm,"\nsector ->", top_sector_company(D1,3,1),"\n")
   cat("Q) Third-best sector based on count of investment for\ncountry = ",country_2_nm,"\nsector ->", top_sector_company(D2,3,1),"\n")
   cat("Q) Third-best sector based on count of investment for\ncountry = ",country_3_nm,"\nsector ->", top_sector_company(D3,3,1),"\n")
   
   ## Question 6
   ## 6. Number of investments in the top sector (refer to point 3)
   cat("Q) Number of investments in the\ntop sector = ",top_sector_company(D1,1,1), "\ncountry = ",country_1_nm,"\nis ->",top_sector_company(D1,1,2),"\n")
   cat("Q) Number of investments in the\ntop sector = ",top_sector_company(D2,1,1), "\ncountry = ",country_2_nm,"\nis ->",top_sector_company(D2,1,2),"\n")
   cat("Q) Number of investments in the\ntop sector = ",top_sector_company(D3,1,1), "\ncountry = ",country_3_nm,"\nis ->",top_sector_company(D3,1,2),"\n")
   
   ## Question 7
   ## 7. Number of investments in the second-best sector (refer to point 4)
   cat("Q) Number of investments in thr\nsecond-best sector = ",top_sector_company(D1,2,1), "\ncountry = ",country_1_nm,"\nis ->",top_sector_company(D1,2,2),"\n")
   cat("Q) Number of investments in the\nsecond_best sector = ",top_sector_company(D2,2,1), "\ncountry = ",country_2_nm,"\nis ->",top_sector_company(D2,2,2),"\n")
   cat("Q) Number of investments in the\nsecond_best sector = ",top_sector_company(D3,2,1), "\ncountry = ",country_3_nm,"\nis ->",top_sector_company(D3,2,2),"\n")

   ## Question 8
   ## 8. Number of investments in the third-best sector (refer to point 5)
   cat("Q) Number of investments in the\nthird-best sector = ",top_sector_company(D1,3,1), "\ncountry = ",country_1_nm,"\nis ->",top_sector_company(D1,3,2),"\n")
   cat("Q) Number of investments in the\nthird_best sector = ",top_sector_company(D2,3,1), "\ncountry = ",country_2_nm,"\nis ->",top_sector_company(D2,3,2),"\n")
   cat("Q) Number of investments in the\nthird_best sector = ",top_sector_company(D3,3,1), "\ncountry = ",country_3_nm,"\nis ->",top_sector_company(D3,3,2),"\n")
   
   ## Question 9
   ##  9. For the top sector count-wise (point 3), which company received the highest investment?
   cat("Q) For the top sector count-wise (point 3), which company received the highest investment in\ncountry : ",country_1_nm,"\nsector : ",top_sector_company(D1,1,1), "\ncompany: ",
       (top_sector_company(D1,1,3)),"\n")
   cat("Q) For the top sector count-wise (point 3), which company received the highest investment in\ncountry : ",country_2_nm,"\nsector : ",top_sector_company(D2,1,1), "\ncompany: ",
       (top_sector_company(D2,1,3)),"\n")
   cat("Q) For the top sector count-wise (point 3), which company received the highest investment in\ncountry : ",country_3_nm,"\nsector : ",top_sector_company(D3,1,1), "\ncompany: ",
       (top_sector_company(D3,1,3)),"\n")
   
   ## Question 10:
   ## 10. For the second-best sector count-wise (point 4), which company received the highest investment?
   cat("Q) For the second-best sector count-wise (point 4), which company received the highest investment in\ncountry : ",country_1_nm,"\nsector : ",top_sector_company(D1,2,1), "\ncompany : ",
       (top_sector_company(D1,2,3)),"\n")
   cat("Q) For the second-best sector count-wise (point 4), which company received the highest investment in\ncountry : ",country_2_nm,"\nsector : ",top_sector_company(D2,2,1), "\ncompany : ",
       (top_sector_company(D2,2,3)),"\n")
   cat("Q) For the second-best sector count-wise (point 4), which company received the highest investment in\ncountry : ",country_3_nm,"\nsector : ",top_sector_company(D3,2,1), "\ncompany : ",
       (top_sector_company(D3,2,3)),"\n")
   
   cat("\n\n--------- Checkpoint 6 - END ---------\n")
   
   ## Now returning final memory consumption during the execution of this code ##



