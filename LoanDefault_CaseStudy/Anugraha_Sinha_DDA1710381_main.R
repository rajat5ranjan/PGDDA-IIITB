# ---------------------------------------------------#
# Code Description : PGDDA - Loan default Case Study #
# Author           : Soumadiptya     (DDA1710089)    #
#                  : Ranjidha Rajan  (DDA1710198)    #
#                  : Nihar Behara    (DDA1710367)    #
#                  : Anugraha Sinha (DDA1710381)     #
#----------------------------------------------------#


# IMPORTANT NOTE FOR EVALUATOR :
# 1) In this code details of Predictive Analytics have also been given 
#    from Line 544. We have built a predictive model which has been explained 
#    through code comments in the out best of abilities. The model aims at reducing the 
#    loss incurred by bank in deciding probable defaulting customers. 
#    Evaluation committee is requested to check the predictive analysis as well.


# ------------------------------------ SETUP ----------------------------------------------- #
# Library Import Segment
required_packages <- c("dplyr","tidyr","lubridate","stringr","ggplot2","gridExtra","corrplot")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(new_packages)) install.packages(new_packages)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(gridExtra)
library(corrplot)

# Working Directory
# I am assuming that the working directory will be set where unzip would have been done
# Hence removing the below. If required, uncomment below line and modify the argument
# if setwd() function as per need.

# setwd("C:/Upgrad/")


loan_data <- read.csv("loan.csv",sep=",",stringsAsFactors = TRUE)
#------- Data Cleaning -------#
# Let us take a look at the data #
glimpse(loan_data)

# There are 111 variables and a large no of them seem to have a number of NA values
#  Let us see whether any columns have all NA's and eliminate them if present

# # If the no of NA's in a column equals the total no. of rows then all values in the column are NA
# # Store the column no. of such columns in a variable

# There are many columns with only a single unique value- e.g. NA, 0, F etc. Lets us drop
# all such columns as they will not be useful for EDA or visualisation
columns_with_single_unique <- as.vector(which(sapply(loan_data, function(x) length(unique(x))==1)))

# Drop the columns from loan data set
loan_data <- loan_data[,-columns_with_single_unique]

# As a next step we should also drop columns which only have 0 or NA's as their unique values
columns_with_only_0orNA <- 
  as.vector(which(sapply(loan_data, function(x) length(unique(x))==2 & 
                           sum(unique(x) %in% c(0,NA))==2)))

# Drop these columns
loan_data <- loan_data[,-columns_with_only_0orNA]

# Since there are still a large no. of variables,
# let us look at them 10 at a time to understand which ones may need cleaning or conversion

# - Checking Columns 0-to-10 #
glimpse(loan_data[,1:10])

# term and int_rate are of char format. We need to convert them to numbers

loan_data <- loan_data %>% mutate(int_rate = gsub("%", "", int_rate),
                                  int_rate = as.numeric(int_rate),
                                  term = gsub(" months", "", term),
                                  term = gsub(" ", "", term),
                                  term = as.numeric(term))

# # - Checking Columns 9-to-19 #
glimpse(loan_data[,9:19])
# Convert emp_length to numbers and issue_d to date

loan_data <- loan_data %>% mutate(emp_length = gsub("years", "", emp_length),
                                  emp_length = gsub("year", "", emp_length),
                                  emp_length = gsub("\\+", "", emp_length),
                                  emp_length = gsub("<", "", emp_length),
                                  emp_length = as.numeric(emp_length),
                                  issue_d = paste("01",issue_d,sep="-"),
                                  issue_d = parse_date_time(issue_d, orders = c("dmy"), locale = "eng"))

# Warning message was expected as emp_length had some missing values. We are not imputing them
# with any value as we are not sure of the reason for missing values

# The variables emp_title and url can also be dropped because:-
# 1) Too many unique character values
# 2) Should not have any logical bearing on the loan_status

loan_data <- loan_data %>% select(-emp_title,-url)

# # - Checking Columns 18-to-28 #
glimpse(loan_data[,18:28])

# Convert earliest_cr_line to date format

loan_data <- loan_data %>% mutate(earliest_cr_line = paste("01",earliest_cr_line,sep="-"),
                                  earliest_cr_line = parse_date_time(earliest_cr_line, orders = c("dmy"), locale = "eng"))

# # - Checking Columns 29-to-43 #
glimpse(loan_data[,29:43])

# Convert revol_util to number by removing % sign

loan_data <- loan_data %>% mutate(revol_util = gsub("%", "", revol_util),
                                  revol_util = as.numeric(revol_util))

# Convert last_pymnt_d, next_pymnt_d and last_credit_pull_d to dates

loan_data <- loan_data %>% mutate(last_pymnt_d = paste("01",last_pymnt_d,sep="-"),
                                  last_pymnt_d = parse_date_time(last_pymnt_d, orders = c("dmy"), locale = "eng"),
                                  next_pymnt_d = paste("01",next_pymnt_d,sep="-"),
                                  next_pymnt_d = parse_date_time(next_pymnt_d, orders = c("dmy"), locale = "eng"),
                                  last_credit_pull_d = paste("01",last_credit_pull_d,sep="-"),
                                  last_credit_pull_d = parse_date_time(last_credit_pull_d, orders = c("dmy"), locale = "eng"))

# Note- There are some warnings generated due to blank values in the last_pymnt_d, next_pymnt_d
# and last_credit_pull_d columns
#------- Data Cleaning complete -------#


## Buildup a data set which has only "Charged off Loans" and "Fully Paid" loans for analysis of
## closed loan instruments for understanding the trend

loan_2_check <- loan_data %>% filter(loan_status != "Current")


## Some univariate & segmented univariate analysis

## Different Categorical Variables divided as per Loan Status ##

# 1. Annual Income wise distribution 
# Note: Annual Income has high number of outliers.
#       Ignoring values which are > Q3 + 1.5(IQR)
annual_income_limit <- as.numeric(quantile(loan_2_check$annual_inc)[4] + 1.5*IQR(loan_2_check$annual_inc))
# For ref : # Output = 145000
annual_income_limit
G01 <- ggplot(loan_2_check %>% 
                filter(annual_inc <= annual_income_limit)) + 
  geom_histogram(aes(x=annual_inc,fill=loan_status),bins=60,position="fill",col="black") +
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  labs(title="G01 - Annual Income - Loan Status",x="Annual Income",y="Percentage",fill="Loan Status")

# 2. Home Ownership distribution - fill = loan_status
G02 <- ggplot(data = loan_2_check %>% 
                group_by(home_ownership,loan_status) %>% 
                summarize(cnt=length(id))) + 
  geom_col(aes(x=home_ownership,y=cnt,fill=loan_status),position="fill") + 
  labs(title="G02 - Home Ownership - Loan Status",x="Home Ownership",y="Ratio/Percentage",fill="Loan Status")

# 3. Verification Status
G03 <- ggplot(data = loan_2_check %>% 
                group_by(verification_status,loan_status) %>% 
                summarize(cnt=length(id))) + 
  geom_col(aes(x=verification_status,y=cnt,fill=loan_status),position="fill") + 
  labs(title="G03 - Verification Status - Loan Status",x="Verification Status",y="Ratio/Percentage",fill="Loan Status")

# 4. Public Derogatory Record 
G04 <- ggplot(data = loan_2_check %>% 
                group_by(pub_rec,loan_status) %>% 
                summarize(cnt=length(id))) + 
  geom_col(aes(x=as.factor(pub_rec),y=cnt,fill=loan_status),position="fill") + 
  labs(title="G04 - Public Derogatory Record\nLoan Status",x="No. of Records",y="Ratio/Percentage",fill="Loan Status")

# 5.Public Bankruptcy Records 
G05 <- ggplot(data = loan_2_check %>% 
                group_by(pub_rec_bankruptcies,loan_status) %>% 
                summarize(cnt=length(id))) + 
  geom_col(aes(x=as.factor(pub_rec_bankruptcies),y=cnt,fill=loan_status),position="fill") + 
  labs(title="G05 - Public Bankruptcy Record\nLoan Status",x="No. of Records",y="Ratio/Percentage",fill="Loan Status")

#6. Term of the loan
G06 <- ggplot(data = loan_2_check %>% 
                group_by(term,loan_status) %>% 
                summarize(cnt=length(id))) + 
  geom_col(aes(x=as.factor(term),y=cnt,fill=loan_status),position="fill") + 
  labs(title="G06 - Loan Term Length\nLoan Status",x="Term",y="Ratio/Percentage",fill="Loan Status")

grid.arrange(G01,G02,G03,G04,G05,G06,nrow=2,ncol=3)

# Inference:
# Variables that effect the charge off Vs Fully paid loans
# 1. home_ownership
# 2. verification_status
# 3. public bankruptcy records
# 4. term

# Analyzing further
# More categorical variables divides as per Loan Status ### 

# 7. Sub Grade
G07 <- ggplot(loan_2_check %>% 
                group_by(sub_grade,loan_status) %>% 
                summarize(cnt=length(id))) + 
  geom_col(aes(x=sub_grade,y=cnt,fill=loan_status),position="fill") +
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  labs(title="G07 - Loan Grade Vs Loan Status",x="Grade",y="Percentage",fill="Loan Status")

grid.arrange(G07)

# Sub-grade shows a clear and deep relationship with loan_status #

# 8. Purpose
G08 <- ggplot(loan_2_check) + 
  geom_bar(aes(x=purpose,fill=loan_status),position="fill") +
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  labs(title="G08 - Purpose Vs Loan Status",x="Purpose",y="Percentage",fill="Loan Status")

grid.arrange(G08)

# Logically and from data purpose of loan is an important parameter for loan_status 

# Inference:
# Variables that effect the charge off Vs Fully paid loans
# 1. home_ownership
# 2. verification_status
# 3. public bankruptcy records
# 4. term
# 5. sub_grade
# 6. purpose

# Analyzing Further

# 9. revol_util
# Check NAs in revol_util: (main data)
length(which(is.na(loan_data$revol_util)))
# For future lets see the median & mean of this revol_util
mean(loan_data$revol_util,na.rm=T)
# 48.832
median(loan_data$revol_util,na.rm=T)
# 49.3

# The mean and median are not that far off, we can say that data is evenly distributed
# So we can choose the median for replacement.

loan_data[which(is.na(loan_data$revol_util)),'revol_util'] <- median(loan_data$revol_util,na.rm=TRUE)    # IMPUTATION #
loan_2_check <- loan_2_check <- loan_data %>% filter(loan_status != "Current")

G09 <- ggplot(loan_2_check) + 
  geom_histogram(aes(x=revol_util,fill=loan_status),col="black",position="fill",bins=60) +
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  labs(title="G09 - Revolving credit Vs Loan Status",x="Revolving Utilization",y="Percentage",fill="Loan Status")
  
grid.arrange(G09)

# There seems to be a clear relationship between revol_utilization and loan_status.
# This is a good candidate for binning

loan_data$revol_util_range <- (gsub("]",")",cut(loan_data$revol_util,60,dig.lab = 10)))    # Derived Metrics #
loan_2_check <- loan_2_check <- loan_data %>% filter(loan_status != "Current")

# Inference:
# Variables that effect the charge off Vs Fully paid loans
# 1. home_ownership
# 2. verification_status
# 3. public bankruptcy records
# 4. term
# 5. sub_grade
# 6. purpose
# 7. revol_util

#10. Loan Amount Based on Loan Status
G10 <- ggplot(loan_2_check) + 
  geom_histogram(aes(x=loan_amnt,fill=loan_status),col="black",bins=60,position="fill") +
  scale_x_continuous(breaks=seq(0,36000,3000)) +
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  labs(title="G10 - Loan Amount Vs\nLoan Status",x="Loan Amount",y="Count",fill="Loan Status")

grid.arrange(G10)

# Based on above analysis, there seems to be a relationship between loan amount
# and the number of people defaulting
# Lets create a column for loan_amt_range in the data.frame (make this for complete data set = loan_2)

loan_data$loan_amnt_range <- (gsub("]",")",cut(loan_data$loan_amnt,60,dig.lab = 10)))    # Derived Metrics #
loan_2_check <- loan_2_check <- loan_data %>% filter(loan_status != "Current")


# 11 - addr_state
G11 <- ggplot(data = loan_2_check %>% 
                group_by(addr_state,loan_status) %>%
                summarize(cnt=length(id))) + 
  geom_col(aes(x=addr_state,y=cnt,fill=loan_status), position="fill") + 
  labs(title="G11 - State - Loan Status",x="State",y="Ratio/Percentage",fill="Loan Status")

# 12. delinq_2yrs
G12 <- ggplot(loan_2_check %>% 
                group_by(delinq_2yrs,loan_status) %>% 
                summarize(cnt=length(id))) + 
  geom_col(aes(x=delinq_2yrs,y=cnt,fill=loan_status),position="fill") +
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  scale_x_continuous(breaks=seq(0,12,1)) +
  labs(title="G12 - Delinquent in last 2 years Vs\nLoan Status",x="Number of delinquent",y="Percentage",fill="Loan Status")

grid.arrange(G11,G12,nrow=2,ncol=1)

## addr_state and delinq_2yrs do not seem to be related, whatever correlation exist, it is not consistent ##

# Inference:
# Variables that effect the charge off Vs Fully paid loans
# 1. home_ownership
# 2. verification_status
# 3. public bankruptcy records
# 4. term
# 5. sub_grade
# 6. purpose
# 7. revol_util_range
# 8. loan_amnt_range

# BIVARIATE ANALYSIS #


# Lets see different co-relations between variables

cor_name= c("loan_amnt", "int_rate", "installment", "sub_grade", "annual_inc","dti","revol_util","delinq_2yrs")

cor_var <- select(loan_2_check, one_of(cor_name))
cor_var <- cor_var[complete.cases(cor_var), ]

# Convert the character types to numeric types #
cor_var$num_subgrade <- as.numeric(as.factor(cor_var$sub_grade))

# Remove the character 
cor_var <- select(cor_var, -sub_grade)

M <- cor(cor_var)
corrplot(M, method = "number", 
         title = "Correlation Map of Subgrade & Factors", 
         type = "lower", 
         order = "FPC", 
         number.cex = 1, 
         tl.cex = 0.9,
         bg="Grey")


# Logical conclusion taken up for Bivariate Analysis #
# When a borrower comes to institution with a request for a loan
# 1) Based on borrowers parameters, he is allocated a "GRADE" & SUBGRADE.
# 2) This GRADE/SUBGRADE decides the interest rate/installment etc for the loan
# Therefore we should check the co-relation between grade/sub_grade and other parameters
# If a parameter gives strong relation with grade/subgrade or other parameters then only
# 1 parameter may be considered for taking decision.

## Analysis of Grade/SubGrade with different parameters
## This is being done, so as to understand, if GRADE/SUB_GRADE can be used as a defining
## factor for different continuous variables

# 13. Influence of GRADES on revol_util #
G13 <- ggplot(loan_2_check) + 
  geom_boxplot(aes(x=sub_grade,y=revol_util,fill=grade)) +
  geom_line(data = (loan_2_check %>% group_by(sub_grade) %>% summarize(avg=mean(revol_util,na.rm=TRUE))),aes(x=sub_grade,y=avg,group=1)) +
  scale_y_continuous(breaks=seq(0,100,5)) +
  labs(title="G13 - Grade Vs Revolving Credit Utilization",x="Sub Grade",y="Revolving Credit Utilization(%)",fill="Grade")

grid.arrange(G13)

# There seems to a relationship between the grade of the loan and the revolving credit utilization
# However, this isnt consistent. So it is better if we consider revol_util as a separate variable


# 14. Influence of GRADES on loan amount #
# Note : the line signifies the mean of the dataset
G14 <- ggplot(loan_2_check) + 
  geom_boxplot(aes(x=sub_grade,y=loan_amnt,fill=grade)) + 
  geom_line(data = (loan_2_check %>% 
                      group_by(sub_grade) %>% 
                      summarize(avg=mean(loan_amnt,na.rm=TRUE))),
            aes(x=sub_grade,y=avg,group=1)) +
  scale_y_continuous(breaks=seq(0,36000,1000)) +
  labs(title="G14 - Grades Vs Loan Amount",x="Sub Grades",y="Loan Amount",fill="Grade")

grid.arrange(G14)

# Inference : Grade/sub grade does seem to be related to loan_amnt, but there seem
# to be exceptions to it. It is better to keep loan_amnt separate for analysis.

# Inference:
# Variables that effect the charge off Vs Fully paid loans
# 1. home_ownership
# 2. verification_status
# 3. public bankruptcy records
# 4. term
# 5. sub_grade
# 6. purpose
# 7. revol_util_range
# 8. loan_amnt_range


# Build a separate column for sub_grade for the numerical value #
loan_data$sub_grade_2 <- sapply(loan_data$sub_grade,function(x) str_split(x,"")[[1]][2])
loan_2_check <- loan_2_check <- loan_data %>% filter(loan_status != "Current")

# 15. Influence of DTI on GRADES
# Note : the line signifies the mean DTI in that sub_grade
G15 <- ggplot(loan_2_check) + 
  geom_boxplot(aes(x=sub_grade,y=dti,fill=grade)) + 
  geom_line(data=(loan_2_check %>% 
                    group_by(sub_grade) %>% 
                    summarize(avg_dti=mean(dti,na.rm=TRUE))),
            aes(x=sub_grade,y=avg_dti,group=1)) +
  scale_y_continuous(breaks=seq(0,32,1)) +
  labs(title="G15 - Grades Vs DTI",x="Sub Grade",y="DTI",fill="Grade")

grid.arrange(G15)

# Since median & median both seems to justify the distribution in a just manner, 
# lets look it from a different perspective

G16 <- ggplot(data = loan_2_check %>% 
                group_by(grade,sub_grade_2) %>% 
                summarize(med_dti = median(dti,na.rm=TRUE)),aes(x=grade,y=sub_grade_2,value=med_dti)) + 
  geom_tile(aes(fill=med_dti)) +
  geom_text(aes(label=med_dti),col="white") +
  labs(title="G16 - DTI (Median) Vs Grades",x="Grade",y="Sub Grade",fill="Median DTI")

# DTI Vs Grades Vs Loan Status
G17 <- ggplot(data = loan_2_check %>% 
                group_by(grade,sub_grade_2,loan_status) %>% 
                summarize(med_dti = median(dti,na.rm=TRUE)),aes(x=grade,y=sub_grade_2,value=med_dti)) + 
  geom_tile(aes(fill=med_dti)) +
  geom_text(aes(label=med_dti),col="white") + 
  labs(title="G17 - DTI Vs Grades Vs Loan Status",x="Grade",y="Sub Grade",fill="Median DTI") +
  facet_wrap(~loan_status)

grid.arrange(G16,G17,nrow=2,ncol=1)

# Inference : THere seems to be direct relationship between the DTI and the Grade, therefore,
#             between dti and grades, grades can be considered as a valid reflection for DTI data as well.


#### IMPORTANT ####
# Grade = G3 seems to oulier in the loan allocation. There seems to be relationship between this
# Grade level and the number of Charged off loans.


# Grade Vs Sub_Grade Vs Median DTI (tile) Vs Percentage Charged Off #
G18 <- ggplot() + 
  geom_tile(data = loan_2_check %>% 
              group_by(grade,sub_grade_2) %>% 
              summarize(med_dti = median(dti,na.rm=TRUE)),aes(x=grade,y=sub_grade_2,fill=med_dti)) +
  geom_text(data = (loan_2_check %>% 
                      group_by(grade,sub_grade_2,loan_status) %>% 
                      summarize(cnt=length(id)) %>% 
                      mutate(ratio=paste("Charged Off =",round(cnt/sum(cnt),4)*100,"%")) %>% 
                      filter(loan_status=="Charged Off")),
            aes(x=grade,y=sub_grade_2,label=ratio),col="white") +
  geom_text(data = (loan_2_check %>% 
                      group_by(grade,sub_grade_2,loan_status) %>% 
                      summarize(cnt=length(id)) %>% 
                      mutate(ratio=paste("Fully Paid =",round(cnt/sum(cnt),4)*100,"%")) %>% 
                      filter(loan_status=="Fully Paid")),
            aes(x=grade,y=sub_grade_2,label=ratio),col="white",vjust=-1.0) +
  geom_text(data = (loan_2_check %>% 
                      group_by(grade,sub_grade_2) %>% 
                      summarize(cnt=length(id)) %>%
                      mutate(cnt2=paste("Total = ",cnt))),
            aes(x=grade,y=sub_grade_2,label=cnt2),col="white",vjust=-2.2) +
  labs(title="G18 - Grade Vs Sub Grade Vs Median DTI\nWith percentage Charged off for each\nSub Grade",
       x="Grade",y="Sub Grade",fill="Median DTI",label="Percentage of Charged Off Loans")

grid.arrange(G18)

# G3 Grade level is a clear risk for LC #


# Influence of Grades on Interest Rate
# Note : the line signifies the mean interest rate in that sub_grade
G19 <- ggplot(loan_2_check) + 
  geom_boxplot(aes(x=sub_grade,y=int_rate,fill=grade)) + 
  geom_line(data=(loan_2_check %>% 
                    group_by(sub_grade) %>% 
                    summarize(avg_dti=mean(int_rate,na.rm=TRUE))),
            aes(x=sub_grade,y=avg_dti,group=1)) +
  scale_y_continuous(breaks=seq(0,25,1)) +
  labs(title="G19 - Grades Vs Interest Rate",x="Sub Grade",y="Interest Rate",fill="Grade")

grid.arrange(G19)

G20 <- ggplot(data = loan_2_check %>% 
                group_by(grade,sub_grade_2) %>% 
                summarize(med=median(int_rate)),
              aes(x=grade,y=sub_grade_2,value=med)) + 
  geom_tile(aes(fill=med)) +
  geom_text(aes(label=med),col="white") +
  labs(title="G20 - Grade Vs Sub_Grade Vs Interest Rate",x="Grade",y="Sub Grade",fill="Median\nInterest Rate")

G21 <- ggplot(data = loan_2_check %>% 
                group_by(grade,sub_grade_2,loan_status) %>% 
                summarize(med=median(int_rate)),
              aes(x=grade,y=sub_grade_2,value=med)) + 
  geom_tile(aes(fill=med)) +
  geom_text(aes(label=med),col="white") +
  facet_wrap(~loan_status) +
  labs(title="G21 - Grade Vs Sub_Grade Vs Interest Rate\nVs Loan Status",x="Grade",y="Sub Grade",fill="Median\nInterest Rate")

grid.arrange(G20,G21,nrow=2,ncol=1)  

#Inference : There seems to be direct relationship between Interest Rate and the Grade, therefore,
#            between interest rate and grades, grades can be considered as a valid reflection of interest
#            data as well.

# Influence of grade on verification status
G22 <- ggplot(loan_2_check %>% 
                group_by(verification_status,sub_grade) %>% 
                summarize(cnt=length(id))) + 
  geom_col(aes(x=verification_status,y=cnt,fill=sub_grade),position="fill") +
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  labs(title="G22 - Verification Status Vs Grade",x="Verification Status",y="Percentage",fill="Grade")

grid.arrange(G22)

# Influence of grade on home ownership
G23 <- ggplot(loan_2_check %>% 
         group_by(home_ownership,sub_grade) %>% 
         summarize(cnt=length(id))) + 
  geom_col(aes(x=home_ownership,y=cnt,fill=sub_grade),position="fill") + 
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  labs(title="G23 - Verification Status Vs Grade",x="Verification Status",y="Percentage",fill="Grade")

grid.arrange(G23)

# Inference : There does not seem to be a relationship between home_ownership and verification status, hence
#             keeping them as separate influential variables

# Inference:
# Variables that effect the charge off Vs Fully paid loans
# 1. sub_grade - G07
# 2. purpose - G08 
# 3. term - G06
# 4. home_ownership - G02
# 5. verification_status - G03
# 6. public bankruptcy records - G05
# 7. revol_util_range - G09
# 8. loan_amnt_range  - G10    

### Final EDA Conclusion Graph - for Influencing Variables #
grid.arrange(G07,G08,G06,G02,G03,G05,G09,G10,nrow=2,ncol=4)


#---------------------- Main Analysis Ends ----------------------#

#---------------------- Predictive Analytics Starts ---------------- #
# Explanation :
# Based on the analysis above, we have zeroed down on 8 critical variables
# which effect the loan_status. The analysis below tries to predict the loans
# which are currently running, with a certain level of accuracy whether
# a loan might end up as "Charged Off" or "Fully Paid"

# Description of Analysis #
# Let us say there is a borrower who has following credentials
# addr_state     = CA
# home_ownership = OWN
# Verification status = Not Verified
# ..... so and so forth
#
# ----- Analysis methodology ------
# Step 1) We take up every such row which needs to be checked
# Step 2) We take up the complete known data set where where loan_status is known
# Step 3) We start by comparing looking for row/rows which match all the 8 conditions
#         mentioned above
# Step 4) There are possibilities that we might get to only 3 out of 8 conditions only, but that is not a problem
# Step 5) One we get to closet matching conditions (whether 3,4,5...), we calculate the ratios
# Step 6) We take the ratio = #Number of Charged Off Loans/#Number of total loans given * (level/9)
# Step 7) Step (6) is the probability of borrower defaulting the loan

# Below, there are 3 functions
# runPrediction -> Step (1)/(2)
# check_current_ratio_level() -> Step (3)/(4)/(5)
# calculate_ratio -> Step (5)/(6)




# --------------------- (Step (1/2) ----------------------- ##
# Take up every row in "evaluation.df" and check if with "reference.df" #

runPrediction <- function(evaluation.df,reference.df,cutoff) {
  pb <- txtProgressBar(min=1,max=nrow(evaluation.df),style = 3)
  for (itr in c(1:nrow(evaluation.df))) {
    setTxtProgressBar(pb,itr)
    current <- evaluation.df[itr,]
    result <- check_current_ratio_level(current,reference.df)
    evaluation.df[itr,'ratio'] <- result[1]
    evaluation.df[itr,'level'] <- result[2]
  }
  evaluation.df <- evaluation.df %>% 
    mutate(loan_status_proj = ifelse(ratio < cutoff,
                                     "Fully Paid",
                                     "Charged Off"))
  return(evaluation.df)
}


# --------------------- (Step 3/4/5) ----------------------- ##
# Based on current row, check uptill which level the matching works
# and what is the probability of the row to be a defaulter

check_current_ratio_level <- function(current,base.ref) {
  current.df <- current
  level<-1
  grade.df <- base.ref %>% filter(sub_grade == current.df$sub_grade)
  if (nrow(grade.df) > 0) {
    level<-2
    purpose.df <- grade.df %>% filter(purpose == current.df$purpose)
    if (nrow(purpose.df) > 0) {
      level<-3
      term.df <- purpose.df %>% filter(term == current.df$term)
      if (nrow(term.df) > 0) {
        level <- 4
        home.df <- term.df %>% filter(home_ownership == current.df$home_ownership)
        if (nrow(home.df) > 0) {
          level <- 5
          verification.df <- home.df %>% filter(verification_status == current.df$verification_status)
          if (nrow(verification.df) > 0) {
            level <- 6
            pubBank.df <- verification.df %>% filter(pub_rec_bankruptcies == current.df$pub_rec_bankruptcies)
            if (nrow(pubBank.df) > 0) {
              level<-7
              revol.df <- pubBank.df %>% filter(revol_util_range == current.df$revol_util_range)
              if (nrow(revol.df) > 0) {
                #print("In Level 8")
                level<-8
                loan.df <- revol.df %>% filter(loan_amnt_range == current.df$loan_amnt_range)
                if (nrow(loan.df) > 0) {
                  #print("In Level 9")
                  level<-9
                  ref <- loan.df
                  return(c(calculate_ratio(ref,level),level))
                } else {
                #print("In Level 8")
                ref <- revol.df
                return(c(calculate_ratio(ref,level),level))
              }
            } else {
              #print("In Level 7")
              ref <- pubBank.df
              return(c(calculate_ratio(ref,level),level))
            }
          } else {
            #print("In Level 6")
            ref <- verification.df
            return(c(calculate_ratio(ref,level),level))
          }
        } else {
          #print("In Level 5")
          ref <- home.df
          return(c(calculate_ratio(ref,level),level))
        }
      } else {
        #print("In Level 4")
        ref <- term.df
        return(c(calculate_ratio(ref,level),level))
      }
    } else {
      #print("In Level 3")
      ref <- purpose.df
      return(c(calculate_ratio(ref,level),level))
    }
  } else {
    #print("In Level 2")
    ref <- grade.df
    return(c(calculate_ratio(ref,level),level))
  }
 } else {
   #print("In Level 1")
    return(c(NA,NA))
 }
  # Just for Sanity #
  return(c(NA,NA))
}

# --------------------- (Step 5/6) ----------------------- #
# Probability of defaulter ("Charged Off") calculated for each
# based on reference data sent as argument, and level uptill which data was found
calculate_ratio <- function(ref,level) {
  if (length(which(ref$loan_status == "Charged Off")) != 0) {
    ratio <- length(which(ref$loan_status == "Charged Off")) / nrow(ref) * (level/9)
  } else {
    if (level == 9) {
      ratio <- 0.00
    } else {
      ratio <- level/9
    }
  }
  return(ratio)
}

loan_2_evaluate <- loan_data %>% filter(loan_status == "Current")

## Lets first CHECK the above algorithm on existing data set #

# OBJECTIVE : Achieve maximum correct predictions

# Method : We randomly take up samples from known data set (where loan_status is "Charged Off" or "Fully Paid")
#          perform prediction, and see how the algorithm has faired

# Important point : We have to decide a good value of probability
#                   below which a loan can be predicted as "Fully Paid"
#                   and above which loan can be predicted as "Charged Off"

# Important Note : BELOW ANALYSIS CAN TAKE UPTO 7-10MINS TO COMPLETE. SO PLEASE BARE WITH THE ITERATIVE
#                  ANALYSIS METHODOLOGY

crossValidationResult <- data.frame(itr=integer(),
                                    cutOff=integer(),
                                    true_positive=integer(),
                                    true_negative=integer(),
                                    false_positive=integer(),
                                    false_negative=integer(),
                                    precision=integer(),
                                    recall=integer(),
                                    fscore=integer(),
                                    correct=integer(),
                                    incorrect=integer(),
                                    totalProspectiveBusinessLoss=integer(),
                                    totalDefaulterBusinessLoss=integer())
# Run for 10 different random samples #
for (i in c(1:10)) {
  cat("\nRunning Check, Iteration = ",i,"\n")
  sample_row <- sample(c(1:nrow(loan_2_check)),nrow(loan_2_evaluate))
  
  evaluate <- loan_2_check[sample_row,]
  
  # The reference df should not have evaluate rows
  ref.df <- loan_2_check[setdiff(c(1:nrow(loan_2_check)),sample_row),]
  evaluate$level <- NA
  evaluate$ratio <- NA
  
  # Run prediction #
  evaluate <- runPrediction(evaluate,ref.df,1)
  
  # Check prediction accuracy at different cutOff values
  for (cutOff in seq(0.5,1.0,0.1)) {
    tempCrossValidationResult <- data.frame(itr=integer(),
                                        cutOff=integer(),
                                        true_positive=integer(),
                                        true_negative=integer(),
                                        false_positive=integer(),
                                        false_negative=integer(),
                                        precision=integer(),
                                        recall=integer(),
                                        fscore=integer(),
                                        correct=integer(),
                                        incorrect=integer(),
                                        totalProspectiveBusinessLoss=integer(),
                                        totalDefaulterBusinessLoss=integer())
    evaluate <- evaluate %>% mutate(loan_status_proj = ifelse(ratio < cutOff,"Fully Paid","Charged Off"))
    evaluate <- evaluate %>% 
      mutate(check_result = (loan_status == loan_status_proj))
    tp <- nrow(evaluate %>% filter(loan_status == "Charged Off",loan_status_proj == "Charged Off"))
    tn <- nrow(evaluate %>% filter(loan_status == "Fully Paid",loan_status_proj == "Fully Paid"))
    fp <- nrow(evaluate %>% filter(loan_status == "Fully Paid",loan_status_proj == "Charged Off"))
    fn <- nrow(evaluate %>% filter(loan_status == "Charged Off",loan_status_proj == "Fully Paid"))
    precision <- tp/(tp+fp)
    recall <- tp/(tp+fn)
    fscore <- (2*(precision*recall))/(precision + recall)
    totalProspectiveBusinessLoss <- sum((evaluate %>% filter(loan_status == "Fully Paid",loan_status_proj == "Charged Off") %>% mutate(earning=((installment*term)-funded_amnt)))$earning)
    totalDefaulterBusinessLoss <- sum((evaluate %>% filter(loan_status == "Charged Off",loan_status_proj == "Fully Paid") %>% mutate(earning=((installment*term)-funded_amnt)))$earning)
    tempCrossValidationResult[1,] <- c(i,cutOff,tp,tn,fp,fn,precision,recall,fscore,(tp+tn)/nrow(evaluate),(fp+fn)/nrow(evaluate),totalProspectiveBusinessLoss,totalDefaulterBusinessLoss)
    crossValidationResult <- rbind(crossValidationResult,tempCrossValidationResult)
  }
}

crossValidationResult$cutOff <- as.factor(crossValidationResult$cutOff)


# Based on the analysis, lets see what is the cutOff values which gives best results #
plot.df <- gather((data.frame(crossValidationResult %>% 
                                group_by(cutOff) %>% 
                                summarize(correct=median(correct),
                                          incorrect=median(incorrect),
                                          fscore=median(fscore,na.rm=TRUE)))),
                  key=cutOff,values=c(2,3,4))
colnames(plot.df) <- c("cutOff","category","value")

G24 <- ggplot(plot.df %>% filter(category %in% c('correct','incorrect'))) + 
  geom_col(aes(x=cutOff,y=value,fill=category),position=position_dodge(width=0.5),alpha=0.8) +
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  labs(title="G24 - Probability of Charged Off Vs\nCorrect & Incorrect Predictions",
       x="Charged Off CutOff Probability",
       y="Percentage",
       fill="Correct\n/Incorrect Prediction")

grid.arrange(G24)

# By above analysis it seems, that cutOff Value = 1 should be chosen
# This mean, which ever customer's probability value < 1 will be termed as Potentially "Fully Paid"
# customer, and for probability value = 1, they will be termed as potentially "Charged Off" loans

# But wait.......
# This does not lower the risk portfolio of the bank, because we will be following what has been
# going on with the bank, and not change the strategy for lower risk #
# ONE OF THE OBJECTIVES, IS TO REDUCE THE BUSINESS LOSS DUE TO LENDING OF LOANS 

#-------------------- ANALYSIS FOR MINIMIZING LOSS TO LOAN LENDING COMPANY --------------------#

# Based on above analysis, we need to reach a proper cutoff probability value which minimizes the
# the loss incurred on the company due to following 2 factors
# 1) Lossing customers which are potentially "GOOD CUSTOMERS", i.e whose loans are most probable to get "Fully Paid".
# 2) Loosing money to potentially "BAD CUSTOMERS", i.e. whose loans are most probable to get "Charged Off"

# For above lets see the mean and median of Prospective Business Loss & Prospective Defaulter Loss
# Prospective Business Loss = Loss of interest for those customers who were predicted as "Charged Off", but
#                             were actually "Fully Paid"
# Prospective Defaulter Loss = Loss for those customers who were predicted as "Full Paid", but were
#                              actually "Charged Off"

# Lets validate how does the mean and median data flows based on different cutOff
crossValidationResult %>% 
  group_by(cutOff) %>% 
  summarize(medPros=median(totalProspectiveBusinessLoss),
            avgPros=mean(totalProspectiveBusinessLoss),
            medDefault=median(totalDefaulterBusinessLoss),
            avgDefault=mean(totalDefaulterBusinessLoss))

# Sample output - Below value can be different for different executions #
# A tibble: 6 × 5
#cutOff    medPros   avgPros medDefault avgDefault
#<fctr>      <dbl>     <dbl>      <dbl>      <dbl>
#1    0.5 1282447.40 1298707.8   374290.8   372336.1
#2    0.6 1201176.02 1215047.3   395634.8   401919.0
#3    0.7 1182793.12 1192408.8   404709.0   411402.7
#4    0.8  809798.20  831736.1   502507.4   505186.0
#5    0.9   13645.04   15634.3   669199.3   690053.6
#6      1   13645.04   15634.3   669199.3   690053.6
#
# The mean and median off Prospective Loss on "Good Customers" and Defaulter Loss on "Bad Customers"
# seems to be consistent(atleast for lower probability values). We will take median as the reference value



G25 <- ggplot(crossValidationResult %>% 
         group_by(cutOff) %>% 
         summarize(medPros=median(totalProspectiveBusinessLoss),
                   medDef=median(totalDefaulterBusinessLoss))) + 
  geom_line(aes(x=cutOff,y=medPros,group=1),col="red",size=2) + 
  geom_line(aes(x=cutOff,y=medDef,group=1),col="blue",size=2) +
  scale_y_continuous(breaks=seq(0,2e+06,1e+05)) +
  geom_text(aes(x=4,y=min(medPros),label="Projected Business Loss of Good Customer",vjust=2.0),col="red") +
  geom_text(aes(x=5,y=max(medDef),label="Projected Business Loss to Defaulters"),col="blue",vjust=-2.0) +
  labs(title="G25 - Business Loss Vs\n Charged Off Probability",y="Probable Business Loss ($)",x="Charged Off Probability Cut Off")

grid.arrange(G25)
  
# Based on above graph, it is clear that charged off cut off probability chosen should be between 0.80 ~ 0.85
# This means that for borrowers whose probability comes out to be below 0.825, will be considered as "Fully Paid"
# and otherwise as "Charged Off"

chargedOffCutOffChosen <- 0.825

loan_2_evaluate <- loan_data %>% filter(loan_status == "Current")
loan_2_evaluate$level <- NA
loan_2_evaluate$ratio <- NA

cat(" ----- Running Prediction of loans where loan_status = \"Current\"")
loan_2_evaluate <- runPrediction(loan_2_evaluate,loan_2_check,chargedOffCutOffChosen)

G26 <- ggplot(loan_2_evaluate %>% group_by(loan_status_proj) %>% summarize(cnt=length(id))) + 
  geom_col(aes(x=loan_status_proj,y=cnt,fill=loan_status_proj)) +
  scale_y_continuous(breaks=seq(0,1500,50)) +
  labs(title="G26 - Loan Status Projection\nFor Currently Running Loans\nProbability Cut Off=0.825",x="Loan Status",y="Count",fill="Loan Status")
grid.arrange(G26)



#-------------------- Predictive Analysis Ends ------------------#
# Thank You! #