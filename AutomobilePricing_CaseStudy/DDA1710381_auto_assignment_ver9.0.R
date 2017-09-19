# ------------------------------------------------------- #
# Code Description : PGDDA - Linear Regression Assignment #
# Author           : Anugraha Sinha                       #
# Roll Number      : DDA1710381                           #
# ------------------------------------------------------- #

# ---- SETUP -----#
required_packages <- c("dplyr","ggplot2","gridExtra","car","MASS","tidyr","corrplot")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(new_packages)) install.packages(new_packages)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(car)
library(MASS)
library(tidyr)
library(corrplot)
# ---- Load Data ----- #
# Set up the working directory, assuming the code is running working directory, from here on #
#setwd("C:/Upgrad")
car_data <- read.delim("CarPrice_Assignment.csv",sep=",",stringsAsFactors = FALSE)

# Lets keep a working copy of the base data #
car_working.df <- car_data

# ---- CRISP-DM Framework Stage 2 - Data Understanding ---- #

    # Lets first take a look at the str of the car_data #
    str(car_working.df)
    
    # Summary #
    # 1. Total of 26 columns
    # 2. 16 numerical columns & 10 chr based columns
    # 3. Data dictionary is made available separately, so not mentioning it here
    # 4. Car names are a composite string of Manufacturer's Name and the model, 
    #    hence we need to work on that.
    # 5. There are a lot of categorical variables, we will have to check the 
    #    importance of each by checking if there are significant number of 
    #    observation for each category
    # 6. Intuitely we can say that car_ID is of no use, as its just an index number
    #    to every row, we can remove that as a first step
    
    # Important point to remember, since we are using MASS library and dplyr both, 
    # we might have to explicit specify some function names using scope resolution
    # operator.

# ---- CRISP-DM Framework Stage 3 - Data Preparation ---- #
    
    # ---- Stage 2.1 - Data cleaning
    # 1) As a first step lets remove the car_ID column for a start
         car_working.df <- car_working.df %>% dplyr::select(-car_ID)
         
    # 2) Lets check the NAs in the data
         sum(is.na(car_working.df))
         # 0 <- No NAs in the data, this is nice! 
         
    # 3) Lets check duplicate rows, as we can have data for same car/model repeated
         which(duplicated(car_working.df))
         # 0 <- no duplicates, interesting
    
    # --- Stage 2.2 - univariate analysis, with some cleaning combined     
    # 1) - Symboling variable
    #      Data dictionary says that it ranges from (-3)-to-(3), +3 being highest on risk
    #      and -3 being lowest.
    #      Lets check the distribution first
           G01 <- ggplot(car_working.df) + geom_bar(aes(x=symboling),fill="blue",col="black") +
             scale_x_continuous(breaks = seq(-3,3,1)) +
             scale_y_continuous(breaks = seq(0,80,5)) +
             labs(title="G01 - symboling distribution",x="Symboling",y="Count")
           grid.arrange(G01)
    #      It would be better to convert this to categorical and then change the levels
    #      -3 -> 0 moving to +3 -> 6
    #      this being done because we can symbolize 0 as a lower value which will be equal to -3 of risk
    #      and 6 as a higher value which will be equal to +3 of risk
           car_working.df$symboling <- as.factor(car_working.df$symboling)
           levels(car_working.df$symboling) <- c(0:6)
           car_working.df$symboling <- as.numeric(levels(car_working.df$symboling))[car_working.df$symboling]
           str(car_working.df$symboling)
           
    # 2) - CarName
    #      Lets split carName into 2, as car manufacturer and another as car model, though we dont need
    #      car_model column, so we will drop car_model (but in the end)
    #      Remember, there will be some warnings, so considering that to be fine because of some NA that
    #      are coming multiple spaces in carName
           car_working.df <- car_working.df %>% 
             tidyr::separate(CarName,c("car_manufacturer","car_model"),sep=" ") %>% dplyr::select(-car_model)
    #      Lets check the levels of these car manufacturers
           levels(as.factor(car_working.df$car_manufacturer))
    #      Some issues here
    #      a) nissan & Nissan are same, we will go with nissan
           car_working.df[which(car_working.df$car_manufacturer == "Nissan"),'car_manufacturer'] <- "nissan"
    #      b) porcshce & porsche are same, we will go with porsche
           car_working.df[which(car_working.df$car_manufacturer == "porcshce"),'car_manufacturer'] <- "porsche"
    #      c) toyouta & toyota are same, we will go with toyota
           car_working.df[which(car_working.df$car_manufacturer == "toyouta"),'car_manufacturer'] <- "toyota"
    #      d) vokswagen, volkswagen & vw are same, we will go with volkswagen
           car_working.df[which(car_working.df$car_manufacturer == "vokswagen"),'car_manufacturer'] <- "volkswagen"
           car_working.df[which(car_working.df$car_manufacturer == "vw"),'car_manufacturer'] <- "volkswagen"
    #      3) maxda & mazda are same, we will go with mazda
           car_working.df[which(car_working.df$car_manufacturer == "maxda"),'car_manufacturer'] <- "mazda"
    #      check levels once more
           levels(as.factor(car_working.df$car_manufacturer))
    #      all well! 
           
    #      Lets check the distribution for cars_manufacturers
           G02 <- ggplot(car_working.df) + geom_bar(aes(x=car_manufacturer),fill="blue",col="black") +
             scale_y_continuous(breaks = seq(0,40,5)) +
             labs(title="G02 - Car Manufacturers",x="Car Manufacturer",y="Number of models")
           grid.arrange(G02)
    #      distribution seems fine for a linear regression model           
             
    
    # 3) - fuel-type
    #      check the levels
           levels(as.factor(car_working.df$fueltype))
    #      on 2 types, diesel and gas, that seems fine, lets just see the distribution
           G03 <- ggplot(car_working.df) + geom_bar(aes(x=fueltype),fill="blue",col="black") +
             scale_y_continuous(breaks = seq(0,190,10)) +
             labs(title="G03 - Fueltype distribution",x="Fuel Type",y="Number of models")
           grid.arrange(G03)

    # 4) - Similar to above, we will plot for following columns
    #      a) aspiration
           G04 <- ggplot(car_working.df) + geom_bar(aes(x=aspiration),fill="blue",col="black") +
             scale_y_continuous(breaks = seq(0,200,10)) +
             labs(title="G04 - Aspiration distribution",x="Aspiration",y="Number of models")
    #      b) doornumber
           G05 <- ggplot(car_working.df) + geom_bar(aes(x=doornumber),fill="blue",col="black") +
             scale_y_continuous(breaks = seq(0,200,10)) +
             labs(title="G05 - Number of Doors\nDistribution",x="Number of doors",y="Number of models")
    #      c) carbody           
           G06 <- ggplot(car_working.df) + geom_bar(aes(x=carbody),fill="blue",col="black") +
             scale_y_continuous(breaks = seq(0,200,10)) +
             labs(title="G06 - Type of body\nDistribution",x="Type of body",y="Number of models")
    #      d) drivewheel
           G07 <- ggplot(car_working.df) + geom_bar(aes(x=drivewheel),fill="blue",col="black") +
             scale_y_continuous(breaks = seq(0,200,10)) +
             labs(title="G07 - Front Wheel/Read Wheel Drive\nDistribution",x="Drive Wheel",y="Number of models")
    #      e) enginelocation
           G08 <- ggplot(car_working.df) + geom_bar(aes(x=enginelocation),fill="blue",col="black") +
             scale_y_continuous(breaks = seq(0,200,10)) +
             labs(title="G08 - Engine Location\nDistribution",x="Engine Location",y="Number of models")
    #      f) enginetype
           G09 <- ggplot(car_working.df) + geom_bar(aes(x=enginetype),fill="blue",col="black") +
             scale_y_continuous(breaks = seq(0,200,10)) +
             labs(title="G09 - Engine Type\nDistribution",x="Engine Type",y="Number of models")
    #      g) cylindernumber
           G10 <- ggplot(car_working.df) + geom_bar(aes(x=cylindernumber),fill="blue",col="black") +
             scale_y_continuous(breaks = seq(0,200,10)) +
             labs(title="G10 - Number of Cylinders\nDistribution",x="Number of cylinders",y="Number of models")
    #      h) fuelsystem
           G11 <- ggplot(car_working.df) + geom_bar(aes(x=fuelsystem),fill="blue",col="black") +
             scale_y_continuous(breaks = seq(0,200,10)) +
             labs(title="G11 - Fuel System\nDistribution",x="Fuel System",y="Number of models")
           grid.arrange(G03,G04,G05,G06,G07,G08,G09,G10,G11,nrow=3,ncol=3)
           
    # All the distributions look fine and there does not seem to be a repeat of duplicate/similar categories
    # with spelling mistakes etc problem that needs to be taken care of.
           
    # Lets see the box plots of different numeric fields also
           G11 <- ggplot(car_working.df) + geom_boxplot(aes(x=1,y=wheelbase),fill="light blue") +
             labs(title="G11 - WheelBase distribution",x="",y="WheelBase")
           G12 <- ggplot(car_working.df) + geom_boxplot(aes(x=1,y=carlength),fill="light blue") +
             labs(title="G12 - CarLength distribution",x="",y="Car Length")
           G13 <- ggplot(car_working.df) + geom_boxplot(aes(x=1,y=carwidth),fill="light blue") +
             labs(title="G13 - Car Width distribution",x="",y="Car Width")
           G14 <- ggplot(car_working.df) + geom_boxplot(aes(x=1,y=carheight),fill="light blue") +
             labs(title="G14 - Car Height distribution",x="",y="Car Height")
           G15 <- ggplot(car_working.df) + geom_boxplot(aes(x=1,y=curbweight),fill="light blue") +
             labs(title="G15 - Curb Weight distribution",x="",y="Curb Weight")
           G16 <- ggplot(car_working.df) + geom_boxplot(aes(x=1,y=enginesize),fill="light blue") +
             labs(title="G16 - Engine Size distribution",x="",y="Engine Size")
           G17 <- ggplot(car_working.df) + geom_boxplot(aes(x=1,y=boreratio),fill="light blue") +
             labs(title="G17 - Bore Ratio distribution",x="",y="Bore Ratio")
           G18 <- ggplot(car_working.df) + geom_boxplot(aes(x=1,y=stroke),fill="light blue") +
             labs(title="G18 - Stroke distribution",x="",y="Stroke")
           G19 <- ggplot(car_working.df) + geom_boxplot(aes(x=1,y=compressionratio),fill="light blue") +
             labs(title="G19 - Compression Ratio\ndistribution",x="",y="Compression Ratio")
           G20 <- ggplot(car_working.df) + geom_boxplot(aes(x=1,y=horsepower),fill="light blue") +
             labs(title="G20 - Hoursepower\ndistribution",x="",y="horsepower")
           G21 <- ggplot(car_working.df) + geom_boxplot(aes(x=1,y=peakrpm),fill="light blue") +
             labs(title="G21 - Peak RPM\ndistribution",x="",y="Peak RPM")
           G22 <- ggplot(car_working.df) + geom_boxplot(aes(x=1,y=citympg),fill="light blue") +
             labs(title="G22 - City Mileage\ndistribution",x="",y="City Mileage")
           G23 <- ggplot(car_working.df) + geom_boxplot(aes(x=1,y=highwaympg),fill="light blue") +
             labs(title="G23 - Highway Mileage\ndistribution",x="",y="Highway Mileage")
           
           grid.arrange(G11,G12,G13,G14,G15,G16)
           
           grid.arrange(G17,G18,G19,G20,G21,G22,G23)
    # Insights from analysis above
    
    # --- Outlier treatments ---- #
    #      a) enginesize has some outliers
           quantile(car_working.df$enginesize,seq(0,1,0.01))
    #      The distrbution jumps after 96 percentile, and goes up to 236.
    #      For all values above 209, we will mark them as 209.00
           car_working.df$enginesize[(which(car_working.df$enginesize > 209))] <- 209
           
    #      b) Horsepower has some outliers
           quantile(car_working.df$horsepower,seq(0,1,0.01))
    #      The distribution jumps after 97 percentile, and goes up from 184 to 288.
    #      For values above 184, we will modify them as 184.00
           car_working.df$horsepower[(which(car_working.df$horsepower > 184))] <- 184
           
    #      c) compression ratio has outliers
           quantile(car_working.df$compressionratio,seq(0,1,0.01))
    #      The distribution jumps after 90 percentile, and goes up from 10.94 to 23.0
    #      For values above 10.9400, we will modify them as 10.9400
           car_working.df$compressionratio[(which(car_working.df$compressionratio > 10.94))] <- 10.94
           
    #      d) broadly speaking there are 3 kinds of engines
    #         1) SOHC - Single overhead camshaft, there is nothing like ohcf, ohcv
    #         2) DOHC - Dual overhead camshaft, there is nothing like a dohcv
    #         3) l type - Generally nissan used to make them
    #         4) Rotary - Rotary engines
    #         So we have some subsitutions to be done
           car_working.df$enginetype[(which(car_working.df$enginetype  %in% c("ohcf","ohcv")))] <- "ohc"
           car_working.df$enginetype[(which(car_working.df$enginetype  %in% c("dohcv")))] <- "dohc"
    
    # - Some Domain aspects to remember
    # 1) Diesel engines are generally heavier than petrol (engines), hence curbweight of diesel engines 
    #    might be more.
    # 2) Diesel engines will have higher compression ratio, because fuel ignition in diesel engine happens
    #    because of compression air and diesel mixture due to compression. As the theory goes, petrol engine's
    #    thermodynamical otto-cycle has a compression ratio of 1/8 and where as that of diesel cycle is 1/16.
    # 3) enginesize and horsepower may be highly co-related.
    # Lets see these bivariate analysis
    G24 <- ggplot(car_working.df) + geom_boxplot(aes(x=fueltype,y=curbweight,fill=fueltype)) +
      labs(title="G24 - FuelType Vs CurbWeight",x="FuelType",y="Curb Weight",fill="Fuel Type")
    
    G25 <- ggplot(car_working.df) + geom_boxplot(aes(x=1,y=compressionratio),fill="light blue") + 
      facet_wrap(~fueltype,scales = "free") + labs(title="G25 - FuelType Vs Compression Ratio",x="Fuel Type",y="COmpression Ratio")
    
    G26 <- ggplot(car_working.df) + geom_point(aes(x=enginesize,y=horsepower),size=3,col="blue") +
      scale_y_continuous(breaks = seq(0,250,20)) +
      scale_x_continuous(breaks = seq(0,300,10)) +
      labs(title="G26 - Engine Size Vs Horsepower",x="Engine Size",y="Horsepower")
    
    grid.arrange(G24,G25,G26,nrow=2,ncol=2)
    
    # Similarly, we can see the corelation between multiple variables, lets just see a corelation matrix for
    # numerical variables, later on once we modify categorical variables we will see this again.
    
    car_corelation_matrix <- cor(car_working.df %>% dplyr::select(peakrpm,wheelbase,carlength,carwidth,carheight,curbweight,enginesize,boreratio,stroke,compressionratio,horsepower,citympg,highwaympg,price,symboling))
    corrplot(car_corelation_matrix, method = "number", 
             title = "Correlation Matrix", 
             type = "lower", 
             order = "FPC", 
             number.cex = 0.8, 
             tl.cex = 0.7,
             bg="light green")
    
    # There is a lot of co-relation between different numeric features. A lot of care needs to be taken during
    # predictive analytics.
    
    # citympg and highwaympg are corelation upto 97%. We can keep kust 1 of them
    # We will go with only citympg
    car_working.df <- car_working.df %>% dplyr::select(-highwaympg)
    
    #curbweight is corelated to a lot of variables to strong extent, hence we can probably remove it
    car_working.df <- car_working.df %>% dplyr::select(-curbweight)
    
    # Car length and wheelbase will obviously have a string co-relation. We just go with wheelbase
    car_working.df <- car_working.df %>% dplyr::select(-carlength)
    
    # It would be better if we do a stepAIC, so that initial by step wise we will able to eliminate maximum 
    # variables
    
    # Before we move in that direction, lets convert all columns to numeric so that we can do mathematical analysis.
    
    
    
    # --- convertion of categorical variables to numeric --- #
    # lets make a copy of car_working.df
    car_math.df <- car_working.df
    # a) car_manufacturer - use model.matrix
         dummy_car_manufacturer <- model.matrix(~car_manufacturer,data=car_math.df)
         car_math.df <- cbind(car_math.df %>% dplyr::select(-car_manufacturer),dummy_car_manufacturer[,-1])
    # b) fueltype - simply change gas = 0 and diesel = 1
         car_math.df$fueltype <- ifelse(car_math.df$fueltype == "gas",0,1)
    # c) aspiration - simply change std = 0 and turbo = 1
         car_math.df$aspiration <- ifelse(car_math.df$aspiration == "std",0,1)
    # d) doornumber - simply change two door = 0 and 4 door = 1      
         car_math.df$doornumber <- ifelse(car_math.df$doornumber == "two",0,1)
    # e) car body - use model.matrix          
         dummy_car_body <- model.matrix(~carbody,data=car_math.df)
         car_math.df <- cbind(car_math.df %>% dplyr::select(-carbody),dummy_car_body[,-1])
    # f) drivewheel - use model.matrix         
         dummy_drivewheel <- model.matrix(~drivewheel,data=car_math.df)
         car_math.df <- cbind(car_math.df %>% dplyr::select(-drivewheel),dummy_drivewheel[,-1])
    # g) enginelocation - simply change rear = 0 and front = 1     
         car_math.df$enginelocation <- ifelse(car_math.df$enginelocation == "rear",0,1)
    # h) enginetype - use model.matrix
         dummy_enginetype <- model.matrix(~enginetype,data=car_math.df)
         car_math.df <- cbind(car_math.df %>% dplyr::select(-enginetype),dummy_enginetype[,-1])
    # i) cylindernumber - use model.matrix
         dummy_cylindernumber <- model.matrix(~cylindernumber,data=car_math.df)
         car_math.df <- cbind(car_math.df %>% dplyr::select(-cylindernumber),dummy_cylindernumber[,-1])
    # j) fuelsystem - use model.matrix
         dummy_fuelsystem <- model.matrix(~fuelsystem,data=car_math.df)
         car_math.df <- cbind(car_math.df %>% dplyr::select(-fuelsystem),dummy_fuelsystem[,-1])
         
    
         
# ---- CRISP-DM Framework Stage 4 - Data Modeling ---- #

         # There are high co-relations between different numerical variables #
         # It is better that we remove the high-corelation variables first, we can consider stepAIC as
         # a method for a start
         
         cols_tobe_removed_for_analysis <- c("")
         
         # Lets make a global function for performing linear model creation, so that it is easy to work #
         car_lm <- function(cols_to_remove=c(""),executeSummaryForModel=0,executeVIF=0,executeStepAIC=0) {
           # Lets get all the col numbers which we need take into consideration #
           # Assuming that the cols_to_remove does not give "price" as a column to be removed
           if ( length(cols_tobe_removed_for_analysis) == 0 ) {
             cols_tobe_removed_for_analysis <- cols_to_remove
           } else if ( ! length(cols_to_remove) == 0) {
             cols_tobe_removed_for_analysis <- c(cols_tobe_removed_for_analysis,cols_to_remove)
           } else {
             # Do nothing #
           }
           colnums_to_consider <- which(! colnames(car_math.df) %in% cols_tobe_removed_for_analysis)
           analysis.df <- car_math.df %>% dplyr::select(colnums_to_consider)
           model <- lm(price~.,data=analysis.df)
           if (executeSummaryForModel == 1) {
             print(summary(model))
           }
           if (executeVIF == 1) {
             print(vif(model))
           }
           if (executeStepAIC == 1) {
             step <- stepAIC(model,direction="both")
             print(step)
           }
           return(model)
         }
         
         
         # Model - 1, all columns
           
           model_1 <- car_lm(cols_to_remove=c(""),executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
           # Lets execute stepAIC
           model_1 <- car_lm(cols_to_remove=c(""),executeSummaryForModel=0,executeVIF=0,executeStepAIC=1)
           
           # Variables given by stepAIC are (earlier their were 60 variables now we have 38 variables)
           # "fueltype", "enginelocation", "wheelbase", "carwidth", "compressionratio", "horsepower",
           # "peakrpm", "citympg", "car_manufacturerbmw", "car_manufacturerbuick", 
           # "car_manufacturerchevrolet", "car_manufacturerdodge", "car_manufacturerhonda", 
           # "car_manufacturerisuzu", "car_manufacturerjaguar", "car_manufacturermazda", 
           # "car_manufacturermercury", "car_manufacturermitsubishi", "car_manufacturernissan", 
           # "car_manufacturerpeugeot", "car_manufacturerplymouth", "car_manufacturerporsche", 
           # "car_manufacturerrenault", "car_manufacturersubaru", "car_manufacturertoyota", 
           # "car_manufacturervolkswagen", "car_manufacturervolvo", "carbodyhardtop", "carbodyhatchback", 
           # "carbodysedan", "carbodywagon", "drivewheelfwd", "enginetypeohc", "cylindernumberfive", 
           # "cylindernumberfour", "cylindernumbersix", "fuelsystem2bbl"
           #
           # Modify cols_tobe_removed_for_analysis variable so that we can analyze things better
           
           aic_preferred_collist <- c("fueltype", "enginelocation", "wheelbase", "carwidth", "compressionratio", "horsepower",
             "peakrpm", "citympg", "car_manufacturerbmw", "car_manufacturerbuick", 
             "car_manufacturerchevrolet", "car_manufacturerdodge", "car_manufacturerhonda", 
             "car_manufacturerisuzu", "car_manufacturerjaguar", "car_manufacturermazda", 
             "car_manufacturermercury", "car_manufacturermitsubishi", "car_manufacturernissan", 
             "car_manufacturerpeugeot", "car_manufacturerplymouth", "car_manufacturerporsche", 
             "car_manufacturerrenault", "car_manufacturersubaru", "car_manufacturertoyota", 
             "car_manufacturervolkswagen", "car_manufacturervolvo", "carbodyhardtop", "carbodyhatchback", 
             "carbodysedan", "carbodywagon", "drivewheelfwd", "enginetypeohc", "cylindernumberfive", 
             "cylindernumberfour", "cylindernumbersix", "fuelsystem2bbl","price")
           cols_tobe_removed_for_analysis <- colnames(car_math.df)[which(! colnames(car_math.df) %in% aic_preferred_collist)]
           
        # Model - 2, with column suggested by AIC
           # Remember, the argument cols_to_remove will get added up into cols_tobe_removed_for_analysis.
           # Therefore, when we call cal_lm custom function, we will send extra columns that need to be removed
           # based on previous model analysis.
           # For example : in model_3, we sent "citympg", this gets added to the list of columns which AIC rejected
           #               (cols_tobe_removed_for_analysis). Further on, "citympg" removal for model_3 is based on
           #               analysis done on model_2.
           # Note: An appendix to the results of the models is given at the end of the code for reference
           #       Please refer for evaluation.
           model_2 <- car_lm(cols_to_remove=c(""),executeSummaryForModel=1,executeVIF=1,executeStepAIC=0)
           model_3 <- car_lm(cols_to_remove=c("citympg"),executeSummaryForModel=1,executeVIF=1,executeStepAIC=0)
           model_4 <- car_lm(cols_to_remove=c("citympg","peakrpm"),executeSummaryForModel=1,executeVIF=1,executeStepAIC=0)
           model_5 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc"),executeSummaryForModel=1,executeVIF=1,executeStepAIC=0)
           model_6 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd"),executeSummaryForModel=1,executeVIF=1,executeStepAIC=0)
           model_7 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl"),
                             executeSummaryForModel=1,executeVIF=1,executeStepAIC=0)
           model_8 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                              "wheelbase"), 
                             executeSummaryForModel=1,executeVIF=1,executeStepAIC=0)
           model_9 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                              "wheelbase","cylindernumbersix"), 
                             executeSummaryForModel=1,executeVIF=1,executeStepAIC=0)
           model_10 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                              "wheelbase","cylindernumbersix","compressionratio"), 
                             executeSummaryForModel=1,executeVIF=1,executeStepAIC=0)
           
           # NO more working on VIF now, we will move only with p-value tests from here
           
           model_11 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                               "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo"), 
                              executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
           model_12 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                               "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                               "car_manufacturerisuzu"), 
                              executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
           model_13 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                               "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                               "car_manufacturerisuzu","car_manufacturerchevrolet"), 
                              executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
           model_14 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                               "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                               "car_manufacturerisuzu","car_manufacturerchevrolet","car_manufacturerpeugeot"), 
                              executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
           model_15 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                               "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                               "car_manufacturerisuzu","car_manufacturerchevrolet","car_manufacturerpeugeot",
                                               "car_manufacturermercury"), 
                              executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
           model_16 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                               "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                               "car_manufacturerisuzu","car_manufacturerchevrolet","car_manufacturerpeugeot",
                                               "car_manufacturermercury","car_manufacturermazda"), 
                              executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
           model_17 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                               "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                               "car_manufacturerisuzu","car_manufacturerchevrolet","car_manufacturerpeugeot",
                                               "car_manufacturermercury","car_manufacturermazda","car_manufacturerhonda"), 
                              executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
           model_18 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                               "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                               "car_manufacturerisuzu","car_manufacturerchevrolet","car_manufacturerpeugeot",
                                               "car_manufacturermercury","car_manufacturermazda","car_manufacturerhonda",
                                               "car_manufacturerplymouth"), 
                              executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
           model_19 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                               "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                               "car_manufacturerisuzu","car_manufacturerchevrolet","car_manufacturerpeugeot",
                                               "car_manufacturermercury","car_manufacturermazda","car_manufacturerhonda",
                                               "car_manufacturerplymouth","car_manufacturerdodge"), 
                              executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
           model_20 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                               "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                               "car_manufacturerisuzu","car_manufacturerchevrolet","car_manufacturerpeugeot",
                                               "car_manufacturermercury","car_manufacturermazda","car_manufacturerhonda",
                                               "car_manufacturerplymouth","car_manufacturerdodge","car_manufacturerrenault"), 
                              executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
           model_21 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                               "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                               "car_manufacturerisuzu","car_manufacturerchevrolet","car_manufacturerpeugeot",
                                               "car_manufacturermercury","car_manufacturermazda","car_manufacturerhonda",
                                               "car_manufacturerplymouth","car_manufacturerdodge","car_manufacturerrenault",
                                               "car_manufacturervolkswagen"), 
                              executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
           model_22 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                               "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                               "car_manufacturerisuzu","car_manufacturerchevrolet","car_manufacturerpeugeot",
                                               "car_manufacturermercury","car_manufacturermazda","car_manufacturerhonda",
                                               "car_manufacturerplymouth","car_manufacturerdodge","car_manufacturerrenault",
                                               "car_manufacturervolkswagen","car_manufacturersubaru"), 
                              executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
           model_23 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                               "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                               "car_manufacturerisuzu","car_manufacturerchevrolet","car_manufacturerpeugeot",
                                               "car_manufacturermercury","car_manufacturermazda","car_manufacturerhonda",
                                               "car_manufacturerplymouth","car_manufacturerdodge","car_manufacturerrenault",
                                               "car_manufacturervolkswagen","car_manufacturersubaru","car_manufacturertoyota"), 
                              executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
           model_24 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                               "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                               "car_manufacturerisuzu","car_manufacturerchevrolet","car_manufacturerpeugeot",
                                               "car_manufacturermercury","car_manufacturermazda","car_manufacturerhonda",
                                               "car_manufacturerplymouth","car_manufacturerdodge","car_manufacturerrenault",
                                               "car_manufacturervolkswagen","car_manufacturersubaru","car_manufacturertoyota",
                                               "car_manufacturernissan"), 
                              executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
           model_25 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                               "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                               "car_manufacturerisuzu","car_manufacturerchevrolet","car_manufacturerpeugeot",
                                               "car_manufacturermercury","car_manufacturermazda","car_manufacturerhonda",
                                               "car_manufacturerplymouth","car_manufacturerdodge","car_manufacturerrenault",
                                               "car_manufacturervolkswagen","car_manufacturersubaru","car_manufacturertoyota",
                                               "car_manufacturernissan","fueltype"), 
                              executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
           model_26 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                               "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                               "car_manufacturerisuzu","car_manufacturerchevrolet","car_manufacturerpeugeot",
                                               "car_manufacturermercury","car_manufacturermazda","car_manufacturerhonda",
                                               "car_manufacturerplymouth","car_manufacturerdodge","car_manufacturerrenault",
                                               "car_manufacturervolkswagen","car_manufacturersubaru","car_manufacturertoyota",
                                               "car_manufacturernissan","fueltype","car_manufacturermitsubishi"), 
                              executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
           model_27 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                               "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                               "car_manufacturerisuzu","car_manufacturerchevrolet","car_manufacturerpeugeot",
                                               "car_manufacturermercury","car_manufacturermazda","car_manufacturerhonda",
                                               "car_manufacturerplymouth","car_manufacturerdodge","car_manufacturerrenault",
                                               "car_manufacturervolkswagen","car_manufacturersubaru","car_manufacturertoyota",
                                               "car_manufacturernissan","fueltype","car_manufacturermitsubishi","cylindernumberfive"), 
                              executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
           model_28 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                               "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                               "car_manufacturerisuzu","car_manufacturerchevrolet","car_manufacturerpeugeot",
                                               "car_manufacturermercury","car_manufacturermazda","car_manufacturerhonda",
                                               "car_manufacturerplymouth","car_manufacturerdodge","car_manufacturerrenault",
                                               "car_manufacturervolkswagen","car_manufacturersubaru","car_manufacturertoyota",
                                               "car_manufacturernissan","fueltype","car_manufacturermitsubishi","cylindernumberfive",
                                               "cylindernumberfour"), 
                              executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
           model_29 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                               "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                               "car_manufacturerisuzu","car_manufacturerchevrolet","car_manufacturerpeugeot",
                                               "car_manufacturermercury","car_manufacturermazda","car_manufacturerhonda",
                                               "car_manufacturerplymouth","car_manufacturerdodge","car_manufacturerrenault",
                                               "car_manufacturervolkswagen","car_manufacturersubaru","car_manufacturertoyota",
                                               "car_manufacturernissan","fueltype","car_manufacturermitsubishi","cylindernumberfive",
                                               "cylindernumberfour","car_manufacturerporsche"), 
                              executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
           model_30 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                               "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                               "car_manufacturerisuzu","car_manufacturerchevrolet","car_manufacturerpeugeot",
                                               "car_manufacturermercury","car_manufacturermazda","car_manufacturerhonda",
                                               "car_manufacturerplymouth","car_manufacturerdodge","car_manufacturerrenault",
                                               "car_manufacturervolkswagen","car_manufacturersubaru","car_manufacturertoyota",
                                               "car_manufacturernissan","fueltype","car_manufacturermitsubishi","cylindernumberfive",
                                               "cylindernumberfour","car_manufacturerporsche","carbodyhardtop"), 
                              executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
           model_31 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                               "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                               "car_manufacturerisuzu","car_manufacturerchevrolet","car_manufacturerpeugeot",
                                               "car_manufacturermercury","car_manufacturermazda","car_manufacturerhonda",
                                               "car_manufacturerplymouth","car_manufacturerdodge","car_manufacturerrenault",
                                               "car_manufacturervolkswagen","car_manufacturersubaru","car_manufacturertoyota",
                                               "car_manufacturernissan","fueltype","car_manufacturermitsubishi","cylindernumberfive",
                                               "cylindernumberfour","car_manufacturerporsche","carbodyhardtop","carbodysedan"), 
                              executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
           model_32 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                               "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                               "car_manufacturerisuzu","car_manufacturerchevrolet","car_manufacturerpeugeot",
                                               "car_manufacturermercury","car_manufacturermazda","car_manufacturerhonda",
                                               "car_manufacturerplymouth","car_manufacturerdodge","car_manufacturerrenault",
                                               "car_manufacturervolkswagen","car_manufacturersubaru","car_manufacturertoyota",
                                               "car_manufacturernissan","fueltype","car_manufacturermitsubishi","cylindernumberfive",
                                               "cylindernumberfour","car_manufacturerporsche","carbodyhardtop","carbodysedan",
                                               "carbodywagon"), 
                              executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
           model_33 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                               "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                               "car_manufacturerisuzu","car_manufacturerchevrolet","car_manufacturerpeugeot",
                                               "car_manufacturermercury","car_manufacturermazda","car_manufacturerhonda",
                                               "car_manufacturerplymouth","car_manufacturerdodge","car_manufacturerrenault",
                                               "car_manufacturervolkswagen","car_manufacturersubaru","car_manufacturertoyota",
                                               "car_manufacturernissan","fueltype","car_manufacturermitsubishi","cylindernumberfive",
                                               "cylindernumberfour","car_manufacturerporsche","carbodyhardtop","carbodysedan",
                                               "carbodywagon","carbodyhatchback"), 
                              executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
           
           # Note: An appendix to the results of the models is given at the end of the code for reference
           #       Please refer for evaluation.
           
           # Final model = model_33, which statistics as follows
           # Number of features chosen : 6 + 1(intercept)
           # List of final features    : enginelocation,carwidth,horsepower,car_manufacturerbmw,car_manufacturerbuick
           #                             car_manufacturerjaguar
           # R-square                  : 0.9175
           # Adjusted R-square         : 0.915
           # The above values of R-square and adjuster R-square are pretty good. Lets evaluate our model.

# ---- CRISP-DM Framework Stage 5 - Model Evaluation ---- #
           
           # Note: A summary of all models, and decision points is given at the end of this file.
           # Lets check the noise based on model_33
           
           car_predict.df <- car_math.df
           car_predict.df$price_predicted <- predict(model_33,(car_predict.df %>% dplyr::select(-price)))
           cor(car_predict.df$price,car_predict.df$price_predicted)
           r_square_prediction <- (cor(car_predict.df$price,car_predict.df$price_predicted))^2
           # 0.9175131
           
           car_predict.df$error <- (car_predict.df$price - car_predict.df$price_predicted)
           
           # Noise plot #
           G27 <- ggplot(car_predict.df) + geom_line(aes(x=c(1:nrow(car_predict.df)),y=0),col="red",size=1) +
             geom_point(aes(x=c(1:nrow(car_predict.df)),y=error)) +
             scale_x_continuous(breaks = seq(0,250,5)) +
             scale_y_continuous(breaks = seq(-6000,12000,1000)) +
             labs(title="G27 - Noise in prediction",x="Number of observations",y="Error")
           grid.arrange(G27)
           
           # The noise in the prediction is indeed random and does not follow a pattern. Hence our analysis
           # seems to be working properly.
           
           # Lets plot the prediction, and see how accurately we are able to predict#
           G28 <- ggplot(car_predict.df) + geom_line(aes(x=c(1:nrow(car_predict.df)),y=price,col="blue"),size=1) +
             geom_line(aes(x=c(1:nrow(car_predict.df)),y=price_predicted,col="red"),size=1) +
             scale_color_discrete(name = "Price", labels = c("Actual Price","Predicted Price")) +
             scale_x_continuous(breaks = seq(0,210,5)) +
             scale_y_continuous(breaks = seq(0,50000,5000)) +
             labs(title="G28 - Actual Vs Predicted Price",x="Number of observations",y="Price")
           grid.arrange(G28)
           
           # The prediction graph, seems to follow a genuine pattern. It is missing some very high peaks
           # but other wise it is able to trace the patterns to a large extent.
           
           # -- Perform bootstrapping method to check more closely --#
           num_iter <- 500
           ratio <- 0.8     #For in-sample data#
           cols_to_consider <- c("enginelocation","carwidth","horsepower","car_manufacturerbmw",
                                 "car_manufacturerbuick","car_manufacturerjaguar","price")
           bootstrap_result.df <- data.frame(iteration=vector(mode="numeric"),train_r_square=vector(mode="numeric"),
                                             train_adjusted_r_square=vector("numeric"),predict_r_square=vector("numeric"))
           set.seed(100)
           pb <- txtProgressBar(min=1,max=num_iter,style = 3)
           for (itr in c(1:num_iter)) {
             setTxtProgressBar(pb,itr)
             random_train_rows_idx <- sample(c(1:nrow(car_math.df)),ratio*nrow(car_math.df),replace=FALSE)
             test_rows_idx <- c(1:nrow(car_math.df))[which(! c(1:nrow(car_math.df)) %in% random_train_rows_idx)]
             train.df <- car_math.df[random_train_rows_idx,cols_to_consider]
             test.df <- car_math.df[test_rows_idx,cols_to_consider]
             # There is problem with model here variable #
             # Sometimes one of variable's coefficient gets predicted as NA, and hence later on predict function
             # call generated a warning like,
             # "prediction from a rank-deficient fit may be misleading"
             # According to following articles, 
             # https://stackoverflow.com/questions/26558631/predict-lm-in-a-loop-warning-prediction-from-a-rank-deficient-fit-may-be-mis
             # https://stackoverflow.com/questions/40774922/how-to-solve-rank-deficient-fit-may-be-misleading-error-on-my-linear-model-in
             # This happens when there is co-relation between the variables (Which is not true in our case)
             # Also it can happen when there is no coeffient available for a predictor because
             #   a) It is following a specific pattern, which cannot be seen in the test data
             #   b) Or there are values which can be modelled in the current dataset given.
             # Lets check this by checking the number of row in the matrix of coefficients given in temp_model
             # If there are not 7 rows (1 intercept + 6 variables), then we continue on the loop
             temp_model <- lm(price~.,data=train.df)
             if (! nrow(summary(temp_model)[[4]]) == 7) {
               #cat("\nDid a continue here : ",itr,"\n")
               next
             }
             #print(nrow(summary(temp_model)[[4]]))
             
             test.df$price_predicted <- predict(temp_model,(test.df %>% dplyr::select(-price)))
             prediction_r_square <- (cor(test.df$price,test.df$price_predicted))^2
             bootstrap_result.df[itr,] <- c(itr,summary(temp_model)[[8]],summary(temp_model)[[9]],prediction_r_square)
           }
           
           # Mean value of the predict_r_square
           mean(bootstrap_result.df$predict_r_square,na.rm=TRUE)
           # 0.9075477 <- this is pretty good! 
           
           bootstrap_nona_rows <- nrow(bootstrap_result.df %>% filter(!is.na(iteration)))
           G29 <- ggplot(bootstrap_result.df %>% filter(!is.na(iteration))) + 
             geom_line(aes(x=c(1:bootstrap_nona_rows),y=train_r_square,col="blue")) +
             geom_line(aes(x=c(1:bootstrap_nona_rows),y=train_adjusted_r_square,col="green")) +
             geom_line(aes(x=c(1:bootstrap_nona_rows),y=predict_r_square,col="red")) +
             geom_line(aes(x=c(1:bootstrap_nona_rows),y=min(predict_r_square)),col="black",size=1) +
             geom_line(aes(x=c(1:bootstrap_nona_rows),y=max(predict_r_square)),col="black",size=1) +
             scale_color_discrete(name = "Graphs",labels = c("Train R-Square","Train Adjusted R-Square","Predict R-Square")) +
             scale_x_continuous(breaks = seq(0,500,20)) +
             scale_y_continuous(breaks = seq(0.7,1.0,0.01)) +
             labs(title="G29 - Bootstrap Result plot",x="Number of iterations",y="r-squares")
           grid.arrange(G29)

# --------- FINAL INFERENCE ----------#           
           # Final inference from model evaluation #
           # The final predictors which can be proposed are
           #
           # enginelocation,carwidth,horsepower,car_manufacturerbmw,car_manufacturerbuick
           # car_manufacturerjaguar
           #
           # Coefficients of these predictors and interept and their error and p-value are as follows:
           # Coefficients:
           # Estimate Std. Error t value Pr(>|t|)    
           # (Intercept)            -70131.585   7067.556  -9.923  < 2e-16 ***
           # enginelocation         -17902.242   1506.776 -11.881  < 2e-16 ***
           # carwidth                 1404.073    118.313  11.867  < 2e-16 ***
           # horsepower                 72.798      6.988  10.418  < 2e-16 ***
           # car_manufacturerbmw     10707.003    875.272  12.233  < 2e-16 ***
           # car_manufacturerbuick   11257.183    978.923  11.500  < 2e-16 ***
           # car_manufacturerjaguar  11435.741   1432.614   7.982 1.14e-13 *** 
           
           # The variables given above have an adjusted R-square value = 0.915 and on bootstrapping
           # process the average prediction r-square = 0.9075
           # These value signify the model is able to predict the car prices to a very strong level
           # based on the above predictors.
           
           # Suggestions for the company #
           # The US automobile market seems to be strongly influenced by 3 brands, BMW, BUICK & JAGUAR
           # Apart from the brands, car-width, horsepower, and engine location, play an important role
           # when it comes to pricing.
           # Additional analysis points
           # We can perform a similar analysis on a model that does not have car manufacturers
           # Based such a model, we will be able to recommmend the prices which can be fixed for a specific
           # kind of car made by Geely Auto, and then the competition market can also be built or decided
           # Combining the 2 models we will have to give an optimum price to the proposed cars manufacturered
           # by Gelly Auto, and also work with marketing team for tackling competition of such cars.
           # However, we are not doing the other model (without car manufacturer analysis)  in this assignment
           # as the problem statement does not state so.
           
           
           

# Appendix #
# Model Preparation results, for reference #
           
           #|----------------------------------------------------------------------------------------------|
           #|Result For | Variable         |VIF of feature   | p-value of      |  Current      |  Current  |
           #|Model      | considered for   |being considered | feature being   |  R-square     | Adjusted  |
           #|Number     | removal after    |for removal      | considered for  |               | R-square  |
           #|           | checking results |                 | removal         |               |           |
           #|----------------------------------------------------------------------------------------------|
           #|Model-2    | citympg          | 8.110680        | 0.113808        |0.9555         |0.9456     |
           #|----------------------------------------------------------------------------------------------|
           #|Model-3    | peakrpm          | 3.370834        | 0.152006        |0.9548         |0.9451     |
           #-----------------------------------------------------------------------------------------------|
           #|Model-4    | enginetypeohc    | 3.333436        | 0.119529        |0.9542         |0.9448     |
           #-----------------------------------------------------------------------------------------------|
           #|Model-5    | drivewheelfwd    | 3.449960        | 0.119977        |0.9536         |0.9443     |
           #-----------------------------------------------------------------------------------------------|
           #|Model-6    | fuelsystem2bbl   | 3.347545        | 0.012511        |0.9529         |0.9438     |
           #-----------------------------------------------------------------------------------------------|
           #|Model-7    | wheelbase        | 7.783491        | 0.042520        |0.9512         |0.9421     |
           #-----------------------------------------------------------------------------------------------|
           #|Model-8    | cylindernumbersix| 5.183960        | 0.027921        |0.9512         |0.941      |
           #-----------------------------------------------------------------------------------------------|
           #|Model-9    | compressionratio | 3.885541        | 0.010438        |0.9486         |0.9397     |
           #-----------------------------------------------------------------------------------------------|
           #|Model-10   | car_manufacturer | NO VIF TEST     | 0.799487        |0.9466         |0.9377     |
           #|           | volvo            |                 |                 |               |           |
           #-----------------------------------------------------------------------------------------------|
           #|Model-11   | car_manufacturer | NO VIF TEST     | 0.783098        |0.9466         |0.9381     |
           #|           | isuzu            |                 |                 |               |           |
           #-----------------------------------------------------------------------------------------------|
           #|Model-12   | car_manufacturer | NO VIF TEST     | 0.300970        |0.9465         |0.9384     |
           #|           | chevrolet        |                 |                 |               |           |
           #-----------------------------------------------------------------------------------------------|
           #|Model-13   | car_manufacturer | NO VIF TEST     | 0.083765        |0.9462         |0.9384     |
           #|           | peugeot          |                 |                 |               |           |
           #-----------------------------------------------------------------------------------------------|
           #|Model-14   | car_manufacturer | NO VIF TEST     | 0.103059        |0.9453         |0.9377     |
           #|           | mercury          |                 |                 |               |           |
           #-----------------------------------------------------------------------------------------------|
           #|Model-15   | car_manufacturer | NO VIF TEST     | 0.052627        |0.9445         |0.9371     |
           #|           | mazda            |                 |                 |               |           |
           #-----------------------------------------------------------------------------------------------|
           #|Model-16   | car_manufacturer | NO VIF TEST     | 0.134744        |0.9433         |0.9361     |
           #|           | honda            |                 |                 |               |           |
           #-----------------------------------------------------------------------------------------------|
           #|Model-17   | car_manufacturer | NO VIF TEST     | 0.138178        |0.9426         |0.9357     |
           #|           | plymouth         |                 |                 |               |           |
           #-----------------------------------------------------------------------------------------------|
           #|Model-18   | car_manufacturer | NO VIF TEST     | 0.138178        |0.9419         |0.9352     |
           #|           | dodge            |                 |                 |               |           |
           #-----------------------------------------------------------------------------------------------|
           #|Model-19   | car_manufacturer | NO VIF TEST     | 0.123638        |0.9413         |0.9349     |
           #|           | renault          |                 |                 |               |           |
           #-----------------------------------------------------------------------------------------------|
           #|Model-20   | car_manufacturer | NO VIF TEST     | 0.082114        |0.9405         |0.9344     |
           #|           | volkswagen       |                 |                 |               |           |
           #-----------------------------------------------------------------------------------------------|
           #|Model-22   | car_manufacturer | NO VIF TEST     | 0.106143        |0.9384         |0.9328     |
           #|           | toyota           |                 |                 |               |           |
           #-----------------------------------------------------------------------------------------------|
           #|Model-23   | car_manufacturer | NO VIF TEST     | 0.089550        |0.9376         |0.9322     |
           #|           | nissan           |                 |                 |               |           |
           #-----------------------------------------------------------------------------------------------|
           #|Model-24   | fueltype         | NO VIF TEST     | 0.050919        |0.9366         |0.9316     |
           #-----------------------------------------------------------------------------------------------|
           #|Model-25   | car_manufacturer | NO VIF TEST     | 0.020506        |0.9353         |0.9305     |
           #|           | mitsubishi       |                 |                 |               |           |
           #-----------------------------------------------------------------------------------------------|
           #|Model-26   | cylindernumber   | NO VIF TEST     | 0.013602        |0.9334         |0.9289     |
           #|           | five             |                 |                 |               |           |
           #-----------------------------------------------------------------------------------------------|
           #|Model-27   | cylindernumber   | NO VIF TEST     | 0.018403        |0.9313         |0.9270     |
           #|           | four             |                 |                 |               |           |
           #-----------------------------------------------------------------------------------------------|
           #|Model-28   | car_manufacturer | NO VIF TEST     | 0.002123        |0.9292         |0.9252     |
           #|           | porsche          |                 |                 |               |           |
           #-----------------------------------------------------------------------------------------------|
           #|Model-29   | carbodyhardtop   | NO VIF TEST     | 0.001070        |0.9257         |0.9219     |
           #-----------------------------------------------------------------------------------------------|
           #|Model-30   | carbodysedan     | NO VIF TEST     | 0.04051         |0.9215         |0.9178     |
           #-----------------------------------------------------------------------------------------------|
           #|Model-31   | carbodywagon     | NO VIF TEST     | 0.3117          |0.9198         |0.9165     |
           #-----------------------------------------------------------------------------------------------|
           #|Model-32   | carbodyhatchback | NO VIF TEST     | 0.0363          |0.9193         |0.9165     |
           #-----------------------------------------------------------------------------------------------|
           #|Model-33   |                  | NO VIF TEST     |                 |0.9175         |0.9150     |
           #-----------------------------------------------------------------------------------------------|