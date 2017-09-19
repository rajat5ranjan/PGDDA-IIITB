# ------------------------------------------ #
# Code Description : PGDDA - Uber Case Study #
# Author           : Anugraha Sinha          #
# Roll Number      : DDA1710381              #
# ------------------------------------------ #


# IMPORTANT NOTE FOR EVALUATOR :
# 1) Key inferences in this analysis are marked with following tag
#    TAG = #***#
#    At different places such TAGs can be found for important note
#    from author
# 2) There are 2 kinds of analysis shown for pickup.point city.
#    1 analysis is the conventional demand/supply gap, but, the 2nd one
#    is based on waittime at airport. Evaluation committee is requested
#    to look at that analysis also.

# ------------------------------------ SETUP ----------------------------------------------- #
   # Library Import Segment
   required_packages <- c("dplyr","tidyr","lubridate","stringr","ggplot2","gridExtra")
   new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
   if (length(new_packages)) install.packages(new_packages)
   library(dplyr)
   library(tidyr)
   library(lubridate)
   library(stringr)
   library(ggplot2)
   library(gridExtra)
   
   # Working Directory
   # I am assuming that the working directory will be set where unzip would have been done
   # Hence removing the below. If required, uncomment below line and modify the argument
   # if setwd() function as per need.
   # setwd("C:/Upgrad/")

  
# ----------------------------------DATA LOADING -------------------------------------------- #
   uber <- read.delim("Uber Request Data.csv",sep = ",", stringsAsFactors = FALSE)
   
   # Lets View and check the str of the data imported #
   View(uber)
   str(uber)
   
# ----------------------------------DATA CLEANING ------------------------------------------- #
   
   # ----- MISSING VALUE ANALYSIS ----- #
   
   # 1. Find the number of NAs are there in the dataset #
      length(which(is.na(uber)))
      # Answer : There are 6564 NAs in the data. ahhnn...#
   
   # 2. Lets see how many NAs in which all columns #
      sapply(colnames(uber), function(x) length(which(is.na(uber[,x]))))
      # Answer :
      # Request.id      Pickup.point         Driver.id            Status Request.timestamp    Drop.timestamp
      #          0                 0              2650                 0                 0              3914
   
   # 3. Lets see how many "" (Blanks) are present in chr type columns ##
      # chr columns are c("Pickup.point","Status","Request.timestamp","Drop.timestamp")
      sapply(c("Pickup.point","Status","Request.timestamp","Drop.timestamp"),
             function(x) length(which(uber[,x]=="")))
      # Answer:
      # Pickup.point            Status Request.timestamp    Drop.timestamp 
      #            0                 0                 0                 0
   
   # Inferences uptill now :
   # a) We have about 6564 NA values, which are distributed between Driver.id and Drop.timestamp
   # b) We do not have any blank values in chr type columns
   
   # 4. Since there are Drop.timestamp as NAs, lets check how many trips did not complete
   #    and the count for them, because if they did not complete we will have NAs in Drop.timestamp
      levels(as.factor(uber$Status))
      uber %>% filter(is.na(Drop.timestamp)) %>% group_by(Status) %>% summarize(cnt=length(Request.id))
      
      # Answer :              Status   cnt
      #                        <chr> <int>
      #          1         Cancelled  1264
      #          2 No Cars Available  2650
      # Total Cnt = 1264 + 2650 = 3914 ---> Same as the number of Drop.timestamp NAs.
      # Final conclusion :
#***# # The number of Drop.timestamp as NAs are because the trips did not complete ##
      
   # 5. Lets see if there is any relation between Driver ID being NA and other columns
      # Checking by Status of the trip
      uber %>% filter(is.na(Driver.id)) %>% group_by(Status) %>% summarize(cnt=length(Request.id))
      # Answer :              Status   cnt
      #                        <chr>  <int>
      #          1 No Cars Available   2650
      # The number of Driver.id which are NA is because there are No Cars Available, hence
      # Driver.id's are blank ##
      
   # ----- FINAL Inferences from Data cleaning process ----- #
   # 1. There are NAs in the data set, but no blanks (chr columns)
   # 2. Drop.timestamp == NAs match with trip which did not get completed (i.e. Status == Cancelled/No Cars)
   # 3. Driver.id == NAs match with trip where Status == No Cars Available
   #
   # ---> What can be done of these NAs <--- #
   # a) Drop.timestamp and Driver.IDs should remain NAs, it would assist in identification later

      
   # ----- Standardizing Data ----- #
   # 1) Converting certain columns to factors, so that it becomes easier in plotting ##
     uber$Pickup.point <- as.factor(uber$Pickup.point)
     uber$Status <- as.factor(uber$Status)
     str(uber)
     
  # 2) Changes for Request.timestamp and Drop.timestamp
     #    Request.timestamp & Drop.timestamp values seem to be in 2 formats
     #    Format 1 : dd/mm/yyyy hh:mm
     #    Format 2 : dd/mm/yyyy hh:mm:ss
     #    We are interested in hh:mm kind of format, so using sapply with str_split
     uber$Request.timestamp <- sapply(uber$Request.timestamp,
                                           function (x) paste(str_split(x,pattern=":")[[1]][1:2],collapse=":"))
     uber$Drop.timestamp <- sapply(uber$Drop.timestamp, 
                                        function (x) paste(str_split(x,pattern=":")[[1]][1:2],collapse=":"))
  
  # 3) Converting Request.timestamp to PosixCt Datetime objects
  #    Note : We are using functions from lubridate package
  #    Remember : There are no NA/blanks here, so we should have all conversions done
      uber <- uber %>% mutate(Request.timestamp_2 = dmy_hm(uber$Request.timestamp,tz="Asia/Calcutta"))
      # Lets check if all values have been converted or not for Request.timestamp ##
      length(which(is.na(uber$Request.timestamp_2)))
      # Answer : 0 
      #          All values for Request timestamp converted successfully ##
     
  # 4) Converting Drop.timestamp to PosixCt Datetime objects
  #    Note: We already have some NAs present in Drop.timestamp, we need to be careful
      uber <- uber %>% mutate(Drop.timestamp_2 = dmy_hm(uber$Drop.timestamp,tz="Asia/Calcutta"))
      # We got some warnings, yes they would be there, as there some NAs in Drop.timestamp
     
      # Check no undue change done #
      length(which(is.na(uber$Drop.timestamp_2)))
      # Answer : 3914
      #          Same as earlier, so thats good.
  
# ----------------------------------- DERIVED METRICS - PHASE 1 ------------------------------- #
  # ------ Type Driven Derived Metrics - Phase 1 ------ #   
  
  # 1. Building following data from Request.timestamp_2 & Drop.timestamp_2
      # a) Lets see how many unique dates do we have
      unique(date(uber$Request.timestamp_2))
      # Answer : "2016-07-11" "2016-07-12" "2016-07-13" "2016-07-14" "2016-07-15"
      unique(date(uber$Drop.timestamp_2))
      # Answer : "2016-07-11" "2016-07-12" "2016-07-13" "2016-07-14" "2016-07-15" "2016-07-16" NA
      
      # b) Hours given in data
      sort(unique(hour(uber$Request.timestamp_2)))
      # Answer : 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
      sort(unique(hour(uber$Drop.timestamp_2)))
      # Answer : 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
      
      # Inference #
      # 1) There are only 5 days of data given, which spans all 24 hours
      # 2) We can make a 
      #     Request.date
      #     Drop.date
      #     Request.hour
      #     Drop.hour column
      
      uber <- uber %>% mutate(Request.date = date(Request.timestamp_2),
                              Request.hour = hour(Request.timestamp_2),
                              Drop.date = date(Drop.timestamp_2),
                              Drop.hour = hour(Drop.timestamp_2))
      
  # ------ Business Driven Derived Metrics - Phase 1 ------ #
  # 1. Lets create a trip length column also
      uber <- uber %>% mutate(Trip.length = interval(Request.timestamp_2,Drop.timestamp_2)/dminutes(1))
      # Check the number of NAs in Trip.length
      length(which(is.na(uber$Trip.length)))
      # Answer : 3914
      #          Same as the number of Drop.timestamp are NAs, so we are good.
      
  
# ------------------------------- UNIVARIATE ANALYSIS --------------------------------- #
  # 1) Request.id 
       # ----------------------------------------------
       # | Variable class      |        Numeric       |
       # | Discrete/continuous |        Discrete      |
       # | Type                |        Ordinal       |
       # ----------------------------------------------
       # This is trip identifier, are unique numbers. Checking
       length(uber$Request.id)
       # Answer : 6745
       length(which(is.na(uber$Request.id)))
       # Answer : 0
       length(unique(uber$Request.id))
       # Answer : 6745
       
       # Inference : Only unique values present. 
       # For records, lets just see the min, max, median of request.ids
       max(uber$Request.id)     # 6766
       min(uber$Request.id)     # 1
       median(uber$Request.id)  # 3387 -> Ahhnn.. this is strange, if there are all in then there should
                                # 6766/2 = 3383. Seems there are some missing
       length(setdiff(c(1:6766),sort(uber$Request.id)))
       # Answer: 21            
       # Inference :
       # which is same as 6766 - 6745. So there are some missing Request.ids
         
  # 2) Pickup.point
       # ----------------------------------------------
       # | Variable class      |        character     |
       # | Discrete/continuous |        Discrete      |
       # | Type                |        Nominal       |
       # ----------------------------------------------
       ## Lets see what is Pickup.point distribution
       # By Numbers #
       uber %>% group_by(Pickup.point) %>% summarize(cnt=length(Request.id)) %>% arrange(desc(cnt)) %>%
         mutate(perc=(cnt/sum(cnt))*100)
       # A plot - G(01)#
       G01 <- ggplot(uber,aes(x=1,fill=Pickup.point)) + 
         scale_x_continuous(breaks = seq(0,0,1)) +
         scale_y_continuous(breaks = seq(0,1,0.1)) +
         geom_bar(position="fill") +
         labs(title="Distribution of all Cab request by\nPickup Point - G(01)",x="",y="Count (%)",fill="Pickup Point")
       grid.arrange(G01)
       ##  Pickup.point   cnt     perc
       ##        <fctr> <int>    <dbl>
       ##          City  3507 51.99407
       ##       Airport  3238 48.00593
       ## Inference : The data is distributed almost evenly between the pickup points
    
  # 3) Status
       # ----------------------------------------------
       # | Variable class      |        character     |
       # | Discrete/continuous |        Discrete      |
       # | Type                |        Nominal       |
       # ----------------------------------------------
       # By numbers #
       uber %>% group_by(Status) %>% summarize(cnt=length(Request.id)) %>% arrange(desc(cnt)) %>% 
         mutate(perc=(cnt/sum(cnt))*100)
       # A plot G(02)#
       G02 <- ggplot(uber,aes(x=1,fill=Status)) + 
         scale_x_continuous(breaks = seq(0,0,1)) +
         scale_y_continuous(breaks = seq(0,1,0.1)) +
         geom_bar(position="fill") +
         labs(title="Distribution of all Cab request\nby Status - G(02)",x="",y="Count (%)")
       grid.arrange(G02)
#***#  # Inference : 
       #                Status   cnt     perc
       #                <fctr>  <int>    <dbl>
       #        Trip Completed  2831 41.97183
       #     No Cars Available  2650 39.28836
       #             Cancelled  1264 18.73981
       # There is high number of "NO CARS AVAILABLE" problem, approx the same as that of
       # trip completed. Cancelled request percentage is also quite high.
       # This needs to be investigated based on Pickup.points and hours
    
  # 4) Request.date
       # ----------------------------------------------
       # | Variable class      |        character     |
       # | Discrete/continuous |        Discrete      |
       # | Type                |        Ordinal       |
       # ----------------------------------------------
       # By Numbers #
       uber %>% group_by(Request.date) %>% 
         summarize(cnt=length(Request.id)) %>% 
         arrange(Request.date) %>% 
         mutate(weekday=wday(Request.date,label=TRUE))
       # A plot - G(03)#
       G03 <- ggplot(uber,aes(x=1,fill=as.factor(Request.date))) + 
         scale_x_continuous(breaks = seq(0,0,1)) +
         scale_y_continuous(breaks = seq(0,1400,100)) +
         geom_bar(position="dodge") +
         labs(title="Distribution of all Cab request by Date - G(03)",x="",y="Count",fill="Date")
       grid.arrange(G03)
#***#  # Inference :
       #   Request.date   cnt weekday
       #         <date> <int>   <ord>
       #     2016-07-11  1367     Mon
       #     2016-07-12  1307    Tues
       #     2016-07-13  1337     Wed
       #     2016-07-14  1353   Thurs
       #     2016-07-15  1381     Fri
       # The number of trips are almost the same over all the dates given
       # The dates given are all WeekDays, hence we do not need to do a separate analysis for weekdays
       # & weekends
         
  # 5) Request.timestamp_2
       # ----------------------------------------------
       # | Variable class      |        POSIXct       |
       # | Discrete/continuous |        Discrete      |
       # | Type                |        Interval      |
       # ----------------------------------------------
       # A Plot - G(04)
       G04 <- ggplot(uber %>% group_by(Request.date,Request.hour) %>% 
                summarize(cnt=length(Request.id))) +
         geom_col(aes(x = Request.hour, y = cnt),fill="blue",col="black") +
         geom_text(aes(x = Request.hour, y = cnt/2, label=cnt),col="white",size=2) +
         scale_x_continuous(breaks=seq(0,23,1)) +
         scale_y_continuous(breaks=seq(0,100,20)) +
         facet_wrap(~Request.date,scales="free") +
         labs(title="Cab Request by hour(all days & all pickup points) - G(04)",x = "Hour", y = "Count")
       grid.arrange(G04)
#***#  # Inference :
       # There are 2 zones when the cab requests seem to be high, however this is not granualar
       # We need build exact segments out of the time.
       # Since, across the dates, we have similar cab request freq, and similar distribution about the hour 
       # therefore, we can take up averages to see how at at which hour and min of the day, the request 
       # increases
       
       # A Plot - G(05) #
       G05 <- ggplot(uber %>% 
               mutate(Request.minute=minute(Request.timestamp_2)) %>% 
               group_by(Request.date,Request.hour,Request.minute) %>% 
               summarize(cnt2=length(Request.id)) %>%
               group_by(Request.hour,Request.minute) %>%
               summarize(count=round(mean(cnt2),digits=2),len=length(cnt2),mini=min(cnt2),maxi=max(cnt2)),
               aes(x=Request.hour,y=Request.minute,value=count)) + 
         scale_fill_gradient(low="blue",high="red",breaks=seq(0,4,1)) + 
         geom_tile(aes(fill=count)) + 
         geom_text(aes(label=count),col="white",size=2) + 
         #facet_wrap(~Request.date) +
         scale_x_continuous(breaks = seq(0,23,1)) + 
         scale_y_continuous(breaks = seq(0,60,1)) +
         labs(title="Average Cab Request (Over all 5 days), by hour and minute - G(05)",x="Hour",y="Minute",fill="Avg Req")
      grid.arrange(G05)
      
      # To sum up, both the plots #
      grid.arrange(G04,G05,nrow=1,ncol=2)
#***#  # Inference : The time segment division stands like this
       # ------------------------------------------------------
       # |      Time of the day    |        SegmentName       |
       # ------------------------------------------------------
       # |        0000 - 0359      |        1_LateNight       |
       # |        0400 - 0559      |       2_EarlyMorning     |
       # |        0600 - 1059      |       3_MorningRush      |
       # |        1100 - 1659      |           4_Day          |
       # |        1700 - 2359      |       5_EveningRush      |
       # ------------------------------------------------------
       uber[which(uber$Request.hour %in% c(0:3)),'Day.segment'] <- "1_LateNight"
       uber[which(uber$Request.hour %in% c(4:5)),'Day.segment'] <- "2_EarlyMorning"
       uber[which(uber$Request.hour %in% c(6:10)),'Day.segment'] <- "3_MorningRush"
       uber[which(uber$Request.hour %in% c(11:16)),'Day.segment'] <- "4_Day"
       uber[which(uber$Request.hour %in% c(17:23)),'Day.segment'] <- "5_EveningRush"
       uber$Day.segment <- as.factor(uber$Day.segment)
  

# -------------------- Segmented Univariate Analysis & Bivariate Analysis ----------------- #
       
       # ------------ ANALYSIS FOR THE PROBLEM AT THE AIRPORT --------------#
         
       # Plot - G(06) #
       # Lets analyze the trips where Pickup.point = Airport
       #     a) ColPlot dodged over Trip Status - Faceted Date wise
       #     b) A line plot showing total number of requests at the hour
       G06 <- ggplot() +
         geom_col(data = (uber %>% filter(Pickup.point=="Airport") %>% 
                            group_by(Request.date,Request.hour,Status) %>% 
                            summarize(total_trip=length(Request.id))),
                  aes(x = Request.hour, y=total_trip, fill=Status),position=position_dodge(width=0.8))+
         geom_line(data = (uber %>% filter(Pickup.point=="Airport") %>% 
                             group_by(Request.date,Request.hour) %>%
                             summarize(total_trip = length(Request.id))),
                   aes(x = Request.hour, y=total_trip)) +
         facet_wrap(~Request.date, scales = "free") +
         #theme_tufte() + theme(axis.line=element_line()) +
         scale_x_continuous(breaks = seq(0,23,2)) +
         scale_y_continuous(breaks = seq(0,95,15)) +
         labs(title="Pickup Point : Airport - Trip Analysis - G(06)",x="Hour",y="Count",fill="Trip Status")
       grid.arrange(G06)
#***#  # Inference : 
       # a) Trips from Airport show an evenly distribution over the days.
       # b) Since the plots are almost similar, we can take an average over the dates 
       #    to see the Day.segment wise Trip Status
       G07 <- ggplot() + 
         geom_col(data = (uber %>% filter(Pickup.point == "Airport") %>% 
                            group_by(Request.date,Day.segment,Status) %>% 
                            summarize(cnt=length(Request.id)) %>% 
                            group_by(Day.segment,Status) %>% 
                            summarize(avg.trips = mean(cnt))),
                  aes(x=Day.segment,y=avg.trips,fill=Status),position=position_dodge(width=0.8)) +
         scale_y_continuous(breaks = seq(0,300,20)) +
         labs(title="Pickup Point : Airport - Average Trip Status over the day - G(07)",
              x="Segment of the day", y="Average Number of Trips",fill="Trip Status")
       grid.arrange(G07)

#***#  # Inference :
       # In the evening rush hours (1700 - 2359), there is problem of No Cars Available.
       # Drivers are cancelling very less number of requests, when seen individually over the
       # all dates data and the averaged data as well. There seems to be a supply and demand
       # gap between what is required at the airport and what is available
       
       # ------------------------------------------------------------------------------------------#
       # Demand @ Airport in evening   = Trip Completed + Cancelled  + No Cabs Available           #
       # Supply @ Airport in evening   = Trip Completed + Cancelled                                #
       # ------------------------------------------------------------------------------------------#
       # Reasons : Cancelled are also considered in supply because even the requests which are 
       #           cancelled by the driver proves that organic supply of cab is there.
       #           Motivating the driver to take up the request, or why driver is dropping the request
       #           is entirely a different analysis.
       
       # Lets check the organic supply available on all days in Evening Rush hours
       # All cabs requests, with pickup.points Airport/City which get completed or cancelled
       # can be considered as organic available supply. 
       
       airport.evening.df <- uber %>% 
         filter(Pickup.point=="Airport",Day.segment=="5_EveningRush") %>% 
         group_by(Request.date,Status) %>% 
         summarize(cnt=length(Request.id))
         
       #The above is in a long format ->convert-> wide format
         
       spread.airport.evening.df <- data.frame(spread(airport.evening.df,key=Status,value=cnt))
         
       # Build new columns as per above definition of demand and supply, pull back by gather#
       # The below is a complex one, so a 3 step understanding is as follows
       # 1) spread.airport.evening.df %>% mutate(demand=Cancelled+No.Cars.Available+Trip.Completed,supply=Trip.Completed+Cancelled)
       # 2) Select only Request.date, demand & supply columns
       # 3) Use gather to bring them to long format for ease of plotting.
         
       airport.evening.demand_supply.df <- gather((spread.airport.evening.df %>% 
                                                     mutate(demand=Cancelled+No.Cars.Available+Trip.Completed,supply=Trip.Completed+Cancelled) %>% 
                                                     select(Request.date,demand,supply)),
                                                  key="demand.supply",values=c(2,3))
       #For ref :
       airport.evening.demand_supply.df
         
       # A plot
       G08 <- ggplot(data = airport.evening.demand_supply.df) +
         geom_col(aes(x=as.character(Request.date),y=value,fill=demand.supply),position=position_dodge(width=0.8)) + 
         scale_y_continuous(breaks=seq(0,450,25)) +
         labs(title="Airport Demand & Supply Gap graph during\nEveningRush Hours - G(08)",
              x="Date",
              y="No. of Requests",
              fill="Demand/Supply")
  
       grid.arrange(G08)
         
       # Combined plot #
       grid.arrange(G07,G08,ncol=2,nrow=1)

#***#  # Problem of "No Cars Available" in EveningRush at Airport #
       # On an average the Requests getting closed as "No Cars Available" ~ 290   in evening
       # The total organic supply available : 125 ~ 150
       # There is a geniuine lack of supply for the shooting demand.
       # We need to stimulate more organic supply for evening rush at airport.
         
       # A plot for final picture #
       grid.arrange(G06,G07,G08,ncol=2,nrow=2)         
         
      
       # ---------------------- ANALYSIS FOR THE PROBLEM AT CITY -----------------#
         
       # Lets analyze the trips where Pickup.point = City
       #     a) ColPlot dodged over Trip Status - Faceted Date wise
       #     b) A line plot showing total number of requests at the hour
       G09 <- ggplot() +
         geom_col(data = (uber %>% filter(Pickup.point=="City") %>% 
                            group_by(Request.date,Request.hour,Status) %>% 
                            summarize(total_trip=length(Request.id))),
                  aes(x = Request.hour, y=total_trip, fill=Status),position=position_dodge(width=0.8))+
         geom_line(data = (uber %>% filter(Pickup.point=="City") %>% 
                             group_by(Request.date,Request.hour) %>%
                             summarize(total_trip = length(Request.id))),
                   aes(x = Request.hour, y=total_trip)) +
         facet_wrap(~Request.date, scales = "free") +
         #theme_tufte() + theme(axis.line=element_line()) +
         scale_x_continuous(breaks = seq(0,23,2)) +
         scale_y_continuous(breaks = seq(0,95,5)) +
         labs(title="Pickup Point : City - Trip Analysis - G(09)",x="Hour",y="Count",fill="Trip Status")
       grid.arrange(G09)
#***#  # Inference : 
       # a) Trips from City show an evenly distribution over the days.
       # b) Since the plots are almost similar, we can take an average to see the Day.segment wise
       #    Trip Status
       G10<-ggplot() +
         geom_col(data = (uber %>% filter(Pickup.point == "City") %>% 
                            group_by(Request.date,Day.segment,Status) %>% 
                            summarize(cnt=length(Request.id)) %>% 
                            group_by(Day.segment,Status) %>% 
                            summarize(avg.trips = mean(cnt))),
                  aes(x=Day.segment,y=avg.trips,fill=Status),position=position_dodge(width=0.8)) +
         scale_y_continuous(breaks = seq(0,300,20)) +
         labs(title="Pickup Point : City - Average Trip Status over the day - G(10)",
              x="Segment of the day", y="Average Number of Trips",fill="Trip Status")
       grid.arrange(G10)
#***#  # Inference :        
       # There are high "Cancellations" & "No Cabs available" during "EarlyMorning" & "MorningRush" hours
       

       # ------------- ANALYSIS TYPE 1 - For CITY problem based on DEMAND & SUPPLY GAP ------------- #
       
       # For analyzing the demand supply gap, lets go with following assumption.
       # ------------------------------------------------------------------------------------------#
       # Demand @ Airport in evening   = Trip Completed + Cancelled  + No Cabs Available           #
       # Supply @ Airport in evening   = Trip Completed + Cancelled                                #
       # ------------------------------------------------------------------------------------------#
       # Reasons : Cancelled are also considered in supply because even the requests which are 
       #           cancelled by the driver proves that organic supply of cab is there.
       #           Motivating the driver to take up the request, or why driver is dropping the request
       #           is entirely a different analysis.
       
       city.morning.df <- uber %>% 
         filter(Pickup.point=="City",(Day.segment %in% c("2_EarlyMorning","3_MorningRush"))) %>% 
         group_by(Request.date,Day.segment,Status) %>%
         summarize(cnt=length(Request.id))
       
       #The above is in a long format ->convert-> wide format
       
       spread.city.morning.df <- data.frame(spread(city.morning.df,key=Status,value=cnt))
       
       # Build new columns as per above definition of demand and supply, pull back by gather#
       # The below is a complex one, so a 3 step understanding is as follows
       # 1) spread.city.morning.df %>% mutate(demand=Cancelled+No.Cars.Available+Trip.Completed,supply=Trip.Completed+Cancelled)
       # 2) Select only Request.date, Day.segment, demand & supply columns
       # 3) Use gather to bring them to long format for ease of plotting.
       
       city.morning.demand_supply.df <- gather((spread.city.morning.df %>% 
                                                     mutate(demand=Cancelled+No.Cars.Available+Trip.Completed,supply=Trip.Completed+Cancelled) %>% 
                                                     select(Request.date,Day.segment,demand,supply)),
                                                  key="demand.supply",values=c(3,4))
       #For ref :
       city.morning.demand_supply.df
       
       # A plot
       G11 <- ggplot(data = city.morning.demand_supply.df) +
         geom_col(aes(x=as.character(Request.date),y=value,fill=demand.supply),position=position_dodge(width=0.8)) +
         scale_y_continuous(breaks=seq(0,450,25)) +
         facet_wrap(~Day.segment,scales = "free") +
         labs(title="City Demand & Supply Gap graph during\nEarlyMorning & MorningRush Hours - G(11)",x="Date",y="No. of Requests",fill="Demand/Supply")
       grid.arrange(G11)
       
       # Composite plot #
       grid.arrange(G10,G11,nrow=1,ncol=2)
       
       # By above analysis it can be said that demand/supply gap is definitely a reason
       # for problems at the city in the morning
       
       # Complete picture #
       grid.arrange(G09,G10,G11,ncol=2,nrow=2)
       
       
       
       # ------------- ANALYSIS TYPE 2 - For CITY problem based on WAIT TIME ANALYSIS ------------- #
       
       # We need to check why cancellations are happening.
       # After manually scanning through the data set is seems that that waiting time
       # for drivers in the morning is high. Lets prove this from analysis.
       #
       # Analysis Method:
       # 1) Filter all trips which are "Trip Completed or Cancelled", and sort by Driver.id & Request.timestamp_2
       # 2) Iterate over different Driver.ids
       # 3) For each driver.id take up all the rows from Point 1 Date.frame
       # 4) Iterate over all the rows, and see if
       #    4.a) Current Trip Pickup.Point == City && Next Trip Pickup.point == "Airport"
       #    4.b) Current Trip Status == "Trip Completed"
       #    4.c) Current Trip Request.date == Next Trip Request.date are same
       # 5) If above are yes, then this a potential candidate for WaitingTime calculation
       # 6) Build a data frame with selected details.
        
       filter.df <- uber %>% filter(Status %in% c("Trip Completed","Cancelled")) %>% arrange(Driver.id,Request.timestamp_2)
       finalOutput.df <- data.frame(matrix(ncol=16,nrow = 0))
       selected.uber <- uber[0,]
       colnames(finalOutput.df) <-  c("Driver.id",
                              "a_Request.id","a_Pickup.point","a_Drop.point","a_Request.date","a_Request.hour","a_Trip.length","a_Day.segment",
                              "b_Request.id","b_Pickup.point","b_Drop.point","b_Request.date","b_Request.hour","b_Trip.length","b_Day.segment",
                              "WaitingTime")
       finalOutput.itr <- 1                     
       for (d.id in unique(filter.df$Driver.id)) {
         driverInfo.df <- filter.df %>% filter(Driver.id == d.id)    # Selected Driver
         itr <- 1
         for ( itr in c(1:(nrow(driverInfo.df)-1)) ) {         # Remember, since we are taking a club of 2, it best to iterate over (nrow - 1)
           if (driverInfo.df[itr,'Pickup.point'] == "City" && 
               driverInfo.df[(itr+1),'Pickup.point'] == "Airport" && 
               driverInfo.df[itr,'Request.date'] == driverInfo.df[(itr+1),'Request.date'] && 
               driverInfo.df[itr,'Status'] == "Trip Completed") {
             # FOUND A CANDIDATE FOR WAITTIME CALCULATION #
       
             # Calculate Wait Time #
             waitTime <- as.numeric(interval(driverInfo.df[itr,'Drop.timestamp_2'],driverInfo.df[(itr+1),'Request.timestamp_2'])/dminutes(1))
             
             # Debug Print Statement #
             #cat("2. d.id = ",d.id," itr = ",itr," WaitTime = ",waitTime," finalOutput.itr = ",finalOutput.itr,"\n")
             
             # Save the data in finalOUtput #
             finalOutput.df[finalOutput.itr,] <- c(d.id,
                                   driverInfo.df[itr,'Request.id'],"City","Airport",as.character(driverInfo.df[itr,'Request.date']),as.character(driverInfo.df[itr,'Request.hour']),driverInfo.df[itr,'Trip.length'],as.character(driverInfo.df[itr,'Day.segment']),
                                   driverInfo.df[(itr+1),'Request.id'],"Airport","City",as.character(driverInfo.df[(itr+1),'Request.date']),as.character(driverInfo.df[(itr+1),'Request.hour']),driverInfo.df[(itr+1),'Trip.length'],as.character(driverInfo.df[(itr+1),'Day.segment']),
                                   waitTime)
             
             # Save in selected.uber for reference #
             selected.uber <- rbind(selected.uber,driverInfo.df[itr,])
             selected.uber <- rbind(selected.uber,driverInfo.df[(itr+1),])
             itr <- itr + 2
             finalOutput.itr <- finalOutput.itr + 1
           }
           else {
             itr <- itr + 1
           }
         }
       }
       # Convert waitingTime as numeric #
       finalOutput.df$WaitingTime <- as.numeric(finalOutput.df$WaitingTime)
       # Lets see the spread of the waiting time across different day segment #
       G12 <- ggplot() + 
         geom_boxplot(data = finalOutput.df, aes(x=as.factor(a_Day.segment),y=WaitingTime,fill=a_Day.segment)) +
         scale_y_continuous(breaks = seq(0,1250,100)) +
         labs(title="Waiting Time (minutes) for Trips from City->Airport\nDistribution Over Different Times\nof the Day - G(12)",x="Segment of the Day",y="WaitingTime",fill="Segment of Day")
       grid.arrange(G12)
       
       # For ref, in numbers #
       finalOutput.df %>% 
         group_by(a_Day.segment) %>% 
         summarize(avg=mean(WaitingTime),median=median(WaitingTime),maximum=max(WaitingTime),minimum=min(WaitingTime))
       
#***#  # Inference :
       # The waiting time is very high during "EarlyMorning" & "MorningRush" hours, and hence
       # this could be a probable reason for drivers cancelling the request from the city, during these
       # segments of the day.
       # Summarization plot #
       grid.arrange(G11,G12,nrow=1,ncol=2)
       
# ----------------------------- FINAL CONCLUSION ------------------------#
# 1. The masked data set is quite evenly distributed amoung the dates,
#    pickup points and also across the hours on each date.
# 2. Binning of hours under different sets is possible due to simitric distribution
#    of data
# 3. The main issue with trip where pickup.point is Airport is evening rush hours
#    where organic supply is missing. This has been proved from demand/supply
#    comparison graph over all dates
# 4. The main issue with trip where pickup.point is City is cancellation in the
#    morning hours. One of the issue is demand supply gap and also the waiting
#    time at the airport.
# 5. PPT proposes recommendations for addressing these problems from general
#    perspective.
# -----------------------------   END ------------------------------------#       