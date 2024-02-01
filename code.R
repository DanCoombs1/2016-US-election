rm(list=ls()) #code used to clear enviorment
#libraries used
library(dplyr)

#Actual results of the election
clinton_votes = 65853625
trump_votes = 62985106
johnson_votes = 4489233
total_votes = 136750000

clinton_real_percent = clinton_votes/total_votes*100
trump_real_percent = trump_votes/total_votes*100
johnson_real_percent = johnson_votes/total_votes*100

clinton_real_percent
trump_real_percent
johnson_real_percent


#plot showing real results of the election
compare_real = c(clinton_real_percent, trump_real_percent,
                 johnson_real_percent)
values_real = c("48.15622%","46.05858%","3.282803%")
Names = c("Clinton", "Trump", "Johnson" )
real_results = barplot(compare_real, names.arg=Names,
                       xlab="Candidate", ylim=c(0,55),
                       ylab="Percentage of Total Votes",col=c("blue", "red", "yellow"),
                       main="Actual 2016 General Election Results")
text(real_results, compare_real+1.5 , paste("", values_real, sep="")
     ,cex=1) 

#read csv file with data from kaggle
Opinion_Poll = read.csv("presidential_polls.csv", encoding = 'UTF-8')
Opinion_Poll_new = read.csv("president_general_polls_2016.csv",
                            encoding = 'UTF-8')

# Remove duplicate rows of the dataframe
distinct(Opinion_Poll)

#what the polls predicted with no adjustment measures
#not all polls had data on Johnson so missing data was eliminated
johnson_na = (Opinion_Poll$rawpoll_johnson)
johnson_omit = na.omit(johnson_na)

#takes an average of all the polls data
clinton_percent = mean(Opinion_Poll$rawpoll_clinton)
trump_percent = mean(Opinion_Poll$rawpoll_trump)
johnson_percent = mean(johnson_omit) 

clinton_percent
trump_percent
johnson_percent

#code to show a bar chart
values = c("41.71758%","39.2270%","7.713394%")
Names = c("Clinton", "Trump", "Johnson" )
compare = c(clinton_percent, trump_percent, johnson_percent)
opinion_barplot = barplot(compare, names.arg=Names, xlab="Candidate",
                          ylim=c(0,50), ylab="Percentage of Total Votes",
                          col=c("blue","red", "yellow"),
        main="Opinion Poll Predictions of the 2016 General Election")
text(opinion_barplot, compare+1.5 , paste("", values, sep="") ,cex=1) 

#what the polls predicted with the weight adjused data

johnson_na = (Opinion_Poll$adjpoll_johnson)
johnson_omit = na.omit(johnson_na)

clinton_percent_adj = mean(Opinion_Poll$adjpoll_clinton)
trump_percent_adj = mean(Opinion_Poll$adjpoll_trump)
johnson_percent_adj = mean(johnson_omit) 

clinton_percent_adj
trump_percent_adj
johnson_percent_adj

values = c("43.0249%","42.04862%","4.804207%")
Names = c("Clinton", "Trump", "Johnson" )
compare = c(clinton_percent_adj, trump_percent_adj, johnson_percent_adj)
opinion_barplot = barplot(compare, names.arg=Names, xlab="Candidate",
                          ylim=c(0,50), ylab="Percentage of Total Votes",
                          col="blue",
                          main="Opinion Poll Predictions of the 2016
                          General Election")
text(opinion_barplot, compare+1.5 , paste("", values, sep="") ,cex=1) 

#code comparing raw and adjusted data on one graph
values_overall = c("41.7%", "43.0%","39.2%", "42.0%","7.71%", "4.80%")
Names_overall = c("Clinton", "Clinton", "Trump", "Trump", "Johnson",
                  "Johnson" )
clinton_join = cbind(clinton_percent,clinton_percent_adj)
trump_join = cbind(trump_percent,trump_percent_adj)
johnson_join = cbind(johnson_percent,johnson_percent_adj)

overall_poll=c(clinton_join,trump_join, johnson_join)
opinion_barplot_overall = barplot(overall_poll, beside = T,
                                  names.arg=Names_overall,
                                  xlab="Candidate", ylim=c(0,50),
                                  ylab="Percentage of Total Votes",
                                  main="Opinion Poll Predictions of the
                                  2016 General Election",
                                  space = c(0.4,0.4, 2.5,0.4,2.5), 
                                  col = c("blue", "blue", "red", "red",
                                          "yellow", "yellow") )
text(opinion_barplot_overall, overall_poll+1.5 , paste("",
                                                       values_overall,
                                                       sep="") ,cex=1) 

# code showing only the grade A opinion polls for the raw and 
#adjusted data

New_poll_A = Opinion_Poll[Opinion_Poll$grade != "D"
                          & Opinion_Poll$grade != "B"
                          & Opinion_Poll$grade != "C"
                          & Opinion_Poll$grade != "C-"
                          & Opinion_Poll$grade != "C+"
                          & Opinion_Poll$grade != "B+"
                          & Opinion_Poll$grade != "B-"
                          & Opinion_Poll$grade != "", ]          

johnson_adjA = na.omit(New_poll_A$adjpoll_johnson)
clinton_percent_adjA = mean(New_poll_A$adjpoll_clinton)
trump_percent_adjA = mean(New_poll_A$adjpoll_trump)
johnson_percent_adjA = mean(johnson_adjA)

johnson_rawA = na.omit(New_poll_A$rawpoll_johnson)
clinton_percent_rawA = mean(New_poll_A$rawpoll_clinton)
trump_percent_rawA = mean(New_poll_A$rawpoll_trump)
johnson_percent_rawA = mean(johnson_rawA)

print(clinton_percent_adjA)
print(trump_percent_adjA)
print(johnson_percent_adjA)

print(clinton_percent_rawA)
print(trump_percent_rawA)
print(johnson_percent_rawA)

values_overall_A = c("43.4%", "43.1%","39.9%", "41.3%","7.39%",
                     "4.61%")

Names_overall = c("Clinton", "Clinton", "Trump", "Trump",
                  "Johnson", "Johnson" )

#joins the raw and adjusted data for each candidate into a group
clinton_join_A = cbind(clinton_percent_rawA,clinton_percent_adjA)
trump_join_A = cbind(trump_percent_rawA,trump_percent_adjA)
johnson_join_A = cbind(johnson_percent_rawA,johnson_percent_adjA)

overall_poll_A=c(clinton_join_A,trump_join_A, johnson_join_A)

opinion_barplot_overall_A = barplot(overall_poll_A, beside = T,
                                    names.arg=Names_overall,
                                    xlab="Candidate", ylim=c(0,50),
                                    ylab="Percentage of Total Votes",
                                    main="Opinion Poll Predictions of
                                    the 2016 General Election 
Based on Grade A Opinion Polls",
                                     space = c(0.4,0.4, 2.5,0.4,2.5), 
                                     col = c("blue", "blue", "red",
                                             "red", "yellow", "yellow") )
text(opinion_barplot_overall_A, overall_poll_A+1.5 , paste("",
                                                           values_overall_A,
                                                           sep="") ,cex=1) 

# Same code as previous except for D grade polls

New_poll_D = Opinion_Poll[Opinion_Poll$grade != "A" 
                          & Opinion_Poll$grade != "B" 
                          & Opinion_Poll$grade != "C" 
                          & Opinion_Poll$grade != "C-" 
                          & Opinion_Poll$grade != "C+" 
                          & Opinion_Poll$grade != "B+" 
                          & Opinion_Poll$grade != "B-" 
                          & Opinion_Poll$grade != "" 
                          & Opinion_Poll$grade != "A+" 
                          & Opinion_Poll$grade != "A-", ]          

johnson_adjD = na.omit(New_poll_D$adjpoll_johnson)
clinton_percent_adjD = mean(New_poll_D$adjpoll_clinton)
trump_percent_adjD = mean(New_poll_D$adjpoll_trump)
johnson_percent_adjD = mean(johnson_adjD) 

johnson_rawD = na.omit(New_poll_D$rawpoll_johnson)
clinton_percent_rawD = mean(New_poll_D$rawpoll_clinton)
trump_percent_rawD = mean(New_poll_D$rawpoll_trump)
johnson_percent_rawD = mean(johnson_rawD) 


print(clinton_percent_adjD)
print(trump_percent_adjD)
print(johnson_percent_adjD)

print(clinton_percent_rawD)
print(trump_percent_rawD)
print(johnson_percent_rawD)

values_overall_D = c("46.3%", "45.5%","38.4%", "39.6%","6.31%",
                     "4.64%")
Names_overall = c("Clinton", "Clinton", "Trump", "Trump", "Johnson",
                  "Johnson" )
clinton_join_D = cbind(clinton_percent_rawD,clinton_percent_adjD)
trump_join_D = cbind(trump_percent_rawD,trump_percent_adjD)
johnson_join_D = cbind(johnson_percent_rawD,johnson_percent_adjD)

overall_poll_D=c(clinton_join_D,trump_join_D, johnson_join_D)
opinion_barplot_overall_D = barplot(overall_poll_D, beside = T,
                                    names.arg=Names_overall,
                                    xlab="Candidate", ylim=c(0,50),
                                    ylab="Percentage of Total Votes",
                                    main="Opinion Poll Predictions of
                                    the 2016 General Election 
Based on Grade D Opinion Polls",
                                    space = c(0.4,0.4, 2.5,0.4,2.5), 
                                    col = c("blue", "blue", "red", "red",
                                            "yellow", "yellow") )
text(opinion_barplot_overall_D, overall_poll_D+1.5 , paste("",
                                                           values_overall_D,
                                                           sep="") ,cex=1) 

#code to show data from opinion polls where sample was from whole of USA

New_poll_state = Opinion_Poll[Opinion_Poll$state == "U.S.", ]

johnson_adjUS = na.omit(New_poll_state$adjpoll_johnson)
clinton_percent_adjUS = mean(New_poll_state$adjpoll_clinton)
trump_percent_adjUS = mean(New_poll_state$adjpoll_trump)
johnson_percent_adjUS = mean(johnson_adjUS) 

johnson_rawUS = na.omit(New_poll_state$rawpoll_johnson)
clinton_percent_rawUS = mean(New_poll_state$rawpoll_clinton)
trump_percent_rawUS = mean(New_poll_state$rawpoll_trump)
johnson_percent_rawUS = mean(johnson_rawUS) 


print(clinton_percent_adjUS)
print(trump_percent_adjUS)
print(johnson_percent_adjUS)

print(clinton_percent_rawUS)
print(trump_percent_rawUS)
print(johnson_percent_rawUS)

values_overall_us = c("43.9%", "44.5%","39.4%", "41.0%","7.25%",
                      "4.24%")
Names_overall = c("Clinton", "Clinton", "Trump", "Trump", "Johnson",
                  "Johnson" )
clinton_join_us = cbind(clinton_percent_rawUS,clinton_percent_adjUS)
trump_join_us = cbind(trump_percent_rawUS,trump_percent_adjUS)
johnson_join_us = cbind(johnson_percent_rawUS,johnson_percent_adjUS)

overall_poll_us=c(clinton_join_us,trump_join_us, johnson_join_us)
opinion_barplot_overall_us = barplot(overall_poll_us, beside = T,
                                     names.arg=Names_overall,
                                     xlab="Candidate", ylim=c(0,60),
                                     ylab="Percentage of Total Votes",
                                       main="Opinion Poll Predictions of
                                     the 2016 General Election Across
                                     the USA",
                                       space = c(0.4,0.4, 2.5,0.4,2.5), 
                                       col = c("blue", "blue", "red",
                                               "red", "yellow", "yellow") )
text(opinion_barplot_overall_us, overall_poll_us+1.5 , paste("",
                                                             values_overall_us,
                                                             sep="") ,cex=1) 

#code to show data from opinion polls where sample was from whole
#state of Wyoming

New_poll_wyom = Opinion_Poll[Opinion_Poll$state == "Wyoming", ]

johnson_wyadj = na.omit(New_poll_wyom$adjpoll_johnson)
clinton_percent_wyadj = mean(New_poll_wyom$adjpoll_clinton)
trump_percent_wyadj = mean(New_poll_wyom$adjpoll_trump)
johnson_percent_wyadj = mean(johnson_wyadj) 

johnson_wyraw = na.omit(New_poll_wyom$rawpoll_johnson)
clinton_percent_wyraw = mean(New_poll_wyom$rawpoll_clinton)
trump_percent_wyraw = mean(New_poll_wyom$rawpoll_trump)
johnson_percent_wyraw = mean(johnson_wyraw) 

print(clinton_percent_wyraw)
print(trump_percent_wyraw)
print(johnson_percent_wyraw)

print(clinton_percent_wyadj)
print(trump_percent_wyadj)
print(johnson_percent_wyadj)

values_overall_wyom = c("21.7%", "25.5%","51.8%", "57.5%","10.0%",
                        "6.86%")
Names_overall = c("Clinton", "Clinton", "Trump", "Trump", "Johnson",
                  "Johnson" )
clinton_join_wyom = cbind(clinton_percent_wyraw,clinton_percent_wyadj)
trump_join_wyom = cbind(trump_percent_wyraw,trump_percent_wyadj)
johnson_join_wyom = cbind(johnson_percent_wyraw,johnson_percent_wyadj)

overall_poll_wyom=c(clinton_join_wyom,trump_join_wyom, johnson_join_wyom)
opinion_barplot_overall_wyom = barplot(overall_poll_wyom, beside = T,
                                       names.arg=Names_overall,
                                       xlab="Candidate", ylim=c(0,60),
                                       ylab="Percentage of Total Votes",
                                       main="Opinion Poll Predictions of
                                       the 2016 General Election in
                                       Wyoming",
                                       space = c(0.4,0.4, 2.5,0.4,2.5), 
                                       col = c("blue", "blue", "red",
                                               "red", "yellow", "yellow") )
text(opinion_barplot_overall_wyom, overall_poll_wyom+1.5 , 
     paste("",values_overall_wyom, sep="") ,cex=1) 

#code to show data from opinion polls where sample was from whole
#state Washington DC

New_poll_wash = Opinion_Poll[Opinion_Poll$state == "Washington", ]

johnson_washraw = na.omit(New_poll_wash$rawpoll_johnson)
clinton_percent_washraw = mean(New_poll_wash$rawpoll_clinton)
trump_percent_washraw = mean(New_poll_wash$rawpoll_trump)
johnson_percent_washraw = mean(johnson_washraw) 

johnson_washadj = na.omit(New_poll_wash$adjpoll_johnson)
clinton_percent_washadj = mean(New_poll_wash$adjpoll_clinton)
trump_percent_washadj = mean(New_poll_wash$adjpoll_trump)
johnson_percent_washadj = mean(johnson_washadj)


print(clinton_percent_washraw)
print(trump_percent_washraw)
print(johnson_percent_washraw)

print(clinton_percent_washadj)
print(trump_percent_washadj)
print(johnson_percent_washadj)



values_overall_wash = c("46.2%", "48.0%","31.7%", "35.3%",
                        "8.86%", "5.84%")
Names_overall = c("Clinton", "Clinton", "Trump", "Trump",
                  "Johnson", "Johnson" )
clinton_join_wash = cbind(clinton_percent_washraw,clinton_percent_washadj)
trump_join_wash = cbind(trump_percent_washraw,trump_percent_washadj)
johnson_join_wash = cbind(johnson_percent_washraw,johnson_percent_washadj)
overall_poll_wash=c(clinton_join_wash,trump_join_wash, johnson_join_wash)

opinion_barplot_overall_wash = barplot(overall_poll_wash, beside = T,
                                       names.arg=Names_overall,
                                       xlab="Candidate", ylim=c(0,50),
                                       ylab="Percentage of Total Votes",
                                  main="Opinion Poll Predictions of the
                                  2016 General Election in Washington D.C.",
                                  space = c(0.4,0.4, 2.5,0.4,2.5), 
                                  col = c("blue", "blue", "red", "red",
                                          "yellow", "yellow") )
text(opinion_barplot_overall_wash, overall_poll_wash+1.5 , 
     paste("", values_overall_wash, sep="") ,cex=1) 


#Code to show how sample size effects results
#data split into groups of above and below a sample size of 500
New_poll_sampleSize_below <- subset(Opinion_Poll, samplesize<500) 
New_poll_sampleSize_above <- subset(Opinion_Poll, samplesize>500) 

#box plots showing the variation in variables for each data group
boxplot(New_poll_sampleSize_below$adjpoll_clinton,
        New_poll_sampleSize_below$adjpoll_trump,
        New_poll_sampleSize_below$adjpoll_johnson,
        names = c("Clinton", "Trump", "Johnson"),
        xlab="Candidates", ylab="Percantage of Total Votes",
        main = "Boxplot Showing the Variation in outcome for Opinion Polls 
with a sample size below 500")
        
boxplot(New_poll_sampleSize_above$adjpoll_clinton,
        New_poll_sampleSize_above$adjpoll_trump,
        New_poll_sampleSize_above$adjpoll_johnson,
        names = c("Clinton", "Trump", "Johnson"),
        xlab="Candidates", ylab="Percantage of Total Votes",
        main = "Boxplot Showing the Variation in outcome for Opinion Polls
with a sample size above 500")
        
#all the above measues in one dataframe
New_poll_1 = Opinion_Poll_new[Opinion_Poll_new$grade != "D" 
                              & Opinion_Poll_new$grade != "B" 
                              & Opinion_Poll_new$grade != "C" 
                              & Opinion_Poll_new$grade != "C-"
                              & Opinion_Poll_new$grade != "C+" 
                              & Opinion_Poll_new$grade != "B+" 
                              & Opinion_Poll_new$grade != "B-" 
                              & Opinion_Poll_new$grade != "", ]          
New_poll_2 <- subset(New_poll_1, samplesize>500) 
New_poll_3 = New_poll_2[New_poll_2$state == "U.S.", ]
New_poll_3$enddate <- as.Date(New_poll_3$enddate, format = "%m/%d/%Y")
final_poll = New_poll_3[New_poll_3$enddate >= "2016-11-01" 
                        & New_poll_3$enddate <= "2016-11-07", ]

johnson_A_na = na.omit(final_poll$rawpoll_johnson)
clinton_percent_all = mean(final_poll$rawpoll_clinton)
trump_percent_all = mean(final_poll$rawpoll_trump)
johnson_percent_all = mean(johnson_A_na) 

clinton_percent_all
trump_percent_all
johnson_percent_all

compare_all = c(clinton_percent_all, trump_percent_all,
                johnson_percent_all)
values_all = c("43.64926%","38.81084%", "4.430466%")
opinion_barplot_all = barplot(compare_all, names.arg=Names,
                              xlab="Candidate", ylim=c(0,50),
                              ylab="Percentage of Total Votes",
                              col=c("blue", "red", "yellow"),
                          main="Opinion Poll Predictions of the
                          2016 General Election
Using All Estimation Techniques")
text(opinion_barplot_all, compare_all+1.5 ,
     paste("", values_all, sep="") ,cex=1) 

# code to split data into time sections
#before november
Opinion_Poll_new$enddate <- as.Date(Opinion_Poll_new$enddate,
                                    format = "%m/%d/%Y")
New_poll_enddate_bef = Opinion_Poll_new[Opinion_Poll_new$enddate >= "2000-11-01" 
                                        & Opinion_Poll_new$enddate <= "2016-10-31", ]

johnson_adjbef = na.omit(New_poll_enddate_bef$adjpoll_johnson)
clinton_percent_adjbef = mean(New_poll_enddate_bef$adjpoll_clinton)
trump_percent_adjbef = mean(New_poll_enddate_bef$adjpoll_trump)
johnson_percent_adjbef = mean(johnson_adjbef) 

johnson_rawbef = na.omit(New_poll_enddate_bef$rawpoll_johnson)
clinton_percent_rawbef = mean(New_poll_enddate_bef$rawpoll_clinton)
trump_percent_rawbef = mean(New_poll_enddate_bef$rawpoll_trump)
johnson_percent_rawbef = mean(johnson_rawbef)

print(clinton_percent_adjbef)
print(trump_percent_adjbef)
print(johnson_percent_adjbef)

print(clinton_percent_rawbef)
print(trump_percent_rawbef)
print(johnson_percent_rawbef)

values_overall_bef = c("41.7%", "43.4%","39.3%", "42.7%","7.66%",
                       "4.55%")
Names_overall = c("Clinton", "Clinton", "Trump", "Trump",
                  "Johnson", "Johnson" )
clinton_join_bef = cbind(clinton_percent_rawbef,clinton_percent_adjbef)
trump_join_bef = cbind(trump_percent_rawbef,trump_percent_adjbef)
johnson_join_bef = cbind(johnson_percent_rawbef,johnson_percent_adjbef)

overall_poll_bef=c(clinton_join_bef,trump_join_bef, johnson_join_bef)
opinion_barplot_overall_bef = barplot(overall_poll_bef, beside = T,
                                      names.arg=Names_overall,
                                      xlab="Candidate", ylim=c(0,50),
                                      ylab="Percentage of Total Votes",
                                    main="Opinion Poll Predictions of
                                    the 2016 General Election 
Based on Polls Conducted Before November",
                                    space = c(0.4,0.4, 2.5,0.4,2.5), 
                                    col = c("blue", "blue", "red",
                                            "red", "yellow", "yellow") )
text(opinion_barplot_overall_bef, overall_poll_bef+1.5 ,
     paste("", values_overall_bef, sep="") ,cex=1) 

#during november
New_poll_enddate_aft = Opinion_Poll_new[Opinion_Poll_new$enddate >= "2016-11-01" 
                                        & Opinion_Poll_new$enddate <= "2016-11-07", ]

johnson_adjaft = na.omit(New_poll_enddate_aft$adjpoll_johnson)
clinton_percent_adjaft = mean(New_poll_enddate_aft$adjpoll_clinton)
trump_percent_adjaft = mean(New_poll_enddate_aft$adjpoll_trump)
johnson_percent_adjaft = mean(johnson_adjaft)

johnson_rawaft = na.omit(New_poll_enddate_aft$rawpoll_johnson)
clinton_percent_rawaft = mean(New_poll_enddate_aft$rawpoll_clinton)
trump_percent_rawaft = mean(New_poll_enddate_aft$rawpoll_trump)
johnson_percent_rawaft = mean(johnson_rawaft) 

print(clinton_percent_adjaft)
print(trump_percent_adjaft)
print(johnson_percent_adjaft)

print(clinton_percent_rawaft)
print(trump_percent_rawaft)
print(johnson_percent_rawaft)

values_overall_aft = c("43.1%", "42.7%","42.5%", "42.8%",
                       "6.37%", "5.04%")
Names_overall = c("Clinton", "Clinton", "Trump", "Trump",
                  "Johnson", "Johnson" )
clinton_join_aft= cbind(clinton_percent_rawaft,clinton_percent_adjaft)
trump_join_aft = cbind(trump_percent_rawaft,trump_percent_adjaft)
johnson_join_aft = cbind(johnson_percent_rawaft,johnson_percent_adjaft)

overall_poll_aft=c(clinton_join_aft,trump_join_aft, johnson_join_aft)
opinion_barplot_overall_aft = barplot(overall_poll_aft, beside = T,
                                      names.arg=Names_overall,
                                      xlab="Candidate", ylim=c(0,50), 
                                      ylab="Percentage of Total Votes",
                                      main="Opinion Poll Predictions of
                                      the 2016 General Election 
Based on Polls Conducted During November",
                                      space = c(0.4,0.4, 2.5,0.4,2.5), 
                                      col = c("blue", "blue", "red",
                                              "red", "yellow", "yellow") )
text(opinion_barplot_overall_aft, overall_poll_aft+1.5 , 
     paste("", values_overall_aft, sep="") ,cex=1) 

#code for betting markets
#odds for trump and corresponding date
Trump_date = c("05/08/2015", "30/10/2015", "01/02/2016", "04/03/2016",
               "09/05/2016", "22/06/2016", "18/08/2016", "26/09/2016",
               "25/10/2016", "01/11/2016")
Trump_odd = c("2500", "500","190" ,"200" ,"225" ,"260" ,"325" ,"175" ,
              "350" ,"190")

df <- data.frame(Trump_odd, Trump_date)
df$Trump_date <- as.Date(df$Trump_date, format = "%d/%m/%Y")

plot(df$Trump_date, df$Trump_odd, 
     main = "Changes in Betting Odds For Trump as President During
     Election Campaign",
     ylab = "Betting Odds", xlab = "Date (Month - Year)",
     pch = 19, frame = FALSE, axis = FALSE, 
     lines(x, y, type = "l"))
axis(1, df$Trump_date, format(df$Trump_date, "%m-%Y"))

