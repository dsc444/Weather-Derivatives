#Needed data sets are shannon4R, Dublin4R, and shannon4RMarkov.

#getting packages needed
library(quantmod)
library(rugarch)
library(rmgarch)
library(tseries)
library(FinTS)
library(fpp2)
library(tseries)
library(urca)
library(MASS)
library("writexl")

#assigning variables
wind<-shannon4R$wdsp
date<-shannon4R$day
maxtp<-shannon4R$maxtp
mintp<-shannon4R$mintp
igmin<-shannon4R$igmin
gmin<-shannon4R$gmin
rain<-shannon4R$rain
cbl<-shannon4R$cbl
wdsp<-shannon4R$wdsp
hm<-shannon4R$hm
ddhm<-shannon4R$ddhm
hg<-shannon4R$hg
sun<-shannon4R$sun
dos<-shannon4R$dos
soil<-shannon4R$soil
pe<-shannon4R$pe
evap<-shannon4R$evap
smd_wd<-shannon4R$smd_wd
smd_md<-shannon4R$smd_md
smd_pd<-shannon4R$smd_pd

#wind speed adjustment for troubled days
shannon4R$wdsp[7397]<-9
shannon4R$wdsp[9132]<-12

#regression modelling
mod1<-lm(wdsp~ maxtp + mintp + igmin + gmin + rain + cbl + hm + ddhm + hg + sun + dos + soil + pe + evap + smd_wd + smd_md + smd_pd, data=shannon4R)
summary(mod1)

mod2<-lm(wdsp~ maxtp + mintp + igmin + rain + cbl + hm + ddhm + hg + sun + dos + soil + pe + evap + smd_pd, data=shannon4R)
summary(mod2)

mod3<-lm(wdsp~ maxtp + mintp + rain + cbl + hm + ddhm + hg + sun + pe + evap, data=shannon4R)
summary(mod3)
plot(mod3)

#Box-Cox transformation code
bc <- boxcox(wdsp~ maxtp + mintp + igmin + gmin + rain + cbl + hm + ddhm + hg + sun + dos + soil + pe + evap + smd_wd + smd_md + smd_pd, data=shannon4R)
(lambda <- bc$x[which.max(bc$y)]) #0.7070707 which is close to a square root transformation

sqwind<-sqrt(wdsp)

mod4<-lm(sqwind~ maxtp + mintp + rain + cbl + hm + ddhm + hg + sun + dos + pe + evap + smd_pd, data=shannon4R)
summary(mod4)

bc <- boxcox(sqwind~ maxtp + mintp + igmin + gmin + rain + cbl + hm + ddhm + hg + sun + dos + soil + pe + evap + smd_wd + smd_md + smd_pd, data=shannon4R)
(lambda <- bc$x[which.max(bc$y)])

mod3<-lm(wdsp~ maxtp + mintp + rain + cbl + hm + ddhm + hg + sun + pe + evap, data=shannon4R)
summary(mod3)
plot(mod3)

ggplot(shannon4R,aes(day,wind))
boxplot(wind~shannon4R$day)

ggplot(shannondaily_split, aes(x=day, y=col_1945)) + geom_line()

#Markov chains
n<-nrow(shannon4RMarkov)

shannon4RMarkov["beaufort"]<-NA #adds beaufort to the dataframe
#gives beaufort column element values
i=1
for(i in 1:n){

if(shannon4RMarkov$wdsp[i]<1){
  shannon4RMarkov$beaufort[i]<-0
}
else if(shannon4RMarkov$wdsp[i]<4){
  shannon4RMarkov$beaufort[i]<-1
}
else if(shannon4RMarkov$wdsp[i]<7){
  shannon4RMarkov$beaufort[i]<-2
}
else if(shannon4RMarkov$wdsp[i]<13){
  shannon4RMarkov$beaufort[i]<-3
}
else if(shannon4RMarkov$wdsp[i]<17){
  shannon4RMarkov$beaufort[i]<-4
}
else if(shannon4RMarkov$wdsp[i]<22){
  shannon4RMarkov$beaufort[i]<-5
}
else if(shannon4RMarkov$wdsp[i]<28){
  shannon4RMarkov$beaufort[i]<-6
}
else if(shannon4RMarkov$wdsp[i]<34){
  shannon4RMarkov$beaufort[i]<-7
}
else if(shannon4RMarkov$wdsp[i]<41){
  shannon4RMarkov$beaufort[i]<-8
}
else if(shannon4RMarkov$wdsp[i]<48){
  shannon4RMarkov$beaufort[i]<-9
}
else if(shannon4RMarkov$wdsp[i]<56){
  shannon4RMarkov$beaufort[i]<-10
}
else if(shannon4RMarkov$wdsp[i]<64){
  shannon4RMarkov$beaufort[i]<-11
}
else if(shannon4RMarkov$wdsp[i]>=64){
  shannon4RMarkov$beaufort[i]<-12
}}


shannon4RMarkov["change"]<-NA
j<-1
for(j in 1:(n-24)){
  shannon4RMarkov$change[j]<-shannon4RMarkov$beaufort[j+24]-shannon4RMarkov$beaufort[j]
}  #adds the change in wind between one day and the next at a certain hour and inputs it into the previous day's place

#changes for the markov chain estimated probabilities
B0_change_neg4<-0
B1_change_neg4<-0
B2_change_neg4<-0
B3_change_neg4<-0
B4_change_neg4<-0
B5_change_neg4<-0
B6_change_neg4<-0
B12_change_neg4<-0
B0_change_neg3<-0
B1_change_neg3<-0
B2_change_neg3<-0
B3_change_neg3<-0
B4_change_neg3<-0
B5_change_neg3<-0
B6_change_neg3<-0
B12_change_neg3<-0
B0_change_neg2<-0
B1_change_neg2<-0
B2_change_neg2<-0
B3_change_neg2<-0
B4_change_neg2<-0
B5_change_neg2<-0
B6_change_neg2<-0
B12_change_neg2<-0
B0_change_neg1<-0
B1_change_neg1<-0
B2_change_neg1<-0
B3_change_neg1<-0
B4_change_neg1<-0
B5_change_neg1<-0
B6_change_neg1<-0
B12_change_neg1<-0
B0_change_none<-0
B1_change_none<-0
B2_change_none<-0
B3_change_none<-0
B4_change_none<-0
B5_change_none<-0
B6_change_none<-0
B12_change_none<-0
B0_change_pos1<-0
B1_change_pos1<-0
B2_change_pos1<-0
B3_change_pos1<-0
B4_change_pos1<-0
B5_change_pos1<-0
B6_change_pos1<-0
B12_change_pos1<-0
B0_change_pos2<-0
B1_change_pos2<-0
B2_change_pos2<-0
B3_change_pos2<-0
B4_change_pos2<-0
B5_change_pos2<-0
B6_change_pos2<-0
B12_change_pos2<-0
B0_change_pos3<-0
B1_change_pos3<-0
B2_change_pos3<-0
B3_change_pos3<-0
B4_change_pos3<-0
B5_change_pos3<-0
B6_change_pos3<-0
B12_change_pos3<-0
B0count<-0
B1count<-0
B2count<-0
B3count<-0
B4count<-0
B5count<-0
B6count<-0
B12count<-0

#gets the change in the beaufort values
for(j in 1:(n-24)){
  if(shannon4RMarkov$beaufort[j]==0){
  B0count<-B0count+1
  if(shannon4RMarkov$change[j]==0){
    B0_change_none<-B0_change_none+1
  }
  else if(shannon4RMarkov$change[j]==1){
    B0_change_pos1<-B0_change_pos1+1
  }
  else if(shannon4RMarkov$change[j]==2){
    B0_change_pos2<-B0_change_pos2+1
  }
  else if(shannon4RMarkov$change[j]==3){
    B0_change_pos3<-B0_change_pos3+1
  }
  }
  else if(shannon4RMarkov$beaufort[j]==1){
    B1count<-B1count+1
    if(shannon4RMarkov$change[j]==-1){
      B1_change_neg1<-B1_change_neg1+1
    }
    else if(shannon4RMarkov$change[j]==0){
      B1_change_none<-B1_change_none+1
    }
    else if(shannon4RMarkov$change[j]==1){
      B1_change_pos1<-B1_change_pos1+1
    }
    else if(shannon4RMarkov$change[j]==2){
      B1_change_pos2<-B1_change_pos2+1
    }
    else if(shannon4RMarkov$change[j]==3){
      B1_change_pos3<-B1_change_pos3+1
    }
    
  }
  else if(shannon4RMarkov$beaufort[j]==2){
    B2count<-B2count+1
    if(shannon4RMarkov$change[j]==-3){
      B2_change_neg3<-B2_change_neg3+1
    }
    else if(shannon4RMarkov$change[j]==-2){
      B2_change_neg2<-B2_change_neg2+1
    }
    else if(shannon4RMarkov$change[j]==-1){
      B2_change_neg1<-B2_change_neg1+1
    }
    else if(shannon4RMarkov$change[j]==0){
      B2_change_none<-B2_change_none+1
    }
    else if(shannon4RMarkov$change[j]==1){
      B2_change_pos1<-B2_change_pos1+1
    }
    else if(shannon4RMarkov$change[j]==2){
      B2_change_pos2<-B2_change_pos2+1
    }
    else if(shannon4RMarkov$change[j]==3){
      B2_change_pos3<-B2_change_pos3+1
    }
    
  }
  else if(shannon4RMarkov$beaufort[j]==3){
    B3count<-B3count+1
    if(shannon4RMarkov$change[j]==-3){
      B3_change_neg3<-B3_change_neg3+1
    }
    else if(shannon4RMarkov$change[j]==-4){
      B3_change_neg4<-B3_change_neg4+1
    }
    else if(shannon4RMarkov$change[j]==-2){
      B3_change_neg2<-B3_change_neg2+1
    }
    else if(shannon4RMarkov$change[j]==-1){
      B3_change_neg1<-B3_change_neg1+1
    }
    else if(shannon4RMarkov$change[j]==0){
      B3_change_none<-B3_change_none+1
    }
    else if(shannon4RMarkov$change[j]==1){
      B3_change_pos1<-B3_change_pos1+1
    }
    else if(shannon4RMarkov$change[j]==2){
      B3_change_pos2<-B3_change_pos2+1
    }
    else if(shannon4RMarkov$change[j]==3){
      B3_change_pos3<-B3_change_pos3+1
    }
    
  }
  else if(shannon4RMarkov$beaufort[j]==4){
    B4count<-B4count+1
    if(shannon4RMarkov$change[j]==-3){
      B4_change_neg3<-B4_change_neg3+1
    }
    else if(shannon4RMarkov$change[j]==-4){
      B4_change_neg4<-B4_change_neg4+1
    }
    else if(shannon4RMarkov$change[j]==-2){
      B4_change_neg2<-B4_change_neg2+1
    }
    else if(shannon4RMarkov$change[j]==-1){
      B4_change_neg1<-B4_change_neg1+1
    }
    else if(shannon4RMarkov$change[j]==0){
      B4_change_none<-B4_change_none+1
    }
    else if(shannon4RMarkov$change[j]==1){
      B4_change_pos1<-B4_change_pos1+1
    }
    else if(shannon4RMarkov$change[j]==2){
      B4_change_pos2<-B4_change_pos2+1
    }
    else if(shannon4RMarkov$change[j]==3){
      B4_change_pos3<-B4_change_pos3+1
    }
    
  }
  else if(shannon4RMarkov$beaufort[j]==5){
    B5count<-B5count+1
    if(shannon4RMarkov$change[j]==-3){
      B5_change_neg3<-B5_change_neg3+1
    }
    else if(shannon4RMarkov$change[j]==-4){
      B5_change_neg4<-B5_change_neg4+1
    }
    else if(shannon4RMarkov$change[j]==-2){
      B5_change_neg2<-B5_change_neg2+1
    }
    else if(shannon4RMarkov$change[j]==-1){
      B5_change_neg1<-B5_change_neg1+1
    }
    else if(shannon4RMarkov$change[j]==0){
      B5_change_none<-B5_change_none+1
    }
    else if(shannon4RMarkov$change[j]==1){
      B5_change_pos1<-B5_change_pos1+1
    }
    else if(shannon4RMarkov$change[j]==2){
      B5_change_pos2<-B5_change_pos2+1
    }
    else if(shannon4RMarkov$change[j]==3){
      B5_change_pos3<-B5_change_pos3+1
    }
    
  }
  else if(shannon4RMarkov$beaufort[j]==6){
    B6count<-B6count+1
    if(shannon4RMarkov$change[j]==-3){
      B6_change_neg3<-B6_change_neg3+1
    }
    else if(shannon4RMarkov$change[j]==-4){
      B6_change_neg4<-B6_change_neg4+1
    }
    else if(shannon4RMarkov$change[j]==-2){
      B6_change_neg2<-B6_change_neg2+1
    }
    else if(shannon4RMarkov$change[j]==-1){
      B6_change_neg1<-B6_change_neg1+1
    }
    else if(shannon4RMarkov$change[j]==0){
      B6_change_none<-B6_change_none+1
    }
    else if(shannon4RMarkov$change[j]==1){
      B6_change_pos1<-B6_change_pos1+1
    }
    else if(shannon4RMarkov$change[j]==2){
      B6_change_pos2<-B6_change_pos2+1
    }
    else if(shannon4RMarkov$change[j]==3){
      B6_change_pos3<-B6_change_pos3+1
    }
    
  }
  else if(shannon4RMarkov$beaufort[j]==12){
    B12count<-B12count+1
    if(shannon4RMarkov$change[j]==-3){
      B12_change_neg3<-B12_change_neg3+1
    }
    else if(shannon4RMarkov$change[j]==-4){
      B12_change_neg4<-B12_change_neg4+1
    }
    else if(shannon4RMarkov$change[j]==-2){
      B12_change_neg2<-B12_change_neg2+1
    }
    else if(shannon4RMarkov$change[j]==-1){
      B12_change_neg1<-B12_change_neg1+1
    }
    else if(shannon4RMarkov$change[j]==0){
      B12_change_none<-B12_change_none+1
    }
    
  }
}

p_B0_change_neg1<-B0_change_neg1/B0count
p_B0_change_neg2<-B0_change_neg2/B0count
p_B0_change_neg3<-B0_change_neg3/B0count
p_B0_change_neg4<-B0_change_neg4/B0count
p_B0_change_none<-B0_change_none/B0count
p_B0_change_pos1<-B0_change_pos1/B0count
p_B0_change_pos2<-B0_change_pos2/B0count
p_B0_change_pos3<-B0_change_pos3/B0count
p_B1_change_neg1<-B1_change_neg1/B1count
p_B1_change_neg2<-B1_change_neg2/B1count
p_B1_change_neg3<-B1_change_neg3/B1count
p_B1_change_none<-B1_change_none/B1count
p_B1_change_pos1<-B1_change_pos1/B1count
p_B1_change_pos2<-B1_change_pos2/B1count
p_B1_change_pos3<-B1_change_pos3/B1count
p_B12_change_neg1<-B12_change_neg1/B12count
p_B12_change_neg2<-B12_change_neg2/B12count
p_B12_change_none<-B12_change_none/B12count
p_B12_change_pos1<-B12_change_pos1/B12count
p_B12_change_pos2<-B12_change_pos2/B12count
p_B12_change_pos3<-B12_change_pos3/B12count
p_B2_change_neg1<-B2_change_neg1/B2count
p_B2_change_neg2<-B2_change_neg2/B2count
p_B2_change_neg3<-B2_change_neg3/B2count
p_B2_change_none<-B2_change_none/B2count
p_B2_change_pos1<-B2_change_pos1/B2count
p_B2_change_pos2<-B2_change_pos2/B2count
p_B2_change_pos3<-B2_change_pos3/B2count
p_B3_change_neg1<-B3_change_neg1/B3count
p_B3_change_neg2<-B3_change_neg2/B3count
p_B3_change_neg3<-B3_change_neg3/B3count
p_B3_change_none<-B3_change_none/B3count
p_B3_change_pos1<-B3_change_pos1/B3count
p_B3_change_pos2<-B3_change_pos2/B3count
p_B3_change_pos3<-B3_change_pos3/B3count
p_B4_change_neg1<-B4_change_neg1/B4count
p_B4_change_neg2<-B4_change_neg2/B4count
p_B4_change_neg3<-B4_change_neg3/B4count
p_B4_change_none<-B4_change_none/B4count
p_B4_change_pos1<-B4_change_pos1/B4count
p_B4_change_pos2<-B4_change_pos2/B4count
p_B4_change_pos3<-B4_change_pos3/B4count
p_B5_change_neg1<-B5_change_neg1/B5count
p_B5_change_neg2<-B5_change_neg2/B5count
p_B5_change_neg3<-B5_change_neg3/B5count
p_B5_change_none<-B5_change_none/B5count
p_B5_change_pos1<-B5_change_pos1/B5count
p_B5_change_pos2<-B5_change_pos2/B5count
p_B5_change_pos3<-B5_change_pos3/B5count
p_B6_change_neg1<-B6_change_neg1/B6count
p_B6_change_neg2<-B6_change_neg2/B6count
p_B6_change_neg3<-B6_change_neg3/B6count
p_B6_change_none<-B6_change_none/B6count
p_B6_change_pos1<-B6_change_pos1/B6count
p_B6_change_pos2<-B6_change_pos2/B6count
p_B6_change_pos3<-B6_change_pos3/B6count
p_B1_change_neg4<-B1_change_neg4/B0count
p_B2_change_neg4<-B2_change_neg4/B0count
p_B3_change_neg4<-B3_change_neg4/B0count
p_B4_change_neg4<-B4_change_neg4/B0count
p_B5_change_neg4<-B5_change_neg4/B0count
p_B6_change_neg4<-B6_change_neg4/B0count
p_B12_change_neg4<-B12_change_neg4/B0count


#probabilities of changing state and by how many jumps
p_change_neg4<-change_neg4/j
p_change_neg3<-change_neg3/j
p_change_neg2<-change_neg2/j
p_change_neg1<-change_neg1/j
p_change_none<-change_none/j
p_change_pos3<-change_pos3/j
p_change_pos1<-change_pos1/j
p_change_pos2<-change_pos2/j

ggplot(shannon4RMarkov, aes(x=date, y=beaufort)) + geom_line()

ggplot(shannon4R,aes(day,beaufort))
ggplot(aes(x=beaufort), data=shannon4R) + geom_histogram(bins=13, colour="purple") + xlab("beaufort")
ggplot(aes(y=beaufort), data=shannon4R) + geom_boxplot() + ylab("beaufort")
boxplot(date~beaufort)
plot(shannon4RMarkov$date,shannon4RMarkov$beaufort)

#Monthly averages
windavgcount1<-0
windavgcount2<-0
windavgcount3<-0
windavgcount4<-0
windavgcount5<-0
windavgcount6<-0
windavgcount7<-0
windavgcount8<-0
windavgcount9<-0
windavgcount10<-0
windavgcount11<-0
windavgcount12<-0
windavg_Jan<-0
windavg_Feb<-0
windavg_Mar<-0
windavg_Apr<-0
windavg_May<-0
windavg_Jun<-0
windavg_Jul<-0
windavg_Aug<-0
windavg_Sep<-0
windavg_Oct<-0
windavg_Nov<-0
windavg_Dec<-0
j<-0
for(j in 1:28112){
  if(shannon4R$day[j]<32){
    windavgcount1<-(windavgcount1+shannon4R$wdsp[j])
    windavg_Jan<-windavg_Jan+1
  }
  else if(shannon4R$day[j]<60){
    windavgcount2<-(windavgcount2+shannon4R$wdsp[j])
    windavg_Feb<-windavg_Feb+1
  }
  else if(shannon4R$day[j]<91){
    windavgcount3<-(windavgcount3+shannon4R$wdsp[j])
    windavg_Mar<-windavg_Mar+1
  }
  else if(shannon4R$day[j]<121){
    windavgcount4<-(windavgcount4+shannon4R$wdsp[j])
    windavg_Apr<-windavg_Apr+1
  }
  else if(shannon4R$day[j]<152){
    windavgcount5<-(windavgcount5+shannon4R$wdsp[j])
    windavg_May<-windavg_May+1
  }
  else if(shannon4R$day[j]<182){
    windavgcount6<-(windavgcount6+shannon4R$wdsp[j])
    windavg_Jun<-windavg_Jun+1
  }
  else if(shannon4R$day[j]<213){
    windavgcount7<-(windavgcount7+shannon4R$wdsp[j])
    windavg_Jul<-windavg_Jul+1
  }
  else if(shannon4R$day[j]<244){
    windavgcount8<-(windavgcount8+shannon4R$wdsp[j])
    windavg_Aug<-windavg_Aug+1
  }
  else if(shannon4R$day[j]<274){
    windavgcount9<-(windavgcount9+shannon4R$wdsp[j])
    windavg_Sep<-windavg_Sep+1
  }
  else if(shannon4R$day[j]<305){
    windavgcount10<-(windavgcount10+shannon4R$wdsp[j])
    windavg_Oct<-windavg_Oct+1
  }
  else if(shannon4R$day[j]<335){
    windavgcount11<-(windavgcount11+shannon4R$wdsp[j])
    windavg_Nov<-windavg_Nov+1
  }
  else if(shannon4R$day[j]>334){
    windavgcount12<-(windavgcount12+shannon4R$wdsp[j])
    windavg_Dec<-windavg_Dec+1
  }
}
Jan_avg<-windavgcount1/windavg_Jan
Feb_avg<-windavgcount2/windavg_Feb
Mar_avg<-windavgcount3/windavg_Mar
Apr_avg<-windavgcount4/windavg_Apr
May_avg<-windavgcount5/windavg_May
Jun_avg<-windavgcount6/windavg_Jun
Jul_avg<-windavgcount7/windavg_Jul
Aug_avg<-windavgcount8/windavg_Aug
Sep_avg<-windavgcount9/windavg_Sep
Oct_avg<-windavgcount10/windavg_Oct
Nov_avg<-windavgcount11/windavg_Nov
Dec_avg<-windavgcount12/windavg_Dec

month_avgs<-c(Jan_avg, Feb_avg, Mar_avg, Apr_avg, May_avg, Jun_avg, Jul_avg, Aug_avg, Sep_avg, Oct_avg, Nov_avg, Dec_avg)
plot(month_avgs, xlab="Month", ylab="wind average")
lines(month_avgs)

plot(rollmean(shannon4R$wdsp,k=3650), xlab="day", ylab="wind moving average")
lines(rollmean(shannon4R$wdsp,k=3650))

############################################################### Time series!

wind <- window(wind, start = 27382, end = 28112) # Uses wind speed data

wind %>%
  ggAcf() # ACF decays slowly; first evidence that this is a non-stationary series.

wind %>%
  adf.test() #p value of 0.01 which is quite low and requires further understanding of what is going on. 

wind %>%
  ur.kpss(use.lag = 9) %>%
  summary() # Reject the null hypothesis of stationarity. (H0 is different in KPSS than ADF test)
#large value 13.8095 

wind %>%
  Box.test(lag=9, type="Ljung-Box")  # Reject null hypothesis of series being white noise (This is a very strong test, use ADF or KPSS instead.)

# ndiffs() function estimates the number of differences required to make a given time series stationary.

ndiffs(wind) # number of differences required to make a this series stationary
nsdiffs(wind) # number of seasonal differences required to make a this series stationary

# take the first difference of wind series and test for stationarity

wind %>%
  diff() %>%
  ggAcf() # No lag is significantly different than zero; first evidence that this is a stationary series

wind %>%
  diff() %>%
  adf.test() # Reject the null hypothesis that this series is non-stationary series.

wind %>%
  diff() %>%
  ur.kpss(use.lag = 8) %>%
  summary() # Fail to reject null hypothesis of stationarity. (H0 is different than for ADF test)

wind %>%
  diff() %>%
  Box.test(lag=8, type="Ljung-Box") # Fail to reject null hypothesis of series being white noise (This is a very strong test, use ADF or KPSS instead.)

auto.arima(wind) #quick way of getting an ARIMA model for the data but is often not the best and so we will also do it manually.
#auto arima gives an ARIMA(1,1,2) model

auto.arima(wind, 
           stepwise = FALSE, 
           approximation = FALSE) #this addition to auto.arima forces the function to test out more models on the data but still gives the same Arima(1,1,2) result as the standard auto.arima

#################################################### building AR and MA parts 


wind %>% 
  ggtsdisplay(main = "") # Declining PACF and very sudden ACF decline

# Step 5: ACF decays exponentially, and there is a significant spike in PACF at lag 3 and none beyond. We will use ARIMA(3, 1, 0) model.

#trial and error of models. We know from previous that the model will be in the form ARIMA(p,1,q)

#starting with p=3 as there are 3 clear significant ACF spikes
fit1 <- Arima(wind, order = c(4, 0, 1)) #AICc of 3815.116 (we wish to minimise AICc)
summary(fit1)
fit1$aicc # This will show you AICc for this model

fit2 <- Arima(wind, order = c(0, 0, 0)) #testing p=4,2 to see if surrounding p is better
summary(fit2) #AICc of 3815.95  
fit2$aicc

fit3 <- Arima(wind, order = c(2, 0, 1))
summary(fit3) #AICc of 3821.25   

fit4 <- Arima(wind, order = c(5, 0, 1)) #significant improvement over p=4 by including q=1 and setting p=3 to avoid errors.
summary(fit4) #AICc of 3817.94 

fit5 <- Arima(wind, order = c(1, 1, 2)) #comparing to the auto arima model we see our manual model is slightly better
summary(fit5) #AICc of 3820.11      

# ARIMA(3,1,1) has a slightly smaller AICc value. We will pick ARIMA (3, 1, 1).

# Step 6: Check the residuals from your chosen model.
checkresiduals(fit1)
checkresiduals(fit2)

# The residuals are behaving like white noise (a large p-value).

# Step 7: Forecasts from the chosen model.
fit1 %>%
  forecast(30) %>%
  autoplot()

fit4 %>%
  forecast(30)




########################################################## GARCH TIME

wind<-shannon4R$wdsp #now using all the data instead of 365 days we see that the GARCH(1,1) model is a better fit.

ug_spec <- ugarchspec(mean.model=list(armaOrder=c(1,0)))
ug_spec

plot.ts(wind)
ArchTest(wind) #there are ARCH effects as p-value very low against H0
garch(wind,grad="numerical",trace=FALSE)

x=ugarchspec(variance.model = list(garchOrder=c(1,1)),mean.model = list(armaOrder=c(0,0)))
xfit=ugarchfit(x,data=wind)
xfit #the beta term (q) is not significant so we try a GARCH(1,0) model which is the same as the ARCH(1) model for 365 days only.

x=ugarchspec(variance.model = list(garchOrder=c(1,0)),mean.model = list(armaOrder=c(0,0)))
xfit=ugarchfit(x,data=wind)
xfit #this is a better model and is simply the ARCH(1) model when looking at 365 days only.

windf<-ugarchforecast(xfit,n.ahead(20))
windf #forecast for the ARCH(1) model on the data for 20 periods/days wind speeds

###############################

windn<-c()
for (i in 1:length(wind)){
  windn<-c(windn,wind[i+1]-wind[i])
}

plot.ts(windn)

ArchTest(windn) #no GARCH effects when analysing the differences instead of the speeds
#garch(windn,grad="numerical",trace=FALSE)

###############################

x=ugarchspec(variance.model = list(garchOrder=c(1,2)),mean.model = list(armaOrder=c(1,0)))
xfit=ugarchfit(x,data=wind)
xfit #GARCH(1,6) possibly a better model. All beta values are still significant. ie. p<0.05

windf<-ugarchforecast(xfit,n.ahead = 30)
windf

#notes from tested ideas:
#GARCH(1,1)ARMA(1,1) shows MA in ARMA to be non-sig
#GARCH(1,1)ARMA(1,0) shows AR in ARMA to be sig
#GARCH(1,6)ARMA(0,0) shows all beta to be sig
#GARCH(1,0)ARMA(0,0) best if only 365 days are looked at
#GARCH(1,1)ARMA(2,0) shows second AR in ARMA to be non-sig
#GARCH(1,3)ARMA(1,0) shows a beta to be non-sig
#Is GARCH(1,2)ARMA(1,0) the best model? Check


ugarchfit(spec = ug_spec, data = wind)

###########################################################

#code shows errors for a year of single day forecasting using ARIMA(3,1,1)
error_mean<-c()

new_vec<-c()
for (i in (335):(699)){
  fit4 <- Arima(wind[1:i], order = c(4, 0, 1)) #iters ARIMA for wind from 1 to i to create model
  thing<-fit4 %>% forecast(30) #uses current model to create 30 day forecast
  new_vec<-c(new_vec,thing[4]) #stores forecast along with all other forecasts
}

newnew_vec<-unlist(new_vec) #makes collection of forecasts into vector


error<-c()
counter<-0
counter2<-0
for (z in 1:365){
  for (i in 1:30){
    error<-c(error,wind[i+335+counter2]-newnew_vec[i+counter]) #stores error
  }
  counter<-counter+30
  counter2<-counter2+1
}

hist(error)
sd(error)
mean(error)
plot(error,type="l",col="red")

###################################################### Test of GARCH forecast
windf_vec<-c()
x=ugarchspec(variance.model = list(garchOrder=c(1,2)),mean.model = list(armaOrder=c(1,0)))
for (i in (335):(699)){
  xfit=ugarchfit(x,data=wind[1:i])
  windf<-ugarchforecast(xfit,n.ahead = 30)
  windf_vec<-c(windf_vec,windf)
}
new_windf<-c()
for (i in 1:365){
  new_windf<-c(new_windf,windf_vec[[i]]@forecast[["seriesFor"]])
}

error_n<-c()
counter_n<-0
counter2_n<-0
for (z in 1:365){
  for (i in 1:30){
    error_n<-c(error_n,wind[i+335+counter2_n]-new_windf[i+counter_n]) #stores error
  }
  counter_n<-counter_n+30
  counter2_n<-counter2_n+1
}

hist(error_n)
sd_error_n<-sd(error_n)
error_mean_n<-mean(error_n)
plot(error_n,type="l",col="red")


####################################################

error_mean<-c()
mean_wind<-mean(wind)
for (z in 1:365){
  error_mean<-c(error_mean,wind[335+z]-mean_wind)
}
mean(error_mean)
sd(error_mean)
hist(error_mean)
plot(error_mean,type="l",col="red")



####################################################

#Temperature in Dublin

maxtp<-Dublin4R$maxtp
mintp<-Dublin4R$mintp

maxtp_n <- window(maxtp, start = 28734, end = 29463)
mintp_n <- window(mintp, start = 28734, end = 29463)

plot(maxtp_n,type="l",col="red", bty="L", xlab="Day",ylab="Temp") #Plot first line
points(mintp_n,type="l",col="blue")

midtp<-c()
for (i in 1:730){
  midtp<- c(midtp,(maxtp_n[i]+mintp_n[i])/2)
}
points(midtp,type="l",col="green")

midtp %>%
  ggAcf() 

midtp %>%
  adf.test() 

midtp %>%
  ur.kpss(use.lag = 8) %>%
  summary() 

midtp %>%
  Box.test(lag=8, type="Ljung-Box")

#All 4 tests are demonstrating non-stationarity. Get graphs into report.

ndiffs(midtp) #1
nsdiffs(midtp) #none

midtp %>%
  diff() %>%
  ggAcf() 

midtp %>%
  diff() %>%
  adf.test() 

midtp %>%
  diff() %>%
  ur.kpss(use.lag = 8) %>%
  summary() 

midtp %>%
  diff() %>%
  Box.test(lag=8, type="Ljung-Box")

auto.arima(midtp, 
           stepwise = FALSE, 
           approximation = FALSE) #ARIMA(2,1,1), AICc=3066.7, this is the best

##################################################

ug_spec <- ugarchspec(mean.model=list(armaOrder=c(1,0)))
ug_spec

plot.ts(midtp)
ArchTest(midtp) #there are ARCH effects as p-value very low against H0
garch(midtp,grad="numerical",trace=FALSE)

x=ugarchspec(variance.model = list(garchOrder=c(1,1)),mean.model = list(armaOrder=c(0,0)))
xfit=ugarchfit(x,data=midtp)
xfit #the beta term (q) is not significant so we try a GARCH(1,0) model which is the same as the ARCH(1) model for 365 days only.

x=ugarchspec(variance.model = list(garchOrder=c(1,0)),mean.model = list(armaOrder=c(0,0)))
xfit=ugarchfit(x,data=midtp)
xfit #this is a better model and is simply the ARCH(1) model when looking at 365 days only.

windf<-ugarchforecast(xfit,n.ahead(30))
windf

#####################################################

error_mean_tp<-c()

new_vec_tp<-c()
for (i in (335):(699)){
  fit4 <- Arima(midtp[1:i], order = c(2, 1, 1)) #iters ARIMA for wind from 1 to i to create model
  thing_tp<-fit4 %>% forecast(30) #uses current model to create 30 day forecast
  new_vec_tp<-c(new_vec_tp,thing_tp[4]) #stores forecast along with all other forecasts
}

newnew_vec_tp<-unlist(new_vec_tp) #makes collection of forecasts into vector


errortp<-c()
countertp<-0
counter2tp<-0
for (z in 1:365){
  for (i in 1:30){
    errortp<-c(errortp,midtp[i+335+counter2tp]-newnew_vec_tp[i+countertp]) #stores error
  }
  countertp<-countertp+30
  counter2tp<-counter2tp+1
}

hist(errortp)
sd(errortp)
mean(errortp)
plot(errortp,type="l",col="red")


################################################


midtp_vec<-c()
x=ugarchspec(variance.model = list(garchOrder=c(1,0)),mean.model = list(armaOrder=c(0,0)))
for (i in (335):(699)){
  xfit=ugarchfit(x,data=midtp[1:i])
  midtpf<-ugarchforecast(xfit,n.ahead = 30)
  midtp_vec<-c(midtp_vec,midtpf)
}
new_midtp<-c()
for (i in 1:365){
  new_midtp<-c(new_midtp,midtp_vec[[i]]@forecast[["seriesFor"]])
}

error_ntp<-c()
counter_ntp<-0
counter2_ntp<-0
for (z in 1:365){
  for (i in 1:30){
    error_ntp<-c(error_ntp,midtp[i+335+counter2_ntp]-new_midtp[i+counter_ntp]) #stores error
  }
  counter_ntp<-counter_ntp+30
  counter2_ntp<-counter2_ntp+1
}

hist(error_ntp)
sd(error_ntp)
mean(error_ntp)
plot(error_ntp,type="l",col="red")

############################################

error_meantp<-c()
mean_windtp<-mean(midtp)
for (z in 1:365){
  error_meantp<-c(error_meantp,midtp[335+z]-mean_windtp)
}
mean(error_meantp)
sd(error_meantp)
hist(error_meantp)
plot(error_meantp,type="l",col="red")


############################################

