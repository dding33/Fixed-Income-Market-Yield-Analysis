library(readxl)
library(dplyr)
library(jrvFinance)
library(kableExtra)
library(xtable)
setwd("~/Desktop/YR4/APM466/A1/Fixed-Income-Market-Yield-Analysis")
##Load the data collected
clean <- read.csv("input/clean.csv")
dirty <- read.csv("input/dirty.csv")

#Making sure the data is in the form of data frames
clean <- as.data.frame(clean)
dirty <- as.data.frame(dirty) 

#Preparing Some data
maturity_date<-as.Date(dirty$MATURITY.DATE)
#The coupon payment needs to be in the form of numbers, and we save it as a vector in order to refer back to it later
cpn_vec <- as.numeric(clean$COUPON)
#This is a list of dates at which we observed the closed price
obs_date_vec <- c("2022-1-10","2022-1-11","2022-1-12","2022-1-13","2022-1-14","2022-1-17","2022-1-18","2022-1-19","2022-1-20","2022-1-21")
#This is a matrix of closing prices only
closing_matrix = matrix(c(dirty$X2022.01.010,dirty$X2022.01.011,dirty$X2022.01.012,dirty$X2022.01.013,dirty$X2022.01.014
                              ,dirty$X2022.01.017,dirty$X2022.01.018,dirty$X2022.01.019,dirty$X2022.01.020,dirty$X2022.01.021), nrow=10, ncol = 10, byrow=TRUE)
#This is an empty matrix of time to maturity in years
year_frac_matrix = matrix('numeric', nrow=10, ncol=10)
#For each unit in the matrix, its start date is the date of observation which is the row name, and its end date is the maturity date which is the column name
#We use yearFraction() to do this
for (i in c(1:10)){
  for (j in c(1:10)){
    year_frac_matrix[i,j] = yearFraction(obs_date_vec[i], maturity_date[j])
  }
}
#This is an empty vector of month to maturity
mtm_vector <- vector()
#We fill it by calculating the difference between Jan 10 and the maturity date then convert it to month.
#Technically we should have a date for this, but to simplify and because the difference is so small, we are just going to creat a vector
for (i in 1:10){
  dtm = -as.numeric(as.Date("2022-01-10"))+ as.numeric(as.Date(maturity_date[i]))
  mtm_vector[i]=dtm/30
}
#This is a vector that gives month since last coupon, we first create a variable that represents the last coupon date
last_coupon <- c("2021-09-01", "2021-08-01", "2021-08-01", "2021-08-01", "2021-08-01",
                 "2021-10-01", "2021-09-01", "2021-09-01", "2021-09-01", "2021-09-01")
#This is an empty vector of months since the last coupon payment
month_since_last_cpn <- vector()
#First we calculate the days since last coupon payment and then we convert it into months
for (i in 1:10){
  days_since = as.numeric(as.Date("2022-01-10"))- as.numeric(as.Date(last_coupon[i]))
  month_since_last_cpn[i]=days_since/30
}
##Preparing the data frames in  order to put the calculated results later
##Building empty data frames
v <- rep(1,10)
yields_raw <- data.frame(v,v,v,v,v,v,v,v,v,v)
spot_matrix <- data.frame(v,v,v,v,v,v,v,v,v,v)
yields_res <- data.frame(v,v,v,v,v,v,v,v,v,v)
spot_res <- data.frame(v,v,v,v,v,v,v,v,v,v)
w <- rep(1,10)
forward_rate <- data.frame(w,w,w,w)


#Q4
#Now we can start calculate the ytm matrix, we will have a specific yield for each bond at each observation date, which gives a 10 by 10 matrix
#This is an empty matrix
ytm_matrix = matrix(0, nrow=10, ncol=10)
#Every column in the closed price matrix is the closing price from day 1 to day 10 for each of our bonds, we will give it a name in order to refer back to it
#For each specific date of each column, use bond.yield() to calculate a yield and then record the result in the ytm matrix
for (j in 1:10){
  close_price = closing_matrix[,j]
  for (i in 1:10){
    ytm_matrix[i,j] = as.numeric(bond.yield(settle=obs_date_vec[i], 
                                  mature = maturity_date[i],
                                  coupon = cpn_vec[i],
                                  price = close_price[i],
                                  freq = 2,#from this line it is all by default
                                  convention = c("30/360", "ACT/ACT", "ACT/360", "30/360E"),
                                  redemption_value = 100,
                                  comp.freq = 2
                                  ))
  }
}

#Plot of ytm
#This is a vector of years
year <- c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5)
#This gives the first line, we want the type of curve to be overplot
png(file="~/Desktop/YR4/APM466/A1/Fixed-Income-Market-Yield-Analysis/output/YTM.png", height=250)
plot(year, ytm_matrix[1,], type='o', main='Yield Curves of 10 Selected \n Canadian Government Bonds', col='white', xlab='Years from Jan 2020', ylab='Yield Rate in decimals', ylim=c(0,0.12), width=5, height=3.5)
#Then we top it up with the rest of lines
#Just some colors I picked
rainbow = c("lightpink", "lightcoral", "hotpink", "magenta", "pink", "peachpuff", "orchid", "plum", "violetred")
#We use lines() to add each lines to the plot
for (i in c(2:10)){
  lines(year, ytm_matrix[i,], type='o', col=rainbow[i-1])
}
legend("topleft", obs_date_vec, lty=c(1,1), lwd =c(1,1),bty='n', col=c(rainbow,"yellow"), cex=0.5)
grid()
dev.off()

#Q5
#First we calculate the price of the first 6 month using the first bond
for (i in 1:10)
{
  price=dirty[1,5+i]#because in my dirty price matrix, closing price start at day 6
  coupon=dirty[1,3]*100/2#because the government bond pays coupon twice a year
  notion=100#they all have a $100 face value
  spot_matrix[1,i]=2*((price/(coupon+notion))^(-1/(2*mtm_vector[1]))-1)
}

#From there we calculate the spot rate of other periods
for (i in c(2:10))#because we already have the first bond calculated
{
  for (j in c(1:10))
  {
    price=dirty[i,5+j]
    coupon=dirty$COUPON[i]*100/2
    notion=100
    pv=0#setting the initial value to 0
    cpn_time=seq((6-month_since_last_cpn[i])/12, (mtm_vector[i]-1)/12, 1/2)#create a vector of time stamps of upcoming coupon payments
    for (k in c(1:length(cpn_time))){
      pv=pv+coupon*(1+spot_matrix[k,j]/2)^(-2*cpn_time[k])#we now use the spot rate of the first bond to calculate the spot rate of the second bond
    }#this line above gives the present value of the coupon
    new_price=price-pv#the new price is the dirty price minus the present value of all coupon payments added together
    pv=0#e=clearing the variable for the next loop
    spot_matrix[i,j]=2*((new_price/(coupon+notion))^(-1/(2*mtm_vector)[i])-1)#add each calculated result to the spots matrix
  }
}

#Plot Spot Rate
#Just like how we plotted ytm curve, we start the plot with the first line using plot() and add additional
png(file="~/Desktop/YR4/APM466/A1/Fixed-Income-Market-Yield-Analysis/output/SPOT.png", height=250)
plot(year, spot_matrix[1,],type='o',main="Spot Curve", col='red', xlab='Years since January 2022', ylab='Spot Rate in Decimal',ylim=c(0.0005,0.004))
for(i in 2:10){
  lines(year, spot_matrix[i,], type='o', col=rainbow[i-1],lwd=0.9)
}
grid()
legend("topleft", obs_date_vec, lty=c(1,1), lwd =c(1,1),bty='n', col=c(rainbow,"yellow"), cex=0.5)
dev.off()

#Q6
#Calculate forward rate 
for (j in c(1:4)){
  for (i in c(1:10)){
    n_of_yr=(1+spot_matrix[2*j,i]/2)^(2*j)#this is the spot rate starting at n years
    one_yr_f=(1+spot_matrix[2+2*j,i]/2)^(2+2*j)#this is the spot rate starting at 1 year from now
    forward_rate[i,j]=2*((one_yr_f/n_of_yr)^(0.5)-1)#and from here we get the forward rate using the formula on slide9 of week2
  }
}

#Plot them with the first dot being the 1yr-1yr forward rate and the last dot being the 1yr-4yr forward rate
png(file="~/Desktop/YR4/APM466/A1/Fixed-Income-Market-Yield-Analysis/output/Forward.png", height=250)
plot(c(2,3,4,5), forward_rate[1,],type='o',main="Forward Curve", col='red', xlab='Number of Years From Jan 2022', ylab='Spot Rate in Decimal',ylim=c(0.001,0.003))
for(i in 2:10){#And we do do this for all bonds
  lines(c(2,3,4,5), forward_rate[i,], type='o', col=rainbow[i-1],lwd=0.9)
}
legend("topleft", obs_date_vec, lty=c(1,1), lwd =c(1,1),bty='n', col=c(rainbow,"yellow"), cex=0.5)
grid()
dev.off()

###covariance matrices and eigenvalues for log yields
#These are some empty vectors of the log return
log_return1=log_return2=log_return3=log_return4=log_return5=vector("numeric",9)
#This generates each column of the log return matrix
for (i in c(1:9)){
  log_return1[i]=log(as.numeric(ytm_matrix[2,i])/as.numeric(ytm_matrix[2,i+1]))#calculate the log return for each year, we combine two entries because the canadian government bonds is coumpounded biannually
  log_return2[i]=log(as.numeric(ytm_matrix[4,i])/as.numeric(ytm_matrix[4,i+1]))
  log_return3[i]=log(as.numeric(ytm_matrix[6,i])/as.numeric(ytm_matrix[6,i+1]))
  log_return4[i]=log(as.numeric(ytm_matrix[8,i])/as.numeric(ytm_matrix[8,i+1]))
  log_return5[i]=log(as.numeric(ytm_matrix[10,i])/as.numeric(ytm_matrix[10,i+1]))
}
#We put them into a matrix
yields_log_matrix <- data.frame(log_return1,log_return2,log_return3,log_return4,log_return5)
#And calculate the covariance
cov_for_log_return <- cov(yields_log_matrix, yields_log_matrix)
#Use eigen() to calculate the yield
eigen_of_yield=eigen(cov_for_log_return,symmetric = TRUE)

###Same thing for forward rates
fwd1=fwd2=fwd3=fwd4=fwd5=fwd6=fwd7=fwd8=fwd9=fwd10=vector("numeric",3)
for (i in c(1:3)){
  fwd1[i]=log(forward_rate[1,i]/forward_rate[1,i+1])
  fwd2[i]=log(forward_rate[2,i]/forward_rate[2,i+1])
  fwd3[i]=log(forward_rate[3,i]/forward_rate[3,i+1])
  fwd4[i]=log(forward_rate[4,i]/forward_rate[4,i+1])
  fwd5[i]=log(forward_rate[5,i]/forward_rate[5,i+1])
  fwd6[i]=log(forward_rate[6,i]/forward_rate[6,i+1])
  fwd7[i]=log(forward_rate[7,i]/forward_rate[7,i+1])
  fwd8[i]=log(forward_rate[8,i]/forward_rate[8,i+1])
  fwd9[i]=log(forward_rate[9,i]/forward_rate[9,i+1])
  fwd10[i]=log(forward_rate[10,i]/forward_rate[10,i+1])
}

fwd <- data.frame(fwd1,fwd2,fwd3,fwd4,fwd5,fwd6,fwd7,fwd8,fwd9,fwd10)
cov_fwd<-(cov(fwd,fwd))
eigen(cov_fwd,symmetric = TRUE)

#xtable(cov_for_log_return)
#xtable(cov_fwd)
#xtable(eigen_of_yield$vectors)
xtable(eigen_fwd$vectors)
##############################################################
##########################################################