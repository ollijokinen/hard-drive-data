#This program analyzes open hard drive data stored locally on Microsoft SQL Server.
#Copyright by Olli Jokinen.

library("RODBC")
library("corrplot")
library("matrixStats")

#sink(file="hard_drive_output.txt")

channel <- odbcConnect("SQL Server")

#select hard drives which have experienced a failure
hdd <- sqlQuery(channel, "SELECT model, smart_9_raw FROM HardDisk.dbo.HardDriveTable WHERE (failure = 1 AND smart_9_raw < 24*10*365)")
levels_model <- levels(factor(hdd$model))
num <- length(levels_model)

#drive lifetime in days before failure
days <- hdd$smart_9_raw / 24.0

#average lifetime for each model
mean_days <- rep(0,num)
std_days <- rep(NA,num)
hist_model <- rep(0,num)
for (k in seq(1,num)){
  p <- which(hdd$model == levels_model[k])
  mean_days[k] = mean(days[p])
  if (length(p) > 1){
    std_days[k] = sd(days[p])
    hist_model[k] = length(p)
  }
}
valid <- (which(hist_model > 1 & mean_days > 100))
model_idx <- seq(1:num)

df_model <- data.frame(model = levels_model, mean_days = mean_days, std_days = std_days)
print(df_model)

mean_hgst <- mean(mean_days[1:5])
mean_hitachi <- mean(mean_days[6:11])
mean_st <- mean(mean_days[12:31])
mean_toshiba <- mean(mean_days[32:34])
mean_wdc <- mean(mean_days[35:56])
df_manufacturer <- data.frame(manufacturer = c("HGST", "Hitachi", "ST", "Toshiba", "WDC"), mean_days = c(mean_hgst, mean_hitachi, mean_st, mean_toshiba, mean_wdc))
print(df_manufacturer)

#pdf("lifetime.pdf")
par(fig=c(0,1,0,1))
plot(model_idx[valid], mean_days[valid], xlab = "index of hard drive model", ylab = "mean lifetime +- standard deviation / days", main="Average lifetime for each hard drive model")
segments(model_idx[valid], mean_days[valid] + std_days[valid], model_idx[valid], mean_days[valid] - std_days[valid])
#dev.off()
readline()

#predict which hard drives will fail within the next delta days from a given date
delta <- 10

#select the data of given date
hdd_day <- sqlQuery(channel, "SELECT serial_number, model, smart_9_raw FROM HardDisk.dbo.HardDriveTable WHERE (date = '2016-07-31' AND smart_9_raw < 24*10*365)")

#restrict the analysis to those models that we have lifetime data
inliers <- (hdd_day$model %in% levels_model[valid])
model_day <- hdd_day$model[inliers]
levels_model_day <- levels(factor(model_day))
serial_number_day <- hdd_day$serial_number[inliers]
uptime_day <- hdd_day$smart_9_raw[inliers] / 24.0

#assume that lifetime follows normal distribution N(mean_days, std_days^2)
failure_probability <- rep(0,length(inliers))

for (i in seq(1,length(levels_model_day))){
  m <- (which(model_day == levels_model_day[i]))
  k <- (which(levels_model == levels_model_day[i]))
  t_now <- (uptime_day[m] - mean_days[k]) / std_days[k]
  t_after <- (uptime_day[m] + as.numeric(delta) - mean_days[k]) / std_days[k]
  failure_probability[m] <- (pnorm(t_after) - pnorm(t_now)) / pnorm(t_now, lower.tail = FALSE)
}

#hundred most probable failures
ord <- order(-failure_probability)
print(paste("Hundred hard drives which will most probably fail (with the normal distribution assumption) within the next",delta,"days from 2016-07-31:"))
df_failure <- data.frame(serial_number = serial_number_day[ord[1:100]], model = model_day[ord[1:100]], probability = failure_probability[ord[1:100]])
print(df_failure)

#actual failures
hdd_actual <- sqlQuery(channel, "SELECT serial_number, model FROM HardDisk.dbo.HardDriveTable WHERE (failure = 1 AND date > '2016-07-31' AND date <= '2016-08-10' AND smart_9_raw < 24*10*365)")

num_actual <- length(hdd_actual$serial_number)
proba <- rep(0,num_actual)
for (j in seq(1,num_actual)){
  n <- which(serial_number_day == as.character(hdd_actual$serial_number[j]))
  if (length(n) > 0){
    proba[j] <- failure_probability[n]
  }
}
ord_actual <- order(-proba)
print("Actual failures within the same period:")
df_actual <- data.frame(serial_number = hdd_actual$serial_number[ord_actual], model = hdd_actual$model[ord_actual], probability = proba[ord_actual])
print(df_actual)

#estimate the probability of no failure as a function of time (probability of a failure has no way occurred by the time in question)

#select the data of last date and failures within the whole database
hdd_time <- sqlQuery(channel, "SELECT serial_number, model, smart_9_raw FROM HardDisk.dbo.HardDriveTable WHERE ((failure = 1 OR date = '2016-09-30') AND smart_9_raw < 24*10*365)")
model_time <- hdd_time$model
levels_model_time <- levels(factor(model_time))
num_time <- length(levels_model_time)
num_all <- length(model_time)
uptime <- hdd_time$smart_9_raw / 24.0

#probability that a failure may happen (data-driven upper bound for the failure)
failure_probability_time <- rep(0,length(inliers))

#go through models of given date
for (i in seq(1,length(levels_model_day))){
  m <- which(model_day == levels_model_day[i])
  s <- which(model_time == as.character(levels_model_day[i]))

  #order uptimes of the specific model
  ord_time <- order(uptime[s])

  for (u in seq(1,length(m))){
    #find the position of given time in the order
    pos_now <- which.max(sign(c(0,uptime[s[ord_time]],4000) - uptime_day[m[u]])) - 1
    pos_after <- which.max(sign(c(0,uptime[s[ord_time]],4000) - uptime_day[m[u]] - delta)) - 1

    #compute the no failure probabilities
    prob_now <- 1 - (pos_now - 1) / length(s)
    prob_after <- 1 - (pos_after - 1) / length(s)

    #compute upper bound for possible failure
    failure_probability_time[m[u]] <- prob_now - prob_after
  }
}

#hundred failures that have biggest upper bounds
ord_failure <- order(-failure_probability_time)
success <- (serial_number_day[ord_failure[1:100]] %in% hdd_actual$serial_number)
print(paste("Hundred hard drives which have the biggest data-driven upper bounds for the probability to fail within the next",delta,"days from 2016-07-31:"))
df_failure_time <- data.frame(serial_number = serial_number_day[ord_failure[1:100]], model = model_day[ord_failure[1:100]], upper_bound = failure_probability_time[ord_failure[1:100]], success = success)
print(df_failure_time)

#study if some smart values can be used to predict the failure before it actually happens

long_run <- 0
if (long_run == 1){
  #select a sequence of data of one hard drive model (leave out smart_4_raw, smart_12_raw which do not change much)
  hdd_seq <- sqlQuery(channel, "SELECT date, serial_number, failure, smart_1_raw, smart_7_raw, smart_190_raw, smart_193_raw, smart_194_raw, smart_240_raw, smart_241_raw, smart_242_raw FROM HardDisk.dbo.HardDriveTable WHERE (model = 'ST4000DM000' AND date > '2016-07-31' AND smart_9_raw < 24*10*365)")

  #find failures in september
  f <- which(hdd_seq$failure == 1 & as.Date(hdd_seq$date) > "2016-08-31")
  smart <- array(NA, dim=c(length(f),30,8))

  for (t in seq(1,length(f))){
    #find data before 30 days of failure for each serial_number
    v <- which(hdd_seq$serial_number == as.character(hdd_seq$serial_number[f[t]]) & as.Date(hdd_seq$date) > as.Date(hdd_seq$date[f[t]]) - 30)
    if (length(v)>0){
      #update smart array by the difference of the smart value with respect to its value on the failure date
      ord_date <- order(hdd_seq$date[v])
      smart[t,(31-length(v)):30,1] <- hdd_seq$smart_1_raw[v[ord_date]] - hdd_seq$smart_1_raw[v[ord_date[length(v)]]]
      smart[t,(31-length(v)):30,2] <- hdd_seq$smart_7_raw[v[ord_date]] - hdd_seq$smart_7_raw[v[ord_date[length(v)]]]
      smart[t,(31-length(v)):30,3] <- hdd_seq$smart_190_raw[v[ord_date]] - hdd_seq$smart_190_raw[v[ord_date[length(v)]]]
      smart[t,(31-length(v)):30,4] <- hdd_seq$smart_193_raw[v[ord_date]] - hdd_seq$smart_193_raw[v[ord_date[length(v)]]]
      smart[t,(31-length(v)):30,5] <- hdd_seq$smart_194_raw[v[ord_date]] - hdd_seq$smart_194_raw[v[ord_date[length(v)]]]
      smart[t,(31-length(v)):30,6] <- hdd_seq$smart_240_raw[v[ord_date]] - hdd_seq$smart_240_raw[v[ord_date[length(v)]]]
      smart[t,(31-length(v)):30,7] <- hdd_seq$smart_241_raw[v[ord_date]] - hdd_seq$smart_241_raw[v[ord_date[length(v)]]]
      smart[t,(31-length(v)):30,8] <- hdd_seq$smart_242_raw[v[ord_date]] - hdd_seq$smart_242_raw[v[ord_date[length(v)]]]
    }
  }

  #mean of each smart value over serial_numbers as a function of time before failure
  mean_smart <- colMeans(smart, dims=1, na.rm=TRUE)
  write(mean_smart, file="mean_smart.txt", ncolumns=30)
} else {
  ms <- scan(file="mean_smart.txt")
  mean_smart <- array(ms, dim=c(30,8))
}

tim <- seq(-29,0)
#pdf("mean_smart.pdf")
par(mfrow=c(2,4))
heading <- c("smart_1_raw", "smart_7_raw", "smart_190_raw", "smart_193_raw", "smart_194_raw", "smart_240_raw", "smart_241_raw", "smart_242_raw")
for (h in seq(1,8)){
  plot(tim, mean_smart[,h], type="n", xlab="time / days", ylab="change in smart value", main=heading[h])
  lines(tim, mean_smart[,h])
}
#dev.off()
readline()

#study smart_7_raw for a longer period before failure

very_long_run <- 0
if (very_long_run == 1){
  hdd_smart_7 <- sqlQuery(channel, "SELECT date, serial_number, failure, smart_7_raw FROM HardDisk.dbo.HardDriveTable WHERE (model = 'ST4000DM000' AND date >= '2016-01-01' AND smart_9_raw < 24*10*365)")

  f7 <- which(hdd_smart_7$failure == 1 & as.Date(hdd_smart_7$date) > "2016-01-31")
  smart7 <- array(NA, dim=c(length(f7),274))

  for (t7 in seq(1,length(f7))){
    v7 <- which(hdd_smart_7$serial_number == as.character(hdd_smart_7$serial_number[f7[t7]]) & as.Date(hdd_smart_7$date) >= as.Date("2016-01-01"))
    if (length(v7)>0){
      ord_date7 <- order(hdd_smart_7$date[v7])
      smart7[t7,(275-length(v7)):274] <- hdd_smart_7$smart_7_raw[v7[ord_date7]] - hdd_smart_7$smart_7_raw[v7[ord_date7[length(v7)]]]
    }
  }

  write(smart7, file="smart7.txt", ncolumns=length(f7))
  mean_smart7 <- colMeans(smart7, dims=1, na.rm=TRUE)
  write(mean_smart7, file="mean_smart7.txt", ncolumns=1)
} else {
  s7 <- scan(file="smart7.txt")
  smart7 <- array(s7, dim=c(length(s7)/274,274))
  outlier <- (which(abs(smart7)>1e+10))
  smart7[outlier] <- NA
  mean_smart7 <- colMeans(smart7, dims=1, na.rm=TRUE)
}

tim7 <- seq(-273,0)
#pdf("mean_smart7.pdf")
par(mfrow=c(1,1))
plot(tim7, mean_smart7, type="l", xlab="time / days", ylab="change in smart value", main="smart_7_raw")
#dev.off()
readline()

#study correlations between smart values for each hard drive model

hdd_cor <- sqlQuery(channel, "SELECT model, smart_1_raw, smart_2_raw, smart_3_raw, smart_7_raw, smart_8_raw, smart_9_raw, smart_190_raw, smart_192_raw, smart_193_raw, smart_194_raw, smart_240_raw, smart_241_raw, smart_242_raw FROM HardDisk.dbo.HardDriveTable WHERE (failure = 1 AND smart_9_raw < 24*10*365)")
for (idx in seq(2,ncol(hdd_cor))){
  hdd_cor[which(hdd_cor[,idx] == 0 | hdd_cor[,idx] == 100), idx] <- NA
}

for (k in seq(1,num)){
  p <- which(hdd_cor$model == levels_model[k])
  if (length(p) >= 3){
    pcol <- which(!is.na(hdd_cor[p[1], 2:ncol(hdd_cor)]) & !is.na(hdd_cor[p[2], 2:ncol(hdd_cor)]) & !is.na(hdd_cor[p[3], 2:ncol(hdd_cor)]))
    if (length(pcol)>1){
      print(levels_model[k])
      #if (levels_model[k] == "ST4000DM000") pdf("correlations.pdf")
      co <- cor(hdd_cor[p, pcol+1], use="pairwise.complete.obs", method="pearson")
      corrplot(co, method="circle", type="upper", order="AOE")
      readline()
      #if (levels_model[k] == "ST4000DM000") dev.off()
    }
  }
}

#learn a model for the lifetime of ST4000DM000 as a function of other smart values

print("ST4000DM000")
st <- which(hdd_cor$model == "ST4000DM000")
days_all <- hdd_cor$smart_9_raw[st] / 24.0
#training set
st_train <- st[seq(1,length(st), by=2)]
days_train <- hdd_cor$smart_9_raw[st_train] / 24.0
#validation set
st_valid <- st[seq(2,length(st), by=2)]
days_valid <- hdd_cor$smart_9_raw[st_valid] / 24.0

fit <- function(day, x, y, mod){
  switch(mod,
    lm(day ~ x),
    lm(day ~ y),
    lm(day ~ x + y),
    lm(day ~ I(x^2)),
    lm(day ~ I(y^2)),
    lm(day ~ y + I(x^2)),
    lm(day ~ x + I(y^2)),
    lm(day ~ I(x^2) + I(y^2)),
    lm(day ~ I(x^2) + I(x*y) + I(y^2)),
    lm(day ~ x + y + I(x^2) + I(x*y) + I(y^2)),
    lm(day ~ x + y + I(x^2) + I(x*y) + I(y^2) + I(x^3) + I(x^2*y) + I(x*y^2) + I(y^3)),
    lm(day ~ 0 + y + I(x*y) + I(y^2) + I(x^2*y) + I(x*y^2) + I(y^3)))
}

evaluate <- function(x, y, co, mod){
  switch(mod,
    d <- co[1] + co[2]*x,
    d <- co[1] + co[2]*y,
    d <- co[1] + co[2]*x + co[3]*y,
    d <- co[1] + co[2]*x^2,
    d <- co[1] + co[2]*y^2,
    d <- co[1] + co[2]*y + co[3]*x^2,
    d <- co[1] + co[2]*x + co[3]*y^2,
    d <- co[1] + co[2]*x^2 + co[3]*y^2,
    d <- co[1] + co[2]*x^2 + co[3]*x*y + co[4]*y^2,
    d <- co[1] + co[2]*x + co[3]*y + co[4]*x^2 + co[5]*x*y + co[6]*y^2,
    d <- co[1] + co[2]*x + co[3]*y + co[4]*x^2 + co[5]*x*y + co[6]*y^2 + co[7]*x^3 + co[8]*x^2*y + co[9]*x*y^2 + co[10]*y^3,
    d <- co[1]*y + co[2]*x*y + co[3]*y^2 + co[4]*x^2*y + co[5]*x*y^2 + co[6]*y^3)
  return(d)  
}

#fit on training set and validate on validation set
sigma <- rep(0,12)
R2 <- rep(0,12)
for (idx in seq(1:12)){
  fit_mod <- fit(days_train, hdd_cor$smart_241_raw[st_train], hdd_cor$smart_242_raw[st_train], idx)
  days_mod <- evaluate(hdd_cor$smart_241_raw[st_valid], hdd_cor$smart_242_raw[st_valid], coefficients(fit_mod), idx)
  sigma[idx] <- sqrt(sum((days_valid - days_mod)^2) / (length(days_valid) - length(coefficients(fit_mod))))
  R2[idx] <- summary(fit_mod)$r.squared
}

#find which model gives the lowest error on validation set
best <- which.min(sigma)

#fit on the whole set
fit_best <- fit(days_all, hdd_cor$smart_241_raw[st], hdd_cor$smart_242_raw[st], best)
print(summary(fit_best))

#model with largest R^2
best2 <- which.max(R2)
fit_best2 <- fit(days_all, hdd_cor$smart_241_raw[st], hdd_cor$smart_242_raw[st], best2)
print(summary(fit_best2))

close(channel)
#sink()
