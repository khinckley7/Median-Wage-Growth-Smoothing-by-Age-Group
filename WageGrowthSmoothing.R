# -------------------- Code for Wage Growth Analysis --------------------

# Clear environment
rm(list = ls())

# Set random number generator seed so results are reproducible
set.seed(1)

# -------------------- Data Manipulation --------------------

# Pull headers from dataset
headers <- read.csv('wage-growth-data-by-age.csv', skip=2, header=TRUE, stringsAsFactors=FALSE, nrows=1)
headers <- headers[1:5]

# Pull median wage growth data from dataset
df <- read.csv('wage-growth-data-by-age.csv', skip=15, header=FALSE, stringsAsFactors=FALSE)
df <- df[1:5]

# Set dataframe's column names
colnames(df) <- headers
colnames(df)[1] <- 'Date'

# Split dataframe by column/age group
df16_24 <- df[,2] # Age 16-24 data
df25_54 <- df[,3] # Age 25-54 data
df55 <- df[,4] # Age 55+ data
dfoverall <- df[,5] # Overall data

# -------------------- Single exponential smoothing --------------------

# Single exp. smoothing test on 16-24 age group
m1_1624 <- HoltWinters(df16_24, beta=FALSE, gamma=FALSE)
m1_1624

# Call:
#   HoltWinters(x = df16_24, beta = FALSE, gamma = FALSE)
# 
# Smoothing parameters:
# alpha: 0.9999529
# beta : FALSE
# gamma: FALSE
# 
# Coefficients:
#   [,1]
# a 8.399967
#
# So baseline forecast for age 16-24: 8.399967
# Best alpha: 0.9999529
#
# Not great result so add trend component (double exp. smoothing)

# -------------------- Double exponential smoothing --------------------

# Double exp. smoothing on 16-24 age group
m2_1624 <- HoltWinters(df16_24, gamma=FALSE)
m2_1624

# Call:
#   HoltWinters(x = df16_24, gamma = FALSE)
# 
# Smoothing parameters:
# alpha: 0.9441361
# beta : 0.2168675
# gamma: FALSE
# 
# Coefficients:
#   [,1]
# a 8.3671845
# b 0.2328385
# 
# New forecast for 16-24 bin: 8.3671845
# Best alpha: 0.9441361
#
# Last trend estimate not too close to 0 
# beta value also not too close to 0 
# This suggests there is a significant trend component

# Double exp. smoothing on 25-54 age group
m2_2554 <- HoltWinters(df25_54, gamma=FALSE)
m2_2554

# Double exp. smoothing on 55+ age group
m2_55 <- HoltWinters(df55, gamma=FALSE)
m2_55

# Double exp. smoothing on overall data
m2_overall <- HoltWinters(dfoverall, gamma=FALSE)
m2_overall

# Plot overall wage growth over time
plot.ts(dfoverall, xlab='Date', ylab='Median Wage Growth Percent', ylim=c(0.5,10.5))
title(main='Median Wage Growth Tracker by Age Group')
# axis.Date(1, at=seq(min(df$Date), max(df$Date), by="months"), format="%m-%Y")

# Plot fitted/smoothed values from HW models against original overall data
lines(m2_1624$fitted[,1], col = "pink") # Age 16-24 
lines(m2_2554$fitted[,1], col = "lightblue") # Age 25-54
lines(m2_55$fitted[,1], col = "green") # Age 55+
lines(m2_overall$fitted[,1], col = "red") # Overall

legend('topright', bty='n',
  legend=c('Age 16-24','Age 25-54','Age 55+','Overall - Smoothed', 'Overall - Raw'),
       col=c('pink','lightblue','green','red','black'), lty=1, cex=0.75,
       title='Legend', bg='darkgrey')





