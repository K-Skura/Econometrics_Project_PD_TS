#===================================
#            Exercise 1
#===================================



#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("readxl")
#install.packages("sandwich")

library(ggplot2)
library(dplyr)
library(sandwich)
library(readxl)


panel_data <- read_excel("IncomeDemocracy.xlsx")

#1a)Is the data set a balanced panel? Explain.

summary(panel_data)

na_columns_count <- colSums(is.na(panel_data))
na_columns_count

#Given data set is not a balanced one. It is because we have missing values for some subjects.
# As balanced panel data means that we have observations for every subject in every period. 

########################################################################

#1b) 
#i) What are the minimum and maximum values of Dem_ind in the data set? What are the
#     mean and standard deviation of Dem_ind in the data set? What are the 10th, 25th, 50th,
#     75th, and 90th percentiles of its distribution?

summary(panel_data$dem_ind)

sd(na.omit(panel_data$dem_ind))

quantile(na.omit(panel_data$dem_ind), probs = c(0.10, 0.25, 0.50, 0.75, 0.90))

#Values of Dem_ind: 
# Min : 0.00, Max: 1.00
# Mean : 0.4991, SD: 0.3713367
# Percentiles of distribution: 10%: 0.00; 25%: 0.1666667; 0.50%: 0.50; 75%: 0.8333333; 90% 1.0; 



#ii)What is the value of Dem_ind for the United States in 2000? Averaged over all years in
#   the data set?

us_sub <- subset(panel_data, panel_data$country == "United States")

print(subset(us_sub, year == '2000')[3])

mean(us_sub$dem_ind)

# Value of Dem_ind for USA in 2000 was 1 and averaged over all years it was 0.9855



#iii) What is the value of Dem_ind for Libya in 2000? Averaged over all years in the data set?

ly_sub <- subset(panel_data, panel_data$country == "Libya")

print(subset(ly_sub, year == '2000')[3])

mean(ly_sub$dem_ind)

# Value of Dem_ind for Libya in 2000 was 0 and averaged over all years it was 0.1092. 

#iv) List five countries with an average value of Dem_ind greater than 0.95; less than 0.10;
#    and between 0.3 and 0.7.

panel_data <- panel_data[complete.cases(panel_data$dem_ind), ]

average_dem_ind <- panel_data %>%
  group_by(country) %>%
  summarize(avg_dem_ind = mean(dem_ind, na.rm = TRUE))

countries_0.95 <- subset(average_dem_ind, avg_dem_ind > 0.95)$country
head(countries_0.95, 5 )

countries_0.3_0.7 <- subset(average_dem_ind, avg_dem_ind >= 0.3 & avg_dem_ind <= 0.7)$country
head(countries_0.3_0.7, 5)

countries_0.1 <- subset(average_dem_ind, avg_dem_ind < 0.1)$country
head(countries_0.1, 5)


#1c) The logarithm of per capita income is labeled Log_GDPPC. Regress Dem_ind on Log_GDPPC.
#Use standard errors that are clustered by country.

panel_data <- panel_data[complete.cases(panel_data$log_gdppc), ]

model_GDPPC <- lm(data = panel_data, 
                  formula = (dem_ind ~ log_gdppc))
summary(model_GDPPC)

clustered_se <- vcovCL(model_GDPPC, cluster = panel_data$country)

se_clustered <- sqrt(diag(clustered_se)["log_gdppc"])
se_clustered
summary(clustered_se)

fitted_values <- predict(model_GDPPC)

ggplot(panel_data, aes(x = log_gdppc, y = dem_ind)) +
  geom_point(color = 'red') +
  geom_line(aes(y = fitted_values), color = "cyan", linewidth = 1.5) +
  labs(x = "Log_GDPPC", y = "Dem_ind", title = "Fitted Values vs. Real Values") +
  theme_minimal()


# https://www.rdocumentation.org/packages/sandwich/versions/3.1-0/topics/vcovCL


#i) How large is the estimated coefficient on Log_GDPPC? Is the coefficient statistically
#significant?

#The coefficient is 0.2356. Taking into account that it is linear-log model we can calculate 
# that increase 1% increase in log_gdppc is associated with (0.01 * 0.2356) = 0.002356 increase in dem_ind. 
# Additionaly the coefficiency is statistically significant, which we see by looking at p-value 
# that is close to 0. 


#ii) If per capita income in a country increases by 20%, by how much is Dem_ind predicted to
# increase? What is a 95% confidence interval for the prediction? Is the predicted increase
# in Dem_ind large or small? (Explain what you mean by large or small.)

predicted_change <- 0.20 * 0.235673

# Increase in a per capita income in a country is associated with average increase of democracy index by 
#  0.0471346. 

se_predicted_change <- se_clustered * 0.20

confidence_interval <- c(predicted_change - 1.96 * se_predicted_change,
                   predicted_change + 1.96 * se_predicted_change)

cat("Predicted change in Dem_ind:", predicted_change, "\n")
cat("95% Confidence Interval:", confidence_interval, "\n")

# I would say that change in democracy index by 0.047 if we increase per capita gdp by 20% is relatively small. 
# It would mean that if we want to have fully democratic country we need to increase gdp per capita 
# by around 400%. Of course if we assume linearity, which can not be the case. 
# Lets say that a country has index = 0... 

#iii) Why is it important to use clustered standard errors for the regression? Do the results
#  change if you do not use clustered standard errors?

# It is usefull as clustered standard errors assume that the regression errors are uncorrelated across clusters. 
# In our context cluster means a country, so clustered standard errors allow heteroskedasticity and 
# arbitrary autocorrelation within country, however treat the erros as uncorrelated across entities. 


#1d) Suggest a variable that varies across countries but plausibly varies little-or not at all-over
#time and that could cause omitted variable bias in the regression in (c).


#The variable that varies across countries but has little changes across time is age_median. 
# Education is changing over time in most of the countries across time, it can be even observed in the 
# most democratic countries already, like Austria. 


#i) Estimate the regression in (c), allowing for country fixed effects.


model_FE <- lm(data = panel_data, 
                  formula = (dem_ind ~ log_gdppc + factor(country) - 1))

summary(model_FE)

yhat <- model_FE$fitted

plot_data <- data.frame(country = panel_data$country, Observed = panel_data$dem_ind, fitted <- yhat)

# Create a ggplot scatter plot with a line for fitted values
ggplot(plot_data, aes(x = Observed, y = fitted, color = country)) +
  geom_point(size = 3, shape = 16) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size = 1) +
  labs(x = "Observed", y = "Fitted", title = "Fixed Effects Model: Fitted Values vs. Observed Values") +
  theme_minimal()


#ii) How do your answers to (c)(i) and (c)(ii) change?

#In the second we took into consideration fixed effect. We can observe the change in the estimated coeff.
# In the c(i) it was 0.235 and in c(ii) with fixed effect the coeff. droped to 0.08. 
# This means that after we take into consideration countries fixed effect the GDP PC importance lowered. 
# It is because know every country has its own intercept and the are taking into our model with this 
# individual unobserved effects without estimating them directly.
# The R^2 is very high with around 92% of explained variation, which can suggests overfitting. 

#e) Exclude the data for Azerbaijan, and rerun the regression. Do the results change? Why or
#why not?

panel_data <- panel_data %>%
  filter(country != "Azerbaijan")

model_FE <- lm(data = panel_data, 
               formula = (dem_ind ~ log_gdppc + factor(country) - 1))

summary(model_FE)


# With Fixed Effect model we can observe that results did not change. It is because, 1 observation is  
#   marginal looking how big is our dataset. Additionally Azerbaijan has only 1 observation from 1 year. 
#   This means that we cannot explore how democracy index is changing with different variables for this country. 


#f)Suggest a variable that varies over time but plausibly varies little-or not at all - across
#   countries and that could cause omitted variable bias in the regression in (c). Estimate the
#   regression in (c), allowing for time and country fixed effects. How do your answers to (c)(i) and
#   (c)(ii) change?

#The variable that varies over time but not across countries are pandemics for example - last example Covid. 
# Additionally some the state-of-the-art technology prices are varying across time, but not that much 
# across countries. 

model_FE_time <- lm(data = panel_data, 
               formula = (dem_ind ~ log_gdppc + factor(country) - 1 + factor(year) - 1))
summary(model_FE_time)

#g) There are additional demographic controls in the data set. Should these variables be included
#in the regression? If so, how do the results change when they are included?

# I don't think all of them should be used. Education, log_pop and age_median may bring some

model_age <- lm(data = panel_data, 
                formula = (age_median ~ age_1 + age_2))
summary(model_age)
# We can see that using only 2 age groups already creates a model with R^2 = 99%. Which means that our 
# variables would be highly correlated and thus breaking one of the assumption of No Multicolinearity.
# Thats why its better to either choose one of the age groups of age_median. 

#Lets do our regression adding education, log_pop and age_median

model_g1 <- lm(data = panel_data, 
                  formula = (dem_ind ~ log_gdppc + educ + log_pop + age_median))
summary(model_g1)
summary(model_GDPPC)
#We can see that all of the variables added are statistically significant for our regression. 
# It means that not including them would result in Omitted Variable Bias. 


#h) Based on your analysis, what conclusions do you draw about the effects of income on democracy?
#Compare this with the conclusion of Acemoglu et al. (2008), which is also posted on
#Moodle.

#The gdp per capita causal effect on democracy index in regression with FE (country, year) 
# is statistically significant for alpha = 0.05. Additionally this effect seems marginal if we compare it 
# to the linear model presented before. Thus, we can conclude that in the model with FE income does not play
# important part to explain the differences in democracy index across countries. 

#Our conclusion above is in line with conclusion from Acemoglu et al. (2008). We can suspect that the 
# co-occurrence of high product per capita and democracy index is based upon historical events. 
# So the source of variation of democracy and income is rooted in history. 




#======================================
#             Exercise 2
#======================================

library(forecast)
library(tseries)

#a)Load the data into R and get an overview. Analyze graphically whether the time series
#  appears to be stationary. Calculate the expected value and variance. Briefly interpret what you
#  find. 

sunspot <- read.csv("Sunspots.csv")

colnames(sunspot) <- c('Index','Date' ,'Monthly_Sunspots')

sunspot <- select(sunspot,-Index)

summary(sunspot)

exp_value <- mean(sunspot$Monthly_Sunspots)
exp_value

ts_var <- var(sunspot$Monthly_Sunspots)
ts_var

ts_plot <- ggplot() +
  geom_line(data = sunspot, aes(x = Date, y = Monthly_Sunspots, group = 1)) + 
  geom_line(data = sunspot, aes(x = Date, y = exp_value, group = 1), color = 'red') +
  geom_line(data = sunspot, aes(y = exp_value + sqrt(ts_var), x = Date, group = 1), color = "green") +
  geom_line(data = sunspot, aes(y = exp_value - sqrt(ts_var), x = Date, group = 1), color = "green") +
  xlab("Time")+
  ylab("Monthly Sunspots")
ts_plot

#The sunspot ts we recognize as stationary. There is no visible trend and the real value is always approaching 
# the expected value on graph. 


#b) Use an Augmented Dickey-Fuller test to investigate stationarity further. What do you conclude?

#We are using ADF test to test if our time series has a stochastic trend. 

adf <- adf.test(sunspot$Monthly_Sunspots)
adf

# Looking at p-value we can reject null-hypothesis. Thus we can also conclude that our ts is stationary.



#c) Use Ljung-Box-Q statistics for lag 6, 12 and 18 to investigate autocorrelation. Interpret what
#you find.

L_box_6 <- Box.test(sunspot$Monthly_Sunspots, lag = 6, type = "Ljung")
L_box_6

L_box_12 <- Box.test(sunspot$Monthly_Sunspots, lag = 12, type = "Ljung")
L_box_12

L_box_18 <- Box.test(sunspot$Monthly_Sunspots, lag = 18, type = "Ljung")
L_box_18

#Our p-values in all of the tests are smaller than 2.2e-16. By this we can reject the null
#  hypothesis of the test and conclude that the data values are independent.


#d) Calculate and analyze ACF, PACF. Which ARMA model should we pick?

acf(sunspot$Monthly_Sunspots)

pacf(sunspot$Monthly_Sunspots)

#As we can see we figured out order of our time series using PACF. Thus, we should use AR process. 


#f) Estimate an AR(2), AR(4), AR(6), MA(2), MA(4), MA(6), ARMA(2,2), ARMA(4,4) and
#ARMA(6,6) process. Create a table containing the following:

matrix <- matrix(nrow = 10, ncol = 9)

table <- data.frame(matrix)
                    
colnames(table) <- c('AR(2)', 'AR(4)', 'AR(6)', 'MA(2)', 'MA(4)', 'MA(6)', 'ARMA(2,2)', 'ARMA(4,4)', 'ARMA(6,6)')
rownames(table) <- c('coeff','t-statistics', 'sum_sq_res', 'AIC', 'BIC', 'Q(6)', 'Q(12)', 'Q(18)', 'Forecast 1', 'Forecast 10')


model_func <- function(order) {
  model <- arima(sunspot$Monthly_Sunspots, order=order)
  se <- sqrt(diag(vcov(model)))
  residu <- residuals(model)
  t_stat <- model$coef / se
  ssr <- sum(resid(model)^2)
  predict1 <- predict(model)
  predict10 <- predict(model,n.ahead=10)
  return(list(coeff=model$coef, 
              t_stat=t_stat,
              ssr=ssr, 
              AIC=AIC(model), 
              BIC=BIC(model),
              residuals = residu,
              predict1 = predict1,
              predict10 = predict10))
}


AR_2 <- model_func(c(2,0,0))
AR_4 <- model_func(c(4,0,0))
AR_6 <- model_func(c(6,0,0))
MA_2 <- model_func(c(0,0,2))
MA_4 <- model_func(c(0,0,4))
MA_6 <- model_func(c(0,0,6))
ARMA_22 <- model_func(c(2,0,2))
ARMA_44 <- model_func(c(4,0,4))
ARMA_66 <- model_func(c(6,0,6))

coeffs <- list()

coeffs[["AR(2)"]] <- AR_2$coeff
coeffs[["AR(4)"]] <- AR_4$coeff
coeffs[["AR(6)"]] <- AR_6$coeff
coeffs[["MA(2)"]] <- MA_2$coeff
coeffs[["MA(4)"]] <- MA_4$coeff
coeffs[["MA(6)"]] <- MA_6$coeff
coeffs[["ARMA(22)"]] <- ARMA_22$coeff
coeffs[["ARMA(44)"]] <- ARMA_44$coeff
coeffs[["ARMA(66)"]] <- ARMA_66$coeff

coeffs_df<- data.frame(t(coeffs))

t_stats <- list()

t_stats[["AR(2)"]] <- AR_2$t_stat
t_stats[["AR(4)"]] <- AR_4$t_stat
t_stats[["AR(6)"]] <- AR_6$t_stat
t_stats[["MA(2)"]] <- MA_2$t_stat
t_stats[["MA(4)"]] <- MA_4$t_stat
t_stats[["MA(6)"]] <- MA_6$t_stat
t_stats[["ARMA(22)"]] <- ARMA_22$t_stat
t_stats[["ARMA(44)"]] <- ARMA_44$t_stat
t_stats[["ARMA(66)"]] <- ARMA_66$t_stat

t_stat_df<- data.frame(t(t_stats))

#i. Coefficients and t-statistics of the parameters

table[1,] <- coeffs_df
table[2,] <- t_stat_df


#ii. Sum of squared residuals
#iii. AIC and BIC


for (num in c(3,4,5)){
  table[num,1] <- AR_2[num]
  table[num,2] <- AR_4[num]
  table[num,3] <- AR_6[num]
  table[num,4] <- MA_2[num]
  table[num,5] <- MA_4[num]
  table[num,6] <- MA_6[num]
  table[num,7] <- ARMA_22[num]
  table[num,8] <- ARMA_44[num]
  table[num,9] <- ARMA_66[num]
}

#iv. Q(6), Q(12) and Q(18)

for (num in c(6,12,18)) {
  if (num == 6){
    table[6,1]<- Box.test(AR_2$residuals, lag = num, type = "Ljung")[3]
    table[6,2]<- Box.test(AR_4$residuals, lag = num, type = "Ljung")[3]
    table[6,3]<- Box.test(AR_6$residuals, lag = num, type = "Ljung")[3]
    table[6,4]<- Box.test(MA_2$residuals, lag = num, type = "Ljung")[3]
    table[6,5]<- Box.test(MA_4$residuals, lag = num, type = "Ljung")[3]
    table[6,6]<- Box.test(MA_6$residuals, lag = num, type = "Ljung")[3]
    table[6,7]<- Box.test(ARMA_22$residuals, lag = num, type = "Ljung")[3]
    table[6,8]<- Box.test(ARMA_44$residuals, lag = num, type = "Ljung")[3]
    table[6,9]<- Box.test(ARMA_66$residuals, lag = num, type = "Ljung")[3]
    }
  else if (num == 12){
    table[7,1]<- Box.test(AR_2$residuals, lag = num, type = "Ljung")[3]
    table[7,2]<- Box.test(AR_4$residuals, lag = num, type = "Ljung")[3]
    table[7,3]<- Box.test(AR_6$residuals, lag = num, type = "Ljung")[3]
    table[7,4]<- Box.test(MA_2$residuals, lag = num, type = "Ljung")[3]
    table[7,5]<- Box.test(MA_4$residuals, lag = num, type = "Ljung")[3]
    table[7,6]<- Box.test(MA_6$residuals, lag = num, type = "Ljung")[3]
    table[7,7]<- Box.test(ARMA_22$residuals, lag = num, type = "Ljung")[3]
    table[7,8]<- Box.test(ARMA_44$residuals, lag = num, type = "Ljung")[3]
    table[7,9]<- Box.test(ARMA_66$residuals, lag = num, type = "Ljung")[3]
  }
  else if (num == 18){
    table[8,1]<- Box.test(AR_2$residuals, lag = num, type = "Ljung")[3]
    table[8,2]<- Box.test(AR_4$residuals, lag = num, type = "Ljung")[3]
    table[8,3]<- Box.test(AR_6$residuals, lag = num, type = "Ljung")[3]
    table[8,4]<- Box.test(MA_2$residuals, lag = num, type = "Ljung")[3]
    table[8,5]<- Box.test(MA_4$residuals, lag = num, type = "Ljung")[3]
    table[8,6]<- Box.test(MA_6$residuals, lag = num, type = "Ljung")[3]
    table[8,7]<- Box.test(ARMA_22$residuals, lag = num, type = "Ljung")[3]
    table[8,8]<- Box.test(ARMA_44$residuals, lag = num, type = "Ljung")[3]
    table[8,9]<- Box.test(ARMA_66$residuals, lag = num, type = "Ljung")[3]
  }
}

# The values added from Ljung test is p-value, if it is 0 in table it means it was around 2.2e-16


#v. Forecast with the model and pick a forecast horizon

#Forecasts for period 1
table[9,1] <- as.numeric(AR_2$predict1[1])
table[9,2] <- as.numeric(AR_4$predict1[1])
table[9,3] <- as.numeric(AR_6$predict1[1])
table[9,4] <- as.numeric(MA_2$predict1[1])
table[9,5] <- as.numeric(MA_4$predict1[1])
table[9,6] <- as.numeric(MA_6$predict1[1])
table[9,7] <- as.numeric(ARMA_22$predict1[1])
table[9,8] <- as.numeric(ARMA_44$predict1[1])
table[9,9] <- as.numeric(ARMA_66$predict1[1])

#Forecasts for period 10

table[10,1] <- AR_2[["predict10"]][["pred"]][10]
table[10,2] <- AR_4[["predict10"]][["pred"]][10]
table[10,3] <- AR_6[["predict10"]][["pred"]][10]
table[10,4] <- MA_2[["predict10"]][["pred"]][10]
table[10,5] <- MA_4[["predict10"]][["pred"]][10]
table[10,6] <- MA_6[["predict10"]][["pred"]][10]
table[10,7] <- ARMA_22[["predict10"]][["pred"]][10]
table[10,8] <- ARMA_44[["predict10"]][["pred"]][10]
table[10,9] <- ARMA_66[["predict10"]][["pred"]][10]

table




#g) In f), which of the models would you prefer? Why? Also examine the residuals!

# I would choose AR(2) model. As we discovered before to explain this model, the best is PACF thus we should use AR.
#   Additionally, Ljung model p-values are the lowest for AR(2), which tells us that we can reject the null
#   hypothesis and conclude that the residuals are independent.
#   The sum of residuals is not the smallest in AR(2), but still one of the smallest. Smaller ones are only in 
#   AR(6) where Ljung p-values are >0.05 and in ARMA processes. However, ARMAS do not reject null hypothesis of Ljung. 
#   BIC and AIC differences between AR models and ARMA models are marginal. 




#=================================================
#         Exercise 3           
#=================================================


phi1 <- 0.9
phi2 <- -0.3

#(a) ’Simulate’ the PACF by generating a time series from this model and then using the pacf()
#command in R; What are the coefficients π1 and π2 ?

set.seed(123)
n <- 100
epsilon <- rnorm(n)
X <- numeric(n)
X[1]<- epsilon[1]
X[2] <- epsilon[2]

for (i in 3:n) {
  X[i] <- phi1 * X[i-1] + phi2 * X[i-2] + epsilon[i]
}

pacf_X <- pacf(X, lag.max = 10)
print(pacf_X)

#Our estimates are the following: π1 = 0.665 π2 = -0.260

#(b) Regress xt on xt−1. Does this give the right (estimated) coefficient π1 ?

lm_result <- lm(X[-1] ~ X[-length(X)])
pi1_hat <- coef(lm_result)[2]
print(pi1_hat)

#Since we got 0.6657345 we see that our estimate was right. 

lm_result <- lm(X[-c(1, n)] ~ X[-c(1, n-1)] + X[-c(1, n-2)])
pi1_hat <- coef(lm_result)[2]  
pi2_hat <- coef(lm_result)[3]  
print(pi1_hat)
print(pi2_hat)

#pi1_hat is overestimated in both cases, but it's closer to the true value in the second regression. pi2_hat 
#is underestimated and deviates significantly from the true value in both regressions.

#============================================
#             Exercise 4
#============================================

Z <- rnorm(1000)

X <- rep(0, 1000)

for(t in 2:1000) {
  X[t] <- 0.9 * X[t-1] + Z[t] - 0.9 * Z[t-1]
}

#Plot of simulated ARMA
plot.ts(X, main='ARMA(1,1)', ylab='Xt', xlab='Time')

#Plot of ACF and PACF
Acf(X)
Pacf(X)

# ARMA(1,1) model for the simulated data
arma <- arima(X, order=c(1,0,1))
summary(arma)

# We can the positive correlation in AR(1) with the previous value in the MA(1) has negative coefficient
#   The standard deviation is close to 1 thus it is in line with the theoretical assumption.
#   Relatively large standard errors may introduce some uncertainty into the model.
#   Adequate model fit is suggested by error measures and near-zero autocorrelation of residuals. 

