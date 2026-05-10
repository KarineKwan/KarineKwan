#Set seed to confirm the result can reproduce
set.seed(123) 
library("DBI") 
library("RSQLite") 

#make sure project1_raw_data.db file are in the same folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
con <- dbConnect(RSQLite::SQLite(), "project1_raw_data.db") # connect to the DB 
dbListTables(con) # list database tables

#check the column and data from the database tables
res_key <- dbGetQuery(con, "SELECT * FROM key")

res_station <- dbGetQuery(con, "SELECT * FROM station")

res_train <- dbGetQuery(con, "SELECT * FROM train")

res_weather <- dbGetQuery(con, "SELECT * FROM weather")

#Section A
#Query to find the top three products with the highest total sales based on unit sold
#Group by product_no to get the total_units_sold for each product
#Desc order and limit 3 to get the top three products
res1 <- dbGetQuery(con, "SELECT item_nbr AS product_no, SUM(units) AS total_units_sold 
                   FROM train 
                   GROUP BY product_no 
                   ORDER BY total_units_sold DESC 
                   LIMIT 3")
#The top three products are 45, 9, and 5

#Join the sales data (train) with the correct weather station using key table
#Inner Join for train and key table so only valid store_nbr can map on key table
#Left Join for weather table to keep sales data even if data from weather table is absent
res2 <- dbGetQuery(con, "SELECT *
                   FROM train AS t
                   INNER JOIN key AS k 
                   ON t.store_nbr = k.store_nbr
                   LEFT JOIN weather AS w 
                   ON w.station_nbr = k.station_nbr AND w.date = t.date")

#Daily sales and average temperature (tavg) for one of the top 3 products
#return top 1 product by order by desc and LIMIT 1 (Save the temp table as top1)
#collapse all stations mapping to a store into a single tavg per store/date by MAX
#Reason of using MAX instead of AVG: MAX is better to handle missing data and outliers
#Inner join top1 and daily sales
#Left join store_weather table after getting top1 product daily sales
#Reason: daily_sales are kept even when there is no matching weather row for the same station and date
#Not dropping the sales rows to affect the result
res3 <- dbGetQuery(con, "WITH top1 AS (SELECT t.item_nbr 
  FROM train AS t
  GROUP BY t.item_nbr
  ORDER BY SUM(t.units) DESC
  LIMIT 1
),
daily_sales AS (
  SELECT t.date, t.store_nbr, t.item_nbr, SUM(t.units) AS daily_units
  FROM train AS t
  INNER JOIN top1
    ON top1.item_nbr = t.item_nbr
  GROUP BY t.date, t.store_nbr, t.item_nbr
),
store_weather AS (
  SELECT k.store_nbr, w.date, MAX(w.tavg) AS tavg 
  FROM key AS k
  INNER JOIN weather AS w
    ON w.station_nbr = k.station_nbr
  GROUP BY k.store_nbr, w.date
)
SELECT d.date, d.item_nbr, SUM(d.daily_units) AS total_daily_units, sw.tavg AS avg_tavg
FROM daily_sales AS d
LEFT JOIN store_weather AS sw
  ON sw.store_nbr = d.store_nbr
  AND sw.date = d.date
GROUP BY d.date, d.item_nbr, sw.tavg
ORDER BY d.date")

dbDisconnect(con) # close the connection

#Section B
#Cleaning the data for res2 (joined dataset)
library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(hms)

#Deal with special character like "T" in preciptotal and snowfall
#Change "T" to 0
#Reason: "T" in meteorology means "Trace", which means the amount is so small that cannot count
#That's why instead of replacing it with mean, we replace these two variables to 0
res2$preciptotal <- as.numeric(str_replace(res2$preciptotal, "T", "0"))
res2$snowfall <- as.numeric(str_replace(res2$snowfall, "T", "0"))

#Change NA and to mean
#Checking the distint values in column tavg and preciptotal
unique_values <- unique(res2$tavg)
print(unique_values)

unique_values <- unique(res2$preciptotal)
print(unique_values)

#Replace NA with mean
mean_tavg <- mean(res2$tavg, na.rm = TRUE)
mean_tavg

res2$tavg[is.na(res2$tavg)] <- mean_tavg

mean_preciptotal <- mean(res2$preciptotal, na.rm = TRUE)
mean_preciptotal

res2$preciptotal[is.na(res2$preciptotal)] <- mean_preciptotal

res2$avgspeed[is.na(res2$avgspeed)] <- mean(res2$avgspeed, na.rm = TRUE)

#Replace NA with 0
#Reason: unlike other weather variables, 
res2$snowfall[is.na(res2$snowfall)] <- 0
res2$heat[is.na(res2$heat)] <- 0

#Use for loop to clear NA in other columns
clear_NA <- c("tmax", "tmin", "dewpoint", "wetbulb", "stnpressure", "sealevel", "resultdir")
for(x in clear_NA) {
  res2[[x]][is.na(res2[[x]])] <- mean(res2[[x]], na.rm = TRUE)
}



#Sunrise and sunset column the data type should change to time
res2$sunrise <-as_hms(res2$sunrise)
class(res2$sunrise)

res2$sunset <-as_hms(res2$sunset)
class(res2$sunset)

#Drop NA to avoid unexpected results when calculating daylight hours
res2 <- res2 %>% drop_na(sunrise, sunset)

#Change "date" format from chr to date
res2$date <-ymd(res2$date)
class(res2$date)

#Remove outliers in column unit using log transform (since the data is zero-inflated)
ROBUST_Z_CUT <- 3

#Compute robust center/scale on positive units only
med_log <- median(log1p(res2$unit[res2$unit > 0]), na.rm = TRUE)
#1.4826 is the scaling constant that makes the MAD comparable to the standard deviation when the data are normally distributed
mad_log <- mad(log1p(res2$unit[res2$unit > 0]), constant = 1.4826, na.rm = TRUE)

res2$is_outlier_log <- ifelse(
  !is.na(res2$unit) & res2$unit > 0 & mad_log > 0,
  abs((log1p(res2$unit) - med_log) / mad_log) > ROBUST_Z_CUT,
  FALSE
)
#Drop the outliers (res2$is_outlier_log == TRUE)
res2 <- res2[which(res2$is_outlier_log == FALSE), ]

#Remove column res2$is_outlier_log after using it
res2 <- res2[, names(res2) != "is_outlier_log"]

#Add column is_bad_weather to improve the model - bad weather might lower the sales
#See if include TS,SN,FZ,+,GR,RA, Or FG
#"+" has \\ in front is because "+" is a special character and now we need to treat it as normal character
res2$is_bad_weather <- ifelse(
  grepl("TS|SN|FZ|\\+|GR|RA|FG", res2$codesum, ignore.case = TRUE), 
  1, 0
)

#Add column res2$daylight_hours to improve the model - the daylight hours might affect sales
res2$daylight_hours <- as.numeric(difftime(res2$sunset, res2$sunrise, units = "hours"))
#Handle case of NA
res2$daylight_hours[is.na(res2$daylight_hours)] <- mean(res2$daylight_hours, na.rm = TRUE)

#Drop other NA
colnames(res2) <- make.unique(colnames(res2))


#Linear regression to predict units sold is affected by weather-related features (item_nbr=3)
res2_prod45 <- subset(res2, item_nbr == 45)

res2_prod45$units <- as.numeric(as.character(res2_prod45$units))

#Train-test-validate split (Train 60%, test 20%, val 20%)
n <- nrow(res2_prod45)
sample_size_train <- floor(0.60 * n)
sample_size_val   <- floor(0.20 * n)

#Random shuffle indices
indices <- sample(seq_len(n))

train_idx <- indices[1:sample_size_train]
val_idx   <- indices[(sample_size_train + 1):(sample_size_train + sample_size_val)]
test_idx  <- indices[(sample_size_train + sample_size_val + 1):n]

#Create the datasets
train_data <- res2_prod45[train_idx, ]
val_data   <- res2_prod45[val_idx, ]
test_data  <- res2_prod45[test_idx, ]

#Fit the model on the train set
lm_model <- lm(units ~ tavg + preciptotal + heat + snowfall + avgspeed + is_bad_weather + daylight_hours, 
               data = train_data)

#Evaluate on the Validation Set (Used for parameter tuning or model comparison)
val_preds <- predict(lm_model, newdata = val_data)
val_rmse  <- sqrt(mean((val_data$units - val_preds)^2))

#Final evaluation on the Test Set (Represents real-world performance on unseen data)
test_preds <- predict(lm_model, newdata = test_data)
test_rmse  <- sqrt(mean((test_data$units - test_preds)^2))

#Display model summary and performance metrics
summary(lm_model)

#R-squared is around 7.3%, which means weather variables explain 7.3% of the variation in sales for Product 45
#F-statistic p-value is < 2.2e-16, which means the overall model is statistically significant

# tavg (Estimate = 0.16207):
#   A 1-degree increase in average temperature is associated with a +0.16 unit change in sales
#   Highly significant (p < 0.001)

# preciptotal (Estimate = -1.44760):
#   A 1-unit increase in precipitation leads to a decrease of ~1.45 units in sales
#   Not statistically significant (p = 0.15381 (> 0.05))

# heat (Estimate = 0.42584):
#   A 1-unit increase in heating degree days leads to a +0.43 increase in units sold
#   Highly significant (p < 0.001)

# snowfall (Estimate = -2.69145):
#   A 1-unit increase in snowfall leads to a decrease of ~2.69 units in sales
#   Significant (p = 0.00142)

# avgspeed (Estimate = 1.85057):
#   A 1-unit increase in wind speed is associated with a +1.85 increase in units sold
#   Highly significant (p < 0.001)

# is_bad_weather (Estimate = -3.63960):
#   Presence of bad weather (1 vs 0) decreases expected sales by ~3.64 units
#   Highly significant (p < 0.001)

# daylight_hours (Estimate = 0.10028):
#   Each additional hour of daylight increases expected sales by 0.10 units
#   Not statistically significant (p = 0.664370 (> 0.05))

cat("Validation RMSE:", val_rmse, "\n")
cat("Test RMSE:", test_rmse, "\n")

#Validation RMSE: 28.52281 
#Test RMSE: 29.4754  
#similar RMSE means the model is stable



#Decision Tree
library(rpart)
library(rpart.plot)

tree_model <- rpart(
  units ~ tavg + preciptotal + heat + snowfall + avgspeed + is_bad_weather + daylight_hours,
  data = test_data,method = "anova")
rpart.plot(tree_model, box.palette = "green")
print(tree_model)
tree_model$cptable

#Split 1: avgspeed < 10.15
# If wind speed is low (< 10.15), sales are ~9.74 units

#Split 2: avgspeed >= 10.15
# If it's moderate Wind (10.15 to 14.85), sales are ~19.63 units
# If it's strong Wind (>= 14.85), sales are ~35.27 units

#Model Performance (CP Table):
# Relative Error: 0.939 (The tree captures about 6.1% of the data's variance)
# X-error: 0.963 (Cross-validation error is stable, indicating no overfitting)
# The tree only used avgspeed, suggesting it is the most dominant 

library(Metrics)

pred_lm <- predict(lm_model, test_data)
pred_tree <- predict(tree_model, test_data)

#Metrics Table to compare
metrics_table <- data.frame(
  Model = c("Linear Regression", "Decision Tree"),
  R2 = c(
    summary(lm_model)$r.squared, 
    1 - sum((test_data$units - pred_tree)^2) / sum((test_data$units - mean(test_data$units))^2)
  ),
  RMSE = c(
    rmse(test_data$units, pred_lm), 
    rmse(test_data$units, pred_tree)
  ),
  MAE = c(
    mae(test_data$units, pred_lm), 
    mae(test_data$units, pred_tree)
  )
)

print(metrics_table)

#In terms of performance, Linear Regression model outperforms the Decision Tree
#Reason: more explanatory power (R² of 0.073 vs 0.061) and lower RMSE, MAE

#In terms of interpretability, Decision Tree performs better than Linear Regression
#Reason: Decision Tree offers more actionable insights


#Section C
#Use item_nbr 9 and 5 with linear regression model

#item_nbr = 9
res2_prod9 <- subset(res2, item_nbr == 9)

res2_prod9$units <- as.numeric(as.character(res2_prod9$units))

#Train-test-validate split (Train 60%, test 20%, val 20%)
n <- nrow(res2_prod9)
sample_size_train <- floor(0.60 * n)
sample_size_val   <- floor(0.20 * n)

#Random shuffle indices
indices <- sample(seq_len(n))

train_idx <- indices[1:sample_size_train]
val_idx   <- indices[(sample_size_train + 1):(sample_size_train + sample_size_val)]
test_idx  <- indices[(sample_size_train + sample_size_val + 1):n]

#Create the datasets
train_data <- res2_prod9[train_idx, ]
val_data   <- res2_prod9[val_idx, ]
test_data  <- res2_prod9[test_idx, ]

#Fit the model on the train set
lm_model1 <- lm(units ~ tavg + preciptotal + heat + snowfall + avgspeed + is_bad_weather + daylight_hours, 
               data = train_data)

#Evaluate on the Validation Set (Used for parameter tuning or model comparison)
val_preds <- predict(lm_model1, newdata = val_data)
val_rmse  <- sqrt(mean((val_data$units - val_preds)^2))

#Final evaluation on the Test Set (Represents real-world performance on unseen data)
test_preds <- predict(lm_model1, newdata = test_data)
test_rmse  <- sqrt(mean((test_data$units - test_preds)^2))

#Display model summary and performance metrics
summary(lm_model1)

cat("Validation RMSE:", val_rmse, "\n")
cat("Test RMSE:", test_rmse, "\n")

#Validation RMSE: 36.99331
#Test RMSE: 37.26519
#similar RMSE means the model is stable

#                Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     6.60177    4.34544   1.519 0.128733    
#tavg            0.56613    0.05257  10.770  < 2e-16 ***
#preciptotal     0.22311    1.28820   0.173 0.862504    
#heat            0.45465    0.07510   6.054 1.46e-09 ***
#snowfall       -3.01408    0.88489  -3.406 0.000661 ***
#avgspeed        1.21841    0.09152  13.313  < 2e-16 ***
#is_bad_weather  0.83703    0.83358   1.004 0.315333    
#daylight_hours -3.00088    0.29434 -10.195  < 2e-16 ***


#Decision Tree (item_nbr = 9)
tree_model <- rpart(
  units ~ tavg + preciptotal + heat + snowfall + avgspeed + is_bad_weather + daylight_hours,
  data = test_data,method = "anova")
rpart.plot(tree_model, box.palette = "green")
print(tree_model)
tree_model$cptable

#1) root 3491 5017153.000 20.3377300  
# 2) daylight_hours>=14.625 157    3116.025  0.3566879 *
# 3) daylight_hours< 14.625 3334 4948404.000 21.2786400  
#  6) avgspeed< 4.55 453  434007.200 10.6335500 *
#  7) avgspeed>=4.55 2881 4454992.000 22.9524500  
#   14) daylight_hours< 9.691667 85       0.000  0.0000000 *
#   15) daylight_hours>=9.691667 2796 4408852.000 23.6502100  
#    30) daylight_hours>=9.941667 2675 3829974.000 22.7091600 *
#    31) daylight_hours< 9.941667 121  524138.000 44.4545500 *


#item_nbr = 5
res2_prod5 <- subset(res2, item_nbr == 5)

res2_prod5$units <- as.numeric(as.character(res2_prod5$units))

#Train-test-validate split (Train 60%, test 20%, val 20%)
n <- nrow(res2_prod5)
sample_size_train <- floor(0.60 * n)
sample_size_val   <- floor(0.20 * n)

#Random shuffle indices
indices <- sample(seq_len(n))

train_idx <- indices[1:sample_size_train]
val_idx   <- indices[(sample_size_train + 1):(sample_size_train + sample_size_val)]
test_idx  <- indices[(sample_size_train + sample_size_val + 1):n]

#Create the datasets
train_data <- res2_prod5[train_idx, ]
val_data   <- res2_prod5[val_idx, ]
test_data  <- res2_prod5[test_idx, ]

#Fit the model on the train set
lm_model2 <- lm(units ~ tavg + preciptotal + heat + snowfall + avgspeed + is_bad_weather + daylight_hours, 
                data = train_data)

#Evaluate on the Validation Set (Used for parameter tuning or model comparison)
val_preds <- predict(lm_model2, newdata = val_data)
val_rmse  <- sqrt(mean((val_data$units - val_preds)^2))

#Final evaluation on the Test Set (Represents real-world performance on unseen data)
test_preds <- predict(lm_model2, newdata = test_data)
test_rmse  <- sqrt(mean((test_data$units - test_preds)^2))

#Display model summary and performance metrics
summary(lm_model2)

cat("Validation RMSE:", val_rmse, "\n")
cat("Test RMSE:", test_rmse, "\n")

#Validation RMSE: 31.3886 
#Test RMSE: 30.54011 
#similar RMSE means the model is stable

#                Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    -0.61384    3.72253  -0.165   0.8690    
#tavg            0.54078    0.04501  12.014  < 2e-16 ***
#preciptotal    -0.87818    1.14267  -0.769   0.4422    
#heat            0.54394    0.06417   8.477  < 2e-16 ***
#snowfall       -1.66970    1.00260  -1.665   0.0959 .  
#avgspeed        0.16081    0.07739   2.078   0.0377 *  
#is_bad_weather -1.54843    0.71448  -2.167   0.0302 *  
#daylight_hours -1.81410    0.25149  -7.214 5.83e-13 ***



#Decision Tree (item_nbr = 5)
tree_model <- rpart(
  units ~ tavg + preciptotal + heat + snowfall + avgspeed + is_bad_weather + daylight_hours,
  data = test_data,method = "anova")
rpart.plot(tree_model, box.palette = "green")
print(tree_model)
tree_model$cptable

#1) root 3491 3325373 15.81810  
# 2) daylight_hours>=14.64167 151       0  0.00000 *
# 3) daylight_hours< 14.64167 3340 3285883 16.53323 *


#Scatter plot (Temperature VS Sales)
library(ggplot2)
ggplot(res2_prod45, aes(x = tavg, y = units)) +
  geom_jitter(alpha = 0.3, color = "steelblue") + 
  geom_smooth(method = "lm", color = "red") +    
  labs(title = "Correlation between Temperature and Sales (Item 45)",
       x = "Average Temperature (tavg)",
       y = "Quantity Sold (units)") +
  theme_minimal()

#item_nbr = 9
ggplot(res2_prod9, aes(x = tavg, y = units)) +
  geom_jitter(alpha = 0.3, color = "steelblue") + 
  geom_smooth(method = "lm", color = "red") +    
  labs(title = "Correlation between Temperature and Sales (Item 9)",
       x = "Average Temperature (tavg)",
       y = "Quantity Sold (units)") +
  theme_minimal()

#item_nbr = 5
ggplot(res2_prod5, aes(x = tavg, y = units)) +
  geom_jitter(alpha = 0.3, color = "steelblue") + 
  geom_smooth(method = "lm", color = "red") +    
  labs(title = "Correlation between Temperature and Sales (Item 5)",
       x = "Average Temperature (tavg)",
       y = "Quantity Sold (units)") +
  theme_minimal()


#Weather Events Impact
res2_prod45$is_bad_weather <- as.factor(res2_prod45$is_bad_weather)

ggplot(res2_prod45, aes(x = is_bad_weather, y = units, fill = is_bad_weather)) +
  geom_boxplot() +
  scale_fill_manual(values = c("skyblue", "tomato"), labels = c("Good Weather", "Bad Weather")) +
  labs(title = "How Weather Impacts Sales (Item 45)",
       x = "Is Bad Weather (0=No, 1=Yes)",
       y = "Quantity Sold (units)",
       fill = "Weather Category") +
  theme_light()

#item_nbr = 9
res2_prod9$is_bad_weather <- as.factor(res2_prod9$is_bad_weather)

ggplot(res2_prod9, aes(x = is_bad_weather, y = units, fill = is_bad_weather)) +
  geom_boxplot() +
  scale_fill_manual(values = c("skyblue", "tomato"), labels = c("Good Weather", "Bad Weather")) +
  labs(title = "How Weather Impacts Sales (Item 9)",
       x = "Is Bad Weather (0=No, 1=Yes)",
       y = "Quantity Sold (units)",
       fill = "Weather Category") +
  theme_light()

#item_nbr = 5
res2_prod5$is_bad_weather <- as.factor(res2_prod5$is_bad_weather)

ggplot(res2_prod5, aes(x = is_bad_weather, y = units, fill = is_bad_weather)) +
  geom_boxplot() +
  scale_fill_manual(values = c("skyblue", "tomato"), labels = c("Good Weather", "Bad Weather")) +
  labs(title = "How Weather Impacts Sales (Item 5)",
       x = "Is Bad Weather (0=No, 1=Yes)",
       y = "Quantity Sold (units)",
       fill = "Weather Category") +
  theme_light()

#Sales Over Time
quarterly_sales <- res2_prod45 %>%
  mutate(quarter = paste0(year(date), "-Q", quarter(date))) %>% 
  group_by(quarter) %>%
  summarise(total_units = sum(units, na.rm = TRUE))

ggplot(quarterly_sales, aes(x = quarter, y = total_units, fill = quarter)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = total_units), vjust = -0.5) + # unit will be shown above the bar
  labs(title = "Quantity Sold on Each Quarter (Item 45)",
       x = "Quarter",
       y = "Total Units",
       fill = "Quarter") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Avoid the text to overlap

#item_nbr = 9
quarterly_sales <- res2_prod9 %>%
  mutate(quarter = paste0(year(date), "-Q", quarter(date))) %>% 
  group_by(quarter) %>%
  summarise(total_units = sum(units, na.rm = TRUE))

ggplot(quarterly_sales, aes(x = quarter, y = total_units, fill = quarter)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = total_units), vjust = -0.5) + # unit will be shown above the bar
  labs(title = "Quantity Sold on Each Quarter (Item 9)",
       x = "Quarter",
       y = "Total Units",
       fill = "Quarter") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Avoid the text to overlap

#item_nbr = 5
quarterly_sales <- res2_prod5 %>%
  mutate(quarter = paste0(year(date), "-Q", quarter(date))) %>% 
  group_by(quarter) %>%
  summarise(total_units = sum(units, na.rm = TRUE))

ggplot(quarterly_sales, aes(x = quarter, y = total_units, fill = quarter)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = total_units), vjust = -0.5) + # unit will be shown above the bar
  labs(title = "Quantity Sold on Each Quarter (Item 5)",
       x = "Quarter",
       y = "Total Units",
       fill = "Quarter") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Avoid the text to overlap
