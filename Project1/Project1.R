library("DBI") 
library("RSQLite") 
con <- dbConnect(RSQLite::SQLite(), 
                 "C:/Users/85259/Documents/project1_raw_data/project1_raw_data.db") # connect to the DB 
dbListTables(con) # list database tables

#check the column and data from the database tables
res_key <- dbGetQuery(con, "SELECT * FROM key")
dbFetch(res_key) 

res_station <- dbGetQuery(con, "SELECT * FROM station")
dbFetch(res_station)

res_train <- dbGetQuery(con, "SELECT * FROM train")
dbFetch(res_train)

res_weather <- dbGetQuery(con, "SELECT * FROM weather")
dbFetch(res_weather) 

#Section A
#Query to find the top three products with the highest total sales based on unit sold
res1 <- dbGetQuery(con, "SELECT item_nbr AS product_no, SUM(units) AS total_units_sold 
                   FROM train 
                   GROUP BY product_no 
                   ORDER BY total_units_sold DESC 
                   LIMIT 3")
dbFetch(res1)

#Join the sales data (train) with the correct weather station using key table
#Inner Join for train and key table so only valid store_nbr can map on key table
#Left Join to keep sales data even if data from weather table is absent
res2 <- dbGetQuery(con, "SELECT *
                   FROM train AS t
                   JOIN key AS k 
                   ON t.store_nbr = k.store_nbr
                   LEFT JOIN weather AS w 
                   ON w.station_nbr = k.station_nbr AND w.date = t.date")
dbFetch(res2)

#Daily sales and average temperature (tavg) for one of the top 3 products
#return top 1 product by order by desc and LIMIT 1 (Save the temp table as top1)
#Inner join top1 and daily sales
#Left join weather table after getting top1 product daily sales
res3 <- dbGetQuery(con, "WITH top1 AS (SELECT t.item_nbr 
  FROM train AS t
  GROUP BY t.item_nbr
  ORDER BY SUM(t.units) DESC
  LIMIT 1
),
daily_sales AS (
  SELECT t.date, t.store_nbr, t.item_nbr, SUM(t.units) AS daily_units
  FROM train AS t
  JOIN top1
    ON top1.item_nbr = t.item_nbr
  GROUP BY t.date, t.store_nbr, t.item_nbr
)
SELECT d.date, d.item_nbr, SUM(d.daily_units) AS total_daily_units, w.tavg AS avg_tavg
FROM daily_sales AS d
JOIN key AS k
  ON k.store_nbr = d.store_nbr
LEFT JOIN weather AS w
  ON w.station_nbr = k.station_nbr
 AND w.date        = d.date
GROUP BY d.date, d.item_nbr
ORDER BY d.date")
dbFetch(res3)

dbDisconnect(con) # close the connection

#Section B
#Cleaning the data for res2 (joined dataset)
library(dplyr)
library(stringr)
library(lubridate)
library(hms)
#Change NA and to mean
#Checked data type is numeric. Therefore no data like "T" is in the column
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
res2$snowfall[is.na(res2$snowfall)] <- 0
res2$heat[is.na(res2$heat)] <- 0


#Sunrise and sunset column the data type should change to time
res2$sunrise <-as_hms(res2$sunrise)
class(res2$sunrise)

res2$sunset <-as_hms(res2$sunset)
class(res2$sunset)

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
#See if include TS,SN,FZ,+, or GR
res2$is_bad_weather <- ifelse(
  grepl("TS|SN|FZ|\\+|GR|RA|FG", res2$codesum, ignore.case = TRUE), 
  1, 0
)

#Add column res2$is_weekend to improve the model - there might be more sales on weekend
#6 is Sat and 0 is Sun
res2$is_weekend <- ifelse(
  as.POSIXlt(res2$date)$wday %in% c(0, 6), 
  1, 0
)

#Drop other NA


colnames(res2) <- make.unique(colnames(res2))


#Linear regression to predict units sold is affected by weather-related features (item_nbr=3)
res2_prod3 <- subset(res2, item_nbr == 3)

res2_prod3$units <- as.numeric(as.character(res2_prod3$units))

lm_model <- lm(units ~ tavg + preciptotal + heat + snowfall + avgspeed + is_bad_weather, 
               data = res2_prod3)
summary(lm_model)

# tavg (Estimate = 0.0001526):
#   +1 degree increase in average temperature -> +0.0001526 units sold (expected), all else equal.
#   but the P value is > 0.05 (not statistically significant)

# preciptotal (Estimate = 0.0428435):
#   +1 unit increase in total precipitation -> +0.0428435 units sold (expected), all else equal.

# heat (Estimate = -0.0003822):
#   +1 unit increase in "heat" -> -0.0003822 units sold (expected), all else equal.
#   but the P value is > 0.05 (not statistically significant)

# snowfall (Estimate = -0.0020805):
#   +1 unit increase in snowfall -> -0.0020805 units sold (expected), all else equal.
#   but the P value is > 0.05 (not statistically significant)

# avgspeed (Estimate = -0.0011564):
#   +1 unit increase in average wind speed -> -0.0011564 units sold (expected), all else equal.

# is_bad_weather (Estimate = -0.0052879):
#   If is_bad_weather is a 0/1 indicator: switching 0 -> 1 -> -0.04485 units sold (expected),
#   holding the continuous weather variables constant.
#   but the P value is > 0.05 (not statistically significant)

#Decision Tree
library(rpart)
library(rpart.plot)

tree_model <- rpart(
  units ~ tavg + preciptotal + heat + snowfall + avgspeed + is_bad_weather,
  data = res2_prod3,method = "class")
rpart.plot(tree_model, box.palette = "blue")
print(tree_model)
tree_model$cptable
#The outcome shows the model is a stump(no splits)


library(Metrics)

pred_lm <- predict(lm_model, res2_prod3)
pred_tree <- predict(tree_model, res2_prod3)

# Metrics Table to compare
metrics_table <- data.frame(
  Model = c("Linear Regression", "Decision Tree"),
  R2 = c(
    summary(lm_model)$r.squared, 
    1 - sum((res2_prod3$units - pred_tree)^2) / sum((res2_prod3$units - mean(res2_prod3$units))^2)
  ),
  RMSE = c(
    rmse(res2_prod3$units, pred_lm), 
    rmse(res2_prod3$units, pred_tree)
  ),
  MAE = c(
    mae(res2_prod3$units, pred_lm), 
    mae(res2_prod3$units, pred_tree)
  )
)

print(metrics_table)

# In terms of performance, Linear Regression performs better than Decision Tree
# Reason: R2, RMSE, and MAE for Linear Regression has better result than Decision Tree

# In terms of interpretability, Linear Regression performs better than Decision Tree
# Reason: Decision Tree's result is a stump which provide less useful information
# Also,Linear Regression can clearly shows how different factors affect the unit

# To sum up, Linear Regression performs better


# Section C
# Use item_nbr 15 and 28 with linear regression model

# item_nbr =15
res2_prod15 <- subset(res2, item_nbr == 15)

res2_prod15$units <- as.numeric(as.character(res2_prod15$units))

lm_model1 <- lm(units ~ tavg + preciptotal + heat + snowfall + avgspeed + is_bad_weather, 
               data = res2_prod15)
summary(lm_model1)

#                Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    -0.648504   0.099910  -6.491 8.63e-11 ***
# tavg            0.008647   0.001330   6.501 8.07e-11 ***
# preciptotal    -0.014071   0.036155  -0.389 0.697140    
# heat            0.032996   0.001851  17.829  < 2e-16 ***
# snowfall        0.178736   0.039640   4.509 6.53e-06 ***
# avgspeed       -0.009045   0.002336  -3.872 0.000108 ***
# is_bad_weather  0.044631   0.020833   2.142 0.032173 *  

# item_nbr = 28
res2_prod28 <- subset(res2, item_nbr == 28)

res2_prod28$units <- as.numeric(as.character(res2_prod28$units))

lm_model2 <- lm(units ~ tavg + preciptotal + heat + snowfall + avgspeed + is_bad_weather, 
                data = res2_prod28)
summary(lm_model2)

#                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     0.4212670  0.0505877   8.327  < 2e-16 ***
# tavg           -0.0034819  0.0006735  -5.170 2.35e-07 ***
# preciptotal    -0.0065964  0.0183064  -0.360   0.7186    
# heat           -0.0003382  0.0009371  -0.361   0.7182    
# snowfall       -0.0405361  0.0200709  -2.020   0.0434 *  
# avgspeed       -0.0126436  0.0011829 -10.689  < 2e-16 ***
# is_bad_weather  0.0455830  0.0105482   4.321 1.55e-05 *** 


# Scatter plot (Temperature VS Sales)
library(ggplot2)
ggplot(res2_prod3, aes(x = tavg, y = units)) +
  geom_jitter(alpha = 0.3, color = "steelblue") + 
  geom_smooth(method = "lm", color = "red") +    
  labs(title = "Correlation between Temperature and Sales",
       x = "Average Temperature (tavg)",
       y = "Quantity Sold (units)") +
  theme_minimal()


# Weather Events Impact
res2_prod3$is_bad_weather <- as.factor(res2_prod3$is_bad_weather)

ggplot(res2_prod3, aes(x = is_bad_weather, y = units, fill = is_bad_weather)) +
  geom_boxplot() +
  scale_fill_manual(values = c("skyblue", "tomato"), labels = c("Good Weather", "Bad Weather")) +
  labs(title = "How Weather Impacts Sales",
       x = "Is Bad Weather (0=No, 1=Yes)",
       y = "Quantity Sold (units)",
       fill = "Weather Category") +
  theme_light()

# most of the units are zero, so below will see the result after filtering zero

#Weather Events Impact (Filter unit =0)
filtered_sales <- res2_prod3 %>% filter(units > 0)

ggplot(filtered_sales, aes(x = is_bad_weather, y = units, fill = is_bad_weather)) +
  geom_boxplot() +
  scale_fill_manual(values = c("skyblue", "tomato"), labels = c("Good Weather", "Bad Weather")) +
  labs(title = "How Weather Impacts Sales",
       x = "Is Bad Weather (0=No, 1=Yes)",
       y = "Quantity Sold (units)",
       fill = "Weather Category") +
  theme_light()

# Sales Over Time
quarterly_sales <- res2_prod3 %>%
  mutate(quarter = paste0(year(date), "-Q", quarter(date))) %>% 
  group_by(quarter) %>%
  summarise(total_units = sum(units, na.rm = TRUE))

ggplot(quarterly_sales, aes(x = quarter, y = total_units, fill = quarter)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = total_units), vjust = -0.5) + # unit will be shown above the bar
  labs(title = "Quantity Sold on Each Quarter",
       x = "Quarter",
       y = "Total Units",
       fill = "Quarter") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Avoid the text to overlap
