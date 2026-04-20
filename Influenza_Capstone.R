#### Import Libraries ####
library(readxl)
library(tidyverse)
library(lubridate)
library(patchwork)
library(gridExtra)
library(ggplotify)
library(rugarch)
library(knitr)
library(kableExtra)



#### Import Data ####
#https://covid19.ncdhhs.gov/dashboard/data-behind-dashboards
#The outcome variable will be from the North Carolina Department of Health and Human Services (NC DHHS) 
#“Data Behind the Dashboards” portal for: Positive Influenza Tests in PHE Facilities
#Variables: Viral gene copies per person, Population served by treatment plant location, Week Ending Date

positive_tests <- read_excel("PHE Facilities test table.xlsx") 
influenza_a <- read_excel("Viral Gene Copies Persons-FLU A.xlsx")



#### Uniform Dates ####
#The wastewater data dates will be transformed to match the week ending date for their respective week given that the 
#influenza tests are based on the week ending date (Saturday)
positive_tests <- positive_tests %>%
  mutate(`week_ending_date` = as.Date(`Week Ending Date`, format = "%m/%d/%Y"))

influenza_a <- influenza_a %>%
  mutate(date = as.Date(Date, format = "%m/%d/%Y"),
         week_ending_date = floor_date(date, unit = "week", week_start = 7) + days(6))

#### Average Weekly Viral Gene Copies Per Person ####
#Given that the values are viral gene copies per person, this means that each water treatment plant takes into account the 
#population served, therefore, it is weighted.

#Since we do not have county level information for the influenza positive tests, the viral gene copies need to be aggregated 
#by week_ending_date to have one row per week.

#Viral Gene Copies per Person = (viral gene copies per L * wastewater flow) / population
influenza_a_aggregated <- influenza_a %>%
  group_by(week_ending_date) %>%
  summarize(avg_viral_load_A = mean(`Viral Gene Copies Per Person`, na.rm = TRUE))

rm(influenza_a)



#### Merge Data ####
merged <- positive_tests %>%
  full_join(influenza_a_aggregated, by = "week_ending_date") %>%
  select(-`# COVID-19 Positive`)

#Keep only complete rows
merged_complete <- merged[complete.cases(merged), ]

rm(positive_tests)
rm(influenza_a_aggregated)
rm(merged) #Reduced from 103 observations to 52 observations



#### Rate Conversion For Influenza Positive Tests  & Log Conversion ####
#The number of positive tests are for the years 2024 and 2025, these values are weighted by the total population for their 
#respective year.
#Multiplied by 100,000
merged_complete <- merged_complete %>%
  mutate(`Week Ending Date` = as.Date(`Week Ending Date`, format = "%m/%d/%Y"))

merged_complete <- merged_complete %>%
  mutate(total_population = case_when(
    year(`Week Ending Date`) == 2024 ~ 11046024,#https://www.osbm.nc.gov/blog/2024/12/20/north-carolina-now-home-over-11-million-people
    year(`Week Ending Date`) == 2025 ~ 11107246),#https://www.ncdhhs.gov/2025-insights-aging-nc-1/open
    influenza_rate= (`# Influenza Positive` / total_population) * 100000)

#Log conversion conducted as our data is positive and vary over several orders of magnitude.
merged_complete <- merged_complete %>%
  mutate(log_viral_load = log(avg_viral_load_A),
         log_influenza_rate = log(influenza_rate))



#### Descriptive Analytics ####
summary(merged_complete$influenza_rate)
summary(merged_complete$log_influenza_rate)
summary(merged_complete$avg_viral_load_A)
summary(merged_complete$log_viral_load)
summary(merged_complete$week_ending_date)

#Time Series Plot For Influenza & Wastewater
ts <- ggplot(merged_complete, aes(x = `Week Ending Date`, y = log_influenza_rate)) +
  geom_line(color = "steelblue") +
  labs(title = "Weekly Positive Influenza A Tests from Sept 2024-Sept 2025", x = "Date", y = "Influenza Positive Cases in PHE Facilities") +
  theme_minimal()
print(ts)

ggplot(merged_complete, aes(x = `Week Ending Date`, y = log_viral_load)) +
  geom_line(color = "steelblue") +
  labs(title = "Weekly Average Influenza A Viral Load from Sept 2024-Sept 2025", x = "Date", y = "Average Influenza A Viral") +
  theme_minimal()

#Identification of starting points
merged_complete <- merged_complete %>%
  arrange(`Week Ending Date`) %>%   # <-- use your real column name here
  mutate(week_index = row_number())

index_values <- c(16, 25, 29, 40, 45)

ggplot(merged_complete, aes(x = week_ending_date, y = log_influenza_rate)) +
  geom_line(color = "steelblue") +
  geom_point(data = merged_complete[merged_complete$week_index %in% index_values, ],
             aes(x = week_ending_date, y = log_influenza_rate),
             color = "red", size = 2) +
  labs(title = "Weeks 16, 25, 29, 40, 45",
       x = "Date", y = "Log-Transformed Influenza Positive Cases in PHE Facilities") +
  theme_minimal()

#Plot Both Together
#Reshape the data to long format
wastewater_long <- merged_complete %>%
  pivot_longer(cols = c(log_influenza_rate, log_viral_load),
               names_to = "Variable",
               values_to = "Value")

line_plots <- ggplot(wastewater_long, aes(x = `Week Ending Date`, y = Value, color = Variable)) +
  geom_line() +
  labs(title = "Influenza A Positive Case and Wastewater Viral Load Trends from\nSept 2024 to Sept 2025",
       subtitle = "Note: Positive Cases from North Carolina PHE Facilities",
       x = "Date", y = "Log-Transformed Values", color = "Variable") +
  theme_minimal() #+ 
#theme(legend.position = "none")
line_plots
rm(wastewater_long)



#### Lagged Correlations ####
#AUTOCORRELATIONL The ACF measures how current values of a time series are correlated with its own past values.
#Bars inside bounds → correlation not significant; Bars outside bounds → significant autocorrelation, meaning that the 
#current data values are statistically related to past values (meaning it can be modeled and it is not just random noise).
acf(merged_complete$avg_viral_load_A, lag.max = 6, main = "ACF of Wastewater Viral Load")
acf(merged_complete$influenza_rate, lag.max = 6, main = "ACF of Influenza Rate")

library(forecast)
acf_1 <- ggAcf(merged_complete$avg_viral_load_A, lag.max = 6) +
  ggtitle("ACF of Wastewater Viral Load")

acf_2 <- ggAcf(merged_complete$influenza_rate, lag.max = 6) +
  ggtitle("ACF of Influenza Rate")

# Check to see if merged_complete is a data frame
is.data.frame(merged_complete)

# Correlation
merged_complete_lagged <- merged_complete %>%
  mutate(
    lag_1 = lag(log_viral_load, 1),
    lag_2 = lag(log_viral_load, 2),
    lag_3 = lag(log_viral_load, 3)
  ) %>%
  select(week_ending_date, log_influenza_rate, lag_1, lag_2, lag_3) %>%
  tidyr::drop_na()

overall_corr <- cor(merged_complete$log_influenza_rate, merged_complete$log_viral_load)
overall_corr

corr_1 <- cor(merged_complete_lagged$log_influenza_rate, merged_complete_lagged$lag_1)  
# 1-week lead
corr_2 <- cor(merged_complete_lagged$log_influenza_rate, merged_complete_lagged$lag_2)  
# 2-week lead
corr_3 <- cor(merged_complete_lagged$log_influenza_rate, merged_complete_lagged$lag_3)  
# 3-week lead

# Create a table
cor_table <- data.frame(
  `Lag Week` = 1:3,
  Correlation = c(corr_1, corr_2, corr_3),
  check.names = FALSE
)

cor_table$Correlation <- round(cor_table$Correlation, 2)

# View the table
print(cor_table)


# Merge correlation and combined line plot
table_plot <- as.ggplot(function() gridExtra::grid.table(cor_table, rows = NULL,
                                                         theme = gridExtra::ttheme_default(base_size = 10)))  # Try 6–10

combined_plot <- line_plots | table_plot / acf_1 / acf_2 + plot_layout(widths = c(8, 1)) +
  plot_annotation(tag_levels = "a")
#Instead of / (which stacks vertically), use | to place them side by side.

combined_plot

combined_plot <- (line_plots | table_plot / acf_1 / acf_2) +
  plot_layout(widths = c(4, 1)) +
  plot_annotation(tag_levels = "a", tag_suffix = ")" )




#### GARCH MODEL ####
#A GARCH model is designed for time series data and estimates both mean and variance (votality)
# We are using a GARCH-X model because we are including an exogenous variable (wastewater)
data <- merged_complete %>%
  arrange(week_ending_date) %>%
  mutate(week_index = row_number()) #creates a new column, numbering the rows in a sequence
head(data) 

# run_garchx function
run_garchx <- function(data, 
                       train_end, #the week_index where you want the training data to end
                       lag_weeks, #this is how many weeks you want to lag the wastewater data
                       arma_p = 1, #this means it assumes this week's flu rate is related to last week's flu rate
                       verbose = TRUE) 
  {data <- data %>% arrange(week_ending_date) %>%
    mutate(wastewater_lag = dplyr::lag(log_viral_load, lag_weeks)) 

# Split training data
  train_data <- data %>% filter(week_index <= train_end) #subset the data based on train_end
  train_fit  <- train_data %>% select(log_influenza_rate, wastewater_lag) %>% drop_na() 
  X <- as.matrix(train_fit$wastewater_lag) #convert it into a matrix as ugarch requires it
 
# GARCH-X modeling  
# ugarchspec function defines the specification of a GARCH model before fitting it
  spec <- ugarchspec( 
    variance.model = list(model = "sGARCH", #("GARCH part) use the standard garch model 
                          garchOrder = c(1,1)), #the volatility (variance) equations depends on: 1 lag of the past squared residual (ARCH term),
                                                #and 1 lag of the past volatility  (GARCH term)
    mean.model     = list(armaOrder = c(arma_p,0),  #(AR part, 0 moving-average terms) since arma_p is 1, by last week's flu rate. 
                                                    #It means this week's flu rate is predicted by last week's flu rate. 
                          external.regressors = X), #("X" part) tells the model to also use the wastewater_lag matrix as a second predictor
    distribution.model = "norm" #assumes the residuals are normally distributed
  )
  
# Fit the model
  fit <- ugarchfit(spec, 
                   data = train_fit$log_influenza_rate, 
                   solver = "hybrid") #combo of optimization methods to finds the best parameters 
  
# Sanity check
  if (train_end < lag_weeks) stop("train_end must be >= lag_weeks")
  exog_path <- matrix(tail(train_data$log_viral_load, lag_weeks), ncol = 1)
 
# Making the forecast   
  fc <- ugarchforecast(fit,
                       n.ahead = lag_weeks, #forecast lag_weeks into the future
                       external.forecasts = list(mregfor = exog_path)) #provides the known predictor values that the model needs to make the forecast
  
  pred_log <- as.numeric(fitted(fc))[lag_weeks] # Final forecast (at week train_end + lag_weeks) on the log scale
  se_log   <- as.numeric(sigma(fc))[lag_weeks] # Standard error
  #Forecasting (log scale → back to original)
  pred  <- exp(pred_log + 0.5 * se_log^2) 
  lower <- exp(pred_log - 1.96 * se_log)
  upper <- exp(pred_log + 1.96 * se_log)
 
# The final output 
  pw <- train_end + lag_weeks #calculate the exact week_index of the week being predicted
  aw <- train_end
  out <- data.frame(
    Train_End_Week = train_end,
    Actual_week = data$week_ending_date[aw],
    Pred_Week = pw,
    Pred_Date = data$week_ending_date[pw],
    Lag_Weeks = lag_weeks,
    Wastewater = round(if ("avg_viral_load_A" %in% names(data)) #actual flu rate
      data$avg_viral_load_A[pw]
      else exp(data$avg_viral_load_A[pw]), 3),
    Actual   = round(if ("influenza_rate" %in% names(data)) #actual flu rate
      data$influenza_rate[pw]
      else exp(data$log_influenza_rate[pw]), 3),
    Predicted = round(pred,3),
    Lower_95 = round(lower,3),
    Upper_95 = round(upper,3)
  )
  if (verbose) print(out)
  out
}



#### FORECAST ####
#index_values <- c(13, 14, 19, 23, 27, 32)

train_end <- 16
x16_17 <- run_garchx(data, train_end, lag_weeks = 1)
x16_18 <- run_garchx(data, train_end, lag_weeks = 2)
x16_19 <- run_garchx(data, train_end, lag_weeks = 3)
x16_20 <- run_garchx(data, train_end, lag_weeks = 4)

train_end <- 25
a25_26 <- run_garchx(data, train_end, lag_weeks = 1)
a25_27 <- run_garchx(data, train_end, lag_weeks = 2)
a25_28 <- run_garchx(data, train_end, lag_weeks = 3)
a25_29 <- run_garchx(data, train_end, lag_weeks = 4)

#train_end <- 26
#b26_27 <- run_garchx(data, train_end, lag_weeks = 1)
#b26_28 <- run_garchx(data, train_end, lag_weeks = 2)
#b26_29 <- run_garchx(data, train_end, lag_weeks = 3)
#b26_30 <- run_garchx(data, train_end, lag_weeks = 4)

train_end <- 29
c29_30 <- run_garchx(data, train_end, lag_weeks = 1)
c29_31 <- run_garchx(data, train_end, lag_weeks = 2)
c29_32 <- run_garchx(data, train_end, lag_weeks = 3)
c29_33 <- run_garchx(data, train_end, lag_weeks = 4)

train_end <- 40
d40_41 <- run_garchx(data, train_end, lag_weeks = 1)
d40_42 <- run_garchx(data, train_end, lag_weeks = 2)
d40_43 <- run_garchx(data, train_end, lag_weeks = 3)
d40_44 <- run_garchx(data, train_end, lag_weeks = 4)

train_end <- 45
e45_46 <- run_garchx(data, train_end, lag_weeks = 1)
e45_47 <- run_garchx(data, train_end, lag_weeks = 2)
e45_48 <- run_garchx(data, train_end, lag_weeks = 3)
e45_49 <- run_garchx(data, train_end, lag_weeks = 4)

#train_end <- 45
#f45_46 <- run_garchx(data, train_end, lag_weeks = 1)
#f45_47 <- run_garchx(data, train_end, lag_weeks = 2)
#f45_48 <- run_garchx(data, train_end, lag_weeks = 3)
#f45_49 <- run_garchx(data, train_end, lag_weeks = 4)

results_X <- bind_rows(x16_17, x16_18, x16_19, x16_20)
results_A <- bind_rows(a25_26, a25_27, a25_28, a25_29)
#results_B <- bind_rows(b26_27, b26_28, b26_29, b26_30)
results_C <- bind_rows(c29_30, c29_31, c29_32, c29_33)
results_D <- bind_rows(d40_41, d40_42, d40_43, d40_44)
results_E <- bind_rows(e45_46, e45_47, e45_48, e45_49)
#results_F <- bind_rows(f45_46, f45_47, f45_48, f45_49)
print(results_X)
print(results_A)
#print(results_B)
print(results_C)
print(results_D)
print(results_E)
#print(results_F)

all_results <- bind_rows(x16_17, x16_18, x16_19, x16_20, a25_26, a25_27, a25_28, a25_29,
                c29_30, c29_31, c29_32, c29_33, d40_41, d40_42, d40_43, d40_44,
                e45_46, e45_47, e45_48, e45_49)
print(all_results)

table_pub <- all_results %>%
  rename(
    `Train end week` = Train_End_Week,
    `Actual week`    = Actual_week,
    `Predicted week` = Pred_Week,
    `Predicted date` = Pred_Date,
    `Lag (weeks)`    = Lag_Weeks,
    `Wastewater`     = Wastewater,
    `Actual rate`    = Actual,
    `Predicted rate` = Predicted,
    `Lower 95% CI`   = Lower_95,
    `Upper 95% CI`   = Upper_95
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

kable(
  table_pub,
  caption = "Table 1. Forecast performance by week",
  align = "c"
) %>%
  kable_styling(
    full_width = FALSE,
    bootstrap_options = c("striped", "hover", "condensed")
  )

library(writexl)

write_xlsx(table_pub, "final_results.xlsx")

write.csv(table_pub, "final_results.csv", row.names = FALSE)


#### Fitted vs Actual Influenza Rates ####
# Convert to long format for plotting
all_results_long <- all_results %>%
  select(Pred_Date, Actual, Predicted, Lag_Weeks) %>%
  pivot_longer(cols = c("Actual", "Predicted"),
               names_to = "Type",
               values_to = "Influenza_Rate") %>%
  mutate(Type = factor(Type, levels = c("Actual", "Predicted")),
         Lag_Weeks = factor(Lag_Weeks))

# Line plot for each lag horizon
p_garch_fit <- ggplot(all_results_long, 
                      aes(x = Pred_Date, y = Influenza_Rate,
                          color = Type, linetype = Type)) +
  geom_line(size = 1) +
  geom_point(size = 2, alpha = 0.8) +
  facet_wrap(~ Lag_Weeks, scales = "free_x",
             labeller = labeller(Lag_Weeks = function(x) paste(x, "Week Lag"))) +
  scale_color_manual(values = c("Actual" = "black", "Predicted" = "steelblue")) +
  scale_linetype_manual(values = c("Actual" = "solid", "Predicted" = "dashed")) +
  labs(
    title = "GARCH-X Forecasts vs Actual Influenza Positive Rates",
    subtitle = "Model forecasts using lagged wastewater viral load as exogenous input",
    x = "Predicted Week Ending Date",
    y = "Influenza Positive Rate (per 100,000)",
    color = "",
    linetype = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    strip.text = element_text(face = "bold")
  )

print(p_garch_fit)

