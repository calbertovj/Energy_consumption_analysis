# libraries --------------

pacman::p_load(caret, ggplot2, DataExplorer, tidyr, dplyr,plyr,
               RMySQL,lubridate, reshape2, OneR, padr, imputeTS,
               forecast, feasts, plotly, ggfortify, stats, prophet)

# Reading data -------------
# Create a database connection
con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', 
                dbname='dataanalytics2018', 
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

# List the tables contained in the database 
dbListTables(con)

# Dowload the database
yr_2006 <- dbGetQuery(con, "SELECT * FROM yr_2006")
yr_2007 <- dbGetQuery(con, "SELECT * FROM yr_2007")
yr_2008 <- dbGetQuery(con, "SELECT * FROM yr_2008")
yr_2009 <- dbGetQuery(con, "SELECT * FROM yr_2009")
yr_2010 <- dbGetQuery(con, "SELECT * FROM yr_2010")

# Preproccessing -----------
# Combine tables into one dataframe using dplyr
energy_data <- bind_rows(yr_2006, yr_2007, yr_2008, yr_2009, yr_2010)
summary(energy_data)
str(energy_data)

# Combine Date and Time attribute values in a new attribute column
energy_data <-cbind(energy_data,paste(energy_data$Date,energy_data$Time),
                    stringsAsFactors=FALSE)

# Give the new attribute a header name 
colnames(energy_data)[11] <-"DateTime"
colnames(energy_data)[8] <-"Kitchen"
colnames(energy_data)[9] <-"Laundry"
colnames(energy_data)[10] <-"HVAC"

# Move the DateTime attribute within the dataset
energy_data <- energy_data[,c(ncol(energy_data), 1:(ncol(energy_data)-1))]
head(energy_data)

# Convert DateTime from POSIXlt to POSIXct 
energy_data$DateTime <- as.POSIXct(energy_data$DateTime, "%Y/%m/%d %H:%M:%S")

# Add the time zone
attr(energy_data$DateTime, "tzone") <- "UTC"

## Inspect the data types
str(energy_data)

## Create new attributes with lubridate
energy_data$year <- year(energy_data$DateTime)
energy_data$month <- month(energy_data$DateTime)
energy_data$quarter <- quarter(energy_data$DateTime)
energy_data$day <- day(energy_data$DateTime)
energy_data$weekday <- weekdays(energy_data$DateTime)
energy_data$hour <- hour(energy_data$DateTime)
energy_data$minute <- minute(energy_data$DateTime)
energy_data$week <- week(energy_data$DateTime)

energy_data1  <- energy_data[-c(1,2,3,4)]
energy_data1$quarter <- as.factor(energy_data1$quarter)
energy_data1$month <- as.factor(energy_data1$month) 
energy_data1$year <- as.factor(energy_data1$year) 
energy_data1$weekday <- as.factor(energy_data1$weekday)

str(energy_data1)

levels(energy_data1$quarter) <- c("1st","2nd","3rd","4th")

levels(energy_data1$month) <- c("01","02","03","04", "05", "06",
                                "07", "08", "09", "10", "11","12")

# General plot settings ----------------
plot.settings <- theme(
  axis.line.x =       element_line(colour = "black", size = 1),                                                       # Settings x-axis line
  axis.line.y =       element_line(colour = "black", size = 1),                                                       # Settings y-axis line 
  axis.text.x =       element_text(colour = "black", size = 12, lineheight = 0.9, vjust = 1, face = "bold"),        # Font x-axis 
  axis.text.y =       element_text(colour = "black", size = 12, lineheight = 0.9, hjust = 1),                         # Font y-axis
  axis.ticks =        element_line(colour = "black", size = 0.3),                                                     # Color/thickness axis ticks
  axis.title.x =      element_text(size = 20, vjust = 1, face = "bold", margin = margin(10,1,1,1)),                   # Font x-axis title
  axis.title.y =      element_text(size = 20, angle = 90, vjust = 1, face = "bold", margin = margin(1,10,1,1)),       # Font y-axis title
  
  legend.background = element_rect(colour=NA),                                                                        # Background color legend
  legend.key =        element_blank(),                                                                                # Background color legend key
  legend.key.size =   unit(1.2, "lines"),                                                                             # Size legend key
  legend.text =       element_text(size = 18),                                                                        # Font legend text
  legend.title =      element_text(size = 20, face = "bold", hjust = 0),                                              # Font legend title  
  legend.position =   "right",                                                                                        # Legend position
  
  panel.background =  element_blank(),                                                                                # Background color graph
  panel.border =      element_blank(),                                                                                # Border around graph (use element_rect())
  panel.grid.major =  element_blank(),                                                                                # Major gridlines (use element_line())
  panel.grid.minor =  element_blank(),                                                                                # Minor gridlines (use element_line())
  panel.margin =      unit(1, "lines"),                                                                               # Panel margins
  
  strip.background =  element_rect(fill = "grey80", colour = "grey50"),                                               # Background colour strip 
  strip.text.x =      element_text(size = 20),                                                                        # Font strip text x-axis
  strip.text.y =      element_text(size = 20, angle = -90),                                                           # Font strip text y-axis
  
  plot.background =   element_rect(colour = NA),                                                                      # Background color of entire plot
  plot.title =        element_text(size = 20, face = "bold", hjust = 0.5),                                                                        # Font plot title 
  plot.margin =       unit(c(1, 1, 1, 1), "lines")                                                                    # Plot margins
)


# Plotting energy year  --------

data_year  <- energy_data1 %>% 
  group_by(year, month, day) %>% 
  dplyr::summarize(sum_power = sum(Global_active_power)) %>% 
  mutate(kWh =sum_power / 60) %>%
  unite("year_month", year:month, remove = FALSE)

data_2006 <- filter(data_year, year=="2006")
data_2007 <- filter(data_year, year=="2007")
data_2008 <- filter(data_year, year=="2008")
data_2009 <- filter(data_year, year=="2009")
data_2010 <- filter(data_year, year=="2010")

plot_year <- ggplot(data_year, aes(day, kWh, color = year)) +
  geom_point() +
  facet_wrap(~month) +
    labs(x="Day of Month",
       y="",
       title = "Global Energy Consumption", 
       fill="Close")

plot_year <- plot_year + plot.settings

plot_2006 <- ggplot(data_2006, aes(day, kWh)) +
  geom_point() + facet_wrap(~month) +
  labs(x="Day of Month", y="", title = "Global Energy Consumption 2006")

plot_2007 <- ggplot(data_2007, aes(day, kWh)) +
  geom_point() + facet_wrap(~month) +
  labs(x="Day of Month", y="", title = "Global Energy Consumption 2007")

plot_2008 <- ggplot(data_2008, aes(day, kWh)) +
  geom_point() + facet_wrap(~month) +
  labs(x="Day of Month", y="", title = "Global Energy Consumption 2008")

plot_2009 <- ggplot(data_2009, aes(day, kWh)) +
  geom_point() + facet_wrap(~month) +
  labs(x="Day of Month", y="", title = "Global Energy Consumption 2009")

plot_2010 <- ggplot(data_2010, aes(day, kWh)) +
  geom_point() + facet_wrap(~month) +
  labs(x="Day of Month", y="", title = "Global Energy Consumption 2010")

# Plotting energy per month  --------

plot_month <- ggplot(data_year, aes(year_month, kWh, fill = year)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#56B4E9","#0072B2", "#56e9d5", "#566be9", "#56e98b")) +
    labs(x="Month",
       y="kWh",
       title = "Global Energy Consumption", 
       fill="Year")

plot_month <- plot_month + theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot_month <- plot_month + plot.settings

# Plotting energy per hour  --------

data_hour  <- energy_data1 %>%
  group_by(year,quarter,month,hour) %>% 
  dplyr::summarize(mean_power = mean(Global_active_power)) %>% 
  mutate(kWh_average =mean_power / 60) %>%
  mutate(season = quarter)

levels(data_hour$season)
levels(data_hour$season) <- c("Winter","Summer","Summer", "Winter")

plot_hour <- ggplot(data_hour, aes(hour, kWh_average)) +
  geom_point() +
  geom_smooth(color = "deepskyblue") +
    labs(x="Hour of day",
       y="Average kWh",
       title = "Global Energy Consumption", 
       fill="Year")

plot_hour <- plot_hour + theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot_hour <- plot_hour + plot.settings

# Plotting energy per season & peak/valley hour --------

hour_winter <- filter(data_hour, season=="Winter")

plot_hour_winter <- ggplot(hour_winter, aes(hour, kWh_average)) +
  geom_point() +
  geom_smooth(color = "deepskyblue") +
  geom_vline(xintercept = c(12,22), color = "cadetblue", size=1.5, linetype = "dotted") +
  labs(x="Hour of day",
       y="Average kWh",
       title = "Global Energy Consumption in Winter", 
       fill="Year")

plot_hour_winter <- plot_hour_winter + theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot_hour_winter <- plot_hour_winter + plot.settings

hour_summer <- filter(data_hour, season=="Summer")

plot_hour_summer <- ggplot(hour_summer, aes(hour, kWh_average)) +
  geom_point() +
  geom_smooth(color = "deepskyblue") +
  geom_vline(xintercept = c(13,23), color = "cadetblue", size=1.5, linetype = "dotted") +
  labs(x="Hour of day",
       y="Average kWh",
       title = "Global Energy Consumption in Summer", 
       fill="Year")

plot_hour_summer <- plot_hour_summer + theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot_hour_summer <- plot_hour_summer + plot.settings


plot_hour_both <- ggplot(data_hour, aes(hour, kWh_average)) +
  facet_wrap(~season) +
  geom_smooth(color = "deepskyblue") +
  geom_vline(xintercept = c(12,22), color = "cadetblue", size=1.5, linetype = "dotted") +
  labs(x="Hour of day",
       y="Average kWh",
       title = "Global Energy Consumption in Winter/Summer Time", 
       fill="Year")

plot_hour_both <- plot_hour_both + theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot_hour_both <- plot_hour_both + plot.settings + theme(strip.background = element_rect(fill="deepskyblue"))


# Calculating energy usage per season and peak/valley hour  --------

data_season  <- energy_data1 %>% 
  group_by(year, quarter, month, hour)%>%
  dplyr::summarize(sum_power = sum(Global_active_power)) %>%
  mutate(kWh =sum_power / 60) %>%
  mutate(season = quarter)

levels(data_season$season)
levels(data_season$season) <- c("Winter","Summer","Summer", "Winter")

data_season <- unite(data_season, "season_hour", c(season,hour), remove = FALSE)
data_season$season_hour <- as.factor(data_season$season_hour)
levels(data_season$season_hour)
levels(data_season$season_hour) <- c("Valley","Valley","Valley","Valley","Valley","Peak","Peak",
                              "Peak","Peak","Peak","Peak","Peak","Valley","Peak",
                              "Peak","Peak","Valley","Valley","Valley","Valley","Valley",
                              "Valley","Valley","Valley","Valley","Valley","Valley","Valley",
                              "Peak","Peak","Peak","Peak","Peak","Peak","Peak",
                              "Peak","Valley","Peak","Peak","Valley","Valley","Valley",
                              "Valley","Valley","Valley","Valley","Valley","Valley")

results_season  <- data_season %>%
  group_by(season, season_hour) %>% 
  dplyr::summarize(sum_power = sum(kWh))

# Plotting energy per submeter  --------

data_submeters  <- energy_data1 %>% group_by(year, quarter, month, weekday, week, day, hour) %>% 
  dplyr::summarize(sum_kitchen = sum(Kitchen),sum_laundry = sum(Laundry), 
                   sum_HVAC = sum(HVAC),sum_power = sum(Global_active_power)) %>%
  mutate(global_kWh =sum_power / 60) %>% 
  mutate(kitchen_kWh =sum_kitchen / 1000) %>% 
  mutate(laundry_kWh =sum_laundry / 1000) %>%
  mutate(HVAC_kWh =sum_HVAC / 1000) %>% 
  mutate(other_kWh = global_kWh - kitchen_kWh - laundry_kWh - HVAC_kWh) %>%
  dplyr::select(-c(sum_kitchen,sum_laundry,sum_HVAC,sum_power))


melt.data_submeters <- melt(data_submeters, id = c("hour","day","month","year","quarter"))

melt.data_submeters$hour <- as.factor(melt.data_submeters$hour)

plot_sub <- ggplot(melt.data_submeters, aes(hour, sqrt(value))) +
  geom_boxplot(fill= "paleturquoise") +
  facet_wrap(~variable, scales="free_y") +
  labs(x="Hour of day",
       y="Total kWh",
       title = "Global Energy Consumption", 
       fill="Year")

plot_sub <- plot_sub + theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot_sub <- plot_sub + plot.settings + theme(strip.background = element_rect(fill="deepskyblue"))

sum(data_submeters$kitchen_kWh)
sum(data_submeters$laundry_kWh)
sum(data_submeters$HVAC_kWh)
sum(data_submeters$other_kWh)
sum(data_submeters$global_kWh)

# Plotting Reactive energy  --------

data_reactive  <- energy_data1 %>%
  group_by(month, year) %>% 
  dplyr::summarize(sum_power = sum(Global_active_power),
                   sum_reactive = sum(Global_reactive_power)) %>% 
  mutate(global_kWh =sum_power / 60) %>% 
  mutate(reactive_kWh =sum_reactive / 60) %>% 
  dplyr::select (-c(sum_power, sum_reactive)) %>% 
  mutate(p_reactive = reactive_kWh / global_kWh * 100) 


melt.data_reactive <- melt(data_reactive, id = c("month","year", "p_reactive"))

plot_reactive <- ggplot(melt.data_reactive, aes(month, value, fill=variable)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("#56B4E9","#0072B2")) +
    labs(x="Months",
       y="Total kWh",
       title = "Global active/reactive Energy Consumption", 
       fill="Energy type")

plot_reactive <- plot_reactive + theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot_reactive <- plot_reactive + plot.settings

# Plotting Power used --------

data_power  <- energy_data1 %>%
  group_by(year, month, day, hour) %>% 
  dplyr::summarize(mean_intensity = mean(Global_intensity),
                   mean_voltage = mean(Voltage)) %>% 
  mutate(global_power =mean_voltage * mean_intensity / 1000) %>% 
  dplyr::select(-c(mean_voltage, mean_intensity))


data_power07_10 <- filter(data_power, year!= "2006")

plot_power_month <- ggplot(data_power07_10, aes(month, global_power)) +
  geom_boxplot() +
  facet_wrap(~year) +
  labs(x="Month",
       y="kWh",
       title = "Global Energy Consumption", 
       fill="Year")

plot_power_month <- plot_power_month + theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot_power_month <- plot_power_month + plot.settings

plot(plot_power_month)
#frequency and binning by power

frq_power <- nrow(filter(data_power, global_power >= 0 & global_power < 1))
frq_power <- c(frq_power,nrow(filter(data_power, global_power >= 1 & global_power < 2)))
frq_power <- c(frq_power,nrow(filter(data_power, global_power >= 2 & global_power < 3)))
frq_power <- c(frq_power,nrow(filter(data_power, global_power >= 3 & global_power < 4)))
frq_power <- c(frq_power,nrow(filter(data_power, global_power >= 4 )))

frq_power <- t(t(frq_power))
frq_power <- as.data.frame(frq_power)
frq_power$Global_Power <- c("0-1", "1-2", "2-3", "3-4", ">4") 
names(frq_power)[1] <- "Frequency_hour"

frq_power <- mutate(frq_power, freq = Frequency_hour / sum(Frequency_hour) * 100)

plot_freq <- ggplot(frq_power, aes(reorder(Global_Power, -freq), freq)) +
  geom_bar(stat = "identity", fill = "#56B4E9") +
  labs(x="Power",
       y="% Frequency per hour",
       title = "Global Power Consumption frequency"
        )

plot_freq <- plot_freq + theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot_freq <- plot_freq + plot.settings + theme(legend.position = "none")

# Dealing with missing values and preparing data for prediction #####

energy_data2 <- energy_data %>%
    pad(break_above = 3)
  
statsNA(energy_data2$Global_active_power)
plotNA.distributionBar(energy_data2$Global_active_power)
plotNA.gapsize(energy_data2$Global_active_power)

energy_data2 <- na_seadec(energy_data2, algorithm = "kalman") %>% 
  dplyr::select (-c(id, Date, Time, year, month,
                    quarter, day, weekday, hour, minute, week))


## Create new attributes with lubridate
energy_data2$year <- year(energy_data2$DateTime)
energy_data2$month <- month(energy_data2$DateTime)
energy_data2$quarter <- quarter(energy_data2$DateTime)
energy_data2$day <- day(energy_data2$DateTime)
energy_data2$weekday <- weekdays(energy_data2$DateTime)
energy_data2$hour <- hour(energy_data2$DateTime)
energy_data2$minute <- minute(energy_data2$DateTime)
energy_data2$week <- week(energy_data2$DateTime)

energy_data2$quarter <- as.factor(energy_data2$quarter)
energy_data2$month <- as.factor(energy_data2$month) 
energy_data2$year <- as.factor(energy_data2$year) 
energy_data2$weekday <- as.factor(energy_data2$weekday)

str(energy_data2)

levels(energy_data2$quarter) <- c("1st","2nd","3rd","4th")

levels(energy_data2$month) <- c("01","02","03","04", "05", "06",
                                "07", "08", "09", "10", "11","12")


# selecting the dataset for prediction in days ------

energy_data_day  <- energy_data2 %>%
  group_by(year, month, day)  %>% 
  dplyr::summarize(sum_kitchen = sum(Kitchen),sum_laundry = sum(Laundry), 
                   sum_HVAC = sum(HVAC),sum_power = sum(Global_active_power)) %>% 
  mutate(global_kWh =sum_power / 60) %>% 
  mutate(kitchen_kWh =sum_kitchen / 1000) %>%
  mutate(laundry_kWh =sum_laundry / 1000) %>%
  mutate(HVAC_kWh =sum_HVAC / 1000) %>%
  mutate(other_kWh = global_kWh - kitchen_kWh - laundry_kWh - HVAC_kWh) %>% 
  filter(year !="2006", year !="2010")

test_data2010 <- energy_data2 %>% 
  group_by(year, month, day)  %>% 
  dplyr::summarize(sum_kitchen = sum(Kitchen),sum_laundry = sum(Laundry), 
                   sum_HVAC = sum(HVAC),sum_power = sum(Global_active_power)) %>% 
  mutate(global_kWh =sum_power / 60) %>% 
  mutate(kitchen_kWh =sum_kitchen / 1000) %>%
  mutate(laundry_kWh =sum_laundry / 1000) %>%
  mutate(HVAC_kWh =sum_HVAC / 1000) %>%
  mutate(other_kWh = global_kWh - kitchen_kWh - laundry_kWh - HVAC_kWh) %>% 
  filter(year == "2010")


# TBATS model --------

data_msts <- msts(energy_data_day$global_kWh, seasonal.periods = c(7, 365.25),
                  start = c(2007, 1))

test_ts <- ts(test_data2010$global_kWh, frequency = 365, start = c(2010, 1))

model_tbats <- tbats(data_msts, use.parallel = TRUE, num.core = 7)

plot(data_msts)

fc_tbats <- forecast(model_tbats,h=330)

checkresiduals(model_tbats)

autoplot(fc_tbats, ylim = c(0, 80)) + 
  autolayer(test_ts) +
  ggtitle("TBATS model")

accuracy(fc_tbats,test_ts)

head(summary(fc_tbats))

data_tbats_components <- tbats.components(model_tbats)
plot.ts(data_tbats_components)

ggseasonplot(data_msts)
ggsubseriesplot(data_msts)

TS_M_Decomposition <- stl(data_msts, s.window = "periodic")
apply(TS_M_Decomposition$time.series, 2, var)/var(data_msts)

# ARIMA model --------

data_ts <- ts(energy_data_day$global_kWh, frequency = 365, start = c(2007, 1))

model_arima <- auto.arima(data_ts, stepwise = FALSE,
                          approximation = FALSE, parallel = TRUE, num.cores = 7)

adf.test(data_ts)
ggAcf(data_ts)
ggPacf(data_ts)

checkresiduals(model_arima)

fc_arima <- forecast(model_arima,h=330)

accuracy(fc_arima,test_ts)

autoplot(fc_arima) + 
  autolayer(test_ts) +
  ggtitle("Auto Arima model")

# Prophet model ------

# changing data to fit prophet model
data_prophet <- energy_data_day %>% 
  unite("Date", year:day, remove = FALSE, sep = "-")

data_prophet$Date <- as.factor(data_prophet$Date)
data_prophet$Date <- as.Date(data_prophet$Date, "%Y-%m-%d")

data_prophet <- data_prophet %>% 
  ungroup() %>% 
dplyr::select(c(Date, global_kWh)) %>% 
  rename(ds = Date, y = global_kWh)

test_prophet <- test_data2010 %>% 
  unite("Date", year:day, remove = FALSE, sep = "-")

test_prophet$Date <- as.factor(test_prophet$Date)
test_prophet$Date <- as.Date(test_prophet$Date, "%Y-%m-%d")

test_prophet <- test_prophet %>% 
  ungroup() %>% 
  dplyr::select(c(Date, global_kWh)) %>% 
  rename(ds = Date, y = global_kWh)

# modeling with prophet
m <- prophet(data_prophet)
t <- prophet(test_prophet)

future <- make_future_dataframe(m, periods = 330)

fs_prophet <- predict(m, future)

plot(m, fs_prophet) 
plot(t, fs_prophet) 


# error metrics
df.cv <- cross_validation(a, initial = 365.25, period = 180, horizon = 330, units = 'days')
df.p <- performance_metrics(df.cv)
plot_cross_validation_metric(df.cv, metric = "mape")
tail(df.p)

# Prophet model using all data ------

all_data <- energy_data2 %>% 
  group_by(year, month, day)  %>% 
  dplyr::summarize(sum_kitchen = sum(Kitchen),sum_laundry = sum(Laundry), 
                   sum_HVAC = sum(HVAC),sum_power = sum(Global_active_power)) %>% 
  mutate(global_kWh =sum_power / 60) %>% 
  mutate(kitchen_kWh =sum_kitchen / 1000) %>%
  mutate(laundry_kWh =sum_laundry / 1000) %>%
  mutate(HVAC_kWh =sum_HVAC / 1000) %>%
  mutate(other_kWh = global_kWh - kitchen_kWh - laundry_kWh - HVAC_kWh) %>% 
  filter(year != "2006") %>%
  unite("Date", year:day, remove = FALSE, sep = "-")

all_data$Date <- as.factor(all_data$Date)
all_data$Date <- as.Date(all_data$Date, "%Y-%m-%d")

all_prophet <- all_data %>% 
  ungroup() %>% 
  dplyr::select(c(Date, global_kWh)) %>% 
  rename(ds = Date, y = global_kWh)

a <- prophet(all_prophet)

future_all <- make_future_dataframe(a, periods = 365)

fs_prophet_all <- predict(a, future_all)

plot(a, fs_prophet_all) 





# Exporting data to PBI ------

data_powerbi  <- energy_data1 %>% group_by(year, quarter, month, day) %>% 
  dplyr::summarize(sum_kitchen = sum(Kitchen),sum_laundry = sum(Laundry), 
                   sum_HVAC = sum(HVAC),sum_power = sum(Global_active_power)) %>%
  mutate(global =sum_power / 60) %>% 
  mutate(kitchen =sum_kitchen / 1000) %>% 
  mutate(laundry =sum_laundry / 1000) %>%
  mutate(HVAC =sum_HVAC / 1000) %>% 
  mutate(other = global - kitchen - laundry - HVAC) %>%
  dplyr::select(-c(sum_kitchen,sum_laundry,sum_HVAC,sum_power, global)) %>% 
  unite("Date", year,month,day, sep = "-", remove = TRUE)

data_powerbi$Date <- as.factor(data_powerbi$Date)
data_powerbi$Date <- as.Date(data_powerbi$Date, "%Y-%m-%d")

data_powerbi_melt <- melt(data_powerbi, id = c("Date","quarter"))

write.csv(data_powerbi_melt, file="data_powerbi_melt.csv", row.names = TRUE)

#new dataframe with money involved

data_powerbi_money  <- energy_data1 %>% group_by(year, month, day) %>% 
  dplyr::summarize(sum_kitchen = sum(Kitchen),sum_laundry = sum(Laundry), 
                   sum_HVAC = sum(HVAC),sum_power = sum(Global_active_power)) %>%
  mutate(global =sum_power / 60 * 0.14) %>% 
  mutate(kitchen =sum_kitchen / 1000 * 0.14) %>% 
  mutate(laundry =sum_laundry / 1000 * 0.14) %>%
  mutate(HVAC =sum_HVAC / 1000 * 0.14) %>% 
  mutate(other = global - kitchen - laundry - HVAC) %>%
  dplyr::select(-c(sum_kitchen,sum_laundry,sum_HVAC,sum_power, global)) %>%
  unite("Date", year,month,day, sep = "-", remove = TRUE)

data_powerbi_money$Date <- as.factor(data_powerbi_money$Date)
data_powerbi_money$Date <- as.Date(data_powerbi_money$Date, "%Y-%m-%d")

data_powerbi_money_melt <- melt(data_powerbi_money, id = c("Date"))

write.csv(data_powerbi_money_melt, file="data_powerbi_money_melt.csv", row.names = TRUE)

# data for peak/valley time

data_bi_season  <- energy_data1 %>% 
  group_by(year, quarter, month, day, hour)%>%
  dplyr::summarize(sum_kitchen = sum(Kitchen),sum_laundry = sum(Laundry), 
                   sum_HVAC = sum(HVAC),sum_power = sum(Global_active_power)) %>%
  mutate(global =sum_power / 60) %>% 
  mutate(kitchen =sum_kitchen / 1000) %>% 
  mutate(laundry =sum_laundry / 1000) %>%
  mutate(HVAC =sum_HVAC / 1000) %>% 
  mutate(other = global - kitchen - laundry - HVAC) %>%
  dplyr::select(-c(sum_kitchen,sum_laundry,sum_HVAC,sum_power, global)) %>% 
  unite("Date", year,month,day, sep = "-", remove = FALSE) %>% 
  mutate(season = quarter)

levels(data_bi_season$season)
levels(data_bi_season$season) <- c("Winter","Summer","Summer", "Winter")

data_bi_season <- unite(data_bi_season, "season_hour", c(season,hour), remove = FALSE)
data_bi_season$season_hour <- as.factor(data_bi_season$season_hour)
levels(data_bi_season$season_hour)
levels(data_bi_season$season_hour) <- c("Valley","Valley","Valley","Valley","Valley","Peak","Peak",
                                     "Peak","Peak","Peak","Peak","Peak","Valley","Peak",
                                     "Peak","Peak","Valley","Valley","Valley","Valley","Valley",
                                     "Valley","Valley","Valley","Valley","Valley","Valley","Valley",
                                     "Peak","Peak","Peak","Peak","Peak","Peak","Peak",
                                     "Peak","Valley","Peak","Peak","Valley","Valley","Valley",
                                     "Valley","Valley","Valley","Valley","Valley","Valley")

data_bi_season1  <- data_bi_season %>% 
  group_by(year, quarter, month, day, season_hour)%>%
  dplyr::summarize(kitchen = sum(kitchen),laundry = sum(laundry), 
                   HVAC = sum(HVAC), other = sum(other)) %>% 
  unite("Date", year,month,day, sep = "-", remove = FALSE) %>%
  na.omit()  

data_bi_season_peak  <- data_bi_season1 %>% 
  filter(season_hour == "Peak") %>% 
  mutate(other = other) %>% 
  mutate(kitchen =kitchen) %>% 
  mutate(laundry =laundry) %>%
  mutate(HVAC =HVAC)
  

data_bi_season_valley  <- data_bi_season1 %>% 
  filter(season_hour == "Valley") %>% 
  mutate(other = other) %>% 
  mutate(kitchen =kitchen) %>% 
  mutate(laundry =laundry) %>%
  mutate(HVAC =HVAC)

data_bi_peak_melt <- melt(data_bi_season_peak, id = c("Date", "year","month", "quarter", "day", "season_hour"))
data_bi_valley_melt <- melt(data_bi_season_valley, id = c("Date", "year","month", "quarter", "day", "season_hour"))

write.csv(data_bi_peak_melt, file="data_bi_peak_melt.csv", row.names = TRUE)
write.csv(data_bi_valley_melt, file="data_bi_valley_melt.csv", row.names = TRUE)


data_bi_season_cost  <- data_bi_season1 %>% 
  mutate(other_cost = ifelse (season_hour == "Peak", other * 0.15, other * 0.07)) %>%
  mutate(kitchen_cost = ifelse (season_hour == "Peak", kitchen * 0.15, kitchen * 0.07)) %>%
  mutate(laundry_cost = ifelse (season_hour == "Peak", laundry * 0.15, laundry * 0.07)) %>%
  mutate(HVAC_cost = ifelse (season_hour == "Peak", HVAC * 0.15, HVAC * 0.07)) %>% 
  group_by(year, quarter, month, day)%>%
  dplyr::summarize(kitchen = sum(kitchen_cost),laundry = sum(laundry_cost), 
                   HVAC = sum(HVAC_cost), other = sum(other_cost)) %>% 
  unite("Date", year,month,day, sep = "-", remove = FALSE) %>% 
  na.omit() 

data_bi_season_cost_melt <- melt(data_bi_season_cost, id = c("Date", "year","month", "quarter", "day"))

write.csv(data_bi_season_cost_melt, file="data_bi_season_cost_melt.csv", row.names = TRUE)

