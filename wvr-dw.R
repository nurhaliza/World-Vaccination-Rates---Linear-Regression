# environment used: rstudio - language: r
# libraries needed
library(tidyverse) 
library(ggplot2)
library(modelr) 

# ================================= DATA TIDYING/WRANGLING ===================================
# saving all datasets to corresponding data names 
data1 <- read_csv("https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/vaccine_data/global_data/time_series_covid19_vaccine_doses_admin_global.csv")
data2 <- read_csv("/Users/nurhalizahassan/Desktop/World-Vaccination-Rates---Linear-Regression/Datasets/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_3011433.csv")
data3 <- read_csv("/Users/nurhalizahassan/Desktop/World-Vaccination-Rates---Linear-Regression/Datasets/demographics.csv")

# data 1 - time_series_covid19_vaccine_doses_admin_global.csv
# note: the following commands are by parts
revised_data1 <- data1 %>% select(-'UID', -'iso2', -'code3', -'FIPS', -'Admin2', -'Province_State', -'Lat', -'Long_', -'Combined_Key')
revised_data1 <- revised_data1[!is.na(revised_data1$Population),]
revised_data1 <- revised_data1 %>% pivot_longer(-c(iso3, Country_Region, Population), names_to = "Date", values_to = "Shots", values_drop_na = TRUE) %>% filter(Shots != 0) 
revised_data1 <- revised_data1 %>% mutate(Vaccination_Rate = Shots / Population) %>% mutate(New_Date = revised_data1$Date %>% as.Date()) %>% select(-Date)

rdata1_newvar <- revised_data1 %>% select(Country_Region, New_Date) %>% group_by(Country_Region) %>% slice(which.min(New_Date))
colnames(rdata1_newvar) <- c("Country_Region","First_Day")
revised_data1 <- revised_data1 %>% left_join(rdata1_newvar)
revised_data1 <- revised_data1 %>% mutate(daysSinceStart = New_Date - First_Day + 1)

# data 2 - API_NY.GDP.MKTP.CD_DS2_en_csv_v2_3011433.csv
# - tidying/wrangling -
# note: the following commands are by parts
revised_data2 <- data2 %>% select(-'Country Code', -'Indicator Name', -'Indicator Code') 
revised_data2 <- revised_data2 %>% pivot_longer(-'Country Name', names_to="Year", values_to="GDP", values_drop_na = TRUE)
revised_data2 <- revised_data2 %>% filter(Year == '2005')

# data 3 - demographics.csv
# - tidying/wrangling -
# note: the following commands are by parts
# must first combine male and female population numbers together (ignore sex-related differences)
# SP.POP.65.UP.IN
# data3 %>% select(-'Series Name') %>% pivot_wider(names_from = 'Series Code', values_from = 'YR2015') %>% mutate(SP.POP.65UP.IN = SP.POP.65UP.FE.IN + SP.POP.65UP.MA.IN) 
# SP.POP.80.UP.IN
# data3 %>% select(-'Series Name') %>% pivot_wider(names_from = 'Series Code', values_from = 'YR2015') %>% mutate(SP.POP.80UP.IN = SP.POP.80UP.FE + SP.POP.80UP.MA) 
# SP.POP.1564.IN
# data3 %>% select(-'Series Name') %>% pivot_wider(names_from = 'Series Code', values_from = 'YR2015') %>% mutate(SP.POP.1564.IN = SP.POP.1564.FE.IN + SP.POP.1564.MA.IN) 
# SP.POP.0014.IN
# data3 %>% select(-'Series Name') %>% pivot_wider(names_from = 'Series Code', values_from = 'YR2015') %>% mutate(SP.POP.0014.IN = SP.POP.0014.FE.IN + SP.POP.0014.MA.IN) 
# SP.DYN.AMRT
# data3 %>% select(-'Series Name') %>% pivot_wider(names_from = 'Series Code', values_from = 'YR2015') %>% mutate(SP.DYN.AMRT = SP.DYN.AMRT.FE + SP.DYN.AMRT.MA) 

# note: comprehensive command for male and female population integrated combination 
data3 %>% select(-'Series Name') %>% pivot_wider(names_from = 'Series Code', values_from = 'YR2015') %>% mutate(SP.POP.65UP.IN = SP.POP.65UP.FE.IN + SP.POP.65UP.MA.IN) %>% mutate(SP.POP.65UP.IN = SP.POP.65UP.FE.IN + SP.POP.65UP.MA.IN) %>% mutate(SP.POP.80UP.IN = SP.POP.80UP.FE + SP.POP.80UP.MA) %>% mutate(SP.POP.1564.IN = SP.POP.1564.FE.IN + SP.POP.1564.MA.IN) %>% mutate(SP.POP.0014.IN = SP.POP.0014.FE.IN + SP.POP.0014.MA.IN) %>% mutate(SP.DYN.AMRT = SP.DYN.AMRT.FE + SP.DYN.AMRT.MA) %>% select(-c(SP.POP.80UP.FE,SP.POP.80UP.MA,SP.POP.1564.MA.IN,SP.POP.1564.FE.IN,SP.POP.0014.MA.IN,SP.POP.0014.FE.IN,SP.DYN.AMRT.FE,SP.DYN.AMRT.MA,SP.POP.TOTL.FE.IN,SP.POP.TOTL.MA.IN,SP.POP.65UP.FE.IN,SP.POP.65UP.MA.IN))
# note: final comprehensive commands
revised_data3 <- data3 %>% select(-'Series Name') %>% pivot_wider(names_from = 'Series Code', values_from = 'YR2015') %>% mutate(SP.POP.80UP=SP.POP.80UP.FE+SP.POP.80UP.MA) %>% mutate(SP.POP.1564=SP.POP.1564.MA.IN+SP.POP.1564.FE.IN) %>% mutate(SP.POP.0014=SP.POP.0014.MA.IN+SP.POP.0014.FE.IN) %>% mutate(SP.DYN.AMRT=SP.DYN.AMRT.FE+SP.DYN.AMRT.MA) %>% mutate(SP.POP.TOTL=SP.POP.TOTL.FE.IN+SP.POP.TOTL.MA.IN) %>% mutate(SP.POP.65UP.IN=SP.POP.65UP.FE.IN+SP.POP.65UP.MA.IN) %>% select(-c(SP.POP.80UP.FE,SP.POP.80UP.MA,SP.POP.1564.MA.IN,SP.POP.1564.FE.IN,SP.POP.0014.MA.IN,SP.POP.0014.FE.IN,SP.DYN.AMRT.FE,SP.DYN.AMRT.MA,SP.POP.TOTL.FE.IN,SP.POP.TOTL.MA.IN,SP.POP.65UP.FE.IN,SP.POP.65UP.MA.IN))
revised_data3 <- revised_data3 %>% select(-'Country Code')

# combining all datasets
final_data <- revised_data1 %>% left_join(revised_data2, by=c("Country_Region" = "Country Name"))
final_data <- final_data %>% left_join(revised_data3, by=c("Country_Region" = "Country Name"))
final_data <- final_data %>% select(-'New_Date', -'Year', -'First_Day')
final_data <- transform(final_data, daysSinceStart = as.integer(daysSinceStart))

# =================================== LINEAR MODELING/REGRESSION ====================================
model_1 = summary(lm(formula = Vaccination_Rate ~ daysSinceStart, data = final_data))$r.squared
model_2 = summary(lm(formula = Vaccination_Rate ~ daysSinceStart / GDP, data = final_data))$r.squared
model_3 = summary(lm(formula = Vaccination_Rate ~ Population + daysSinceStart + GDP + SP.DYN.LE00.IN + SP.URB.TOTL, data = final_data))$r.squared
model_4 = summary(lm(formula = Vaccination_Rate ~ SP.URB.TOTL / Population, data = final_data))$r.squared
model_5 = summary(lm(formula = Vaccination_Rate ~ SP.POP.80UP + SP.POP.65UP.IN, data = final_data))$r.squared
models <- data.frame(mod = c(1,2,3,4,5), r2 = c(model_1, model_2, model_3, model_4, model_5))

plot_data <- final_data %>% group_by(Country_Region) %>% slice_max(daysSinceStart)
ggplot(plot_data) + geom_point(mapping = aes(x=daysSinceStart, y=Vaccination_Rate)) + scale_x_continuous(limits=c(225, 345), breaks=seq(225, 345, 10)) + scale_y_continuous(limits=c(0.0, 2.0), breaks=seq(0.0, 2.0, 0.2))
ggplot(models, aes(x=mod,y=r2)) + geom_bar(stat="identity") + scale_y_continuous(limits=c(0,1))

# same scatterplot as line 67 - only difference: this one includes line of best fit
# modelPred <- lm(Vaccination_Rate ~ daysSinceStart, data = plot_data)
# plot_data <- plot_data %>% add_predictions(modelPred) %>% View()
# ggplot(plot_data) + geom_point(mapping = aes(x=daysSinceStart, y=Vaccination_Rate)) + scale_x_continuous(limits=c(225, 345), breaks=seq(225, 345, 10)) + scale_y_continuous(limits=c(0.0, 2.0), breaks=seq(0.0, 2.0, 0.2)) + geom_line(mapping=aes(x=daysSinceStart, y=pred))
