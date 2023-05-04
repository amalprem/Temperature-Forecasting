# Temperature-Forecasting

Problem Statement

The ability to accurately predict temperature based on weather conditions is critical for various industries and applications, from agriculture and energy to transportation and public health.
In this study, the aim is to develop a predictive model for temperature forecasting using Time Series Analysis techniques.
Specifically, the study objective is to determine how time series analysis can be used to forecast temperature and test a few hypotheses that can provide valuable insights for stakeholders. The analysis will examine the relationship between temperature and other weather parameters, such as humidity, wind, and dewpoint, and determine the extent to which these variables can be used to reliably forecast temperature.
The study also involves plotting the time series of temperature data and summarizing the autocorrelations, cross-correlations, and spectral analysis for the frequency domain. Additionally, various linear dynamic models will be fitted to the data and compared, including ARIMAX, SARIMAX, and VAR, to determine the most effective approach for forecasting temperature.


Hypotheses

H1: Temperature exhibits a significant seasonal pattern over the years.
H2: Temperature at a given day is positively correlated with the temperature on the previous day.
H3: Temperature is influenced by various weather parameters, such as humidity, dew point, and wind speed, as well as their past values, including the lagged temperature.

Dataset

In this study, data was collected from Kaggle, specifically from the "Weather Data - Boston Jul 2012-Aug 2015" dataset provided by Naveen Pandian. The dataset contains daily weather measurements, including temperature, humidity, dew point, wind, precipitation, and date, from July 2012 to August 2015. The data was collected from a weather station located in Boston, Massachusetts, USA. The dataset is publicly available and was downloaded in CSV format for further analysis.The dataset consists of 1157 rows of daily data and 8 features:
Precipitation: The amount of rainfall in millimeters
Day: Day of the month
Month: Month of the year
Year: Year
Temperature: The average temperature over the day in Fahrenheit
Dewpoint: The average dew point over the day in Fahrenheit
Humidity: Relative percent amount of moisture in the air.
Wind: The average wind speed in miles per hour.

Methodology

1) Checking seasonality by plotting the time series.
2) Checking Stationarity in the time series data using ADF and KPSS tests
3) Using Differencing to remove seasonality from data.
4) Plotting types of correlations
  Auto Correlation
  Partial Auto Correlation
  Cross Correlation
5) Spectral Analysis ( Checking signals in frequency domain)
6) Model Selection and Fitting
  ARIMAX 
  SARIMAX
  VAR
7) Comapring different models and choosing the best model to forecast temperature



