# Executive Summary {-}

## Problem {-} 

Unemployment rates fluctuate over time as the economy goes through periods of recession and depression. Every month the Bureau of Labor Statistics releases an "Unemployment Situation Summary" and a "Gross Domestic Product Summary". While it is often assumed that factors in these two reports are linked to the United States unemployment rate, analysts often do not know which of the thousands of variables in these reports they should focus their attention on. Variables examined range from net government lending to the number of employees by industry sector. While using data from 1960 to 2020 inherently ignores shifts in employment patterns across time, the purpose of this report is to determine which factors have historically been predictive of the U.S. unemployment rates, not to predict future unemployment rates. Therefore, this report aims to identify and explain how different factors in these two reports relate to the U.S. unemployment rate.

## Data {-}

>(1) Employment Situation Summary^[https://www.bls.gov/news.release/empsit.nr0.htm]

>(2) Gross Domestic Product^[https://www.bea.gov/data/gdp/gross-domestic-product]

>(3) Additional Federal Reserve of Economic Data Variables: Unemployment Rate, Inflation, Federal Funds Rate 

My dataset combines data from two sources. First, I pulled the "Unemployment Situation Summary" and the "Gross Domestic Product Summary" time-series data sets from the U.S. Bureau of Labor Statistics from 1954 to 2021. Next, I pulled three additional variables from the Federal Reserve Bank of St. Louis (unemployment rate, inflation, and federal funds rate). For the two time-series data sets, I pulled every key variable available. My primary response variable of interest was the unemployment rate, which is defined as the number of unemployed persons as a percentage of the labor force. While the original data set has 804 observations and 2004 variables, the clean data set has 732 observations and 160 variables.   

## Analysis {-}

Before exploring the dataset or running any analyses, I split the data into train and test data sets. I trained my model using the training data set and reserved the test dataset for measuring model performance. Next, I accessed correlations between variables and found multicollinearity issues, which I systematically dealt with. Next, I explored my data to look for relationships between variables and the response and checked to make sure all linear regression assumptions were met. To determine which features are predictive of the unemployment rate, I built six different cross-validated models: ordinary least squares, ridge regression, LASSO regression, elastic net regression, random forest, and boosting. Of the regression models, the ordinary least squares (OLS) regression has the lowest test error. The boosted model had the lowest test error of all models (and of the tree-based models). Finally, I drew conclusions based on what I learned from my models.  

## Conclusions {-}

I found that both regression and tree-based methods found similar variables to be strong predictors of the unemployment rate. Specifically, the boosted model found depreciation of fixed assets and the number of mining/logging employees to be the strongest predictors, revealing how changes in the number of employees for “blue-collar” professions are more predictive than changes in the number of employees for “white collar” professions. I also found that unemployment rises as government debt and spending rise. Additionally, I found that the unemployment rate falls when net exports as a percentage of GDP, money invested in fixed assets, and corporate profits increase. I hope this analysis can reframe how economists and analysts think about unemployment, both in the context of what signals potentially high future unemployment and the effects of unemployment. 
