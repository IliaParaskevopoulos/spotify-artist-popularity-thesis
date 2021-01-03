# spotify-artist-popularity-thesis
All related files for my thesis


Guide as to how to run the model

- Run code untill line 1828, to process the raw data and create the final dataset
- Find Descriptive information for the variables (standard deviation, pearson, spearman) in lines 1830:1900
- Skip section "Log Transform" at 1902:1922, as it is used to log transform specific variables in order to prepare them for the log-log regression model testing in 2023:2062
- Delete a few outliers at 2014:2019 to improve the quality of the model (optional)
- Run OLS multiple regression at 2064:2073
- See graphical depiction of residuals at 2075:2079 and at 2081:2089


