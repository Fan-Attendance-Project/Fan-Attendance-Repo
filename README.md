# Fan-Attendance-and-Stadium-Capacity-Analysis-Repo
This repo contains all the webscraping tools, data, hypothesis testing, regression models, and data visuals on our Fan Attendance Project.

For an overview of the project, check out here (https://github.com/Fan-Attendance-Project/Fan-Attendance-Repo/blob/main/Fan%20Attendance%20Report/Fan%20Attendance%20%26%20Stadium%20Capacity%20Project%20Overview.pdf).

You can find our methodology, findings, hypothesis tests, and model in our full report here(https://github.com/Fan-Attendance-Project/Fan-Attendance-Repo/blob/main/Fan%20Attendance%20Report/Fan%20Attendance%20%26%20Stadium%20Capacity%20Soccer%20Analytics.pdf).

You can play around with our data and seasonal stadium win rate model in our R-Shiny Dashboard here (https://team-analytica.shinyapps.io/Final_Report/).

The Fan Attendance Project is a Northwestern School of Professional Studies, Masters' in Data Science Capstone Project which seeks to identify the impact that fan attendance has on professional soccer matches played across Europe's top 5 leagues (England, Spain, France, Germany, and Italy).

In the Data Files Section you will see all of our aggregated webscraped data from FBRef.com. Use of this data must abide by FBRef's SR and Data Use policies which can be found here (https://www.sports-reference.com/data_use.html).

The provider of the dataset is StatsPerform. The dataset is comprised of match report summary statistics for over 9,130 matches from 2016-2021.

The Webscraping Notebooks Folder contains jupyter notebooks that leverage the beautiful soup package to scrape through the HTML on FBRef's website in a manner inspired by Christopher B. Martin, PhD's fabulous FBRef webscraping repo found here (https://chmartin.github.io/2019/02/18/EPL-History-Scraping.html).

The Testing Notebooks contain the high level and league level hypothesis testing methods performed on the data.

The OLS Model Folder is where the data preparation, testing and modeling was performed for our seasonal stadium win rate model. Note data transformation are done in Python, but model testing and training is done in R.

The R-Shiny App Folder contains all the code related to a dashboard which visualizes our data and the model using the R-Shiny Package.

The Last folder, Presentation and Final Report contains the documentation and slide presentation about this project. 
