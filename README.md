# DSCapstone2
This project will implement a predictor system for the ALL NBA team members for the National Basketball Association. At the end of an NBA season, the coaches, media and, players select the best players of the year to form three teams known as the ALL NBA teams. These teams have a spot for the Center position, two spots for the Forward positions and, two spots for the Guard positions. Therefore, if a player is Guard, he could only be eligible to take one of the Guard positions. 

The data used in the project is a combination of two datasets. The first one is a dataset retrieved from [Kaggle NBA-players-stats] (https://www.kaggle.com/drgilermo/nba-players-stats) contains statistical information about all the players in the league since 1950. The dataset has 20376 observations and 53 variables, including the season's totals and advanced statistics.

The second one is a generated dataset from scraping the official NBA site listing the [members of the ALL NBA teams](https://www.nba.com/history/awards/all-nba-team) since 1947. The code for scraping the website is written in Python and is beyond the scope of this project. However, if you are interested in taking a look at the code, you can find it on the file web_scraper.py in this repository. It is necessary to add that the program only recovers the members of the First and Second ALL NBA Teams because the Third team didn't exist until the late 80s. Also, both datasets can be found in the Data directory of this project.

To create the predictor system, we are going to analyze and wrangle the data from both datasets. Then, merge the datasets to identify the selected players for the ALL NBA teams, creating a binary classification problem. Next, determine the best predictors from the 53 variables. Finally, compare three different binary classification algorithms: logistic regression, knn, and decision trees to decide the best predicting algorithm.