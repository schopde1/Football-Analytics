# Football-Analytics

### Business Context
Data-driven decision making is a vital aspect of virtually all business, and the sports industry is no exception. Over the past two decades, the influence of Data Analytics has been growing in every aspect of our lives: in businesses of every kind, but also in healthcare, media and sports. Until a few years ago, football was thought to be immune from this trend. Now, the early adopters in the major football leagues are thriving thanks to the competitive advantage that investments in data analytics are beginning to provide them: Liverpool, AZ Alkmaar and Brentford are just a few in the fast-growing list of successful case studies. In our view, the clubs that aren’t planning to jump on the analytics bandwagon run the risk of being left behind.

Football is played in over 200 countries (most popular sport globally). According to WorldAtlas, Football is the most popular sport with an estimated fan base of 4 billion. 

### Data
The data was sourced from the pre-approved API
[Dataset](https://www.football-data.org/documentation/quickstart )

Additional relevant data relating to in-game statistics-
[Dataset](http://football-data.co.uk)

### Problem Statement
We will try to looks at some of the play-by-play statistics involved in a game, which would helps us evaluate a team’s performance, instead of just leveraging the actual number of "goals scored" metric from prior matches. 

We would combine some key metrics of a team like their half time scores, full time score based on whether its their home ground or away ground, team's overall offensive and defensive ratings which are updated after each game to build model predicting the outcome of future matches as to who the winner will be - Home Team or Away Team.

Leveraging in-game statistics can provide us interesting and deeper perspective rather than the goals metrics, creating an opportunity to look beyond the match result itself.


### Data summary, exploration, and discussion

We extracted our data from 2 sources in the API provided - Competition and Match. We could freely access data to 13 Competition ID's.
Based on that we retrieved all match details of the available competitions from the API. We could extract 3612 records from the API for the matches. We refereed to another secondary data source to enrich the existing primary data retrieved from the API. Based on the data extracted we are trying to perform analysis on whether the winner will be a Home Team, a Away Team or will it be a draw. The Features we have used to make this predictions are - Half Time Home Team Score, Half Time Away Team Score, Full Time Home Team Score, Full Time Away Team Score, Home Team, Away Team and Winner. The outcome is the Winner field.

Most of the Data extracted from the API was in the form of a list. So we had to un-nest the data and convert it into data frame. We performed Data Cleaning, Data Wrangling and Tidying on the data extracted. We also performed data un-nesting on columns which had data present in them as a list. Post enriching our data with secondary data we checked for Null Values and dropped those records from our data frame. 

### ## Conclusion


The results from the Auto ML were superior than the simple ML model with hyperparameter tuning with grid search.
From the Auto ML confusion matrix, the total error across each class is 0.41. The RMSE is also very good at 0.56. We also observed Auto ML classified more cases correctly that the other 3 models.

As per the variable importance generated , the predictors that made the impact were: hTAway, hTHome

We used the Classification accuracy, confusion matrix(precision and Recall) and RMSE for evaluation.

