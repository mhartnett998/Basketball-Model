# Basketball-Model
A basic predictive model for the outcomes of basketball games with additional gambling information

This model uses single game NBA box score data to predict the winners of daily matchups. Data are downloaded into R using tools from the "nbastatR" package (https://github.com/abresler/nbastatR). It works by fitting linear models to predict each teams offensive and defensive rating then perfroms a simulation study with 10000 replications to find a simulation based estimate of the probability of each team winning (and covering the spread if that information is inculded). Betting tools come from the package "bettoR" (https://github.com/papagorgio23/bettoR).

A few notes about the model performance

1: The model is fit using singe game data but the predictions are based off of teams season averages. This means that predicitons are inaccurate when teams are missing key players.

2: In order to save time running the model, box scores are stored in the file "bs.1.csv" and each time the model is run it adds the previous days games. The file inculded here is not a complete list of all NBA games played this season but it is large enough that the missing games are not likely to dramatically change parameter estimates. 
