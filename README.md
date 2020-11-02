# NBA-Win-Projection-Model
A model to simulate NBA seasons with lineup performance data, and evaluate the effect of a play-in game if the NBA decides to take that route. The code scrapes opening lineups via RealGM, does a simple estimate of offensive and defensive ratings powered by Jacob Goldstein's Player Impact Plus-Minus, predicts minutes by league-wide trends, and predicts the likelihood of the home team winning based on projected team net ratings, rest, and travel distance, and is simulated 10,000 times. 

Teams' top three options have played a lower share of overall minutes in recent years, much in part to load management. This is accounted for in predicting minutes. 

![image](https://user-images.githubusercontent.com/23176357/97824482-49d13500-1c79-11eb-8da3-ac215b9cd95c.png)

Obviously hypothetical as the season was already played and the bubble existed after 2018-19, this research found that 70% of simulated 2018-19 Eastern Conference seasons and 68% of Western Conference seasons would have a play-in game if the NBA implemented a bubble style play-in if the difference in wins between the 8th and 9th seed was <= 2.  Of those simulations with a play-in game, the true 9th seed advanced 25% of the time, which meant overall a true 9th seed makes the playoffs 16-17% of conference seasons with a play-in game. 

![image](https://user-images.githubusercontent.com/23176357/97824626-b1878000-1c79-11eb-9f15-753c52f479a0.png)

I found that the rule gives a boost to the best net ratings teams in the conference, as they have a chance to win two play-in games over an inferior team in a poor season. The Celtics are a 2.1 net rating team, while the Nets are a -2.6 net rating by my projections. Therefore, the Celtics see a boost, while the Nets see a loss, as they are likelier to lose as the 8th seed.

![image](https://user-images.githubusercontent.com/23176357/97824835-44281f00-1c7a-11eb-9d19-ecc2f090bee9.png)
