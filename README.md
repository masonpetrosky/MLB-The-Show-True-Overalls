# MLB-The-Show-True-Overalls
This repository houses code and files used to find "True Overalls" for players in MLB The Show. Note that within the MLB The Show videogame, players' overall ratings have no decimal points, so a 98 overall player could be anywhere from a 97.51 to a 98.49, but these differences actually really matter for performance. More importantly, the overall within the game caps at 99, but some players have stats that would far exceed this. This "True Overall" allows us to say, for example, that a player should really be a 107.15 overall because their stats are much better.

Instructions:
1. The sample dataset "Catcher.csv" can be used for this example. Note that 99 overalls should not be included when training the model, as they are capped and do not reflect a true relationship for the purposes of the model.
2. Use the "CV.xlsx" file to begin with the full model using the cross validation code provided within the "TheShowCode.R" file.
3. Remove one variable at a time until the cross validated error no longer improves by removing a variable.
4. Use new model to calculate "True Overalls" for the entire population.
