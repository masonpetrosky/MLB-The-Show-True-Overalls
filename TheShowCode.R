# Below code creates model. See bottom code for cross validation using leave-one-out method (due to small quantity of data available).
# Import data table library and set working directory.
library(data.table)
setwd("~/TheShow")
# Remove scientific notation
options(scipen=999)
# Import data
train <- fread("Catcher.csv")
# Create linear model using all factors. Use CV to determine worst factor and remove from model. Repeat.
lm <- lm(Overall ~ ConR + ConL + PowR + PowL + Vis + Disc + Clt + Bunt + Dbunt + Dur + Fld + Arm + Acc + Reac + Blk + Spd + Steal + BrAgg, data = train)
# View final model parameters
summary(lm)

# See example below for another position (i.e., Starting Pitcher)
library(data.table)
setwd("~/TheShow")
options(scipen=999)
train <- fread("StartingPitcher.csv")
lm <- lm(Overall ~ Sta + H9 + K9 + BB9 + HR9 + Pclt + Ctrl + Vel + Brk + Fld + Arm + Acc + Reac + ConR + ConL + PowR + PowL + Vis + Disc + Clt + Bunt + Dbunt + Dur + Spd + Steal + BrAgg, data = train)
summary(lm)

# Cross validation code example with "Catcher" position above:
# Import data table library and set working directory.
library(data.table)
setwd("~/TheShow")
# Create an empty "fitted_value" variable so that we can append values to it to aggregate errors.
fitted_value <- NULL
# Reset squaredError to 0. This is necessary if running the function more than once to clear prior error values.
squaredError <- 0
# Create for loop to cover entire training dataset.
  for(i in 1:nrow(train)){
    # Set the "validation" amount to only include one value. This is the "ONE" of our "leave-ONE-out"
    validation <- train[i,]
    # Send the rest of the data to be included within "training"
    training <- train[-i,]
    # Train the model off of the training set.
    linearMod <- lm(Overall ~ ConR + ConL + PowR + PowL + Vis + Disc + Clt + Bunt + Dbunt + Dur + Fld + Arm + Acc + Reac + Blk + Spd + Steal + BrAgg, data = training)
    # Predict the validation overall based on the model created from the training set.
    fitted_value[i] <- predict(linearMod, validation)
    # Take the squared error between the actual value and our prediction. Add to the already aggregated squared error from prior iterations.
    squaredError <- squaredError + (fitted_value[i] - validation$Points)^2
  }
# Root mean squared error result  
sqrt(squaredError/nrow(train))