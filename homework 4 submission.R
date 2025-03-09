## Homework 4 Submission ## 

## Load packages:
library(MASS)
library(tidyverse)
library(marginaleffects)
library(modelr)
library(pROC)

## Question 1:

mistletoe <- read.csv("mistletoes.csv")
head(mistletoe)

## 1a)

# The response variable (seedlings) is discrete and bounded between 0 and infinity (count data).
# For this research question, it looks like the only predictor is treatment, which is categorical.
# I could use Poisson or Negative Binomial distributions for this data. Between the two, I will choose Nbin, as it is able to capture all shapes that Poisson can but I do not have to worry about overdispersion.

# the glm.nb() function from the MASS package automatically fits a Nbin GLM using the log link function without having to specify family or link in the model.

# Fit model
seedling_mod <- glm.nb(Seedlings ~ Treatment, data = mistletoe)
summary(seedling_mod)

# Create predictions from model. Because the model is on the log scale, I need to convert predictions to the scale of the response so they are comparable.
mistletoe$yhat <- exp(predict(seedling_mod, mistletoe))

# create MAE function
MAE <- function(observed, yhat) {
  (mean(abs(observed - yhat)))
}

# Calculate model fit using MAE
MAE(observed = mistletoe$Seedlings, yhat = mistletoe$yhat) # MAE = 145.8
mean(mistletoe$Seedlings) # 160.6 seedlings
hist(mistletoe$Seedlings)

range(mistletoe$Seedlings[mistletoe$Treatment == "parasitized"]) #0 - 2472
range(mistletoe$Seedlings[mistletoe$Treatment == "unparasitized"])  #0 - 209

hist(mistletoe$Seedlings[mistletoe$Treatment == "parasitized"])
hist(mistletoe$Seedlings[mistletoe$Treatment == "unparasitized"])
#Huge difference in range between the treatments, but both treatments have the highest count of 0 by far.

# MAE = 145.8. This can be interpreted the model's average error in predicting the number of seedlings is about 146 seedlings off. This is quite high, especially considering that value is close to the average count of seedlings in our dataset. I believe the large MAE is due to the handful of extreme seedling counts in our dataset; because MAE is the absolute average error, these counts will inflate the average error within our dataset.

# Because RMSE gives higher weight to larger errors, I think the RMSE value should be much higher than the MAE value. I will calculate RMSE to test my idea that the high MAE is due to the extreme counts in our dataset and is not due to error in my code.
rmse <- sqrt(mean((mistletoe$Seedlings - mistletoe$yhat)^2))
print(rmse) #301

# The RMSE value is over double my MAE value, which I believe supports my suspicions above.
# I will conclude that this model is poorly fitting.


## 1b)

plot_predictions(seedling_mod, condition = "Treatment")
# I can visualize here that the predicted seedlings for parasitized are much higher than the prediction for unparasitized treatment, although there is very high error for parasitized trees, as represented by the large confidence interval.

predictions(seedling_mod, newdata = data.frame(Treatment = c("parasitized", "unparasitized")))
# These predicted values should be on the scale of the response variable, and should reflect the parameter estimates of the model summary after converting those values back to the scale of the predictor.

#checking
exp(5.7308)
exp(5.7308-3.1575)
#correct

#slope on scale of response
exp(-3.1575) #0.04

# My analyses indicate that the number of predicted seedlings underneath parasitized and unparasitized trees is about 308 seedlings (95% CI = 218.6 - 434.6) and 13 seedlings (95% CI = 9.3 - 18.6), respectively. The predicted mean seedling count under unparasitized trees is about 4% that of parasitized trees. These results support the alternative hypothesis that mistletoe infection alters seedling density. However, the MAE value of 145 suggests there is a lot of error in these predictions due to the high variability in seedling counts under trees, particularly for that of parasitized trees, which is also represented by the large confidence interval. There are likely other factors that are affecting the number of seedlings under parasitized/unparasitized trees, and our model representing the predictor of mistletoe treatment alone is a poor fit for predicting seedling count under trees.


## 1c) 

# New model with an interaction term with year
mistletoe$Year <- as.factor(mistletoe$Year)
seedling_mod2 <- glm.nb(Seedlings ~ Treatment*Year, data = mistletoe)
summary(seedling_mod2)
# There is not a significant interaction between treatment and year according to the p value (P = 0.0599), but it is very close

mistletoe$yhat2 <- exp(predict(seedling_mod2, mistletoe))
MAE(observed = mistletoe$Seedlings, yhat = mistletoe$yhat2) #140
# MAE is very close to the last value, but slightly lower.

plot_predictions(seedling_mod2, condition = c("Treatment", "Year"))
# Seedlings are higher in 2012

predictions(seedling_mod2, newdata = data.frame(Treatment = c("parasitized", "parasitized", "unparasitized", "unparasitized"),
                                         Year = c("2011", "2012", "2011", "2012")))
# CI for the two parasitized treatments in 2011/2012 overlap each other, but not for the unparasitized treatments between years. There is a significant effect of year on seedlings for the unparasitized group but not the parasitized group.
#new slope on scale of response for unparasitized treatment in this new model
exp(-3.8178) #0.022

#In conclusion, there is technically not a significant effect of year on treatment, as the interaction term has a p value above 0.05, although just barely. The confidence intervals of parasitized treatments between the two years overlap quite a lot, suggesting a large amount of uncertaintly around the estimates. However, the MAE is slightly lower for the second model, indicating slightly less average error. There are likely other factors that can help to better explain seedlings under parasitized and unparasitized trees.




## Question 2:

## 2a)
treemortality <- read.csv("treemortality.csv")
head(treemortality)

thinningmod <- glm(mortality ~ thinning, data = treemortality, family = binomial (link="logit"))
summary(thinningmod)

int <- summary(thinningmod)$coefficients["(Intercept)", "Estimate"] # 0.99
slope <- summary(thinningmod)$coefficients["thinning", "Estimate"] # -1.855

plogis(int) #0.73
plogis(slope) #0.135
plogis(int + (int+slope*1)) #0.53


plot_predictions(thinningmod, condition="thinning") + 
  ylab("Probability of tree mortality") +
  xlab("Forest thinning treatment") +
  theme_bw()

thinningpreds <- predictions(thinningmod, 
                     newdata = datagrid(thinning = c("0", "1")))
                     
#Model fit in binomial glms: ROC from pROC package
test_prob <- predict(thinningmod, type = "response")
test_roc <- roc(treemortality$mortality # Actual mortality data
                ~ test_prob, plot = TRUE,  # Test probabilities
                print.auc = TRUE)

test_roc #AUC: 0.71

# Probability of tree mortality in unthinned vs. thinned forests is 73% (95% CI: 0.683 - 0.771) and 29.7% (95% CI: 0.261 - 0.335), respectively; the probability of tree mortality drops about 53% when forests are thinned. Because small and large trees were sampled equally across thinned and unthinned forests, the affect of tree size on mortality is controlled for. The AUC of 0.71 indicates this model is able to explain the relationship between forest thinning and tree mortality better than random chance. The AUC, along with fairly small confidence intervals around the estimates and a lack of confidence intervals overlapping, indicate this model is able to explain the affect of forest thinning on tree mortality fairly well and supports the hypothesis that thinning decreases the probability of tree mortality in wildfire.


## 2b)
# Given that the researchers controlled for tree size by sampling small and large trees equally in thinned vs unthinned plots, tree size does not need to be included in the model because it is being controlled for by the study design. If the researchers had not considered tree size prior to sampling and/or had not randomized the sampling by tree size, then including tree size in the model would be necessary.


## 2c) 
thinningmod2 <- glm(mortality ~ thinning + roaddist + slope, data = treemortality, family = binomial (link="logit"))
summary(thinningmod2)

int2 <- summary(thinningmod2)$coefficients["(Intercept)", "Estimate"] # -22.99
slope2 <- summary(thinningmod2)$coefficients["thinning", "Estimate"] # -0.92
sloperoad <- summary(thinningmod2)$coefficients["roaddist", "Estimate"] #0.54
slopeslope <- summary(thinningmod2)$coefficients["slope", "Estimate"] #0.82

plogis(int2) #1.031735e-10
#probability of mortality in unthinned forests with a slope and road distance of 0. Not relevant to the system/research question

plogis(int2 + slope2*0 + sloperoad*(mean(treemortality$roaddist)) + 
         slopeslope*(mean(treemortality$slope))) #0.53
#probability of mortality in unthinned forests with slope and road distance held at the mean. Should reflect what plot_predictions shows me

plogis(slope2) #0.29
plogis(int2 + slope2*1 + sloperoad*(mean(treemortality$roaddist)) + 
         slopeslope*(mean(treemortality$slope))) #0.31
#probability of mortality in THINNED forests with slope and road distance held at the mean. Should reflect what plot_predictions shows me


plot_predictions(thinningmod2, condition= "thinning") + 
  ylab("Probability of tree mortality") +
  xlab("Forest thinning treatment") +
  theme_bw()
#This plot is showing me the effect of thinning on tree mortality while holding road distance and slope at their means

thinningpreds2 <- predictions(thinningmod2, 
                             newdata = datagrid(thinning = c("0", "1")))

#Model fit in binomial glms: ROC from pROC package
test_prob2 <- predict(thinningmod2, type = "response")
test_roc2 <- roc(treemortality$mortality # Actual mortality data
                ~ test_prob2, plot = TRUE,  # Test probabilities
                print.auc = TRUE)

test_roc2 #AUC: 0.96

(53-73)/73 #27% drop -unthinned
(31-29.7)/29.7 #4% raise -thinned
53 - 31 #22% drop in thinned forests for second model

# The probability of tree mortality with the second model that accounts for the confounding variables of road distance and slope dropped 27%, from 73% to 53% (95% CI: 0.443 - 0.613) in unthinned forests, and raised 4% in thinned forests between the first and second model from 29.7% to 31% (95% CI: 0.25 - 0.36).  Accounting for slope and road distance dramatically changed the estimate of tree mortality in unthinned forests, but did not change the estimate in thinned forests much. With this second model accounting for confounding variables, the probability of tree mortality drops about 22% when forests are thinned, compared to a drop of 53% from the first model. The effect of thinning on tree mortality is therefore smaller in the second model than the first, because the first model was assigning the effects of slope and road distance on tree mortality to the thinning treatment and therefore inflating the estimate. The AUC for the first model was 0.71 and the AUC for the second model is 0.96, indicating the second model explains variation in tree mortality better than the first model.


