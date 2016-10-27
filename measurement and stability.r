# Don't forget to change your working directory below
setwd("C:/Users/Cory/Dropbox/Work/UO Coursework/T7-Fall 2016/R Club/SEM in Lavaan")

# If you haven't used lavaan before, you will need to uncomment the install.packages line below

#install.packages("lavaan")
library(lavaan)

# This is the first dataset that we'll work with. The data were generated using simsem, which John will cover
# in more detail soon.
# These data are scores on Extraversion across three years, once per year.
# While they are simulated data, some of the population parameters to simulate the data come from an actual
# study, so this should be relatively close to what you might see in real data

# This dataset contains 10 variables (only 9 that are particularly interesting, one, 'x', is just an index for
# participants)
# These 9 variables consist of 3 extraversion parcels at 3 time points.
# A parcel is just an average of a set of items.
# There are reasons to use and not use parcels, but they make sense in the present
# context because we're not particulary concerned with the quality of single items.
# If you need to know more about parcels, see Little, Cunningham, Shahar, & Widaman, 2002

# The basic idea here is that we have 3 indicators of extraversion at each time point,
# which is the minimum* # of indicators you need to define a latent variable.
# *this is not perfectly true - if interested, I can explain how you can run it with 2

# In the following, we'll first test measurement models of extraversion, and then test
# two different models of stability of Extraversion across time.

#Load up the data
stability_data <- read.csv("stability_simdata.csv")

# First, we need to specify the model. In lavaan, we do this by creating a text object that lavaan
# can interpret. 

# A quick note about notation we use to specify a model:
# =~ is used to define indicators of a latent variable
# ~ is a regression path
# ~1 is an intercept (the tilda is used bc it's just a special kind of regression path)
# ~~ b/w 2 vars (e.g., x ~~ y) is a covariance (the standardized parameter is the correlation)
# ~~ when variables on both sides are the same (e.g., x ~~ x) is a variance.
# a number multiplied by a variable constrains that path to be equal to that number
# for example the 1*E_T1_P1 below constrains that loading to 1; a 0 will constrain it to 0.
# Using a letter instead of a number constrains a value to be equal to that letter.
# We can utilize this for equality constraints (more on this very soon)

# Notation not used today (but that you might want in the future):
# := is a defined parameter; most often used for indirect effects in mediation
# == can be used for equality constraints (if you don't want to use letters, or for more complex equality
# constraints; e.g., path_a == 2*sqrt(path_b))

# The first model we're going to run is a very simple model. We're simply going to define 3 latent variables
# which represent extraversion at each time point. We define these as consisting of the three parcels for
# each time point. The first parcel is set to 1 (it's called the scaling indicator) - this is a default
# We're also going to tell it to estimate factor intercepts, because we need those later. We'll also set
# factor covariances to be equal to make standardized estimates a bit easier to interpret.
uneq.meas.model <-'
E_T1 =~ 1*E_T1_P1 + E_T1_P2 + E_T1_P3
E_T2 =~ 1*E_T2_P1 + E_T2_P2 + E_T2_P3
E_T3 =~ 1*E_T3_P1 + E_T3_P2 + E_T3_P3

E_T1 ~~ LV_V*E_T1
E_T2 ~~ LV_V*E_T2
E_T3 ~~ LV_V*E_T3

E_T1~1
E_T2~1
E_T3~1
'
# Now we fit the object using the 'sem' function (note: we could use CFA instead, but in this particular 
# case they give us the same result). Mean structure = TRUE is set because we are going to be working with
# The means, and not just the variances and covariances (which is all you need to do a vanilla SEM)
fit.uneq.meas.model <-sem(uneq.meas.model, data=stability_data, meanstructure = TRUE)


summary(fit.uneq.meas.model, fit.measures=TRUE, standardized=TRUE)

# Okay, now we're going to going to test a type of measurement model called 
# a strict measurement invariance model. In this case, we are testing a longitudinal invariance model
# since we are working with a hypothetical longitudinal dataset.
# The idea here is that we constrain all of the measurement properties to be equal across time points
# testing whether or not the measurement quality is the same at each administration.
# Often, we would test different levels of measurement invariance one at a time, but right now we're
# just going to go 0 - 100 and test strict off the bat.
# Strict measurement invariance means that loadings, intercepts, and error variances are the same
# across time.
eq.meas.model <-'
E_T1 =~ 1*E_T1_P1 + L_P2*E_T1_P2 + L_P3*E_T1_P3
E_T2 =~ 1*E_T2_P1 + L_P2*E_T2_P2 + L_P3*E_T2_P3
E_T3 =~ 1*E_T3_P1 + L_P2*E_T3_P2 + L_P3*E_T3_P3

E_T1_P1~~evp1*E_T1_P1
E_T2_P1~~evp1*E_T2_P1
E_T3_P1~~evp1*E_T3_P1
E_T1_P2~~evp2*E_T1_P2
E_T2_P2~~evp2*E_T2_P2
E_T3_P2~~evp2*E_T3_P2
E_T1_P3~~evp3*E_T1_P3
E_T2_P3~~evp3*E_T2_P3
E_T3_P3~~evp3*E_T3_P3

E_T1_P1~intp1*1
E_T2_P1~intp1*1
E_T3_P1~intp1*1
E_T1_P2~intp2*1
E_T2_P2~intp2*1
E_T3_P2~intp2*1
E_T1_P3~intp3*1
E_T2_P3~intp3*1
E_T3_P3~intp3*1

E_T1 ~~ LV_V*E_T1
E_T2 ~~ LV_V*E_T2
E_T3 ~~ LV_V*E_T3

E_T1~1
E_T2~1
E_T3~1
'
fit.eq.meas.model <-sem(eq.meas.model, data=stability_data, meanstructure = TRUE)
summary(fit.eq.meas.model, fit.measures=TRUE, standardized=TRUE)

# Finally, we can test the two models against eachother, and see which fits the data best.
anova(fit.uneq.meas.model, fit.eq.meas.model)

# Okay, now we're going to test the first stability model. In this model, stability is equal across lags
# of the same amount of time (1 year), but not equal across different amounts of time (i.e., 2 year lags
# are allowed to differ from 1 year lags). This model can be considered modeling decay in stability, 
# in that stability can decrease (i.e., decay) over longer intervals. 
# Note that we have retained the strict measurement invariance model above because it fit the data adequately.

stability.decay.model <-'
E_T1 =~ 1*E_T1_P1 + L_P2*E_T1_P2 + L_P3*E_T1_P3
E_T2 =~ 1*E_T2_P1 + L_P2*E_T2_P2 + L_P3*E_T2_P3
E_T3 =~ 1*E_T3_P1 + L_P2*E_T3_P2 + L_P3*E_T3_P3

E_T1_P1~~evp1*E_T1_P1
E_T2_P1~~evp1*E_T2_P1
E_T3_P1~~evp1*E_T3_P1
E_T1_P2~~evp2*E_T1_P2
E_T2_P2~~evp2*E_T2_P2
E_T3_P2~~evp2*E_T3_P2
E_T1_P3~~evp3*E_T1_P3
E_T2_P3~~evp3*E_T2_P3
E_T3_P3~~evp3*E_T3_P3

E_T1_P1~intp1*1
E_T2_P1~intp1*1
E_T3_P1~intp1*1
E_T1_P2~intp2*1
E_T2_P2~intp2*1
E_T3_P2~intp2*1
E_T1_P3~intp3*1
E_T2_P3~intp3*1
E_T3_P3~intp3*1

E_T1 ~~ LV_V*E_T1
E_T2 ~~ LV_V*E_T2
E_T3 ~~ LV_V*E_T3

E_T1~1
E_T2~1
E_T3~1

E_T1 ~~ lag1*E_T2
E_T2 ~~ lag1*E_T3
E_T1 ~~ lag2*E_T3
'
fit.stability.decay.model <- sem(stability.decay.model, data=stability_data, meanstructure = TRUE)
summary(fit.stability.decay.model, fit.measures=TRUE, standardized=TRUE)

# This model is similar to the last, but constrains the estimates of stability across different lengths
# of time to be equal. This model is consistent with stability remaining constant across time. 

stability.constant.model <-'
E_T1 =~ 1*E_T1_P1 + L_P2*E_T1_P2 + L_P3*E_T1_P3
E_T2 =~ 1*E_T2_P1 + L_P2*E_T2_P2 + L_P3*E_T2_P3
E_T3 =~ 1*E_T3_P1 + L_P2*E_T3_P2 + L_P3*E_T3_P3

E_T1_P1~~evp1*E_T1_P1
E_T2_P1~~evp1*E_T2_P1
E_T3_P1~~evp1*E_T3_P1
E_T1_P2~~evp2*E_T1_P2
E_T2_P2~~evp2*E_T2_P2
E_T3_P2~~evp2*E_T3_P2
E_T1_P3~~evp3*E_T1_P3
E_T2_P3~~evp3*E_T2_P3
E_T3_P3~~evp3*E_T3_P3

E_T1_P1~intp1*1
E_T2_P1~intp1*1
E_T3_P1~intp1*1
E_T1_P2~intp2*1
E_T2_P2~intp2*1
E_T3_P2~intp2*1
E_T1_P3~intp3*1
E_T2_P3~intp3*1
E_T3_P3~intp3*1

E_T1 ~~ LV_V*E_T1
E_T2 ~~ LV_V*E_T2
E_T3 ~~ LV_V*E_T3

E_T1~1
E_T2~1
E_T3~1

E_T1 ~~ c*E_T2
E_T2 ~~ c*E_T3
E_T1 ~~ c*E_T3

'
fit.stability.constant.model <- sem(stability.constant.model, data=stability_data, meanstructure = TRUE)
summary(fit.stability.constant.model, fit.measures=TRUE, standardized=TRUE)

# And now we can test the models against each other:
anova(fit.stability.decay.model, fit.stability.constant.model)
