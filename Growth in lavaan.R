setwd("C:/Users/Cory/Dropbox/Work/UO Coursework/T7-Fall 2016/R Club/SEM in Lavaan")

library(lavaan)

# Now we'll do some latent growth modeling. In an LGM, we model the intercept and slope as latent variables 
# with scores at different time points as the observed indicators (like parcels were in the last example).
# This has the advantage of allowing us to estimate variability in the slope
# (i.e., individual differences in change), and also to predict differences in the slope using 
# variables we might be interested in (here we're using gender because it was easy to 
# add to the dataset that I simulated for the last example). You can also use contunious variables
# to predict differences in the slope or intercept as well.

# Scores that we're modeling in LGM are observed rather than LVs, so we just have an average across parcels.
# There is a technique called curve of factors that models change among LVs, but lavaan does not seem to
# support that yet.

# This model is just an intercept
nogrowth.model <-'

int =~ 1*E_T1 + 1*E_T2 + 1*E_T3

'
fit.nogrowth.model<- growth(nogrowth.model, data=growth_data)
summary(fit.nogrowth.model, fit.measures=TRUE)

# This model is an intercept and a linear slope.
# Here, our pretend data is spaced 1 year apart, and so the loadings for the latent linear slope
# change by 1 unit across time. This could be any other values that make sense with the data
# E.g., if measurements took place every 4 years, we could have 0, 4, and 8 as loadings instead.

# Also note, we won't test a qudratic model here, because we only have three data points. If you wanted to,
# you would just define the loadings of that slope as the square of the linear slope's loadings (0, 1, 4 here)


lingrowth.model<-'
int =~ 1*E_T1 + 1*E_T2 + 1*E_T3
lin_s =~ 0*E_T1 + 1*E_T2 + 2*E_T3
'
fit.lingrowth.model <- growth(lingrowth.model, data=growth_data)
summary(fit.lingrowth.model, fit.measures=TRUE)

anova(fit.nogrowth.model, fit.lingrowth.model)

# Because we have significant variability in the linear slope, we'll see if gender predicts it by regressing
# a gender dummy code on the linear slope.
gendergrowth.model<-'
int =~ 1*E_T1 + 1*E_T2 + 1*E_T3
lin_s =~ 0*E_T1 + 1*E_T2 + 2*E_T3
int ~ gender
lin_s ~ gender
'
fit.gendergrowth.model <- growth(gendergrowth.model, data=growth_data)
summary(fit.gendergrowth.model, fit.measures=TRUE)

# Again, you can use this method to do much more complicated things if you're so inclined. You
# can see if continuos predictors predict change in the slope. You can see if time-varying predictors
# (i.e., things that change across time) predict changes in the slope. The one problem with lavaan is that
# it currently does not support a few of the more complicated analyses such as curve of factors,
# factor of curves, or random effects SEM (which is necessary if you have variability in space between
# measurement occassions). For those purposes, I believe you need to use another software (e.g., Mplus)
# for the time being. Also, if you see yourself using SEM in your own research, Sanjay teaches a great
# seminar in it, and I have heard the SEM classes offered by the Ed department are good as well.