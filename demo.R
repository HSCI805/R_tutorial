
# Borrowed from "Basic epi hacks in R"

# generate some random data
set.seed(8675309)
exposure <- rbinom(100,1,0.25) # ~25% of participants are exposed


mu <- exposure # predicted outcome (linear)
p <- 1/(1+exp(mu)) # squish into 0 - 1
casecontrol <- rbinom(100,1,p) # predict cases

#these are raw data
df <- data.frame(cbind(exposure,casecontrol))

#we can put these data into a 2x2 table
dt <- table(df$exposure,df$casecontrol)

#look at the 2by2 table
dt

# logistic regression
out <- glm(df$casecontrol ~ df$exposure,family=binomial(link='logit'))
summary(out)

#structure of output
coef(summary(out))

#odds ratios
exp(coef(summary(out))[, 1][2])

#standard error of odds ratio
exp(coef(summary(out))[, 2][2])

#lower bound
exp(coef(summary(out))[, 1][2] - 1.96*coef(summary(out))[, 2][2])

#upper bound
exp(coef(summary(out))[, 1][2] + 1.96*coef(summary(out))[, 2][2])

#annual probability of some adverse health event in two groups
absolute_risk_exposed <- 0.00121
absolute_risk_unexposed <- 0.000819

#the increased risk associated with being in the exposed group
relative_risk <- absolute_risk_exposed/absolute_risk_unexposed

#the amount of risk that exposure is responsible for
attributable_risk <- (absolute_risk_exposed - absolute_risk_unexposed)*10000

#fraction of risk in exposed
risk_fraction <- (absolute_risk_exposed - absolute_risk_unexposed)/absolute_risk_exposed


 
