#### Learn Mixed Effects Model with R
#### https://www.youtube.com/watch?v=VhMWPkTbXoY&t=1s

'''
Oats dataset
- 3 varieties
- 4 nitrogen concents
- 6 blocks, 3 plots each
-- 4 subpltos per plot


Purpose: identify fixed vs random effects


Use nlme, lme
'''
library(nlme)
data(Oats)
str(Oats)
plot(Oats)

# assume to use linear model, yield as response variable
model1=lm(yield~Variety*nitro, data=Oats)
summary(model1)

# same model use mixed effects model, specify random effects
# random as "~1", block
model2=lme(yield~Variety*nitro, data=Oats, random=~1|Block/Variety/nitro)
# Problem: saturated with random effects
model2=lme(yield~Variety*nitro, data=Oats, random=~1|Block/Variety)
summary(model2)
'''
Linear mixed-effects model fit by REML (restricted maximum likelihood)
 Data: Oats 
       AIC      BIC    logLik  # used when comparing different models
  581.2372 600.9441 -281.6186

Random effects:
 Formula: ~1 | Block
        (Intercept)
StdDev:    14.64485

 Formula: ~1 | Variety %in% Block
        (Intercept) Residual
StdDev:    10.39931 12.99039

Fixed effects: yield ~ Variety * nitro 
                            Value Std.Error DF   t-value p-value
(Intercept)              81.90000  8.570709 51  9.555802  0.0000
VarietyMarvellous         8.51667  8.684675 10  0.980655  0.3499
VarietyVictory           -8.60000  8.684675 10 -0.990250  0.3454
nitro                    75.33333 11.858549 51  6.352660  0.0000
VarietyMarvellous:nitro -10.75000 16.770521 51 -0.641006  0.5244
VarietyVictory:nitro      5.75000 16.770521 51  0.342864  0.7331
 Correlation: 
                        (Intr) VrtyMr VrtyVc nitro  VrtyM:
VarietyMarvellous       -0.507                            
VarietyVictory          -0.507  0.500                     
nitro                   -0.415  0.410  0.410              
VarietyMarvellous:nitro  0.294 -0.579 -0.290 -0.707       
VarietyVictory:nitro     0.294 -0.290 -0.579 -0.707  0.500

Standardized Within-Group Residuals:
        Min          Q1         Med          Q3         Max 
-1.78878616 -0.64954437 -0.06301233  0.57818020  1.63463799 

Number of Observations: 72
Number of Groups: 
             Block Variety %in% Block 
                 6                 18 
'''

## Compare models
# Parameters don't change between model1 and model2
# Std Error estimates, affected by including the random effects
coef(model1)
coef(model2)

plot(ranef(model2)) # plot random effect
plot(model2) # plot residual to inspect Heteroscedasticity