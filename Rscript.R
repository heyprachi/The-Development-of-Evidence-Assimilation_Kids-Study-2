# --------------------------------------------------------------------------------------------------
# The development of evidence assimilation in a motivated reasoning context
# --------------------------------------------------------------------------------------------------
# Authors: Prachi Solanki and Zach Horne
# Experiment: Experiment 2
# Subject pool: Children's Museum of Phoenix
# Details about subject recruitment: 
# Children are between the ages of approximately 4 and 12 years old
# All children who want to participate in the study are allowed to if given permission by their parents
# This is a requirement of the Children's Museum of Phoenix
# --------------------------------------------------------------------------------------------------

require(brms)
require(rstan)
require(Rcpp)
require(ggplot2)
require(scales)
require(gridExtra)
require(tidyverse)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# --------------------------------------------------------------------------------------------------
# 2. Excluding children who failed to complete the study
# --------------------------------------------------------------------------------------------------



data1 <- data %>%
  filter(ClickThrough == "No")

# --------------------------------------------------------------------------------------------------
# 3. Computing the reduced model -- no effect of condition
# --------------------------------------------------------------------------------------------------

##reduced model

model0 <- brm(Judgment ~ (1|Item) + (1|Subject),
              data=data1,family="bernoulli", chains=4, 
              iter=4000, warmup=2000, control = list(adapt_delta = .90), save_all_pars=TRUE)
summary(model0)

# --------------------------------------------------------------------------------------------------
# 4. Computing the full model to test whether kids respond to different amounts of evidence
# --------------------------------------------------------------------------------------------------


model1 <- brm(Judgment ~ Condition + (1|Item) + (1|Subject),
              data=data1,family="bernoulli", chains=4, 
              iter=4000, warmup=2000, control = list(adapt_delta = .90), save_all_pars = TRUE,
              prior = c(set_prior("normal(0,1)", class = "b")))

summary(model1)

# --------------------------------------------------------------------------------------------------
# 5. Performing model comparison between full and reduced model 
# --------------------------------------------------------------------------------------------------

bayes_factor(model1,model0)

h0<- bridge_sampler(model0)
h1<- bridge_sampler(model1)

post_prob(h0,h1)

# --------------------------------------------------------------------------------------------------
# 6. Testing whether children of different ages show an overall preference for choosing the badge
# --------------------------------------------------------------------------------------------------


model2 <- brm(Judgment ~ Condition + Age + (1|Item) + (1|Subject),
              data=data1,family="bernoulli", chains=4, 
              iter=4000, warmup=2000, control = list(adapt_delta = .90), save_all_pars=TRUE,
              prior = c(set_prior("normal(0,.5)", class = "b")))

summary(model2)


# --------------------------------------------------------------------------------------------------
# 7. Performing model comparison between model2 (which includes Age effect) and model 1 (which does not)
# --------------------------------------------------------------------------------------------------
bayes_factor(model2,model1)

h1<- bridge_sampler(model1)
h2<- bridge_sampler(model2)

post_prob(h1,h2)

# --------------------------------------------------------------------------------------------------
# 8. Testing whether there is an age*condition interaction -- see preregistered hypothesis
# --------------------------------------------------------------------------------------------------

model3 <- brm(Judgment ~ Condition + Age + Age*Condition + (1|Item) + (1|Subject),
              data=data1,family="bernoulli", chains=4, 
              iter=4000, warmup=2000, control = list(adapt_delta = .90), save_all_pars=TRUE,
              prior = c(set_prior("normal(0,1)", class = "b")))

# Don't interpret main effects Condition and Age because uncentered

summary(model3)

# --------------------------------------------------------------------------------------------------
# 9. Performing model comparison between model 3 (which has age*condition interaction) and model 2 (which doesn't)
# --------------------------------------------------------------------------------------------------

bayes_factor(model3,model2)

h2<- bridge_sampler(model2)
h3<- bridge_sampler(model3)

post_prob(h2,h3)
# --------------------------------------------------------------------------------------------------
# 10. Testing the effect of memory
# --------------------------------------------------------------------------------------------------

model4 <- brm(Judgment ~ Condition + MemoryAccuracy + Condition*MemoryAccuracy + (1|Item) + (1|Subject),
              data=data1,family="bernoulli", chains=4, 
              iter=4000, warmup=2000, control = list(adapt_delta = .90), save_all_pars = TRUE,
              prior = c(set_prior("normal(0,1)", class = "b")))

summary(model4)

# --------------------------------------------------------------------------------------------------
# 11. Testing whether memory for the relevant evidence is worse in the half condition
# Note to also run corresponding reduced models for model comparison
# --------------------------------------------------------------------------------------------------

model5 <- brm(MemoryAccuracy ~ Condition + (1|Item) + (1|Subject),
              data=data1,family="bernoulli", chains=4, 
              iter=4000, warmup=2000, control = list(adapt_delta = .90), save_all_pars = TRUE,
              prior = c(set_prior("normal(0,1)", class = "b")))

summary(model5)

# --------------------------------------------------------------------------------------------------
# 12. Testing whether memory for the relevant evidence is worse in the half condition, specifically for younger children
# Note to also run corresponding reduced models for model comparison
# --------------------------------------------------------------------------------------------------
model6 <- brm(Judgment ~ Condition + MemoryAccuracy + Condition*MemoryAccuracy*Age + (1|Item) + (1|Subject),
              data=data1,family="bernoulli", chains=4, 
              iter=4000, warmup=2000, control = list(adapt_delta = .90), save_all_pars = TRUE,
              prior = c(set_prior("normal(0,2)", class = "b")))

summary(model6)
