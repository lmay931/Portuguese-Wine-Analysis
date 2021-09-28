wine.df <- read.csv('/Users/lawrence/Google Drive/DS/Portuguese Wine Quality/winequality-red.csv')

require(dplyr)
wine.df <- wine.df %>% mutate(quality = ifelse(quality %in% c(3, 4, 5), 0, 1))

head(wine.df)
lr <- glm(quality ~ fixed.acidity + volatile.acidity + 
            citric.acid + residual.sugar + chlorides + 
            free.sulfur.dioxide  + total.sulfur.dioxide + 
            density + pH + sulphates + alcohol, data = wine.df, family = "binomial")

summary(lr)

library(MuMIn)
best_models <- dredge(lr)
head(best_models)
summary(get.models(best_models, 1)[[1]])

lr_quad <- glm(quality ~ .^2, data = wine.df, family = "binomial")
step(lr_quad, direction = "both")
