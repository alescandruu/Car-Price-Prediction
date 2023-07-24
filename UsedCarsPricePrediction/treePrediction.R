library(rsample)
library(tidyverse)
library(dplyr)
library(rpart)
library(rpart.plot)
library(caret)

carData <- read_csv("Dataset/audi.csv")
carData <- carData %>% 
  select(-model)


carData %>%
  select_if(is.numeric) %>%
  gather(metric, value) %>%
  ggplot(aes(value, fill=metric)) +
  geom_density(show.legend = FALSE) +
  facet_wrap(~metric, scales = "free")

set.seed(123)
carData_split <- initial_split(carData, prop = 0.7)
carData_train <- training(carData_split)
carData_test <- testing(carData_split)

carData %>%
  ggplot(aes(price)) +
  geom_density()

model1 <- rpart(
  formula = price ~ .,
  data = carData_train,
  method = "anova"
)

print(model1)

model1
rpart.plot(model1)
plotcp(model1)
model1$cptable

hyper_grid <- expand.grid(
  minsplit = seq(5, 20, 1),
  maxdepth = seq(8, 15, 1)
)

head(hyper_grid)
models <- list()
for(i in 1:nrow(hyper_grid)) {
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  models[[i]] <- rpart(
    formula = price ~ .,
    data = carData_train,
    method = "anova",
    control = list(minsplit = minsplit, maxdepth = maxdepth)
  )
}

get_cp <- function(x) {
  min <- which.min(x$cptable[, "xerror"])
  cp <- x$cptable[min, "CP"]
}

get_min_error <- function(x) {
  min <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"]
}

mutated_grid <- hyper_grid %>%
  mutate(
    cp = purrr::map_dbl(models, get_cp),
    error = purrr::map_dbl(models, get_min_error)
  )

mutated_grid %>%
  arrange(error) %>%
  top_n(-5, wt=error)

optimal_model <- rpart(
  formula = price ~ .,
  data = carData_train,
  method = "anova",
  control = list(minsplit = 7, maxdepth = 15, cp = 0.01)
)

pred <- predict(optimal_model, newdata = carData_test)
print(RMSE(pred = pred, obs = carData_test$price))
