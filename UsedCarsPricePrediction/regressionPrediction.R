library(tidyverse)
library(caret)
library(dplyr)

carData <- read_csv("Dataset/audi.csv")

carData <- carData %>%
  mutate(mpg = 235.21 / mpg,
         engine = engine * 1000,
         mileage = mileage * 1.60934,
         tax = tax * 0.91,
         price = price * 0.91)

options(scipen = 100)

mod_price_year <- lm(data = carData, price ~ year)
summary(mod_price_year)

mod_price_engine <- lm(data = carData, price ~ engine)
summary(mod_price_engine)

mod_price_mileage <- lm(data = carData, price ~ mileage)
summary(mod_price_mileage)

mod_price_mpg <- lm(data = carData, price ~ mpg)
summary(mod_price_mpg)

mod_price_transmission <- lm(data = carData, price ~ transmission)
summary(mod_price_transmission)

mod_price_tax <- lm(data = carData, price ~ tax)
summary(mod_price_tax)

mod_price_fuel <- lm(data = carData, price ~ fuel)
summary(mod_price_fuel)

mod_price_all_params0 <- lm(data = carData, price ~ year + mileage + engine + mpg + tax + fuel + transmission)
summary(mod_price_all_params0)
 
mod_price_all_params <- lm(data = carData, price ~ year + mileage + engine + mpg + tax + fuel)
summary(mod_price_all_params)

mod_price_all_params_relations <- lm(data = carData, price ~ year + mileage + year * mileage + engine + mpg + engine * mpg + tax + fuel)
summary(mod_price_all_params_relations)

newCar <- tibble(
  model = "Audi S7",
  year = 2020,
  mileage = 10000,
  fuel = "Hybrid",
  tax = 200,
  mpg = 20,
  engine = 4000
)

predict(mod_price_all_params, newdata = newCar, interval = "confidence")
predict(mod_price_all_params_relations, newdata = newCar, interval = "prediction")
