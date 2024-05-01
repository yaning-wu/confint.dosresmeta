# Loading required packages
library(dosresmeta)
library(tidyverse)

# Loading example data
data(alcohol_cvd)

# Creating dosresmeta object
example_object <- dosresmeta(formula = logrr ~ dose, 
                  type = type, 
                  id = id, 
                  se = se, 
                  cases = cases, 
                  n = n, 
                  data = alcohol_cvd)

# Producing confidence intervals
confint(example_object) # with defaults
