
library(tidyverse)

set.seed(42)
random_normal <-rnorm(100, mean =10, sd =2)# Sample from a Normal distribution
random_poisson <-rpois(100, lambda =5)# Sample from a Poisson distribution
# Do they come from the same underlying distribution?
ks.test(random_normal, random_poisson)
