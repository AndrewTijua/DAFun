library(plyr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(data.table)
library(viridis)
library(broom)
library(Hmisc)
library(doFuture)
library(e1071)
library(ggmosaic)

registerDoFuture()
plan(multiprocess, workers = availableCores() - 1)

options(datatable.fread.datatable = FALSE)

scale_fill_discrete <- scale_fill_viridis_d
scale_colour_discrete <- scale_colour_viridis_d

data_raw <- fread(file = "data/StudentsPerformance.csv")
data_raw <- data_raw %>% mutate(pedubin = pedu %in% c('bachelor\'s degree', 'master\'s degree'))

j_w = 3
j_h = 1e-3

dp1 <-
  ggplot(data = data_raw) + geom_density(aes(x = math, fill = gender), alpha = 0.3) + geom_jitter(aes(x = math, y = 0), width = j_w, height = j_h)# + facet_grid(~New.boiler)
dp2 <-
  ggplot(data = data_raw) + geom_density(aes(x = math, fill = lunch), alpha = 0.3) + geom_jitter(aes(x = math, y = 0), width = j_w, height = j_h)# + facet_grid(~Type)
dp3 <-
  ggplot(data = data_raw) + geom_density(aes(x = math, fill = prep), alpha = 0.3) + geom_jitter(aes(x = math, y = 0), width = j_w, height = j_h)# + facet_grid(~Loft.depth)
dp4 <-
  ggplot(data = data_raw) + geom_density(aes(x = math, fill = pedubin), alpha = 0.3) + geom_jitter(aes(x = math, y = 0), width = j_w, height = j_h)# + facet_grid(~Floor.area)
dp5 <-
  ggplot(data = data_raw) + geom_density(aes(x = math, fill = race), alpha = 0.1) + geom_jitter(aes(x = math, y = 0), width = j_w, height = j_h)# + facet_grid(~Loft.depth)
dp6 <-
  ggplot(data = data_raw) + geom_density(aes(x = math), alpha = 0.1) + geom_jitter(aes(x = math, y = 0), width = j_w, height = j_h)# + facet_grid(~Floor.area)
ggarrange(dp1, dp2, dp3, dp4, dp5, dp6)

ggplot(data = data_raw) + geom_mosaic(aes(x = product(pedubin, lunch), fill = lunch))
