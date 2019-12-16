library(fitdistrplus)
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
library(ggrepel)
library(corrplot)
library(ggcorrplot)
library(reshape2)

registerDoFuture()
plan(multiprocess, workers = availableCores() - 1)

options(datatable.fread.datatable = FALSE)


scale_fill_continuous <- scale_fill_viridis_c
scale_colour_continuous <- scale_colour_viridis_c

scale_fill_discrete <- scale_fill_viridis_d
scale_colour_discrete <- scale_colour_viridis_d

data_raw <- fread(file = "data/StudentsPerformance.csv")
data_raw <-
  data_raw %>% mutate(pedubin = pedu %in% c('bachelor\'s degree', 'master\'s degree'))

j_w = 3
j_h = 1e-3

dp1 <-
  ggplot(data = data_raw) + geom_density(aes(x = math, fill = gender), alpha = 0.3) + geom_jitter(aes(x = math, y = 0), width = j_w, height = j_h)
dp2 <-
  ggplot(data = data_raw) + geom_density(aes(x = math, fill = lunch), alpha = 0.3) + geom_jitter(aes(x = math, y = 0), width = j_w, height = j_h)
dp3 <-
  ggplot(data = data_raw) + geom_density(aes(x = math, fill = prep), alpha = 0.3) + geom_jitter(aes(x = math, y = 0), width = j_w, height = j_h)
dp4 <-
  ggplot(data = data_raw) + geom_density(aes(x = math, fill = pedubin), alpha = 0.3) + geom_jitter(aes(x = math, y = 0), width = j_w, height = j_h)
dp5 <-
  ggplot(data = data_raw) + geom_density(aes(x = math, fill = race), alpha = 0.1) + geom_jitter(aes(x = math, y = 0), width = j_w, height = j_h)
dp6 <-
  ggplot(data = data_raw) + geom_density(aes(x = math), alpha = 0.1) + geom_jitter(aes(x = math, y = 0), width = j_w, height = j_h)
ggarrange(dp1, dp2, dp3, dp4, dp5, dp6)

d <- data_raw

d <- d %>% top_frac(0.995, math)

densityplot(d$math)
densityplot((d$math))

rped <- d %>% group_by(race, pedu) %>% summarise(mean = mean(math))
ped <- d %>% group_by(race) %>% summarise(mean = mean(math))
r <- d %>% group_by(pedu) %>% summarise(mean = mean(math))

descdist(d$math)

fit.norm <- fitdist(d$math, "norm")
fit.weibull <- fitdist(d$math, "weibull")

plot(fit.weibull)
wbpboot <- bootdist(fit.weibull)

bscale <- wbpboot$estim$scale
bshape <- wbpboot$estim$shape

xr <- seq(1,100,0.5)

wbbootpdf <- lapply(xr, dweibull, shape = bshape, scale = bscale)
wbbootcdf <- lapply(xr, pweibull, shape = bshape, scale = bscale)
esshape <- fit.weibull$estimate['shape']
esscale <- fit.weibull$estimate['scale']

espdf <- dweibull(xr, shape = esshape, scale = esscale)
escdf <- pweibull(xr, shape = esshape, scale = esscale)
qupdf <- sapply(wbbootpdf, quantile, c(0.05,0.95))

plot_data <- data.frame(pdf = espdf, pdfupr = qupdf[2,], pdflwr = qupdf[1,], x = xr)

library(modeest)
mde <- weibullMode(esshape, esscale)
mde_y <- dweibull(mde, shape = esshape, scale = esscale)

ggplot(data = plot_data, aes(x = xr)) +
  geom_ribbon(aes(ymin = pdflwr, ymax = pdfupr), alpha = 0.5) +
  geom_histogram(data = d, aes(math, stat(density)), alpha = 0.3, color = 'black', binwidth = 5) +
  geom_line(aes(y = espdf), color = 'red', size = 1.5)


co <- cor(select(d, math, reading, writing))
mco <- melt(co)

ggplot(data = mco, aes(x = Var1, y = Var2, fill = value)) + geom_tile() + geom_text(aes(Var1, Var2, label =signif(value, 3)))
