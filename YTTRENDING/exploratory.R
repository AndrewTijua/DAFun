library(data.table)
library(tidyverse)
library(lubridate)
library(sentimentr)
library(reshape2)
library(modeest)


trending <- fread("data/USvideos.csv")
trending <- trending[, ':=' (times_trending = .N), by = video_id]
trending <- distinct_at(trending, 'video_id', .keep_all = TRUE)

trending$trending_date <- ydm(trending$trending_date)
trending$publish_time <- gsub('.{5}$', '', trending$publish_time)
trending$publish_time <- ymd_hms(trending$publish_time)
trending$tags <- gsub('[\"]', '', trending$tags)
trending$tags <- gsub('[|]', ',', trending$tags)
trending$category_id <- as.factor(trending$category_id)

# trending %>%
#   group_by(comments_disabled, ratings_disabled) %>%
#   summarise(
#     lci = quantile(views, 0.05),
#     med = quantile(views, 0.5),
#     uci = quantile(views, 0.95),
#     mean = mean(views)
#   )

trending[, .(
  mean = mean(views),
  lci = quantile(views, 0.05),
  med = quantile(views, 0.5),
  uci = quantile(views, 0.95)
), by = c('comments_disabled', 'ratings_disabled')]

title_sentences <- get_sentences(trending$title)
title_sentiment <- sentiment(title_sentences)
#title_sentiment <- title_sentiment %>% group_by(element_id) %>% summarise(total_sentiment = sum(sentiment))
title_sentiment <-
  title_sentiment[, .(total_sentiment = sum(sentiment)), by = element_id]
trending$title_sentiment <- title_sentiment$total_sentiment
trending <- cbind(trending, trending[, .(ts_pos = title_sentiment > 0)])

#trending <- trending %>% mutate(ld_ratio = likes / dislikes)
trending <- trending[, ld_ratio := likes / dislikes]
trending[ld_ratio %in% c(NA, NaN, Inf), 'ld_ratio'] <- 0

# trending %>%
#   group_by(title_sentiment > 0) %>%
#   summarise(
#     lci = quantile(views, 0.05),
#     med = quantile(views, 0.5),
#     uci = quantile(views, 0.95),
#     mean = mean(views)
#   )

trending[, .(
  mean = mean(views),
  lci = quantile(views, 0.05),
  med = quantile(views, 0.5),
  uci = quantile(views, 0.95)
), by = title_sentiment > 0]

trending[, .(
  mean = mean(views),
  lci = quantile(views, 0.05),
  med = quantile(views, 0.5),
  uci = quantile(views, 0.95)
), by = ld_ratio > 1]

trending[, .(
  mean = mean(views),
  lci = quantile(views, 0.05),
  med = quantile(views, 0.5),
  uci = quantile(views, 0.95),
  mode = mlv(as.numeric(views), method = "l", bw = 0.2)
), by = month(trending_date)]

trending[, .(
  mean = mean(views),
  lci = quantile(views, 0.05),
  med = quantile(views, 0.5),
  uci = quantile(views, 0.95),
  mode = mlv(as.numeric(views), method = "l", bw = 0.2)
), by = week(trending_date)]

month_plot_dt <-
  trending[, views, .(month = as.factor(month(trending_date, label = TRUE)))]
week_plot_dt <-
  trending[, views, .(week = as.factor(week(trending_date)))]

ggplot(data = month_plot_dt, aes(y = views, x = month, fill = month)) +
  geom_violin(draw_quantiles = c(0.5, 0.95), size = 1) + coord_cartesian(ylim = c(0, quantile(month_plot_dt$views, 0.95)))

ggplot(data = week_plot_dt, aes(y = views, x = week, fill = week)) +
  geom_violin(draw_quantiles = c(0.5, 0.95), size = 1) + coord_cartesian(ylim = c(0, quantile(week_plot_dt$views, 0.75)))

ts_df <-
  trending[, .(views, title_sentiment, ts_pos = title_sentiment > 0)]

ggplot(data = ts_df, aes(y = views)) + geom_point(aes(x = title_sentiment))
ggplot(data = ts_df) + geom_density(aes(x = title_sentiment), bw = 0.03)

trending[, .(
  mean = mean(views),
  lci = quantile(views, 0.05),
  med = quantile(views, 0.5),
  uci = quantile(views, 0.95),
  mode = mlv(as.numeric(views), method = "l", bw = 0.2)
), by = 'category_id']

ggplot(data = trending, aes(y = views, x = category_id, fill = category_id)) +
  geom_violin(draw_quantiles = c(0.5, 0.95), size = 1) +
  coord_cartesian(ylim = c(0, quantile(trending$views, 0.75)))

cat_sen <- trending[, .(
  mean = mean(views),
  lci = quantile(views, 0.05),
  med = quantile(views, 0.5),
  uci = quantile(views, 0.95),
  mode = mlv(as.numeric(views), method = "l", bw = 0.2)
), by = c('category_id', "ts_pos")]

ggplot(data = trending, aes(
  y = views,
  x = as.factor(times_trending),
  fill = as.factor(times_trending)
)) +
  geom_violin(draw_quantiles = c(0.5, 0.95), size = 1) +
  coord_cartesian(ylim = c(0, quantile(trending$views, 0.75)))

ggplot(data = week_plot_dt, aes(x = week, y = views)) + geom_point()

ggplot(data = cat_sen, aes(y = mean)) + geom_point(aes(x = category_id, colour = ts_pos)) + geom_errorbar(aes(x = category_id, colour = ts_pos, ymin = lci, ymax = uci))
