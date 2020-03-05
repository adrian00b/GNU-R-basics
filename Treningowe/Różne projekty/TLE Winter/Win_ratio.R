setwd("C:/Users/abrodowicz.ARDA/Desktop/Analizy/TLE Winter/")
library(openxlsx)
library(bigrquery)
library(tibble)
library(lattice)
library(latticeExtra) # combine two xyplot from lattice in one chart
library(dplyr)

wr_benchmark = read.xlsx('Assumptions/Winter Fest Balance.xlsx', sheet = 'Arkusz1')
wr_benchmark <- as_tibble(wr_benchmark)

project <- 'tinydragons-983ab'

win_ratio_sql <- "
  SELECT levels.level_no 
    , COUNTIF(levels.won)/COUNT(*) AS win_ratio
  , COUNT(*) AS sample
  FROM `tinydragons-983ab.analytics.sessions`, UNNEST(levels) levels
  WHERE levels.path = 'WinterEvent'
  GROUP BY levels.level_no
  ORDER BY 1
"

win_ratio_query <- bq_project_query(project, win_ratio_sql)
wr_real <- bq_table_download(win_ratio_query)

# all levels comparison

p1 <- xyplot(win_ratio ~ level_no
             , main = 'All levels benchmark vs real'
             , data = wr_benchmark
             , type = c('b', 'r')
             , pch = 16
             , lwd = 1.5
             , key = list(columns = 2
                          , text = list(lab = c('Benchmark', 'Real'))
                          , points = list(pch = 16, col = c('blue', 'firebrick')))
             )
p2 <- xyplot(win_ratio ~ level_no, data = wr_real, type = c('b', 'r'), pch = 16, col = 'firebrick', lwd = 1.5)
p1 + as.layer(p2)

# comparison for level_no <= 20 (sample about 10k for this levels)
wr_benchmark_20 <- filter(wr_benchmark, level_no <= 20)
wr_real_20 <- filter(wr_real, level_no <= 20)

p3 <- xyplot(win_ratio ~ level_no
             , main = 'First 20 levels benchmark vs real'
             , data = wr_benchmark_20
             , type = c('b', 'r')
             , pch = 16
             , lwd = 1.5
             , key = list(columns = 2
                          , text = list(lab = c('Benchmark', 'Real'))
                          , points = list(pch = 16, col = c('blue', 'firebrick'))
                          )
)
p4 <- xyplot(win_ratio ~ level_no, data = wr_real_20, type = c('b', 'r'), pch = 16, col = 'firebrick', lwd = 1.5)
p3 + as.layer(p4)

# gradient almost the same but the line is about 20 percent point below benchmark!!!

percentage_point_diff = tibble(level_no = wr_benchmark_20$level_no
                               , diff = wr_real_20$win_ratio - wr_benchmark_20$win_ratio)

xyplot( diff ~ level_no
  , data = percentage_point_diff
  , type = 'b'
  , pch = 16
  , panel = function(...){
    panel.xyplot(...)
    panel.abline(h = 0, lty = 'dashed')
    panel.abline(h = mean(percentage_point_diff$diff), col = 'indianred1')
    panel.text(20.5, mean(percentage_point_diff$diff) + 0.015, 'average', size = 14, cex = 0.9, col = 'indianred1')
  }
  , key = list(columns = 2
               , text = list(lab = c ('Benchmark', 'Average diff'))
               , lines = list(lty = c('dashed', 'solid'), col = c('black', 'indianred1'))
               , cex = 0.8
               )
)

# jeszcze jedno pytanie sie nasuwa - punkty procentowe okej sa zanizone, ale co innego zmienic z 90 na 70 win ratio
# a co innego z 50 na 30 ?? zbadac z rozkladem bernulliego o ile gier wiecej trzeba bylo rozegrac


# binomial distribution
# dbinom(x, size, prob)
# probability of exactly x succeses in size tries where probability of succes in each try = 0.5
dbinom(1, 1, prob = 0.5)

dbinom(2, 5, prob = 0.5)

# but what if we want probability of at least one wining in 5 tries?
dbinom(1, 5, prob = 0.5) + 
  + dbinom(2, 5, prob = 0.5) + 
  + dbinom(3, 5, prob = 0.5) +
  + dbinom(4, 5, prob = 0.5) +
  + dbinom(5, 5, prob = 0.5)

# equivalent to
a = 1 - dbinom(0, 5, prob = 0.5)
# it's equivalent to use cumulative probability function pbinom


# negative binomial distibution

dnbinom(1, 1, prob = 0.5)

# rozklad geomatryczny - ile porazek musi miec aby wygrac
# zwyciestwo za 1szym razem
dgeom(0, prob = 0.5)
# 1 porazka, po czym zwyciestwo
dgeom(1, prob = 0.5)
# 2 porazki, po czym zwyciestwo
dgeom(2, prob = 0.5)

# ponizej prawdopodobienstwo ze peirwsze zwyciestwo nastapi maksymalnie po 2 porazkach (czyli maksymalnie w 3cim ruchu)
dgeom(0, prob = 0.5) + dgeom(1, prob = 0.5) + dgeom(2, prob = 0.5)

# pgeom to dystrybuanta
pgeom(2, prob = 0.5)

# qgeom to funkcja odwrotna do pgeom
qgeom(0.875, prob = 0.5)
# tutaj wiemy ze aby miec pewnosc 87,5% ze wygramy potrzebujemy 3 ruchow (wystapia 2 porazki)

# 95% pewnosci ze wygra vs zalozenia
attempts_benchmark <- tibble(level_no = wr_benchmark_20$level_no
       , bechmark_attempts = wr_benchmark_20$attempts
       , attempts = qgeom(0.95, prob = wr_benchmark_20$win_ratio) + 1)

attempts_real <- tibble(level_no = wr_real_20$level_no
                        , attempts = qgeom(0.95, prob = wr_real_20$win_ratio) + 1)


