library(Przewodnik)
library(dplyr)
library(car)

boxplot(daneSoc$cisnienie.skurczowe, daneSoc$cisnienie.rozkurczowe
        , names = c('skurczowe','rozkurczowe')
        , main = 'Ciœnienie'
        , horizontal = F
)

boxplot(wiek~wyksztalcenie, data = daneSoc
        , col = 'lightgrey'   
        , ylab = 'Wiek'
        , las = 1
)

density(daneSoc$wiek, bw = 'SJ') %>%
  plot( , main = 'Age density'
        , xlab = 'Age')

# argument smooth - regresja z wygladzaniem 
sp(daneSoc$cisnienie.skurczowe~daneSoc$cisnienie.rozkurczowe
   , xlab = 'rozkurczowe'
   , ylab = 'skurczowe'
   , main = 'Cisnienie'
   , smooth = T
   , pch = 1
)

sp(daneSoc$cisnienie.skurczowe~daneSoc$cisnienie.rozkurczowe|daneSoc$plec
   , xlab = 'rozkurczowe'
   , ylab = 'skurczowe'
   , main = 'Cisnienie'
   , smooth = F
   , pch = c(8, 1)
   , legend = list(title = 'Plec')
)





