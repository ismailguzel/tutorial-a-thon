
# Author:  Ismail Güzel
# e-mail: iguzel@itu.edu.tr
# Michigan State University (Visiting Scholar)
# Istanbul Technical University (PhD Student)

library(remotes)
# remotes::install_github("rrrlw/ripserr")
library(ripserr)
library(TDA)

library(quantmod)
library(ggplot2)
library(gplots)
library(tidyverse)
library(tidyquant)
library(rdist)

# Part 1:  Point Cloud and TDA
pc1 = circleUnif(n = 100, r = 3) 
pc2 <- circleUnif(n = 100, r = 2.5)
pc3 <- circleUnif(n = 100, r = 1)
pc <- rbind(pc1, pc2, pc3)

par(mfrow = c(1, 1))
plot(pc1, col = 2,
     xlim = c(min(pc[,1]),max(pc[,1])),
     ylim = c(min(pc[,2]),max(pc[,2])),
     lwd = 1, lty = 2,
     main = "Point Clouds")
points(pc2, col = 3, lwd = 2, lty = 2)
points(pc3, col = 4, lwd = 3, lty = 2)



## Calculate Persistence Diagram
RipsDiag <- ripserr::vietoris_rips(dataset = pc,
                                   max_dim = 2,
                                   threshold = max(dist(pc)) )

Diag <- as.matrix(RipsDiag)

## Obtain Persistence Landscape
Land <- landscape(Diag = Diag,
                  dimension = 1,
                  KK = 1)

## Plot all results
par(mfrow = c(1, 4))
plot(pc, main = "Point Cloud")
plot.diagram(Diag,
             main = "Persistence Diagram")
plot.diagram(Diag, barcode = TRUE,
             main = "Persistence Barcode")

plot(Land,
     main = "Peristence Landscape")


# Part 2:  Time Series Clustering with TDA

## Small case study : Financial Time Series

## time series ==> VR complex (Taken's Embedding Theorem)
VR.TS <- function(x){
  as.data.frame(
    vietoris_rips.ts(dataset = x,
                     dim_lag = 3,
                     sample_lag=5)
  )
}

## Follows Steps
# 1. Log Return
# 2. Persistent landscape
# 3. Distance matrix
# 4. Clustering

## Get Financial Time Series
symbols <- c(
  "AAPL", "GOOGL","MSFT", "IWM", "QQQ","SPY","TLT", "XLB",
  "XLE", "XLF", "XLI", "XLK", "XLP","XLU", "XLV","XLY" 
)

stock.df <- tidyquant::tq_get(symbols,
                              get = "stock.prices",
                              from = "2020-06-17",
                              to = "2021-11-01")

stock <- stock.df %>%
  select(symbol,date, close)

stock.spread <- stock %>% 
  spread(key = symbol, value = close) %>% 
  na.omit()

# Log Return
stock_ret <- stock %>% 
  group_by(symbol) %>% 
  arrange(date,.by_group=T) %>% 
  mutate(log_Returns=log(close/dplyr::lag(close))) %>% 
  na.omit()

stock_ret %>%
  ggplot(aes(date,log_Returns))+
  geom_line()+ facet_wrap(~symbol, scales = "free")+
  theme_tq()

## Topological Features
stock_nested <- stock_ret %>%
  select(symbol,date,log_Returns) %>%
  nest(data = c(date, log_Returns))

Landscapes <- stock_nested %>%
  mutate(Diagrams=map(data,~VR.TS(as.matrix(.x[,2])))) %>% 
  mutate(P_Landscapes=map(Diagrams,landscape))

# Visualization of PL
Landscapes %>% 
  select(symbol,P_Landscapes) %>%
  unnest(cols = c(P_Landscapes)) %>% 
  group_by(symbol) %>% 
  mutate(index = 1:500) %>% 
  ggplot(aes(index,P_Landscapes[,1]))+
  geom_line(col="darkorange1", lty = 1, lwd  =2)+
  labs(x = "index",
       y= "Landscape Values",
       title = "Persistence Landscapes")+
  facet_wrap(~symbol)+
  theme_tq(base_size = 15)

Landscape_Matrix <- Landscapes %>% 
  select(symbol, P_Landscapes) %>%
  unnest(cols = c(P_Landscapes)) %>% 
  mutate(row = row_number()) %>% 
  spread(symbol, P_Landscapes) %>% 
  select(-row) %>% 
  as.matrix()


## Distance matrices with using PL
dist_all <- rdist(X = t(Landscape_Matrix),
                  metric = 'euclidean', p = 2)

## Hierarchical clustering on distance matrix
hc <- hclust(d = dist_all,method = "ward.D2")

## Plot clustering results
nodePar <- list(lab.cex = 1.2, pch = c(NA, 19), 
                cex = 0.7, col = "red")
par(mfrow=c(1,1))
plot(as.dendrogram(hc),
     nodePar = nodePar,
     main = 'Clustering Time Series with its PL')

## cutting level
rect.hclust(hc , k = 4, border = 2:5)
clusterCut <- cutree(hc, 4)

## Obtain clustering information
table(symbols,clusterCut)

