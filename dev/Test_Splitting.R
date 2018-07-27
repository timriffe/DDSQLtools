# Fri Jul 27 11:13:50 2018 --------- Marius D. Pascariu ---
remove(list = ls())
library(DDSQLtools)
library(DemoTools)
library(tidyverse)


P1 <- DDSQLtools.data$Pop1_Egypt_DB
P5 <- DDSQLtools.data$Pop5_Egypt_DB

V1 <- doSplitting(P1, fn = "beers") 
V2 <- doSplitting(P1, fn = "grabill") 
V3 <- doSplitting(P1, fn = "sprague") 

W1 <- doSplitting(P5, fn = "beers") 
W2 <- doSplitting(P5, fn = "grabill")
W3 <- doSplitting(P5, fn = "sprague") 


# ----------------------------------------------
X = P1
fn = "grabill"


A <- X$AgeStart
B <- X$DataValue
names(B) <- A
OAG <- is.OAG(X)
# fn  <- match.arg(fn)
DTF <- get(fn)  # DemoTools Function
# E   <- DTF(popmat = B, Age = A, OAG = OAG, ...)
E   <- DTF(popmat = as.matrix(B), Age = A, OAG = OAG)
G   <- E %>% as.data.frame() %>% 
  mutate(AgeStart = as.numeric(rownames(E)), 
         AgeSpan = 1, 
         AgeEnd = AgeStart + AgeSpan,
         AgeMid = AgeStart + AgeSpan/2,
         AgeLabel = AgeStart) %>% 
  gather(key = ReferencePeriod, value = DataValue, -(AgeStart:AgeLabel)) %>% 
  mutate(ReferencePeriod = unique(X$ReferencePeriod))

H <- data.frame(matrix(NA, ncol = ncol(X), nrow = nrow(G)))
colnames(H) <- colnames(X)
CnameX <- colnames(X)
CnameG <- colnames(G)


# ----------------------------------------------


# 5 year age groups ----------------------------

x5 <- seq(0, 100, by=5)
p5 <- c(54170, 44775, 42142, 38464, 34406, 30386, 26933, 23481, 20602, 
        16489, 14248,  9928,  8490,  4801,  3599,  2048,   941,   326,    
        80, 17, 0) 
names(p5) <- x5


x5 <- P5$AgeStart
p5 <- P5$DataValue
names(p5) <- x5

beers(p5)  # OK
grabill(p5) # OK
sprague(p5) # OK

# 1 year age groups ----------------------------

x1 <- 0:100
p1 <- c(9544, 7472, 11590, 11882, 11873, 12968, 11993, 10034, 14312, 8112, 15311,  
        6862, 13305, 7455, 9015, 10325, 9056, 5519, 12547, 4784, 13365, 4630, 
        9596, 4728, 5195, 15061, 5467, 4012, 8034, 1972, 17396, 1647, 6540, 
        2234, 2101, 16768, 3212, 1923, 4473, 1182, 15874, 1018, 3674, 1247, 
        1029, 12619, 1500, 1250, 2862, 723, 12397, 734, 2187, 777, 811, 7298, 
        1116, 650, 1465, 412, 9479, 429, 1190, 446, 363, 4998, 389, 335, 594, 178,
        4560, 179, 481, 159, 156, 1606, 167, 94, 182, 54, 1716, 127, 151, 52, 
        49, 456, 47, 34, 44, 19, 329, 48, 29, 9, 7, 75, 13, 6, 19, 21, 72)
names(p1) <- x1

beers(  p1) # OK
grabill(p1) # OK
sprague(p1) # OK

beers(  p1[-101]) # OK
grabill(p1[-101]) # Not OK
sprague(p1[-101]) # Not OK



# x1 <- P1$AgeStart
# p1 <- P1$DataValue
# names(p1) <- x1

