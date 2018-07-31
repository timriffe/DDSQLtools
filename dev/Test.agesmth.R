remove(list = ls())
library(DemoTools)
library(DDSQLtools)


# MalePop <- c(642367, 515520, 357831, 275542, 268336, 278601, 242515, 
#              198231, 165937, 122756, 96775, 59307, 63467, 32377, 29796, 16183, 34729)
# Ages    <- seq(0, 80, by = 5)
# 
# names(MalePop) <- Ages
# MalePop

P5 <- DDSQLtools.data$Pop5_Egypt_DB

fn = "Carrier-Farrag"
X = P5
ageMin = 10
ageMax = 65
young.tail = c("Original", "Arriaga", "Strong", NA)[1]
old.tail = young.tail

# doSmoothing <- function(X, fn = c("Carrier-Farrag", "Arriaga", "Strong",
#                                   "United Nations", "Karup-King-Newton"), 
#                         ageMin = 10, ageMax = 65, 
#                         young.tail = c("Original", "Arriaga", "Strong", NA)[1],
#                         old.tail = young.tail, ...) {
#   AgeStart = AgeSpan <- NULL # hack CRAN note
  
  A <- X$DataValue
  B <- X$AgeStart
  names(A) <- B
  OAG <- is.OAG(X)
  fn  <- match.arg(fn)
  E   <- DemoTools::agesmth(A, B, method = fn, OAG, ageMin, ageMax, young.tail, old.tail)
  G   <- E %>% as.data.frame() %>% 
    dplyr::rename(DataValue = ".") %>%
    mutate(AgeStart = B, 
           AgeSpan = max(B) - min(B) + 1, 
           AgeEnd = max(B),
           AgeMid = AgeStart + AgeSpan/2,
           AgeLabel = AgeStart,
           DataProcessType = fn,
           ReferencePeriod = unique(X$ReferencePeriod)) 
  
  G$DataProcess <- deparse(match.call())
out <- formatOutputTable(X, G)
#   return(out)
# }

# ----------------------------------------------

# Example 1 --------------
MalePop <- c(642367, 515520, 357831, 275542, 268336, 278601, 242515,
             198231, 165937, 122756, 96775, 59307, 63467, 32377, 29796, 16183, 34729)
Ages    <- seq(0, 80, by = 5)
names(MalePop) <- Ages

A1 <- agesmth(MalePop, Ages, method = "Carrier-Farrag", OAG = TRUE)
length(Ages) == length(A1) # OK
sum(MalePop) == sum(A1)

# Example 2 --------------
Ages2 <- c(0, 1, seq(5, 75, by = 5))
length(Ages2)
MalePop2 <- MalePop
names(MalePop2) <- Ages2

A2 <- agesmth(MalePop2, Ages2, method = "Carrier-Farrag", OAG = TRUE)

length(Ages2) == length(A2)  # Not OK
sum(MalePop2) == sum(A2)

A2 # Age 1 is missing!

# Example 3 --------------
Ages3 <- c(0, .25, .5, 1, 5, 10, 15, seq(20, 80, 10), 95, 110, 125)
length(Ages3)
MalePop3 <- MalePop
names(MalePop3) <- Ages3

A3 <- agesmth(MalePop3, Ages3, method = "Carrier-Farrag", OAG = TRUE)












