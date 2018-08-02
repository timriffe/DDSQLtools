

#' Wrapper for Smoothing Populations in 5-year Age Groups
#' 
#' @param X Input data. UN format.
#' @inheritParams DemoTools::agesmth
#' @inheritParams doSplitting
#' @inherit DemoTools::agesmth description details
#' @inherit doSplitting return
#' @seealso \code{\link[DemoTools]{agesmth}}
#' @examples 
#' P5 <- DDSQLtools.data$Pop5_Egypt_M_DB
#' 
#' M <- c("Carrier-Farrag", "KKN", "Arriaga",
#'        "United Nations", "Strong", "Zigzag")
#' 
#' S1 <- doSmoothing(P5, method = M[1])
#' S2 <- doSmoothing(P5, method = M[2])
#' S3 <- doSmoothing(P5, method = M[3])
#' S4 <- doSmoothing(P5, method = M[4])
#' S5 <- doSmoothing(P5, method = M[5])
#' S6 <- doSmoothing(P5, method = M[6])
#' 
#' S <- cbind(S1$DataValue, S2$DataValue, S3$DataValue, 
#'            S4$DataValue, S5$DataValue, S2$DataValue)
#' dimnames(S) <- list(Age = S1$AgeLabel, SmoothingMethod = M)
#' 
#' S 
#' @export
#' 
doSmoothing <- function(X, method = c("Carrier-Farrag", "KKN", "Arriaga",
                                  "United Nations", "Strong", "Zigzag"),
                        ageMin = 10, ageMax = 65,
                        young.tail = c("Original", "Arriaga", "Strong", NA),
                        old.tail = young.tail, ...) {
  AgeStart = AgeSpan = AgeEnd <- NULL # hack CRAN note
  
  A <- X$DataValue
  B <- X$AgeStart
  names(A) <- B
  OAG    <- is_OAG(X)
  method <- match.arg(method)
  if (!is.na(young.tail[1])) young.tail <- match.arg(young.tail)
  E      <- agesmth(A, B, method, OAG, ageMin, ageMax, young.tail, old.tail)
  E.age  <- as.numeric(names(E))
  
  G   <- E %>% as.data.frame() %>% 
    dplyr::rename(DataValue = ".") %>%
    mutate(AgeStart = E.age, 
           AgeSpan = 5, 
           AgeEnd = AgeStart + AgeSpan,
           AgeMid = AgeStart + AgeSpan/2,
           AgeLabel = paste0(AgeStart, "-", AgeEnd - 1),
           DataProcessType = "agesmth",
           ReferencePeriod = unique(X$ReferencePeriod)) 
  
  cx <- c("AgeSpan", "AgeEnd", "AgeMid", "AgeLabel")
  G[nrow(G), cx] <- X[nrow(X), cx]
  
  C <- match.call()
  # controlOutputMsg(fn, C)
  G$DataProcess <- deparse(C)
  out <- formatOutputTable(X, G)
  return(out)
}
