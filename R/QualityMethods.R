

#' Wrapper for Performing Age-Sex Male-Female Data Quality Checks
#' 
#' @param XY Input data for male population. UN format.
#' @param XX Input data for female population. UN format.
#' @param ageMin Integer. The lower age bound used for calculations. 
#' If \code{NULL}, the smallest age in input data is considered. 
#' Default: \code{NULL}.
#' @param ageMax Integer. The upper age bound used for calculations. 
#' #' If \code{NULL}, the largest age in input data is considered. 
#' Default: \code{NULL}.
#' @inheritParams doSplitting
#' @inherit doSplitting return
#' @seealso \code{\link[DemoTools]{sexRatioScore}}, 
#' \code{\link[DemoTools]{ageSexAccuracy}},
#' \code{\link[DemoTools]{ageSexAccuracyDasGupta}}.
#' @examples 
#' M5 <- DDSQLtools.data$Pop5_Egypt_M_DB
#' F5 <- DDSQLtools.data$Pop5_Egypt_F_DB
#' 
#' Q1 <- doQualityChecks(M5, F5, fn = "sexRatioScore")
#' Q2 <- doQualityChecks(M5, F5, fn = "ageSexAccuracy")
#' Q3 <- doQualityChecks(M5, F5, fn = "ageSexAccuracyDasGupta")
#' 
#' Q <- rbind(Q1, Q2, Q3)
#' Q[, c("DataProcessType", "DataValue")]
#' @export
#' 
doQualityChecks <- function(XY, XX, 
                            fn = c("sexRatioScore", "ageSexAccuracy", "ageSexAccuracyDasGupta"), 
                            ageMin = NULL, ageMax = NULL, ...) {
  input <- c(as.list(environment()))
  validateInput(input)
  
  A1  <- XY$DataValue
  A2  <- XX$DataValue
  B   <- XY$AgeStart
  OAG <- is_OAG(XY)
  fn  <- match.arg(fn)
  
  if (is.null(ageMin)) ageMin = min(B)
  if (is.null(ageMax)) ageMax = max(B)
  
  E <- switch(fn,
    sexRatioScore = sexRatioScore(A1, A2, B, ageMin, ageMax, OAG),
    ageSexAccuracy = ageSexAccuracy(A1, A2, B, ageMin, ageMax, OAG = OAG,  ...),
    ageSexAccuracyDasGupta = ageSexAccuracyDasGupta(A1, A2, B, ageMin, ageMax, OAG)
  )
  
  AgeMid = AgeStart = AgeEnd <- NULL # hack CRAN note
  G <- E %>% as.data.frame() %>% 
    dplyr::rename(DataValue = ".") %>%
    mutate(AgeStart = min(B), 
           AgeMid = sum(XY$AgeMid - XY$AgeStart),
           AgeEnd = AgeMid * 2,
           AgeSpan = AgeEnd - AgeStart, 
           AgeLabel = paste0(min(B), "-", rev(XY$AgeLabel)[1]),
           DataProcessType = fn,
           ReferencePeriod = unique(XY$ReferencePeriod),
           SexID = 2,
           SexName = "Both sexes")
  
  C <- match.call()
  controlOutputMsg(fn, C)
  G$DataProcess <- deparse(C)
  out <- formatOutputTable(XY, G)
  return(out)  
}



#' Internal function that validates the input in \code{doQualityChecks} as
#' stop it if data is not alright
#' @param z A list containing the input supplied in \code{doQualityChecks}.
#' @return Nothing. Just lets you pass through... or not.
#' @keywords internal
#' 
validateInput <- function(z) {
  mismatch <- "Mismatch between the two datasets."
  
  if (!identical(dim(z$XY), dim(z$XX))) {
    stop(mismatch, "Different dimensions.", call. = F)
  }
  if (!identical(z$XY$AgeStart, z$XX$AgeStart)) {
    stop(mismatch, "Different 'AgeStart' in input.", call. = F)
  }
  if (is_OAG(z$XY) != is_OAG(z$XX)) {
    stop(mismatch, "Different 'AgeSpan' in input.", call. = F)
  }
}
