

#' Perform age-sex male-female data quality checks
#' 
#' @param xy Input data for male population. UN format.
#' @param xx Input data for female population. UN format.
#' @param ageMin Integer. The lower age bound used for calculations. 
#' If \code{NULL}, the smallest age in input data is considered. 
#' Default: \code{NULL}.
#' @param ageMax Integer. The upper age bound used for calculations. 
#' #' If \code{NULL}, the largest age in input data is considered. 
#' Default: \code{NULL}.
#' @param ... Other arguments to be passed to \code{\link[DemoTools]{ageSexAccuracy}}.
#' @seealso \code{\link[DemoTools]{sexRatioScore}}, 
#' \code{\link[DemoTools]{ageSexAccuracy}},
#' \code{\link[DemoTools]{ageSexAccuracyDasGupta}}.
#' @examples 
#' M5 <- DDSQLtools.data$Pop5_Egypt_M_DB
#' F5 <- DDSQLtools.data$Pop5_Egypt_F_DB
#' 
#' Q <- doQualityChecks(M5, F5)
#' 
#' Q[, c("DataProcessType", "DataValue")]
#' @export
doQualityChecks <- function(xy, xx, ageMin = NULL, ageMax = NULL, ...) {
  input <- c(as.list(environment()))
  validateInput(input)
  
  A1 <- xy$DataValue
  A2 <- xx$DataValue
  B  <- xy$AgeStart
  OAG <- is.OAG(xy)
  
  if (is.null(ageMin)) ageMin = min(B)
  if (is.null(ageMax)) ageMax = max(B)
  
  fn <- c("sexRatioScore", "ageSexAccuracy", "ageSexAccuracyDasGupta")
  G1 <- sexRatioScore(A1, A2, B, ageMin, ageMax, OAG = OAG)
  G2 <- ageSexAccuracy(A1, A2, B, ageMin, ageMax, OAG = OAG,  ...)
  G3 <- ageSexAccuracyDasGupta(A1, A2, B, ageMin, ageMax, OAG = OAG)
  
  AgeMid = AgeStart = AgeEnd <- NULL # hack CRAN note
  G <- c(G1, G2, G3) %>% as.data.frame() %>% 
    dplyr::rename(DataValue = ".") %>%
    mutate(AgeStart = min(B), 
           AgeMid = sum(xy$AgeMid - xy$AgeStart),
           AgeEnd = AgeMid * 2,
           AgeSpan = AgeEnd - AgeStart, 
           AgeLabel = paste0(min(B), "-", rev(xy$AgeLabel)[1]),
           DataProcessType = fn,
           ReferencePeriod = unique(xy$ReferencePeriod),
           SexID = 2,
           SexName = "Both sexes")
  
  G$DataProcess <- deparse(match.call())
  out <- formatOutputTable(xy, G)
  return(out)  
}



#' Internal function that validates the input in \code{doQualityChecks} as
#' stop it if data is not alright
#' @param z A list containing the input supplied in \code{doQualityChecks}.
#' @return Nothing. Just lets you move on.
#' @keywords internal
#' 
validateInput <- function(z) {
  if (!identical(dim(z$xy), dim(z$xx))) {
    stop("Mismatch between the two datasets. Different dimensions.", call. = F)
  }
  if (!identical(z$xy$AgeStart, z$xx$AgeStart)) {
    stop("Mismatch between the two datasets. Different 'AgeStart'.", call. = F)
  }
  if (is.OAG(z$xy) != is.OAG(z$xx)) {
    stop("Mismatch between the two datasets. Different 'AgeSpan'.", call. = F)
  }
}
