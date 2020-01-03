#' Wrapper for Performing Age-Sex Male-Female Data Quality Checks
#' 
#' @param XY Input data for male population. UN format.
#' @param XX Input data for female population. UN format.
#' @param fn Method to be called from DemoTools. Available aternatives: 
#' \code{"sexRatioScore", "ageSexAccuracy", "ageSexAccuracyDasGupta"}.
#' @inheritParams do_splitting
#' @inherit do_splitting return
#' @seealso \code{\link[DemoTools]{sexRatioScore}}, 
#' \code{\link[DemoTools]{ageSexAccuracy}},
#' \code{\link[DemoTools]{ageSexAccuracyDasGupta}}.
#' @examples 
#' M5 <- DDSQLtools.data$Pop5_Egypt_M_DB
#' F5 <- DDSQLtools.data$Pop5_Egypt_F_DB
#' 
#' Q1 <- do_qualitychecks(M5, F5, fn = "sexRatioScore")
#' Q2 <- do_qualitychecks(M5, F5, fn = "ageSexAccuracy")
#' Q3 <- do_qualitychecks(M5, F5, fn = "ageSexAccuracyDasGupta")
#' 
#' select_columns <- c("AgeID", "AgeStart", "AgeMid", "AgeEnd", "AgeLabel",
#'                     "DataTypeName", "DataTypeID", "DataValue")
#' Q <- rbind(Q1, Q2, Q3)
#' Q[, select_columns]
#' @export
do_qualitychecks <- function(XY, 
                            XX, 
                            fn = c("sexRatioScore", 
                                   "ageSexAccuracy", 
                                   "ageSexAccuracyDasGupta"), 
                            verbose = TRUE, 
                            ...) {
  
  input <- as.list(environment())
  arg_names <- c(names(input), names(list(...)))
  validate_input(input)
  
  A1  <- XY$DataValue
  A2  <- XX$DataValue
  B   <- XY$AgeStart
  C   <- match.call()
  OAG <- is_OAG(XY)
  fn  <- match.arg(fn)
  sex <- c("Male", "Female", "Both sexes")
  sex_id   <- if (XX$SexID[1] == XY$SexID[1]) XX$SexID[1] else 3
  sex_name <- sex[sex_id]
  
  E <- switch(fn,
    sexRatioScore = sexRatioScore(A1, A2, Age = B, OAG = OAG),
    ageSexAccuracy = ageSexAccuracy(A1, A2, Age = B, OAG = OAG,  ...),
    ageSexAccuracyDasGupta = ageSexAccuracyDasGupta(A1, A2, Age = B, OAG = OAG)
  )
  
  AgeStart = AgeEnd <- NULL # hack CRAN note
  G <- data.frame(DataValue = E) %>%
        mutate(AgeID = NA,
               AgeStart = min(XY$AgeStart), 
               AgeEnd = max(XY$AgeEnd),
               AgeMid = sum(XY$AgeMid - XY$AgeStart),
               AgeSpan = AgeEnd - AgeStart, 
               AgeLabel = paste0(AgeStart, "-", rev(XY$AgeLabel)[1]),
               DataTypeName = paste0("DemoTools::", fn),
               DataTypeID = paste(deparse(C), collapse = ""),
               ReferencePeriod = unique(XY$ReferencePeriod),
               SexID = sex_id,
               SexName = sex_name)
  
  if (verbose) output_msg(fn, arg_names)
  out <- formatOutputTable(XY, G)
  out  
}



#' Internal function that validates the input in \code{do_qualitychecks} as
#' stop it if data is not alright
#' @param z A list containing the input supplied in \code{do_qualitychecks}.
#' @return Nothing. Just lets you pass through... or not.
#' @keywords internal
#' 
validate_input <- function(z) {
  mismatch <- "Mismatch between the two datasets. "
  
  if (!identical(dim(z$XY), dim(z$XX))) {
    stop(mismatch, "Different dimensions.", call. = FALSE)
  }
  if (!identical(z$XY$AgeStart, z$XX$AgeStart)) {
    stop(mismatch, "Different 'AgeStart' in input.", call. = FALSE)
  }
  if (is_OAG(z$XY) != is_OAG(z$XX)) {
    stop(mismatch, "Different 'AgeSpan' in input.", call. = FALSE)
  }
}
