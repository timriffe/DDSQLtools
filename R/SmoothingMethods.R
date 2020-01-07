#' Wrapper for Smoothing Populations in 5-year Age Groups
#' 
#' @param X Input data. UN format.
#' @inheritParams DemoTools::agesmth
#' @inheritParams do_splitting
#' @inherit DemoTools::agesmth description details
#' @inherit do_splitting return
#' @seealso \code{\link[DemoTools]{agesmth}}
#' @examples 
#' P5 <- DDSQLtools.data$Pop5_Egypt_M_DB
#' 
#' M <- c("Carrier-Farrag", "KKN", "Arriaga",
#'        "United Nations", "Strong", "Zigzag", "MAV")
#' 
#' S1 <- do_smoothing(P5, method = M[1])
#' S2 <- do_smoothing(P5, method = M[2])
#' S3 <- do_smoothing(P5, method = M[3])
#' S4 <- do_smoothing(P5, method = M[4])
#' S5 <- do_smoothing(P5, method = M[5])
#' S6 <- do_smoothing(P5, method = M[6])
#' S7 <- do_smoothing(P5, method = M[7])
#' 
#' select_columns <- c("AgeID", "AgeStart", "AgeMid", "AgeEnd", "AgeLabel",
#'                     "DataTypeName", "DataTypeID", "DataValue")
#' S1[, select_columns]
#' 
#' S <- cbind(S1$DataValue, S2$DataValue, S3$DataValue,
#'            S4$DataValue, S5$DataValue, S6$DataValue, S7$DataValue)
#' dimnames(S) <- list(Age = S1$AgeLabel, SmoothingMethod = M)
#' 
#' S
#' @export
do_smoothing <- function(X, 
                         method = c("Carrier-Farrag",
                                    "KKN",
                                    "Arriaga",
                                    "United Nations",
                                    "Strong",
                                    "Zigzag",
                                    "MAV"),
                         ageMin = 10, 
                         ageMax = 65, 
                         n = 3,
                         young.tail = c("Original",
                                        "Arriaga",
                                        "Strong",
                                        NA),
                         old.tail = young.tail, 
                         verbose = TRUE, 
                         ...) {
  
  # input <- as.list(environment())
  # arg_names <- c(names(input), names(list(...)))
  AgeStart = AgeSpan = AgeEnd <- NULL # hack CRAN note
  
  A <- X$DataValue
  B <- X$AgeStart
  names(A) <- B
  C   <- match.call()
  OAG <- is_OAG(X)
  method <- match.arg(method)
  if (!is.na(young.tail[1])) young.tail <- match.arg(young.tail)
  
  E <- smooth_age_5(Value = A, 
                    Age = B, 
                    method = method, 
                    OAG = OAG, 
                    ageMin = ageMin, 
                    ageMax = ageMax, 
                    n = n, 
                    young.tail = young.tail, 
                    old.tail = old.tail)
  E.age <- as.numeric(names(E))
  
  G <- data.frame(DataValue = E) %>%
        mutate(AgeID = NA,
               AgeStart = E.age, 
               AgeSpan = 5, 
               AgeEnd = AgeStart + AgeSpan,
               AgeMid = AgeStart + AgeSpan/2,
               AgeLabel = X$AgeLabel,
               DataTypeName = paste0("DemoTools::agesmth_", method),
               DataTypeID = deparse(C),
               ReferencePeriod = unique(X$ReferencePeriod)) 
  
  cx <- c("AgeSpan", "AgeEnd", "AgeMid", "AgeLabel")
  G[nrow(G), cx] <- X[nrow(X), cx]
  out <- format_output(X, G)
  out
}

