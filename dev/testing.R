# Author: tim
###############################################################################

# TR messing around with wrappers here:

library(devtools)
load_all("/home/tim/git/DDSQLtools")

?doHeaping

P1 <- DDSQLtools.data$Pop1_Egypt_M_DB

H1 <- doHeaping(P1, fn = "Whipple")
H2 <- doHeaping(P1, fn = "Myers")
H3 <- doHeaping(P1, fn = "Bachi")
H4 <- doHeaping(P1, fn = "CoaleLi")
H5 <- doHeaping(P1, fn = "Noumbissi")
H6 <- doHeaping(P1, fn = "Spoorenberg")
H7 <- doHeaping(P1, fn = "ageRatioScore")
H8 <- doHeaping(P1, fn = "AHI")
H9 <- doHeaping(P1, fn = "WI")

H <- rbind(H1, H2, H3, H4, H5, H6, H7, H8, H9)
H[, c("DataProcessType", "DataValue")]

# If `digit` is in input the message is not printed
H1 <- doHeaping(P1, fn = "Whipple", digit = 1)

# COMMENTS: love the printed tips. Maybe an extra argument:
# verbose = TRUE (by default) for whether or not to print.
# also, maybe make printing conditional on interactive()?
# or maybe it's no big deal for them to print as such.
# this is very usable

# Q: can @seealso refer to other packages like that?
# Q: ageMax ageMin defaults apply to different functions: Can different called
# functions have different defaults?


# -------------------------
?doQualityChecks
M5 <- DDSQLtools.data$Pop5_Egypt_M_DB
F5 <- DDSQLtools.data$Pop5_Egypt_F_DB

Q1 <- doQualityChecks(M5, F5, fn = "sexRatioScore")
Q2 <- doQualityChecks(M5, F5, fn = "ageSexAccuracy")
Q3 <- doQualityChecks(M5, F5, fn = "ageSexAccuracyDasGupta")

Q <- rbind(Q1, Q2, Q3)
Q[, c("DataProcessType", "DataValue")]

# "Additional arguments to control" -> Additional (optional) arguments to control 
?doSmoothing
P5 <- DDSQLtools.data$Pop5_Egypt_M_DB

M <- c("Carrier-Farrag", "KKN", "Arriaga",
		"United Nations", "Strong", "Zigzag","MAV")

S1 <- doSmoothing(P5, method = M[1])
S2 <- doSmoothing(P5, method = M[2])
S3 <- doSmoothing(P5, method = M[3])
S4 <- doSmoothing(P5, method = M[4])
S5 <- doSmoothing(P5, method = M[5])
S6 <- doSmoothing(P5, method = M[6])
S7 <- doSmoothing(P5, method = M[7]) # add MAV

S1[, c("DataProcessType", "DataValue")]
S2[, c("DataProcessType", "DataValue")]
# would be nice to record not just agesmth, but also which method was used.
# perhaps also note optional args with message like above.

?doSplitting

P5 <- DDSQLtools.data$Pop5_Egypt_M_DB

W1 <- doSplitting(P5, fn = "beers") 
W2 <- doSplitting(P5, fn = "grabill")
W3 <- doSplitting(P5, fn = "sprague")

W1[, c("DataProcessType", "DataValue")] # OK

# Example 2 --- 1-year age group   
P1 <- DDSQLtools.data$Pop1_Egypt_M_DB

V1 <- doSplitting(P1, fn = "beers") 
V2 <- doSplitting(P1, fn = "grabill") 
V3 <- doSplitting(P1, fn = "sprague") 

