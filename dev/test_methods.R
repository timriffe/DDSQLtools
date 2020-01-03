# Author: tim
###############################################################################

# TR messing around with wrappers here:
remove(list = ls())
library(devtools)
# load_all("/home/tim/git/DDSQLtools")
library(DDSQLtools)
??do_heaping

P1 <- DDSQLtools.data$Pop1_Egypt_M_DB
P5 <- DDSQLtools.data$Pop5_Egypt_M_DB

data.frame(P1)

H1 <- do_heaping(P1, fn = "Whipple")

t(H1)

H2 <- do_heaping(P1, fn = "Myers") # warning w P1
H3 <- do_heaping(P1, fn = "Bachi")
H4 <- do_heaping(P1, fn = "CoaleLi")
H5 <- do_heaping(P1, fn = "Noumbissi")
H6 <- do_heaping(P1, fn = "Spoorenberg")
H7 <- do_heaping(P1, fn = "ageRatioScore") # ***
H8 <- do_heaping(P1, fn = "KannistoHeap") # error w P5
H9 <- do_heaping(P1, fn = "Jdanov", verbose = F) # ***

?do_heaping

H <- rbind(H1, H2, H3, H4, H5, H6, H7, H8, H9)
H[, c("DataTypeName", "DataValue")]


# Silence the function with verbose = FALSE
H1 <- do_heaping(P1, fn = "Whipple", verbose = FALSE)
# ... or by specifying all arguments
H1 <- do_heaping(P1, fn = "Whipple", ageMin = 10, ageMax = 90, digit = 1)


# COMMENTS: love the printed tips. Maybe an extra argument:
# verbose = TRUE (by default) for whether or not to print.  *** DONE!
# also, maybe make printing conditional on interactive()?
# or maybe it's no big deal for them to print as such.
# this is very usable

# Q: can @seealso refer to other packages like that?  *** YES!
# Q: ageMax ageMin defaults apply to different functions: Can different called
# functions have different defaults?  *** Fixed!


# -------------------------
?do_qualitychecks
M5 <- DDSQLtools.data$Pop5_Egypt_M_DB
F5 <- DDSQLtools.data$Pop5_Egypt_F_DB

Q1 <- do_qualitychecks(M5, F5, fn = "sexRatioScore")
Q2 <- do_qualitychecks(M5, F5, fn = "ageSexAccuracy")
Q3 <- do_qualitychecks(M5, F5, fn = "ageSexAccuracyDasGupta")

Q <- rbind(Q1, Q2, Q3)
Q[, c("DataTypeName", "DataValue")]

t(Q1)
# "Additional arguments to control" -> Additional (optional) arguments to control *** DONE!
# Q: if I add args in demotools does this message update automatically? *** YES!
?do_smoothing
P5 <- DDSQLtools.data$Pop5_Egypt_M_DB

M <- c("Carrier-Farrag", "KKN", "Arriaga",
		"United Nations", "Strong", "Zigzag","MAV")

data.frame(P5)
S1 <- do_smoothing(P5, method = M[1])
data.frame(S1)

S2 <- do_smoothing(P5, method = M[2])
S3 <- do_smoothing(P5, method = M[3])
S4 <- do_smoothing(P5, method = M[4])
S5 <- do_smoothing(P5, method = M[5])
S6 <- do_smoothing(P5, method = M[6])
S7 <- do_smoothing(P5, method = M[7]) # add MAV

S1[, c("AgeStart", "DataTypeName", "DataValue")]
S2[, c("AgeStart", "DataTypeName", "DataValue")]
# would be nice to record not just agesmth, but also which method was used. *** DONE!
# perhaps also note optional args with message like above. *** NOT NEEDED.

?doSplitting

P5 <- DDSQLtools.data$Pop5_Egypt_M_DB

W1 <- doSplitting(P5, fn = "beers") 
W2 <- doSplitting(P5, fn = "grabill")
W3 <- doSplitting(P5, fn = "sprague")

W1[, c("DataTypeName", "DataValue")] # OK

# Example 2 --- 1-year age group   
P1 <- DDSQLtools.data$Pop1_Egypt_M_DB

V1 <- doSplitting(P1, fn = "beers") 
V2 <- doSplitting(P1, fn = "grabill") 
V3 <- doSplitting(P1, fn = "sprague") 


# 1) make doComparePops()
# calls IRDID.R functions, also UN sex compare, also LIFIT.R RDM, ADM (relative, absolute)

# 2) make doLTabr , calling LTabr() (inside LTPOPDTH). That is a messy messy function, I'm afraid. 
# My guess is you'll uncoveer problems that I need to attend to.

# 3) MortalityLaws() wrapper? Q: make sense for quality checks, forced smoothing, old-age extrapolation?
# I think it'd be cool to be able to bring all lifetables out to HMD age 110, even if highest closed 
# age group is 70-74-- make sense to wrap MortalityLaws to do this? This would be superior to MortPak
# extrapolation methods I think.
