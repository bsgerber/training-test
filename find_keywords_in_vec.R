# Find keywords in a vector of strings

v1 <- c("doctor said", "this MD is", "medical ok nurse nurse", "something else", "watch out for doctors")
m1 <- c("doctor", "MD", "physician", "nurse")

fun <- function(x) grepl(x, v1, ignore.case = T)

keywordsFound <- as.data.frame(sapply(m1, fun))
keywordsFound