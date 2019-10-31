install.packages("stringr")
library(stringr)

# Load data
data <- read.delim("Arrowsmith.txt", header = TRUE, skip = 4)

# clean data
df=data[-c(16:29)]
df$nof.MeSH.in.common[df$nof.MeSH.in.common == 99999] <- 0.5
colnames(df)

# add new attribute columns
df[,"x1"] <- NA
df[,"x2"] <- NA
df[,"x3"] <- NA
df[,"x4"] <- NA
df[,"x5"] <- NA
df[,"x6"] <- NA
df[,"x7"] <- NA
df[,"I1"] <- NA
df[,"I2"] <- NA
df[,"I3"] <- NA
df[,"I4"] <- NA
df[,"I5"] <- NA
df[,"I6"] <- NA
df[,"y"] <- NA


# function to calculate x1 attribute
forX1 <- function(nA, a_litsize, nC, c_litsize) {
  if((nA > 1 | a_litsize < 1000) & (nC > 1 | c_litsize < 1000)) {
    return(1)
  } else {
    return(0)
  }
}

# function to calculate x2 attribute
forX2 <- function(noOfMeSHinCommon) {
  if(noOfMeSHinCommon >= 1) {
    return(1)
  } else if (noOfMeSHinCommon == 0.5) {
    return(0.5)
  } else {
    return(0)
  }
}

# function to calculate x3 attribute
forX3 <- function(nofSemanticCategories) {
  if(nofSemanticCategories > 0) {
    return(1)
  } else {
    return(0)
  }
}

# function to calculate x4 attribute
forX4 <- function(cohesionscore) {
  if(cohesionscore < 0.3) {
    return(cohesionscore)
  } else {
    return(0.3)
  }
}

# function to calculate x5 attribute
forX5 <- function(nInMedline) {
  logvalue = log10(nInMedline)
  totalvalue = -1 * abs(logvalue - 3)
  
  return(totalvalue)
}

# function to calculate x6 attribute
forX6 <- function(firstYearMedline) {
  minvalue = min(firstYearMedline, 2005)
  maxvalue = max(minvalue, 1950)
  
  return(maxvalue)
}

# function to calculate x7 attribute
forX7 <- function(pAC) {
  logvalue = -1 * log10(pAC + 0.000000001)
  minvalue = min(8, logvalue)
  
  return(minvalue)
}

# function to calculate i1 attribute
forI1 <- function(term) {
  if(is.na(str_extract(term, "retinal detachment"))) {
    return(0)
  } else {
    return(1)
  }
}

# function to calculate i2 attribute
forI2 <- function(term) {
  if(is.na(str_extract(term, "NO and mitochondria vs PSD"))) {
    return(0)
  } else {
    return(1)
  }
}

# function to calculate i3 attribute
forI3 <- function(term) {
  if(is.na(str_extract(term, "mGluR5 vs lewy bodies"))) {
    return(0)
  } else {
    return(1)
  }
}

# function to calculate i4 attribute
forI4 <- function(term) {
  if(is.na(str_extract(term, "magnesium vs migraine"))) {
    return(0)
  } else {
    return(1)
  }
}

# function to calculate i5 attribute
forI5 <- function(term) {
  if(is.na(str_extract(term, "Calpain vs PSD"))) {
    return(0)
  } else {
    return(1)
  }
}

# function to calculate i6 attribute
forI6 <- function(term) {
  if(is.na(str_extract(term, "APP vs reelin"))) {
    return(0)
  } else {
    return(1)
  }
}

# function to calculate y attribute
forY <- function(target) {
  if(target == 0 | target == 2) {
    return(1)
  } else {
    return(0)
  }
}

# Applying functions to the columns to generate attributes
df$x1 = mapply(forX1, df$nA, df$A.lit.size, df$nC, df$C.lit.size)
df$x2 = mapply(forX2, df$nof.MeSH.in.common)
df$x3 = mapply(forX3, df$nof.semantic.categories)
df$x4 = mapply(forX4, df$cohesion.score)
df$x5 = mapply(forX5, df$n.in.MEDLINE)
df$x6 = mapply(forX6, df$X1st.year.in.MEDLINE)
df$x7 = mapply(forX7, df$pAC)
df$I1 = mapply(forI1, df$Arrowsmith.search)
df$I2 = mapply(forI2, df$Arrowsmith.search)
df$I3 = mapply(forI3, df$Arrowsmith.search)
df$I4 = mapply(forI4, df$Arrowsmith.search)
df$I5 = mapply(forI5, df$Arrowsmith.search)
df$I6 = mapply(forI6, df$Arrowsmith.search)
df$y = mapply(forY, df$target)

fit = glm(y~x1+x2+x3+x4+x5+x6, family = binomial(), df)
fit

##fit=glm(on.long.stoplist.~nA+nC+nof.MeSH.in.common+target+cohesion.score,data=df)
