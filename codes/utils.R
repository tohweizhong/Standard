
# utils.R

# Houses various functions that are commonly used
# Divided into several sections, related to
# 1. Data structures
# 2. EDA, data mining, Kaggle
# 3. Geospatial

# All function names are to start with caps and use CamelCase
# No periods are to be used


# ========================================================================
# Data structures-related
# @ PushList
# @ PopList

# Function to push new element to end of list
PushList <- function(lst, newitem){
    
    lst[[length(lst) + 1]] <- newitem
    return(lst)
}

# Function to pop last item from list
PopList <- function(lst){
    
    lst_length <- length(lst)
    if(lst_length > 1)
        return(lst[1:lst_length - 1])
    else if(lst_length == 1)
        return(list())
}

# ========================================================================
# EDA, data mining, Kaggle
# @ ComputeCEL
# @ Cate2Prob
# @ WhichAreFactors
# @ OneHotEncode
# @ GenerateSubmission

# Function to compute CEL
# require a data.frame with the following columns
# @ first column: predicted probability to resign
# @ second column: correct answers
ComputeCEL <- function(df){
    One.row.CEL <- function(one.row){
        y_i <- one.row[1]
        p_i <- one.row[2]
        p_i <- max(min(p_i, 1 - 10^-15), 10^-15)
        return(y_i * log10(p_i) + (1 - y_i)*log10(1 - p_i))
    }
    total.CEL <- 0
    for(i in seq(nrow(df))){
        one.row <- df[i,]
        total.CEL <- total.CEL + One.row.CEL(one.row)
    }
    return(-total.CEL/nrow(df))
}

# example
#ComputeCEL(data.frame(rbind(c(1,0.5),c(0, 0.5), c(1, 0.5), c(0, 0.5))))
# this should be 0.30103

# Function to generate one-hot encoded variables for all variables in a data.frame
OneHotEncode <- function(df, type = "train",  yvar){
    
    if(type == "train"){
        y  <- df[, which(colnames(df) == yvar)]
        df <- df[,-which(colnames(df) == yvar)]
        
        mm <- model.matrix(~ 0 + ., data = df)
        df <- cbind(y, data.frame(mm))
        colnames(df)[1] <- yvar
        return(df)
    }
    else if(type == "test"){
        return(data.frame(model.matrix(~ 0 + ., data = df)))
    }
    
}

# Function to generate a submission file for competitions
GenerateSubmission <- function(predictions, filename, samplefilename){
    subm <- read.csv(samplefilename)
    subm$RESIGNED <- predictions
    write.csv(subm, file = paste("submissions/", filename, ".csv", sep = ""), row.names = F)
}

# Function to convert a categorical variable to numerical,
# replacing the values with empirical probabilities
Cate2Prob <- function(one.col){
    tab <- table(one.col) / length(one.col)
    classes <- names(tab)
    
    # check
    if(!all(classes %in% unique(one.col))) stop("Something wrong with the classes")
    
    tmp.col <- NULL
#     # use a simple for-loop for now
#     # very slow...
#     for(i in seq(length(one.col))){
#         val <- one.col[i]
#         idx <- which(classes ==  val)
#         tmp.col <- c(tmp.col, tab[idx])
#     }
    
    tmp.col <- sapply(one.col, FUN = function(x){
        idx <- which(classes == x)
        return(tab[idx])
    })
    
    
    # check
    if(length(tmp.col) != length(one.col)) stop("Lengths dont tally")
    return(tmp.col)
}

# Function to identify which columns in a data.frame are factors
WhichAreFactors <- function(df){
    datatypes <- sapply(df, class)
    tmp <- NULL
    for(i in seq(length(datatypes))){
        if(datatypes[i] == "factor")
            tmp <- c(tmp, TRUE)
        else tmp <- c(tmp, FALSE)
    }
    return(tmp)
}


# ========================================================================
# Geospatial
# @ GcdSlc
# @ Deg2Rad
# @ GcdHf
# @ GcdVif
# @ Gcd

# Calculates the geodesic distance between two points specified by radian latitude/longitude using the
# Spherical Law of Cosines (slc)
GcdSlc <- function(long1, lat1, long2, lat2) {
    R <- 6371 # Earth mean radius [km]
    d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
    return(d) # Distance in km
}

# Convert degrees to radians
Deg2Rad <- function(deg) return(deg*pi/180)

# Calculates the geodesic distance between two points specified by radian latitude/longitude using the
# Haversine formula (hf)
GcdHf <- function(long1, lat1, long2, lat2) {
    R <- 6371 # Earth mean radius [km]
    delta.long <- (long2 - long1)
    delta.lat <- (lat2 - lat1)
    a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
    c <- 2 * asin(min(1,sqrt(a)))
    d = R * c
    return(d) # Distance in km
}

# Calculates the geodesic distance between two points specified by radian latitude/longitude using
# Vincenty inverse formula for ellipsoids (vif)
GcdVif <- function(long1, lat1, long2, lat2) {
    
    # WGS-84 ellipsoid parameters
    a <- 6378137         # length of major axis of the ellipsoid (radius at equator)
    b <- 6356752.314245  # ength of minor axis of the ellipsoid (radius at the poles)
    f <- 1/298.257223563 # flattening of the ellipsoid
    
    L <- long2-long1 # difference in longitude
    U1 <- atan((1-f) * tan(lat1)) # reduced latitude
    U2 <- atan((1-f) * tan(lat2)) # reduced latitude
    sinU1 <- sin(U1)
    cosU1 <- cos(U1)
    sinU2 <- sin(U2)
    cosU2 <- cos(U2)
    
    cosSqAlpha <- NULL
    sinSigma <- NULL
    cosSigma <- NULL
    cos2SigmaM <- NULL
    sigma <- NULL
    
    lambda <- L
    lambdaP <- 0
    iterLimit <- 100
    while (abs(lambda-lambdaP) > 1e-12 & iterLimit>0) {
        sinLambda <- sin(lambda)
        cosLambda <- cos(lambda)
        sinSigma <- sqrt( (cosU2*sinLambda) * (cosU2*sinLambda) +
                              (cosU1*sinU2-sinU1*cosU2*cosLambda) * (cosU1*sinU2-sinU1*cosU2*cosLambda) )
        if (sinSigma==0) return(0)  # Co-incident points
        cosSigma <- sinU1*sinU2 + cosU1*cosU2*cosLambda
        sigma <- atan2(sinSigma, cosSigma)
        sinAlpha <- cosU1 * cosU2 * sinLambda / sinSigma
        cosSqAlpha <- 1 - sinAlpha*sinAlpha
        cos2SigmaM <- cosSigma - 2*sinU1*sinU2/cosSqAlpha
        if (is.na(cos2SigmaM)) cos2SigmaM <- 0  # Equatorial line: cosSqAlpha=0
        C <- f/16*cosSqAlpha*(4+f*(4-3*cosSqAlpha))
        lambdaP <- lambda
        lambda <- L + (1-C) * f * sinAlpha *
            (sigma + C*sinSigma*(cos2SigmaM+C*cosSigma*(-1+2*cos2SigmaM*cos2SigmaM)))
        iterLimit <- iterLimit - 1
    }
    if (iterLimit==0) return(NA)  # formula failed to converge
    uSq <- cosSqAlpha * (a*a - b*b) / (b*b)
    A <- 1 + uSq/16384*(4096+uSq*(-768+uSq*(320-175*uSq)))
    B <- uSq/1024 * (256+uSq*(-128+uSq*(74-47*uSq)))
    deltaSigma = B*sinSigma*(cos2SigmaM+B/4*(cosSigma*(-1+2*cos2SigmaM^2) -
                                                 B/6*cos2SigmaM*(-3+4*sinSigma^2)*(-3+4*cos2SigmaM^2)))
    s <- b*A*(sigma-deltaSigma) / 1000
    
    return(s) # Distance in km
}

# Calculates the geodesic distance between two points specified by degrees (DD) latitude/longitude using
# Haversine formula (hf), Spherical Law of Cosines (slc) and Vincenty inverse formula for ellipsoids (vif)
Gcd <- function(long1, lat1, long2, lat2) {
    
    # Convert degrees to radians
    long1 <- deg2rad(long1)
    lat1 <- deg2rad(lat1)
    long2 <- deg2rad(long2)
    lat2 <- deg2rad(lat2)
    
    return(list(haversine = gcd.hf(long1, lat1, long2, lat2),
                sphere = gcd.slc(long1, lat1, long2, lat2),
                vincenty = gcd.vif(long1, lat1, long2, lat2)) )
}
