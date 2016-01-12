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