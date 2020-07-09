library(xts)
library(ggplot2)
library(ggmap)
library(mapproj)

source("/home/colliera/proj/P-010-research-R/wwlln.R")

# MAP ---------------------------------------------------------------------------------------------

if (!exists("map.base")) {
    DATAFILE = "data/map-bases.RData"
    #
    if (file.exists(DATAFILE)) {
        load(DATAFILE)
    } else {
        #map.base.3 <- get_map(location = c(lon = +27.5, lat =  -5.0), zoom = 3, maptype = 'watercolor', source = 'stamen')
        #map.base.4 <- get_map(location = c(lon = +27.5, lat = -15.0), zoom = 4, maptype = 'watercolor', source = 'stamen')
        map.base <- get_map(location = c(lon = +25.0, lat = -23.0), zoom = 5, maptype = 'watercolor', source = 'stamen')
        #map.base.6 <- get_map(location = c(lon = +23.0, lat = -30.0), zoom = 6, maptype = 'watercolor', source = 'stamen')
        #                      maptype = 'watercolor', source = 'stamen')
        #
        save(map.base, file = DATAFILE)
    }
    #
    rm(DATAFILE)
}

map = ggmap(map.base)

# LOAD WWLLN DATA ---------------------------------------------------------------------------------

LONMIN = 10
LONMAX = 40
LATMIN = -35
LATMAX = -10

if (!exists("wwlln")) {
    DATAFILE = "data/wwlln-data.RData"
    #
    if (file.exists(DATAFILE)) {
        load(DATAFILE)
    } else {
        wwlln <- list()
        
        for (f in list.files("data", pattern = "loc$", full.names = TRUE)) {
            label = sub(".loc", "", basename(f))
            #
            d <- load.wwlln(f)
            #
            d <- subset(d, lon >= LONMIN & lon <= LONMAX & lat >= LATMIN & lat <= LATMAX)
            #
            d <- transform(d,
                           hour = as.integer(strftime(epoch, "%H", tz = "GMT")),
                           nday = as.integer(strftime(epoch, "%j", tz = "GMT"))
            )
            #
            wwlln[[label]] <- d
        }
        #
        rm(f)
        #
        # Strip off leading "AE"
        #
        names(wwlln) <- substr(names(wwlln), 3, 10)
        #
        save(wwlln, file = DATAFILE)
    }
    #
    rm(DATAFILE)
}
#
# Interesting days:
#
# 20130108
# 20130109
# 20130110
# 20130111
# 20130112
# 20130116 --- western cape
# 20130117 --- western cape
# 20130206 --- western cape (storm front off Cape Point)