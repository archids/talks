wwlln.dates <- names(wwlln)

# MAKE FULL DAY PLOTS -------------------------------------------------------------------------------------------------

day.plot <- function(label) { 
    m = map +
        geom_point(aes(x = lon, y = lat), data = wwlln[[label]], alpha = 1/2, size = 3, color = "darkgreen"), xlim = c(20, 30)
    png(file.path("fig", paste0("map-overview-", label, ".png")), width = 800, height = 800)
    print(m)
    dev.off()    
}

for (d in wwlln.dates) {  
    day.plot(d)
}

# MAKE HOURLY PLOTS ---------------------------------------------------------------------------------------------------

hourly.plot <- function(label) {
    m = map +
        geom_point(aes(x = lon, y = lat), data = wwlln[[label]], alpha = 1/2, size = 2, color = "darkgreen") +
        facet_wrap(~ hour, nrow = 4)
    png(file.path("fig", paste0("map-hourly-", label, ".png")), width = 1200, height = 800)
    print(m)
    dev.off()
}

for (d in wwlln.dates) {    
    hourly.plot(d)
}

# MAKE DAY/HOUR PLOT ------------------------------------------------------------------------------

day.hour.plot <- function(label, hour) {
    W = wwlln[[label]]
    #
    W = W[W$hour == hour,]
    #
    m = map +
        geom_point(aes(x = lon, y = lat), data = W, alpha = 1/2, size = 4, color = "darkgreen")
    png(file.path("fig", sprintf("map-hour-%s-%02d.png", label, hour)), width = 800, height = 800)
    print(m)
    dev.off()
}