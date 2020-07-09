library(cluster)
library(geosphere)
library(ggplot2)
library(gridExtra)

# =================================================================================================
#  TODO!!! TODO!!! TODO!!! TODO!!! TODO!!! TODO!!! TODO!!! TODO!!! TODO!!! TODO!!! TODO!!! TODO!!! 
# =================================================================================================
#
# 1. we can actually do something very much like k-means by using pam() (in cluster package), which
#    does partitioning around medoids (which are median/centroids). And you can pass in a distance
#    matrix!

# K-MEANS CLUSTERING ------------------------------------------------------------------------------

# We can't use k-means clustering since
# 
# (i) it does not work on a distance matrix and
# (ii) it does not cater for great circle distances.

# -------------------------------------------------------------------------------------------------

# Focus on one day's data
#
nday = 32
#
W = wwlln[[nday]]
#
label = names(wwlln)[nday]

# Choose an hour
#
hour = 19
#
W = W[W$hour == hour,]

full.label = sprintf("%s-%02d", label, hour)

day.hour.plot("20120201", 19)

# -------------------------------------------------------------------------------------------------

distance.matrix <- distm(W[, c("lon", "lat")], fun = distHaversine)
dim(distance.matrix)
#
# Convert from m to km
#
distance.matrix = distance.matrix / 1e3

png(paste0("fig/distance-matrix-", full.label, ".png"), width = 1200, height = 1200)
ggplot(melt(distance.matrix, value.name = "km"), aes(x = Var1, y = Var2, fill = log10(km))) +
    geom_tile(colour='black') +
    xlab("") + ylab("") +
    theme_classic() +
    theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
dev.off()

# -------------------------------------------------------------------------------------------------

# Perform Agglomerative Nesting (Hierarchical Clustering)
#
clusters <- agnes(distance.matrix, method = "complete")
#
png(sprintf("fig/wwlln-cluster-%s-%02d.png", label, hour), width = 1200, height = 800)
par(mar = c(2.1, 4.1, 2.1, 1.1))
plot(clusters, which.plots = 2, main = "", sub = "", xlab = "", labels = FALSE)
# abline(h = seq(5000, 20000, 5000), lty = "dotted", col = "grey")
dev.off()

# -------------------------------------------------------------------------------------------------

NCUTS <- 20

# Experiment with different numbers of clusters
#
group = cutree(clusters, k = 1:NCUTS)

# Within-cluster distances
#
dist.stats <- list()
#
for (k in 1:NCUTS) {
    # Retrieve allocations for k groups
    #
    G = group[,k]
    #
    # Split distance matrix into groups
    #
    P = lapply(1:k, function(n) {distance.matrix[G == n, G == n]})
    #
    # Count the number of points in each group
    #
    gcount = sapply(P, function(m) ifelse(class(m) == "matrix", nrow(m), 1))
    #
    # Find maximal distance between all pairs within each group
    #
    gmax = sapply(P, max)
    #
    # Split distance matrix into groups: splitting is different this time because we are looking
    # at the distances between points that are in a group and all other points that are not in
    # that group.
    #
    P = lapply(1:k, function(n) {distance.matrix[G != n, G == n]})
    #
    # Find minimal distance between points in one group and all points not in that group
    #
    if (k == 1) {
        gmin = NA
    } else {
        gmin = sapply(P, min)
        
    }
    #
    dist.stats[[k]] = data.frame(k, nmax = max(gcount), nmin = min(gcount), dmax = max(gmax), dmin = min(gmax), omax = max(gmin), omin = min(gmin))
}
#
dist.stats = do.call(rbind, dist.stats)

# -------------------------------------------------------------------------------------------------

png("fig/cluster-wwlln-distance-minmax.png", width = 800, height = 800)
p1 <- ggplot(dist.stats, aes(x = k)) +
    geom_ribbon(aes(ymin = dmin, ymax = dmax), fill = "lightblue") +
    geom_point(aes(y = dmax), size = 3) + geom_line(aes(y = dmax)) +
    geom_point(aes(y = dmin), size = 3) + geom_line(aes(y = dmin)) +
    ylab("maximum within-cluster distance") + xlab("") +
    scale_x_continuous(breaks=1:NCUTS) +
    theme_classic()
p2 <- ggplot(dist.stats, aes(x = k)) +
    geom_ribbon(aes(ymin = omin, ymax = omax), fill = "lightgreen") +
    geom_point(aes(y = omax), size = 3) + geom_line(aes(y = omax)) +
    geom_point(aes(y = omin), size = 3) + geom_line(aes(y = omin)) +
    ylab("minimum between-cluster distance") + xlab("") +
    scale_x_continuous(breaks=1:NCUTS) +
    theme_classic()
p3 <- ggplot(dist.stats, aes(x = k)) +
    geom_ribbon(aes(ymin = nmin, ymax = nmax), fill = "red", alpha = 0.5) +
    geom_point(aes(y = nmax), size = 3) + geom_line(aes(y = nmax)) +
    geom_point(aes(y = nmin), size = 3) + geom_line(aes(y = nmin)) +
    ylab("number of points per cluster") +
    scale_x_continuous(breaks=1:NCUTS) +
    theme_classic()
#
grid.arrange(p1, p2, p3, nrow = 3)
dev.off()

# -------------------------------------------------------------------------------------------------

for (k in 6:20) {
    W$group <- cutree(clusters, k = k)
    #
    W$group = factor(W$group)

    m = map +
        geom_point(aes(x = lon, y = lat, color = group), data = W, alpha = 1/2, size = 5) +
        annotate("text", label = paste("k =", k), x = 35, y = -34, size = 6) +
        theme(legend.position = "none")
    
    png(sprintf("fig/wwlln-cluster-map-%s-%02d-k-%02d.png", label, hour, k), width = 800, height = 800)
    print(m)
    dev.off()
}