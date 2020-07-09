library(cluster)
library(geosphere)
library(ggplot2)
library(gridExtra)
library(reshape2)

X11.options(antialias = "subpixel")

# hclust() [stats]
# agnes() [cluster]
# diana()
# 
# kmeans()
#
# DCluster package
# 
# Function pam() from package cluster implements partitioning around medoids and can work with arbitrary distances.
# Function clara() is a wrapper to pam() for larger data sets. Silhouette plots and spanning ellipses can be used
# for visualization.

# CLUSTER DEMONSTRATION ---------------------------------------------------------------------------

set.seed(1)

lonlat <- data.frame(
    lon = c(rnorm(10, 0, 10), rnorm(15, 90, 10), rnorm(10, -50, 10), rnorm(5, 10, 5)),
    lat = c(rnorm(10, 0, 10), rnorm(15, 45, 10), rnorm(10, 20, 10), rnorm(5, 50, 5)))

png("fig/cluster-demo-points.png", width = 800, height = 800)
ggplot(lonlat, aes(x = lon, y = lat, label = rownames(lonlat))) +
    geom_point(size = 5) +
    geom_text(hjust=-0.5, vjust=1.5, size = 5, alpha = 0.5) +
    theme(panel.background = element_blank())
dev.off()

# Calculate distance matrix using Great Circle Distances
#
distance.matrix <- distm(lonlat[, c("lon", "lat")], fun = distHaversine)
dim(distance.matrix)
#
# Convert from m to Mm
#
distance.matrix = distance.matrix / 1e6

png("fig/cluster-demo-distance-matrix.png", width = 800, height = 800)
ggplot(melt(distance.matrix, value.name = "Mm"), aes(x = Var1, y = Var2, fill = Mm)) +
    geom_tile(colour='black') +
    xlab("") + ylab("") +
    theme_classic() +
    theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
dev.off()

# CHOOSING NUMBER OF CLUSTERS ---------------------------------------------------------------------

# http://en.wikipedia.org/wiki/Determining_the_number_of_clusters_in_a_data_set

# DIRECT METHOD -----------------------------------------------------------------------------------

# Good information on determining the number of clusters can be found here:
#
# http://stackoverflow.com/questions/15376075/cluster-analysis-in-r-determine-the-optimal-number-of-clusters
#
# One method that works well for this data is tried below.
#
# PAM = "Partitioning Around Medoids"

library(fpc)
#
pamk.best <- pamk(distance.matrix)
#
cat("number of clusters estimated by optimum average silhouette width:", pamk.best$nc, "\n")

# -------------------------------------------------------------------------------------------------

# Perform Agglomerative Nesting (Hierarchical Clustering)
#
clusters <- agnes(distance.matrix)
#
png("fig/cluster-demo-simple-tree.png", width = 800, height = 800)
par(mar = c(2.1, 4.1, 2.1, 1.1))
plot(clusters, which.plots = 2, main = "", sub = "", xlab = "")
abline(h = 20, lty = "dashed")
abline(h = 30, lty = "dotted", col = "grey")
abline(h = 10, lty = "dotted", col = "grey")
dev.off()

# Divide into groups
#
lonlat$group <- cutree(clusters, k = 4)
#
# Convert to factor so that we get discrete colours in plot
#
lonlat$group = factor(lonlat$group)

png("fig/cluster-demo-points-colour-groups.png", width = 800, height = 800)
ggplot(lonlat, aes(x = lon, y = lat, label = rownames(lonlat))) +
    geom_point(size = 5, aes(color = group)) +
    geom_text(hjust=-0.5, vjust=1.5, size = 5, alpha = 0.5) +
    theme(panel.background = element_blank()) +
    theme(legend.position = "none")
dev.off()

# DIFFERENT NUMBER OF CLUSTERS --------------------------------------------------------------------

NCUTS <- 10

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

png("fig/cluster-demo-distance-minmax.png", width = 800, height = 800)
p1 <- ggplot(dist.stats, aes(x = k)) +
    geom_ribbon(aes(ymin = dmin, ymax = dmax), fill = "lightblue") +
    geom_point(aes(y = dmax), size = 3) + geom_line(aes(y = dmax)) +
    geom_point(aes(y = dmin), size = 3) + geom_line(aes(y = dmin)) +
    ylab("maximum within-cluster distance") +
    scale_x_continuous(breaks=1:10) +
    theme_classic()
p2 <- ggplot(dist.stats, aes(x = k)) +
    geom_ribbon(aes(ymin = omin, ymax = omax), fill = "lightgreen") +
    geom_point(aes(y = omax), size = 3) + geom_line(aes(y = omax)) +
    geom_point(aes(y = omin), size = 3) + geom_line(aes(y = omin)) +
    ylab("minimum between-cluster distance") +
    scale_x_continuous(breaks=1:10) +
    theme_classic()
p3 <- ggplot(dist.stats, aes(x = k)) +
    geom_ribbon(aes(ymin = nmin, ymax = nmax), fill = "red", alpha = 0.5) +
    geom_point(aes(y = nmax), size = 3) + geom_line(aes(y = nmax)) +
    geom_point(aes(y = nmin), size = 3) + geom_line(aes(y = nmin)) +
    geom_hline(yintercept = seq(5, 40, 5), linetype = "dashed") +
    ylab("number of points per cluster") +
    scale_x_continuous(breaks=1:10) +
    theme_classic()
#
grid.arrange(p1, p2, p3, nrow = 3)
dev.off()

# -------------------------------------------------------------------------------------------------
