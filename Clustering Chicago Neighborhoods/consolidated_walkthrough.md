Comparing Clustering Methods for Housing Segmentation
================

### For purposes of this demo, we'll assume you've already done data exploration

``` r
library(cluster) # For PAM
library(kohonen) # For SOM
library(ggplot2) # Plotting
library(stats) # To plot a smooth curve on silhouette plot
library(clusterCrit) # Evalution metrics for clustering
library(kknn) # For spectral
library(dplyr)
source('impute_missing.R')

set.seed(12)
```

Set up the data
==============

``` r
df = read.csv('housing_data.csv', header=TRUE, stringsAsFactors = FALSE)
cvars = na.omit(df) # SOM and spectral won't work with missing values. We'll impute the missing ones later.
vars_only = cvars[,-c(1)]  # Remove the census tract column
var_matrix = as.matrix(vars_only)
```

K-Medoids
=========

Choose the optimal K
--------------------

### This is a very ugly way of getting a vector of all of the silhouette widths to plot.

``` r
pam_silh = data.frame()
for (i in seq(1:12)){
  k = pam(vars_only, i, metric='euclidian')
  pam_silh = rbind(pam_silh, k$silinfo$avg.width)
}
pam_silh$k = seq(1:11)

scatter.smooth(pam_silh[,2], pam_silh[,1], xlab='K', ylab='Silhouette Width', main='Average Silhouette Width by K \n(k-Medoids)')
abline(v=7,lty=2,col="blue")
```

![](consolidated_walkthrough_files/figure-markdown_github/unnamed-chunk-3-1.png)
 \#\#\# It looks like k=7 is a good option.

Cluster the data
----------------

### K-medoids will be the baseline method to which we compare more advanced methods.

``` r
pam_clus = pam(vars_only, 7, metric='euclidian')  # K-medoids can handle missing values, so no need to use the no-NA version
pam_medoids = pam_clus$medoids
```

Evaluate clustering results
---------------------------

``` r
table(pam_clus$clustering)
```

    ## 
    ##   1   2   3   4   5   6   7 
    ## 389 345 252 227 315 306  59

### We have one very small cluster, but otherwise the balance looks pretty good.

``` r
intCriteria(var_matrix,as.integer(pam_clus$clustering),c("silhouette")) # Compute internal validity metrics. Can use "all" to get many, many more.
```

    ## $silhouette
    ## [1] 0.07190662

Self-Organizing Maps
--------------------

### First, we'll build a SOM grid with the census tracts

### To set up the grid, we're using roughly the recommendation of 5 \* sqrt(num rows \* num columns)

``` r
som_grid <- somgrid(xdim = 11, ydim=26, topo="hexagonal")
som_model <- som(var_matrix,
                 grid=som_grid,
                 rlen=100,
                 alpha=c(0.05,0),
                 toroidal = TRUE,
                 keep.data = TRUE,
                 n.hood="circular")
summary(som_model)
```

    ## som map of size 11x26 with a hexagonaltoroidal topology.
    ## Training data included; dimension is 1893 by 52
    ## Mean distance to the closest unit in the map: 0.2713122

### Now, we'll cluster the nodes in the map

``` r
som.kmeans <- kmeans((dist(som_model$codes)), 7)
table(som.kmeans$cluster) # Number of NODES per cluster
```

    ## 
    ##  1  2  3  4  5  6  7 
    ## 73 16 68  5 53 65  6

### We have a list of node clusters, but now we need to join the node cluster assignments with the ids of the tracts that were assigned to that node. This join will give us the cluster number for each tract.

``` r
cluster_assignments = as.data.frame(cbind(som.kmeans$cluster, som_model$codes)) # This gives the values for the centroids of each node
cluster_assignments$ind = seq.int(nrow(cluster_assignments)) # This gives each node an index number

tracts_plus_node_assignments = as.data.frame(cbind(cvars$CensusTract,som_model$unit.classif)) # This joins each census tract id with the node number to which it was assigned by the SOM model

cluster_assignments$V2 = cluster_assignments$som.kmeans
tracts_plus_node_assignments$V1 = as.character(tracts_plus_node_assignments$V1)

tract_clusters = left_join(tracts_plus_node_assignments, cluster_assignments, by = c("V2" = "ind")) # Match the dataframe with the census tracts and their node assignments with the corresponding cluster ID for that node.
```

Evaluate clustering results
---------------------------

``` r
table(tract_clusters$V1.y) # Cluster sizes
```

    ## 
    ##   1   2   3   4   5   6   7 
    ## 539  52 429  20 303 532  18

### We have two very small clusters with SOM. This isn't a great thing to see.

``` r
intCriteria(as.matrix(tract_clusters[,-c(1:3)]),as.integer(tract_clusters$V1.y),c("silhouette")) # Compute internal validity metrics
```

    ## $silhouette
    ## [1] 0.09302045

### Silhouette width is wider, one indication that this might be a slightly poorer clustering than k-medoids.

### We need to get back the tracts that were dropped because they had NAs. We need one list of the dropped items with the clustered items to use the completed items to impute clusters for items with missing values.

``` r
tract_clusters$V2 = NULL # Remove join columns that are no longer needed
names(tract_clusters)[1] <- "CensusTract"

som_cluster_centroids <-aggregate(tract_clusters[,-c(1)], by=list(tract_clusters$V1.y), FUN=mean, na.rm=FALSE) # Centroids of the CLUSTER are needed for imputation

som_tracts = tract_clusters$CensusTract

som_missings <- df[!(df$CensusTract %in% som_tracts),]
som_missings$ClusterNum = NA # Need to make the number of columns match
tract_clusters$ClusterNum = tract_clusters$V1.y # We need this line and the next to make sure column names and ordering match
tract_clusters$V1.y = NULL
all_som_obs = rbind(tract_clusters, som_missings) # Make a df with all census tracts

# Drop columns we don't need for imputation
som_cluster_centroids$Group.1 = NULL
som_cluster_centroids$V1.y = NULL
som_cluster_centroids$`sp_knn_7$cluster` = NULL
som_cluster_centroids$ClusterNum = NA # Need a placeholder column
```

Impute missing
--------------

### Now we impute cluster assignments for the items that had missing values.

``` r
som_all_with_imputed = impute_missing(all_som_obs, som_cluster_centroids)
```

Spectral Clustering
===================

Cluster the data
----------------

``` r
sp_knn_7 = specClust(var_matrix, centers=7, nn=5)

k7_vars = cbind(cvars, sp_knn_7$cluster) # Join the complete records with their assigned cluster

spectral_medoids <-aggregate(k7_vars[,-c(1)], by=list(k7_vars$`sp_knn_7$cluster`), FUN=mean, na.rm=TRUE) # Calculate the medoids/centroids for each cluster
spectral_medoids$Group.1 <- NULL # Drop redundant column
```

Evaluate clustering
-------------------

``` r
table(k7_vars$`sp_knn_7$cluster`) # Cluster sizes
```

    ## 
    ##   1   2   3   4   5   6   7 
    ## 146 342 203 310 140 273 479

### Cluster sizes are more balanced than results from the other algorithms.

``` r
intCriteria(var_matrix,sp_knn_7$cluster,c("silhouette")) # Compute internal validity metrics
```

    ## $silhouette
    ## [1] 0.03512038

### Silhouette width is quite a bit smaller than before, suggesting this clustering better describes the underlying patterns in the data.

### 

### As above, we need to get back the tracts that were dropped because they had NAs. We need one list of the dropped items with the clustered items to use the completed items to impute clusters for items with missing values.

``` r
tracts = k7_vars$CensusTract
missings <- df[!(df$CensusTract %in% tracts),]
missings$`sp_knn_7$cluster` = NA # Need to make the number of columns match
all_k7_obs = rbind(k7_vars, missings) # Make a df with all census tracts

# Setup for imputation- make sure cluster number column names match
names(spectral_medoids)[53] <- "ClusterNum"
names(all_k7_obs)[54] <- "ClusterNum"
spectral_medoids$ClusterNum = NA
```

Impute missing
--------------

### Now we impute cluster assignments for the items that had missing values.

``` r
spectral_all_with_imputed = impute_missing(all_k7_obs, spectral_medoids) # Our final dataset with all tracts assigned to a cluster
```

### Now that we have clustered datasets, we're ready to map them to see if the clustering makes sense with what we know about Chicago neighborhoods.
