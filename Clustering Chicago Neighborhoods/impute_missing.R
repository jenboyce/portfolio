# Compute distance of incomplete records to the centroid of each cluster and assign each case to the closest cluster
# Input is a df that has all records, including incomplete ones, with cluster numbers for complete records
# output is list of cluster numbers saved in vector assignedcluster

# Be sure columns in observations and centroids are in the same order!
# Before running this, change the variable name for your cluster assignments to "ClusterNum":
# names(clusters)[54] <- "ClusterNum"


impute_missing = function(all_obs, medoids) {
  clusters.inc=all_obs[!complete.cases(all_obs),] # subset of the incomplete records that need to be assigned to a cluster
  tracts = clusters.inc$CensusTract # Grab the census tract IDs for later
  clusters.inc = clusters.inc[,-c(1)] # Remove $CensusTract, so we only have actual data
  clusters_complete=all_obs[complete.cases(all_obs),] # Complete records only

  n.inc=nrow(clusters.inc) # Number of incomplete observations

  assignedcluster=c()

  for (i in 1:n.inc){
    outd=c() # Holding tank for the calculated distances
    for(j in 1:7){
      outd=c(outd, c(dist(rbind(medoids[j,], clusters.inc[i,])))) # Calculate the euclidian distance between each observation i and the cluster centroid j
    }
    cl=which.min(outd) # Choose the cluster with the smallest distance
    assignedcluster=c(assignedcluster,cl)
  }

  clusters.inc$ClusterNum=assignedcluster  # create new column “cluster” in dataframe to include clustering information for each record.

  clusters.inc$CensusTract = tracts # Rejoin the tract labels so we know the tract ID of the newly-assigned items
  return(rbind(clusters.inc, clusters_complete)) # Join the newly imputed data with complete cases so we have all observations in one place.
}


