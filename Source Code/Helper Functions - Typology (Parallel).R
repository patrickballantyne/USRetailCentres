# Parallel Typology Functions

## Region State Lists
ne <- c("CT", "ME", "MA", "NH", "NJ", "NY", "PA", "RI", "VT")
mw <- c("IL", "IN", "MI", "OH", "WI", "IA", "KS", "MN", "MO", "NE", "ND", "SD")
s <- c("DE", "FL", "GA", "MD", "NC", "SC", "VA", "DC", "WV",
       "AL", "KY", "MS", "TN",
       "AR", "LA", "OK", "TX")
w <- c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY",
       "AK", "CA", "HI", "OR", "WA")


## Function that computes average silhouette scores across values of K, for determing K value used in PAM
get_silhouette_scores <- function(db, seed = 123) {
  
  ## Params
  set.seed(seed)
  k_ls <- c(2:10)
  
  ## Run PAM
  cl <- mclapply(k_ls, function(x) pam(db, k = x), mc.cores = 8)
  cl <- mclapply(cl, function(x) {
    X <- x$clustering
    return(X)
  }, mc.cores = 8)
  
  cl_out <- as.data.frame(do.call(cbind, cl))
  colnames(cl_out) <- c("k=2", "k=3", "k=4", "k=5", "k=6", "k=7", "k=8", "k=9", "k=10")
  #db_cl <- cbind(db, cl_out)
  
  ## Computation of silhouette scores
  col_ls <- c(1:9)
  ss <- mclapply(col_ls, function(x) silhouette(cl_out[, x], dist(db)), mc.cores = 8)
  
  ## Computation of average silhouette scores
  avg_ss <- lapply(ss, function(x) mean(x[, 3]))
  avg_ss <- as.data.frame(do.call(rbind, avg_ss))
  avg_ss$k <- c(2:10)
  colnames(avg_ss) <- c("avg_silhouette_score", "k")
  avg_ss ## Print output
  
}

## Function for extracting cluster id's and cluster medoids from PAM
run_typology <- function(db, k = 3) {
  
  ## Run it
  pm <- pam(x = db, k = k, metric = "euclidean")
  
  ## Extract Clustering 
  cl <- as.data.frame(pm$clustering)
  cl <- cbind(cl, db)
  colnames(cl)[1] <- "cluster"
  
  ## Extract medoids
  med <- as.data.frame(pm$medoids)
  med$cluster <- seq_along(med[,1])
  med <- med %>%
    gather(variable, cluster_vals, -c(cluster)) %>%
    mutate(pos = cluster_vals >= 0)
  
  out <- list(cl, med)
  return(out)
}

## Function that performs PCA on each group to identify variables worth removing 
run_type_pca <- function(groups, cl = 1) {
  
  ## Filter and process
  groups <- groups %>%
    filter(cluster == cl) %>%
    as.data.frame() %>%
    select(-c(rcID, rcName, cluster, geom))
  
  ## Run PCA
  pca <- PCA(groups, graph = FALSE)
  
  ## Visualise 
  fviz_contrib(pca, choice = "var", axes = 1:2)
}

## Function used to help identify best k value to split each group into types
identify_type_k <- function(groups, cl = 1, col_list) {
  
  ## Filter and process
  groups <- groups %>%
    filter(cluster == cl) %>%
    as.data.frame() %>%
    select(-c(rcID, rcName, cluster, geom))
  
  ## Drop insignificant vars
  groups = groups[,!(names(groups) %in% col_list)]
  
  ## Get the silhouette scores
  ss <- get_silhouette_scores(groups)
  
  ## Plot silhouettes
  silhouettes <- fviz_nbclust(groups,  cluster::pam, method = "silhouette", k.max = 10) +
    labs(subtitle = "Silhouette Method")
  
  ## Plot the elbow
  elbow <- fviz_nbclust(groups,  cluster::pam, method = "wss", k.max = 10) +
    labs(subtitle = "Elbow Method")
  
  ## Return
  ls <- list(ss, silhouettes, elbow)
  return(ls)
}


## Function for re-running the typology, to get nested types 
get_nested_types <- function(groups, cl = 1, k_vals, medoids = FALSE) {
  
  ## Filter to cluster
  groups <- groups %>%
    dplyr::filter(cluster == cl)
  
  ## Remove names/id's
  groups_df <- groups %>%
    as.data.frame() %>%
    select(-c(rcID, rcName, cluster, geom))
  
  ## Filter k vals to get only the cluster we want
  k <- k_vals %>%
    filter(cluster == cl)
  
  ## Run typology
  pm <- run_typology(groups_df, k = k$k)
  
  ## Formatting for output
  clustering <- pm[[1]]
  clustering <- clustering %>%
    select(cluster) %>%
    dplyr::rename(type = cluster)
  
  ## Join with original groups and merge cluster and type columns together
  out <- cbind(groups, clustering)
  return(out)
  out <- out %>%
    dplyr::rename(group = cluster) %>%
    select(rcID, rcName, group, type) %>%
    dplyr::rename(group_id = group) %>%
    transform(type_id = paste(group_id, type, sep = "."))
  
  
  if (medoids == FALSE) {
    out <- out %>% select(-c(type))
    return(out)
  } else if (medoids == TRUE) {
    
    ## Get id's to merge
    type_ids <- out %>%
      as.data.frame() %>%
      select(group_id, type, type_id) %>%
      unique()
    
    ## Extract medoids
    medoids <- pm[[2]]
    
    ## Attach new ID's 
    medoids <- merge(medoids, type_ids, by.x = "cluster", by.y = "type", all.x = TRUE)
    medoids <- medoids %>%
      select(type_id, variable, cluster_vals, pos) %>%
      dplyr::rename(cluster = type_id)
    return(medoids)
  }
  
}

## Compute p values for corrplot
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

## Plotting of Clustergram
## Clustergram
plot_clustergram <- function(Data, k.range = 2:10 , 
                             clustering.function = clustergram.kmeans,
                             clustergram.plot = clustergram.plot.matlines, 
                             line.width = .00005, add.center.points = T)
{
  # Data - should be a scales matrix.  Where each column belongs to a different dimension of the observations
  # k.range - is a vector with the number of clusters to plot the clustergram for
  # clustering.function - this is not really used, but offers a bases to later extend the function to other algorithms 
  #			Although that would  more work on the code
  # line.width - is the amount to lift each line in the plot so they won't superimpose eachother
  # add.center.points - just assures that we want to plot points of the cluster means
  Data <- as.matrix(Data)
  n <- dim(Data)[1]
  
  PCA.1 <- Data %*% princomp(Data)$loadings[,1]	# first principal component of our data
  
  if(require(colorspace)) {
    COL <- heat_hcl(n)[order(PCA.1)]	# line colors
  } else {
    COL <- rainbow(n)[order(PCA.1)]	# line colors
    warning('Please consider installing the package "colorspace" for prittier colors')
  }
  
  line.width <- rep(line.width, n)
  
  Y <- NULL	# Y matrix
  X <- NULL	# X matrix
  
  centers.points <- list()
  
  for(k in k.range)
  {
    k.clusters <- clustering.function(Data, k)
    
    clusters.vec <- k.clusters$cluster
    # the.centers <- apply(cl$centers,1, mean)
    the.centers <- k.clusters$centers 
    
    noise <- unlist(tapply(line.width, clusters.vec, cumsum))[order(seq_along(clusters.vec)[order(clusters.vec)])]	
    # noise <- noise - mean(range(noise))
    y <- the.centers[clusters.vec] + noise
    Y <- cbind(Y, y)
    x <- rep(k, length(y))
    X <- cbind(X, x)
    
    centers.points[[k]] <- data.frame(y = the.centers , x = rep(k , k))	
    #	points(the.centers ~ rep(k , k), pch = 19, col = "red", cex = 1.5)
  }
  
  
  x.range <- range(k.range)
  y.range <- range(c(-5,10))
  
  clustergram.plot(X,Y, k.range, 
                   x.range, y.range , COL, 
                   add.center.points , centers.points)
  
  
}


## Plot medoids from PAM
plot_medoids <- function(pm) {
  
  ## Extract medoids from clustering object
  md <- pm[[2]]
  
  ## Split by cluster
  md_ls <- split(md, md$cluster)
  
  ## Plot all at once
  plots <- lapply(md_ls, function(x) ggplot(data = x) +
                    aes(x = reorder(variable, -cluster_vals), y = cluster_vals, fill = pos) +
                    geom_col(position = "identity", size = 0.25, colour = "black") + 
                    scale_fill_manual(values = c("#FFDDDD", "#CCEEFF"), guide = FALSE) +
                    xlab("Variable") +
                    ylab("Median Values") +
                    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), axis.ticks = element_line(),
                          axis.line = element_line(colour = "black"), 
                          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                          panel.background = element_blank()))
  ggpubr::ggarrange(plotlist = plots, labels = md$cluster)
}

## Plot type medoids 
plot_type_medoids <- function(medoids) {
  
  ## Split by cluster
  md_ls <- split(medoids, medoids$cluster)
  
  ## Plot all at once
  plots <- lapply(md_ls, function(x) ggplot(data = x) +
                    aes(x = reorder(variable, -cluster_vals), y = cluster_vals, fill = pos) +
                    geom_col(position = "identity", size = 0.25, colour = "black") + 
                    scale_fill_manual(values = c("#FFDDDD", "#CCEEFF"), guide = FALSE) +
                    ylab("Median Values") +
                    theme(axis.text.x = element_text(angle = 90), axis.ticks = element_line(),
                          axis.line = element_line(colour = "black"), axis.title.x = element_blank(),
                          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                          panel.background = element_blank()))
  ggpubr::ggarrange(plotlist = plots, vjust = 1.0, align = "hv")
  
}


## Dependency function of Clustergram
clustergram.kmeans <- function(Data, k, ...)
{
  # this is the type of function that the clustergram
  # 	function takes for the clustering.
  # 	using similar structure will allow implementation of different clustering algorithms
  
  #	It returns a list with two elements:
  #	cluster = a vector of length of n (the number of subjects/items)
  #				indicating to which cluster each item belongs.
  #	centers = a k dimensional vector.  Each element is 1 number that represent that cluster
  #				In our case, we are using the weighted mean of the cluster dimensions by 
  #				Using the first component (loading) of the PCA of the Data.
  
  cl <- kmeans(Data, k,...)
  
  cluster <- cl$cluster
  centers <- cl$centers %*% princomp(Data)$loadings[,1]	# 1 number per center
  # here we are using the weighted mean for each
  
  return(list(
    cluster = cluster,
    centers = centers
  ))
}	

## Dependency function of Clustergram
clustergram.plot.matlines <- function(X,Y, k.range, 
                                      x.range, y.range , COL, 
                                      add.center.points , centers.points)
{
  plot(0,0, col = "white", xlim = x.range, ylim = y.range,
       axes = F,
       xlab = "Number of clusters (k)", ylab = "PCA weighted Mean of the clusters")
  axis(side =1, at = k.range)
  axis(side =2)
  abline(v = k.range, col = "grey")
  
  matlines(t(X), t(Y), pch = 19, col = COL, lty = 1, lwd = 1.5)
  
  if(add.center.points)
  {
    require(plyr)
    
    xx <- ldply(centers.points, rbind)
    points(xx$y~xx$x, pch = 19, col = "red", cex = 1.3)
    
    # add points	
    # temp <- l_ply(centers.points, function(xx) {
    # with(xx,points(y~x, pch = 19, col = "red", cex = 1.3))
    # points(xx$y~xx$x, pch = 19, col = "red", cex = 1.3)
    # return(1)
    # })
    # We assign the lapply to a variable (temp) only to suppress the lapply "NULL" output
  }	
}