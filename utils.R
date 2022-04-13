##
## mclapply.hack.R
##
## Nathan VanHoudnos
## nathanvan AT northwestern FULL STOP edu
## July 14, 2014
##
## A script to implement a hackish version of 
## parallel:mclapply() on Windows machines.
## On Linux or Mac, the script has no effect
## beyond loading the parallel library. 

require(parallel)    

## Define the hack
# l_env a character string of environment variables needed for mclapply.hack
mclapply.hack <- function(mc.cores = 24, l_env =NULL, ...) {
  ## Create a cluster
 size.of.list <- mc.cores # length(list(...)[[1]]) # Lzhang 27Feb2022
  cl <- makeCluster( min(size.of.list, detectCores()) )
  cat("mclapply.hack uses ", min(size.of.list, detectCores()),
      " clusters.\n") # Lzhang 27Feb2022
  
  ## Find out the names of the loaded packages 
  loaded.package.names <- c(
    ## Base packages
    sessionInfo()$basePkgs,
    ## Additional packages
    names( sessionInfo()$otherPkgs ))
  
  tryCatch( {
    
    ## Copy over all of the objects within scope to
    ## all clusters. 
    this.env <- environment()
    while( identical( this.env, globalenv() ) == FALSE ) {
      clusterExport(cl,
                    ls(all.names=TRUE, env=this.env),
                    envir=this.env)
      this.env <- parent.env(environment())
    }
cat("1\n")
    l <- ls(all.names = T, env = globalenv()) # lzhang 27Feb2022 takes long time if big data in memeory
    if(!is.null(l_env)) {
      l <- c(l_env, ".Random.seed")
    }
    clusterExport(cl,
                  l, # ls(all.names=TRUE, env=globalenv()), # lzhang 27Feb2022
                  envir=globalenv())
cat("2\n")    
    ## Load the libraries on all the clusters
    ## N.B. length(cl) returns the number of clusters
    parLapply( cl, 1:length(cl), function(xx){
      lapply(loaded.package.names, function(yy) {
        require(yy , character.only=TRUE)})
    })
cat("3\n")    
    ## Run the lapply in parallel 
    return( parLapply( cl, ...) )
  }, finally = {        
    ## Stop the cluster
    stopCluster(cl)
  })
}

## Warn the user if they are using Windows
if( Sys.info()[['sysname']] == 'Windows' ){
  message(paste(
    "\n", 
    "   *** Microsoft Windows detected ***\n",
    "   \n",
    "   For technical reasons, the MS Windows version of mclapply()\n",
    "   is implemented as a serial function instead of a parallel\n",
    "   function.",
    "   \n\n",
    "   As a quick hack, we replace this serial version of mclapply()\n",
    "   with a wrapper to parLapply() for this R session. Please see\n\n",
    "     http://www.stat.cmu.edu/~nmv/2014/07/14/implementing-mclapply-on-windows \n\n",
    "   for details.\n\n"))
}

## If the OS is Windows, set mclapply to the
## the hackish version. Otherwise, leave the
## definition alone. 
mclapply <- switch( Sys.info()[['sysname']],
                    Windows = {mclapply.hack}, 
                    Linux   = {mclapply},
                    Darwin  = {mclapply})

## end mclapply.hack.R

#' A script to print significiant digits in R markdown output
inline_hook <- function (x) { #, digits = 2) {
  if (is.numeric(x)) {
    # ifelse does a vectorized comparison
    # If integer, print without decimal; otherwise print two places
    res <- ifelse(x == round(x),
                  sprintf("%d", x),
                  signif(x, 2) #sprintf(paste0("%.", digits, "f"), x)
    )
    paste(res, collapse = ", ")
  }
}

knitr::knit_hooks$set(inline = inline_hook)
knitr::knit_hooks$get("inline")
