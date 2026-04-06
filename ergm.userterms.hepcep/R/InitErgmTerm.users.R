#  File R/InitErgmTerm.users.R in package ergm.userterms, part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 2003-2013 Statnet Commons
#######################################################################
######################################################################
#
# !! USERS: READ THIS FIRST!!
#
# Each term must have its own InitErgmTerm function. You can either
# add all functions to the bottom of this file or have them in 
# separate files (with the extension .R).
#
# This file contains 
#    - a description of the input and output parameters to each
#      InitErgmTerm function
#    - a description of the input parameters that the C changestats
#      function will receive
#    - sample InitErgmTerm functions. These are identical from 
#      "statnet"'s perspective.
#
######################################################################


#  ------------------------------------------------------------------ 
#   Description of the input and output parameters of the  
#   InitErgmTerm.xxx function, where xxx is the name of your term
#  ------------------------------------------------------------------ 
#
#  INPUTS:
#  Each InitErgmTerm function takes three arguments:
#	  		nw: The network of interest
#      arglist: The list of arguments passed to the term xxx
#         ... : There may be other arguments passed by 
#               ergm.getmodel, so each InitErgmTerm function 
#               must include the ... argument
#  These inputs are automatically supplied by ergm.getmodel.
#
#  OUTPUTS:
#  Each InitErgmTerm function should return a list.  
#     REQUIRED LIST ITEMS:
#          name: This names the C changestats function for term xxx, 
#                but does so by excluding the d_ prefix. The 
#                changestats function is named d_xxxy and 'name' is
#                consequently "xxxy". For example, the b1starmix
#                term has 2 changestats functions based on
#                whether the homophily argument is set. These are
#                d_b1starmix and d_b1starmixhomophily. The 'name' 
#                returned by InitErgmTerm.b1starmix is then one of 
#                "b1starmix" or "b1starmixhomophily" as appropriate.
#    coef.names: Vector of names for the coefficients (parameters)
#                as they will be reported in the output.
#       pkgname: This names the package containing the C changestats
#                function d_[name]. The default is "ergm", which means
#                that if you have code that exists as part of the 
#                (say) "ergm.userterms" package, you MUST specify 
#                pkgname="ergm.userterms"
#
#    OPTIONAL LIST ITEMS:
#        inputs: Vector of (double-precision numeric) inputs that the 
#                changestat function called d_'name' may require.
#                The default is NULL; no inputs are required.  But it
#                MUST be a vector!  Thus, if some of the inputs are,  
#                say, matrices, they must be "flattened" to vectors; if 
#                some are categorical character-valued variables, they
#                must be converted to numbers. Optionally, the inputs 
#                vector may have an attribute named "ParamsBeforeCov",
#                which is the number of input parameters preceding the 
#                covariate vector in 'inputs'.  This is necessary for 
#                compatibility with some of the existing d_xxx changestats 
#                functions in ergm, but is not necessary in general.
#    dependence: Logical variable telling whether addition of this term to
#                the model makes the model into a dyadic dependence model.
#                If none of the terms sets dependence==TRUE, then the model
#                is assumed to be a dyadic independence model, which means
#                that the pseudolikelihood estimate coincides with the
#                maximum likelihood estimate.  The default value is TRUE.
#  emptynwstats: Vector of values (if nonzero) for the statistics evaluated
#                on the empty network.  If all are zero for this term, this
#                argument may be omitted.  For example, the degree0 term 
#                would require 'emptynwstats' since degree0 = number of 
#                nodes for the empty network.
#        params: For curved exponential family model terms only, a list of 
#                (numeric) initial values for the parameters of  
#                curved exponential family model terms. Each item in the  
#                list should be named with the corresponding parameter name 
#                (one or more of these will probably coincide with the 
#                 coef.names).  For example, the gwesp term returns 
#                params=list(gwesp=NULL,gwesp.alpha=alpha), where alpha
#                was specified as an argument to the gwesp term. 
#           map: For curved exponential family model terms only, a function 
#                giving the map from the canonical parameters, theta,
#                associated with the statistics for this term, to eta, 
#                the corresponding curved parameters.  The length of eta 
#                is the same as the length of the 'params' list above.
#                The function takes two arguments:  theta and length(eta).
#      gradient: For curved exponential family model terms only, a function 
#                giving the gradient of the 'map'. If theta has length p 
#                and eta has length q, then gradient should return a
#                p by q matrix. This function takes two arguments:  theta 
#                and length(eta).
#


#  ------------------------------------------------------------------------- 
#   Description of the input parameters to the d_xxxy changestats function, 
#   where xxxy corresponds to the 'name' returned by InitErgmTerm.xxx.
#  -------------------------------------------------------------------------- 
#
#  INPUTS:
#  Each d_xxxy function takes five arguments:
#	    ntoggles: the number of toggles as described in 
#                 "ergm.userterms: A template package"
#          heads: a pointer to the array of the head nodes of the 
#                 proposed edges to be toggled
#          tails: a pointer to the array of the tail nodes of the
#                 proposed edges to be toggled
#            mtp: a pointer to the model, which includes the following:
#                 dstats      : a pointer to the array of changestats,
#                               macro-ed as CHANGE_STAT
#                 nstats      : the length of 'dstats', macro-ed as
#                               N_CHANGE_STATS
#                 inputparams : a pointer to the vector of input 
#                               parameters. This is supplied by the
#                               'inputs' returned by InitErgmTerm.xxx
#                               and is macro-ed as INPUT_PARAM
#                 ninputparams: the length of 'inputparams', macro-ed
#                               as N_INPUT_PARAMS
#            nwp: a pointer to the network.  This includes several 
#                 components and several macros exist for accessing
#                 these. See the changestat.h file for a list of these
#                 components and their macros. 
#  These inputs are automatically supplied to the d_xxxy function by the 
#  network_stats_wrapper function 



#  ------------------------------------------------------------------------- 
#  Sample InitErgmTerm function(s)
#  -------------------------------------------------------------------------- 


InitErgmTerm.dist <- function(nw, arglist, ...) {
  a <- check.ErgmTerm(nw, arglist, directed=NULL, bipartite=FALSE,
      varnames = c("dist"),
      vartypes = c("numeric"),
      required = c(TRUE),
      defaultvalues = list(NULL))
  dist<-a$dist
  if(length(dist)==0){return(NULL)}
  coef.names <- paste("dist",dist,sep="")
  name <- "dist"
  nodelat <- get.node.attr(nw, "lat")
  nodelon <- get.node.attr(nw, "lon")
  list(name = name,
      coef.names = coef.names,
      pkgname = "ergm.userterms.hepcep",
      inputs = c(dist, nodelat, nodelon),
      dependence = FALSE
  )
}



# Name: dnf (distance near far)
# args:
#   by          category name, e.g., "chicago", with assumption that the n categories are 1,...,n
#   thresholds  distance (in km) demarcating near vs. far for each category, e.g, c(2,4) => 2km for chicago == 1, 4km for chicago == 2
#   base        statistic to omit (default is the last one), e.g., if there are 2 categories, there are 4-1 total terms, 
#               where the terms would be dnf.<by>.<i>.<n/f>, where i = 1,.., number of categories
#               e.g., dnf.chicago.1.n , dnf.chicago.1.f, dnf.chicago.2.n, dnf.chicago.2.f
InitErgmTerm.dnf <- function(nw, arglist, ...) {
  # should only apply to directed networks
  a <- check.ErgmTerm(nw, arglist, directed=TRUE, bipartite=FALSE,
                      varnames = c("by","thresholds","base"),
                      vartypes = c("character","numeric","numeric"), 
                      required = c(TRUE, TRUE, FALSE),
                      defaultvalues = list(NULL,NULL,NULL))
  
  
  # Category attribute
  cat.name <- a$by
  nodecat <- get.node.attr(nw, cat.name)
  
  if(length(a$thresholds)==0){
      stop("The argument thresholds to dnf expected a vector of length at least ",
           "1, but received a vector of length 0")}
  
  thresholds = a$thresholds
  # Note: number of categories depends on the number of thresholds specified
  num.cats = length(thresholds)
  
  # Assign end term as default base if base is unspecified
  if(is.null(a$base)) {
    base = num.cats * 2
  } else {
    base = a$base
    if (base > num.cats * 2){
      stop("Base needs to be between 1 and the (number of thresholds * 2), ",
           "inclusive, but it was specified as ", base, ".")
    }
  }
  
  # This will generate e.g.,:
  # dnf.chicago.1.n , dnf.chicago.1.f, dnf.chicago.2.n, dnf.chicago.2.f 
  # and then we remove the base term
  coef.names <- paste("dnf", cat.name, rep(1:num.cats, each = 2), c("n","f") ,sep=".") 
  # remove base
  coef.names <- coef.names[-base]
  
  name <- "dnf"
  nodelat <- get.node.attr(nw, "lat")
  nodelon <- get.node.attr(nw, "lon")
  
  list(name = name,
       coef.names = coef.names, 
       pkgname = "ergm.userterms.hepcep",
       inputs = c(num.cats, thresholds, base, nodelat, nodelon, nodecat),
       dependence = FALSE
  )
}




