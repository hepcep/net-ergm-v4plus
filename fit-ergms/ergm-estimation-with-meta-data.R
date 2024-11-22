# Analyze synthetic dataset with 32K nodes 

# Fit ERGM with 5 dyadic independent terms

rm(list=ls())

# Activate R environment ------------------------------

library(renv)
renv::activate()


# Libraries ----------

library(network)
library(ergm)
library(dplyr)
library(ergm.userterms)
library(here)


# Read Synthpop Data ------------------------------ 

data_path <- here("data", "synthpop-2023-10-12 12_01_32.csv")
data <- read.csv(data_path, header=TRUE)
glimpse(data)

# Input  In/Outdegree Data ------------------------------
# (from networks simulated form the fit, as an alternate source of targets)

inedges <- read.csv(here("data", "pplrss.csv")) #in- and out-edges
outedges <- read.csv(here("data", "ppldss.csv"))
negbin_indeg <- read.csv(here("data", "negbin-indeg.csv"))
negbin_outdeg <- read.csv(here("data", "negbin-outdeg.csv"))
inedges.data <- read.csv(file = here("data", "inedges_data.csv"), header = TRUE)
outedges.data <- read.csv(file = here("data", "outedges_data.csv"), header = TRUE)

# Input  Degree Distribution Data ------------------------------

#degree_distribution_target_stats <- 
 # readRDS(here("simulate-from-ergms/out/degree_distribution_target_stats.RDS"))
#names(degree_distribution_target_stats)

 

# Initialize network ----------

n <- nrow(data)
n0 <- network.initialize(n = n, directed = TRUE)


# Compute target number of edges ----------

inedges_target <- sum(inedges.data$k * inedges.data$nbprob * n)
outedges_target <- sum(outedges.data$k * outedges.data$nbprob * n)
edges_target <- mean(c(inedges_target, outedges_target))




# Assign vertex attributes to the network ----------

synthpop_cols <- colnames(data)
set.vertex.attribute(n0, colnames(data), data)


# Test assignment between `n0` vertex attributes and `synthpop_cols` ----------

## For sex variable
identical(n0%v%"sex", data$sex)
table(n0%v%"sex", exclude=NULL)

## For all variables:
### get vertex attribute names from the n0 graph object
vertex_attr_names <- list.vertex.attributes(n0)

# define a function to check if a vertex attribute is identical to the corresponding column in data
check_identical <- function(attr_name) {
  graph_attr <- n0 %v% attr_name
  data_attr <- data[[attr_name]]
  
  return(identical(graph_attr, data_attr))
}

### use sapply() to apply the check_identical() function to all vertex attribute names
identical_results <- sapply(vertex_attr_names, check_identical)

### print the results
print(identical_results)

### check if any values other than 'vertex.names' are FALSE
if (any(identical_results[!(names(identical_results) %in% c("vertex.names", "na"))] == FALSE)) {
  stop("Error: Some vertex attributes do not match the corresponding columns in data.")
} else {
  cat("Test passed: All relevant vertex attributes match the corresponding columns in data.\n")
}


# Recode to add new variables to dataset ------------------------------

# young (< 26, to be set = 1) vs old (>= 26, to be set = 0) 
age.cat <- n0 %v% "age"
age.cat.df <- as.data.frame(age.cat)
age.cat.df <-  
  mutate(age.cat, young = ifelse(age.cat <26, 1, 0), .data = age.cat.df)
xtabs(~age.cat+young, data = age.cat.df)

# recode race variable to set ordering of categories
race <- n0 %v% "race"
race.num <- recode(race, 
                   Wh = 1, Bl = 2,
                   Hi = 3, Ot = 4)

# Initialize network and add attributes ----------

n0 %v% "young" <- age.cat.df$young
n0 %v% "race.num" <- race.num


# Generate target statistics from meta-mixing data ----------
  ## Update with values meta mixing values from 08/31/2021
  ## See https://anl.box.com/s/jrf5usly4ujuv3gagmdxlwxaiuvnlq1o
  ## All network mixing parameters with CIs: https://anl.box.com/s/jg6y221hcbql1sd9kvsxpq0cei03qlr0

## gender 
### mixing information from meta-analysis of sathcap AND socnet
edges.male.end <- 0.58
edges.female.end <- 0.40

male.pctmale <- 0.68 
male.pctfemale <- 0.32
female.pctmale <- 0.61
female.pctfemale <- 0.39

### set gender targets 
tgt.male.pctmale <- edges_target*edges.male.end*male.pctmale
tgt.male.pctfemale <- edges_target*edges.male.end*male.pctfemale
tgt.female.pctmale <- edges_target*edges.female.end*female.pctmale  
tgt.female.pctfemale <- edges_target*edges.female.end*female.pctfemale

## young (1=young; 0=old)
### mixing information from meta-analysis of sathcap AND socnet
edges.young.end <- 0.10
edges.old.end <- 0.90
  
young.pctyoung <- 0.61
young.pctold <- 0.39
old.pctyoung <- 0.14
old.pctold <- 0.86
 
## set young targets from meta data
tgt.young.pctyoung <- edges_target * edges.young.end * young.pctyoung
tgt.young.pctold <- edges_target * edges.young.end * young.pctold
tgt.old.pctyoung <- edges_target * edges.old.end * old.pctyoung
tgt.old.pctold <- edges_target * edges.old.end * old.pctold



## race (1=Black, 2=hispani,3=other, 4=white)

table(n0 %v% "race.num", exclude=NULL) # will be sorted as per 1=W, 2=B, 3=H, 4=O
pct_to_white	<- 0.30
pct_to_black	<- 0.41
pct_to_hispanic	<- 0.21
pct_to_other	<- 0.04

race.w.w <- 0.74 # (marks sum to 1)
race.b.w <- 0.31 #-
race.h.w <- 0.37 ##
race.o.w <- 0.54 #--
race.w.b <- 0.11 #
race.b.b <- 0.57 #-
race.h.b <- 0.10 ##
race.o.b <- 0.23 #--
race.w.h <- 0.13 #
race.b.h <- 0.07 #-
race.h.h <- 0.51 ##
race.o.h <- 0.16 #--
race.w.o <- 0.02 #
race.b.o <- 0.05 #-
race.h.o <- 0.02 ##
race.o.o <- 0.07 #--

target.w.w <- edges_target * pct_to_white * race.w.w
target.b.w <- edges_target * pct_to_white * race.b.w
target.h.w <- edges_target * pct_to_white * race.h.w
target.o.w <- edges_target * pct_to_white * race.o.w

target.w.b <- edges_target * pct_to_black * race.w.b
target.b.b <- edges_target * pct_to_black * race.b.b
target.h.b <- edges_target * pct_to_black * race.h.b
target.o.b <- edges_target * pct_to_black * race.o.b

target.w.h <- edges_target * pct_to_hispanic * race.w.h
target.b.h <- edges_target * pct_to_hispanic * race.b.h
target.h.h <- edges_target * pct_to_hispanic * race.h.h
target.o.h <- edges_target * pct_to_hispanic * race.o.h

target.w.o <- edges_target * pct_to_other * race.w.o
target.b.o <- edges_target * pct_to_other * race.b.o
target.h.o <- edges_target * pct_to_other * race.h.o
target.o.o <- edges_target * pct_to_other * race.o.o

## degree distributions
inedges <- inedges %>% 
  mutate(n_nodes = n*nbprob)
outedges <- outedges %>% 
  mutate(n_nodes = n*nbprob)

negbin_inedges <- negbin_indeg %>% 
  mutate(n_nodes = n*nbprob)
negbin_outedges <- negbin_outdeg %>% 
  mutate(n_nodes = n*nbprob)

## distance term

dist.prop.distribution <- c(15.7, 35.1, 24.1, 22)/100
dist.nedge.distribution <- edges_target*dist.prop.distribution

# Fit ERGM (with SATHCAP mixing) ----------

deg.terms <- 0:3
indeg.terms <- 0:1  

dist.terms <- 1:3 #fourth is left out
#dist.terms <- 3 #only try the third term

# Specify initial coefs ----------

initial_coeffs <- c("edges" = -9.319, 
                    "mix.race.num.4.1" = 0.16349, 
                    "odegree0" = -0.1386842, 
                    "mix.race.num.2.4" = 0.13625, 
                    "mix.race.num.4.4" = 0.20408)


# Code before the stop point
  ## print("Running code before the stop point")
  ## stop("Stopping here")


fit_nonemmpty_network <- 
  ergm(
    n0 ~
      edges + 
      nodemix("sex", levels2=-1)+
      nodemix("young", levels2=-1),
    target.stats = 
    c(
      edges_target,
      c(tgt.female.pctmale, tgt.male.pctfemale, tgt.male.pctmale),
      c(tgt.old.pctyoung, tgt.young.pctold, tgt.young.pctyoung)      
    ),
    eval.loglik = FALSE    
    )
  

non_empty_net <- simulate(fit_nonemmpty_network, nsim=1)



fit.metadata.mixing <-
  ergm(
    n0 ~
      edges + 
      nodemix("sex", levels2=-1)+
      nodemix("young", levels2=-1)+
      nodemix("race.num", levels2=-1)+
      idegree(indeg.terms)+
      odegree(deg.terms)+
      dist(dist.terms),
    target.stats = 
    c(
      edges_target,
      c(tgt.female.pctmale, tgt.male.pctfemale, tgt.male.pctmale),           
      c(tgt.old.pctyoung, tgt.young.pctold, tgt.young.pctyoung),
      c(target.b.w, target.h.w, target.o.w,
        target.w.b, target.b.b, target.h.b, target.o.b,
        target.w.h, target.b.h, target.h.h, target.o.h,
        target.w.o, target.b.o, target.h.o, target.o.o),
      c(negbin_inedges$n_nodes[c(indeg.terms+1)]),
      c(outedges$n_nodes[c(deg.terms+1)]),
      c(dist.nedge.distribution[dist.terms])
    ),
    eval.loglik = FALSE,
    control = control.ergm(
      MCMLE.maxit = 500,
      main.method = c("Stochastic-Approximation"),
      MCMC.interval = 1e6,
      MCMC.samplesize = 1e6,
      MCMLE.termination = "Hotelling",
      MCMC.effectiveSize=NULL,
      SAN = control.san(
        SAN.maxit = 500, 
        SAN.nsteps = 1e8
      )
    )
                           
    )
  
  

save.image(file=here("fit-ergms", "out", "new-mixing-data-with-hotelling-stochasticapprox.RData"))  