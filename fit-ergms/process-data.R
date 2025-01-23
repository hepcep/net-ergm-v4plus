# Process the synthetic dataset with 32K nodes to enable ERGM fitting

rm(list=ls())

# Activate R environment ------------------------------

library(renv)
renv::activate()


# Libraries ------------------------------ 

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



## race (1=white, 2=black, 3=hispanic, 4=other)

table(n0 %v% "race")
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

target_race_num  <- c(
            target.b.w, target.h.w, target.o.w,
  target.w.b, target.b.b, target.h.b, target.o.b,
  target.w.h, target.b.h, target.h.h, target.o.h,
  target.w.o, target.b.o, target.h.o, target.o.o
)

target_race_num

# Fit network only with edges ----------
fit_edges_only_net <- 
  ergm(
    n0 ~
      edges,
    target.stats = edges_target,
    eval.loglik = FALSE    
    )

edges_only_net <- simulate(fit_edges_only_net, nsim=1)

# Test alignment of race_num ----------

  # Define the known mapping
  num_to_race <- c("Wh", "Bl", "Hi", "Ot")  # 1=Wh,2=Bl,3=Hi,4=Ot

  # Retrieve the summaries
  race_num_alignment <- summary(edges_only_net ~ nodemix("race.num", levels2 = -1))
  race_term_alignment <- summary(edges_only_net ~ nodemix("race", levels2 = -1))

  # Print them for reference
  print(race_num_alignment)
  print(race_term_alignment)

  cat("\nChecking all numeric-to-category race mappings:\n\n")

  # Loop over all combinations (i,j)
  for (i in 1:4) {
    for (j in 1:4) {
      # Construct term names
      num_term <- paste0("mix.race.num.", i, ".", j)
      race_term <- paste0("mix.race.", num_to_race[i], ".", num_to_race[j])
      
      # Check if both terms exist
      if (num_term %in% names(race_num_alignment) && race_term %in% names(race_term_alignment)) {
        # Compare counts
        num_count <- race_num_alignment[num_term]
        race_count <- race_term_alignment[race_term]
        
        if (num_count == race_count) {
          cat("Confirmed: (", i, ",", j, ") in race.num = (", num_to_race[i], ",", num_to_race[j],
              ") in race. Count:", num_count, "\n")
        } else {
          cat("Mismatch: (", i, ",", j, ") in race.num = (", num_to_race[i], ",", num_to_race[j], 
              "). Counts differ: ", num_count, " vs ", race_count, "\n")
        }
      } else {
        # At least one of the terms wasn't found in the alignment vectors
        cat("Term not found for (", i, ",", j, "): ", num_term, " or ", race_term, "\n")
      }
    }
  }

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

# Save RDS Objects ----

# create a list of objects to save
data_objects <- list(
  data = data,
  n0 = n0,
  edges_only_net = edges_only_net,
  tgt.old.pctyoung=tgt.old.pctyoung, 
  tgt.young.pctold=tgt.young.pctold, 
  tgt.young.pctyoung=tgt.young.pctyoung,
  edges_target = edges_target,
  tgt.female.pctmale = tgt.female.pctmale, 
  tgt.male.pctfemale=tgt.male.pctfemale, 
  tgt.male.pctmale=tgt.male.pctmale,
  target_race_num = target_race_num,
  negbin_inedges = negbin_inedges,
  outedges = outedges,
  dist_nedge_distribution = dist.nedge.distribution
)

# Print Objects for comparison --------
edges_only_net
data_objects$tgt.old.pctyoung
data_objects$tgt.young.pctold
data_objects$tgt.young.pctyoung
data_objects$edges_target
data_objects$tgt.female.pctmale
data_objects$tgt.male.pctfemale
data_objects$tgt.male.pctmale
names(data_objects$target_race_num) <- 
  c("target.b.w", "target.h.w", "target.o.w",
    "target.w.b", "target.b.b", "target.h.b", "target.o.b",
    "target.w.h", "target.b.h", "target.h.h", "target.o.h",
    "target.w.o", "target.b.o", "target.h.o", "target.o.o")
data_objects$target_race_num
data_objects$negbin_inedges[1:2,]
data_objects$outedges[1:4,]
data_objects$dist_nedge_distribution


# Save the list as an RDS file
saveRDS(data_objects, file = here("fit-ergms", "out", "processed_data.rds"))












