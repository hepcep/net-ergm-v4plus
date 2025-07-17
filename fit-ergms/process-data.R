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

# Input  In/Outdegree Data (Specified per January 23, 2025) ------------------------------
## https://anl.box.com/s/01zwyn331jquyps4os0cirtopczes39m

indegree_data <- read.table(text = "
  in_degree lower_nbprob lower_n mean_nbprob mean_n upper_nbprob upper_n
  0 0.5821942 18630.21 0.64583354 20666.67 0.70507399 22562.37 
  1 0.17457059 5586.26 0.2031094 6499.50 0.23448797 7503.62
  2 0.0669945 2143.82 0.083 2657.25 0.10152704 3248.87
  3 0.02875131 920.04 0.0366 1169.96 0.04498079 1439.39
  4 0.01299143 415.73 0.0167 533.52 0.02015486 644.96
  5 0.00604716 193.51 0.00776 248.32 0.00909181 290.94
  6 0.00286968 91.83 0.00366 117.14 0.00411961 131.83
  7 0.00138042 44.17 0.00174 55.79 0.00187257 59.92
  8 0.00067075 21.46 0.000836 26.76 0.0008532 27.30
  9 0.00032845 10.51 0.000403 12.90 0.00038946 12.46
  10 0.00016183 5.18 0.000195 6.25 0.00017804 5.70
  11 0.00008014 2.56 0.0000949 3.04 0.00008149 2.61
  12 0.00003985 1.28 0.0000463 1.48 0.00003733 1.19
  13 0.00001988 0.64 0.0000226 0.72 0.00001712 0.55
  14 0.00000995 0.32 0.0000111 0.35 0.00000785 0.25
  15 0.00000499 0.16 0.00000543 0.17 0.00000361 0.12
  16 0.00000251 0.08 0.00000267 0.09 0.00000166 0.05
  17 0.00000127 0.04 0.00000131 0.04 0.00000076 0.02
  18 0.00000064 0.02 0.00000065 0.02 0.00000035 0.01
  19 0.00000032 0.01 0.00000032 0.01 0.00000016 0.01
  20 0.00000016 0.01 0.00000016 0.01 0.00000007 0.00
  ", 
  header = TRUE)

  # Print the data frame to check
  print(indegree_data)

outdegree_data <- read.table(text = "
  out_degree lower_nbprob lower_n mean_nbprob mean_n upper_nbprob upper_n
  0 0.54372877 17399.32 0.60585013 19387.20 0.65685532 21019.37
  1 0.18338748 5868.40 0.20216566 6469.30 0.22882848 7322.51
  2 0.07935995 2539.52 0.0928 2968.26 0.11074794 3543.93
  3 0.03840462 1228.95 0.0464 1485.72 0.05592998 1789.76
  4 0.01956799 626.18 0.0242 774.64 0.02883424 922.70
  5 0.0102708 328.67 0.0129 413.58 0.01504728 481.51
  6 0.00549605 175.87 0.00701 224.26 0.00791582 253.31
  7 0.0029812 95.40 0.00384 122.94 0.00418801 134.02
  8 0.00163343 52.27 0.00212 67.95 0.00222519 71.21
  9 0.00090194 28.86 0.00118 37.79 0.0011862 37.96
  10 0.00050111 16.04 0.00066 21.12 0.000634 20.29
  11 0.00027981 8.95 0.00037 11.85 0.00033959 10.87
  12 0.00015689 5.02 0.000209 6.67 0.00018222 5.83
  13 0.00008828 2.82 0.000118 3.77 0.00009792 3.13
  14 0.00004982 1.59 0.0000667 2.13 0.00005269 1.69
  15 0.00002819 0.90 0.0000378 1.21 0.00002838 0.91
  16 0.00001599 0.51 0.0000215 0.69 0.00001531 0.49
  17 0.00000908 0.29 0.0000122 0.39 0.00000826 0.26
  18 0.00000517 0.17 0.00000698 0.22 0.00000446 0.14
  19 0.00000295 0.09 0.00000398 0.13 0.00000241 0.08
  20 0.00000168 0.05 0.00000228 0.07 0.0000013 0.04
  ", 
header = TRUE)

  # Print the data frame to check
  print(outdegree_data)



# Initialize network ----------

n <- nrow(data)
n0 <- network.initialize(n = n, directed = TRUE)


# Compute target number of edges ----------

inedges_target <- sum(indegree_data$in_degree * indegree_data$mean_n)
outedges_target <- sum(outdegree_data$out_degree * outdegree_data$mean_n)
edges_target <- mean(c(inedges_target, outedges_target))

inedges_target
outedges_target
edges_target


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
  ## Update with values meta mixing values from 10/17/2024
  ## See https://anl.box.com/s/jrf5usly4ujuv3gagmdxlwxaiuvnlq1o
  ## All network mixing parameters with CIs: https://anl.box.com/s/jg6y221hcbql1sd9kvsxpq0cei03qlr0 (Dec 5, 2024)

## gender 
### mixing information from meta-analysis of sathcap AND socnet
edges.male.end <- 0.60
edges.female.end <- 0.38

male.pctmale <- 0.55
male.pctfemale <- 0.44
female.pctmale <- 0.72
female.pctfemale <- 0.28

### set gender targets 
tgt.male.pctmale <- edges_target*edges.male.end*male.pctmale
tgt.male.pctfemale <- edges_target*edges.male.end*male.pctfemale
tgt.female.pctmale <- edges_target*edges.female.end*female.pctmale  
tgt.female.pctfemale <- edges_target*edges.female.end*female.pctfemale

## young (1=young; 0=old)
### mixing information from meta-analysis of sathcap AND socnet
edges.young.end <- 0.16
edges.old.end <- 0.84
  
young.pctyoung <- 0.48
young.pctold <- 0.52
old.pctyoung <- 0.10
old.pctold <- 0.90
 
## set young targets from meta data
tgt.young.pctyoung <- edges_target * edges.young.end * young.pctyoung
tgt.young.pctold <- edges_target * edges.young.end * young.pctold
tgt.old.pctyoung <- edges_target * edges.old.end * old.pctyoung
tgt.old.pctold <- edges_target * edges.old.end * old.pctold



## race (1=white, 2=black, 3=hispanic, 4=other)

table(n0 %v% "race")
table(n0 %v% "race.num", exclude=NULL) # will be sorted as per 1=W, 2=B, 3=H, 4=O


pct_to_white	<- 0.59
pct_to_black	<- 0.18
pct_to_hispanic	<- 0.17
pct_to_other	<- 0.03

race.w.w <- 0.758 # (marks sum to 1, i.e., sum of all race.w.* = 1, sum of all race.b.* = 1)
race.b.w <- 0.333 #-
race.h.w <- 0.345 ##
race.o.w <- 0.638 #--
race.w.b <- 0.091 #
race.b.b <- 0.495 #-
race.h.b <- 0.011 ##
race.o.b <- 0.106 #--
race.w.h <- 0.111 #
race.b.h <- 0.161 #-
race.h.h <- 0.632 ##
race.o.h <- 0.202 #--
race.w.o <- 0.040 #
race.b.o <- 0.011 #-
race.h.o <- 0.011 ##
race.o.o <- 0.053 #--

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

# ## degree distributions
# inedges <- inedges %>% 
#   mutate(n_nodes = n*nbprob)
# outedges <- outedges %>% 
#   mutate(n_nodes = n*nbprob)

# negbin_inedges <- negbin_indeg %>% 
#   mutate(n_nodes = n*nbprob)
# negbin_outedges <- negbin_outdeg %>% 
#   mutate(n_nodes = n*nbprob)

## distance term

dist.prop.distribution <- c(15.7, 35.1, 24.1, 22)/100
dist.nedge.distribution <- edges_target*dist.prop.distribution

# Save RDS Objects ----

## stop()


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
  indegree_data = indegree_data,
  outdegree_data = outdegree_data,
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












