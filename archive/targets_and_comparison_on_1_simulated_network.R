rm(list=ls())


# Libraries ----------

library(network)
library(ergm)
library(dplyr)
library(ergm.userterms)


# Data ----------

load("../fit-ergms/out/racemix-plus-dist-plus-negbin-odeg0-3-indeg0-1-orignialdata.RData")


# Model summary
summary(fit.metadata.mixing)


# Simulate 1 network ----------

net <- simulate(fit.metadata.mixing)
net


# Investigate netstats on 1 network ----------

summary(net ~ edges)
edges_target #target

summary(net ~ nodemix("gender"))
round(c(tgt.female.pctfemale, tgt.female.pctmale, tgt.male.pctfemale, tgt.male.pctmale), 0) #targets

summary(net ~ nodemix("young"))
round(c(tgt.old.pctold, tgt.old.pctyoung, tgt.young.pctold, tgt.young.pctyoung)) #target

summary(net ~ nodemix("race.num"))
round(
  c(target.w.w, target.b.w, target.h.w, target.o.w,
    target.w.b, target.b.b, target.h.b, target.o.b,
    target.w.h, target.b.h, target.h.h, target.o.h,
    target.w.o, target.b.o, target.h.o, target.o.o),
  0
)#targets



summary(net ~ idegree(0:4)) 
inedges$n_nodes[1:4] #targets

summary(net ~ odegree(0:4)) 
outedges$n_nodes[1:5] #targets

summary(net ~ dist(1:4))
round(dist.nedge.distribution)#target