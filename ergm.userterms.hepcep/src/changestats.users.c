/*  File src/changestats.users.c in package ergm.userterms, part of the Statnet suite
 *  of packages for network analysis, http://statnet.org .
 *
 *  This software is distributed under the GPL-3 license.  It is free,
 *  open source, and has the attribution requirements (GPL Section 7) at
 *  http://statnet.org/attribution
 *
 *  Copyright 2003-2013 Statnet Commons
 */
#include "changestats.users.h"
#include <math.h>

#ifndef M_PI
  #define M_PI 3.1415926535
#endif

// c(dist, nodelat, nodelon),
CHANGESTAT_FN(d_dist) {
  Vertex t, h;
  int i, j;
  double t_nodecov, h_nodecov;
  int dist_cat, target_cat;
  int change;
  double t_lat, t_lon, h_lat, h_lon;
  double t_lat_rad, t_lon_rad, h_lat_rad, h_lon_rad;
  double radius, xunit, yunit, dist;

  ZERO_ALL_CHANGESTATS(i);
  FOR_EACH_TOGGLE(i) {
    t = TAIL(i); h = HEAD(i);
    t_lat = INPUT_PARAM[t + N_CHANGE_STATS - 1];
    t_lon = INPUT_PARAM[t + N_NODES + N_CHANGE_STATS - 1];
    h_lat = INPUT_PARAM[h + N_CHANGE_STATS - 1];
    h_lon = INPUT_PARAM[h + N_NODES + N_CHANGE_STATS - 1];
    //printf("%f,%f,%f,%f\n", t_lat,t_lon,h_lat,h_lon);
    // Turn degrees into radians
    t_lat_rad = M_PI*t_lat/180.0;
    t_lon_rad = M_PI*t_lon/180.0;
    h_lat_rad = M_PI*h_lat/180.0;
    h_lon_rad = M_PI*h_lon/180.0;
    
    // Average radius of the earth (kilometers)
    radius = 6334.02;
    
    // Equirectangular approximation (good for close distances)
    xunit = (h_lon_rad - t_lon_rad) * cos((t_lat_rad + h_lat_rad)/2);
    yunit = h_lat_rad - t_lat_rad;
    dist = sqrt( (xunit * xunit) + (yunit * yunit) ) * radius;
    
    
    // Find dist category
    //printf("%f\n", dist);
    /* 
    Categories are:
     < 0.2km ; 0.2km  - 1.6km ; 1.6km - 32.2km ; > 32.2km
     corresponding to
     < 1/8 mile ; 1/8 mile - 1 mile ; 1 mile - 20 miles ; > 20 miles
    */
    if (dist < 0.2){
      target_cat = 1;
    } else if (dist < 1.6) {
      target_cat = 2;
    } else if (dist < 32.2) {
      target_cat = 3;
    } else target_cat = 4;
    
    change = IS_OUTEDGE(t,h) ? -1 : 1;
    for(j = 0; j < N_CHANGE_STATS; j++) {
      if ((int)INPUT_PARAM[j] == target_cat){
        CHANGE_STAT[j] += change;
      }
    }
    TOGGLE_IF_MORE_TO_COME(i);
  }
  UNDO_PREVIOUS_TOGGLES(i);
}

// c(num.cats, thresholds, base, nodelat, nodelon, nodecat)

/** 
 dnf (distance near/far)
 
 
 
 
 **/
CHANGESTAT_FN(d_dnf) {
  Vertex t, h;
  int i, j;
  double t_nodecov, h_nodecov;
  int dist_cat, target_cat;
  int num_cats, base, t_cat;
  int change, change_stat_idx;
  int offset, termid;
  double t_lat, t_lon, h_lat, h_lon;
  double t_lat_rad, t_lon_rad, h_lat_rad, h_lon_rad;
  double radius, xunit, yunit, dist;
  double t_threshold; 
  // Number of categories
  num_cats = INPUT_PARAM[0];
  base = INPUT_PARAM[num_cats + 1];
  
  ZERO_ALL_CHANGESTATS(i);
  FOR_EACH_TOGGLE(i) {
    t = TAIL(i); h = HEAD(i);
    t_lat = INPUT_PARAM[t + num_cats + 1];
    t_lon = INPUT_PARAM[t + N_NODES + num_cats + 1];
    h_lat = INPUT_PARAM[h + num_cats + 1];
    h_lon = INPUT_PARAM[h + N_NODES + num_cats + 1];
    // printf("%f,%f,%f,%f\n", t_lat,t_lon,h_lat,h_lon);
    // Turn degrees into radians
    t_lat_rad = M_PI*t_lat/180.0;
    t_lon_rad = M_PI*t_lon/180.0;
    h_lat_rad = M_PI*h_lat/180.0;
    h_lon_rad = M_PI*h_lon/180.0;
    
    // Average radius of the earth (kilometers)
    radius = 6334.02;
    
    // Equirectangular approximation (good for close distances)
    xunit = (h_lon_rad - t_lon_rad) * cos((t_lat_rad + h_lat_rad)/2);
    yunit = h_lat_rad - t_lat_rad;
    dist = sqrt( (xunit * xunit) + (yunit * yunit) ) * radius;
    
    
    // Find node category
    t_cat = INPUT_PARAM[t + (2*N_NODES) + num_cats + 1];
    t_threshold = INPUT_PARAM[t_cat];
    
    if (dist < t_threshold){
      offset = 1;
    } else offset = 2;
    
    termid = (t_cat - 1) * 2 + offset;
    
    if (termid != base){
      change_stat_idx = termid < base ? termid - 1 : termid - 2;
      change = IS_OUTEDGE(t,h) ? -1 : 1;
      CHANGE_STAT[change_stat_idx] += change;
    }
    
    // Debug
    // CHANGE_STAT[0] = num_cats;
    // CHANGE_STAT[0] = -99;
    // CHANGE_STAT[1] = base;
    // CHANGE_STAT[2] = -99;
    
   
    
    TOGGLE_IF_MORE_TO_COME(i);
  }
  UNDO_PREVIOUS_TOGGLES(i);
}


