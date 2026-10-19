#!/bin/bash
# Wrapper to launch R in VS Code with correct shared libraries

# Make 'module' available in non-interactive shells (VS Code often runs this way).
# The init script moved in the RHEL 9.6 upgrade, so try the usual locations.
if ! command -v module >/dev/null 2>&1; then
  for f in /etc/profile.d/zz_activate_lmod_user.sh "${LMOD_PKG:-/nonexistent}/init/bash" \
           /etc/profile.d/lmod.sh /etc/profile.d/modules.sh; do
    [ -f "$f" ] && source "$f"
    command -v module >/dev/null 2>&1 && break
  done
fi

module purge 

# Load the R module
 #OLD R MODULE module load r/4.4.0-yycctsj
 module load glpk/5.0-55rr
 module load r/4.5.1-iikl
 

# Optional: activate renv if needed here instead of .Rprofile
# source ./renv/activate.R

# Launch R
# exec /oscar/rt/9.2/software/0.20-generic/0.20.1/opt/spack/linux-rhel9-x86_64_v3/gcc-11.3.1/r-4.4.0-yycctsjvszuj5o2q4gfbaehsq7rkl4bz/bin/R --no-save --no-restore "$@"

# Launch R from PATH (provided by the module)
exec R --no-save --no-restore "$@"
