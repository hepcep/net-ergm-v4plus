git ls-files \
    fit-ergms/ \
    simulate-from-ergms/ \
    tests/ \
    renv.lock \
    ergm.userterms.hepcep/ \
  | grep -v '\.\(RData\|rds\|qs\|pdf\)$' \
  | zip hepcep-repo-stripped.zip -@

# TO Download
# scp akhann16@ssh.ccv.brown.edu:/oscar/home/akhann16/code/net-ergm-v4plus/hepcep-repo-stripped.zip ~/Downloads/