#!/bin/sh

BESTWEIGHT=${1:-1.0}
almostbest="python `dirname $0`/almostbest.py"

echo NSMPLS BCT ECT VCT
for time in 5000 7000 10000 15000; do
 echo -n $time
 for agent in bct ect vct; do
  echo -n "" `$almostbest $BESTWEIGHT < uct$agent-time=$time.shomersu.dat`
 done
 echo ""
done
  
