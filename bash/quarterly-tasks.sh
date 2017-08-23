#! /bin/sh
#
#Rscript /home/tim/code/Hap.py-Rob.ot/R/twitter-hourly-trends.R
echo $PATH
echo $(date)
/usr/local/bin/Rscript /Users/tim/code/Hap.py-Rob.ot/R/twitter-getdm.R
echo $(date)
/usr/local/bin/Rscript /Users/tim/code/Hap.py-Rob.ot/R/twitter-senddm.R
