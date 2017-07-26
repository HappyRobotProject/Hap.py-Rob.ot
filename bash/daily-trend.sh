#! /bin/sh
#
#Rscript /home/tim/code/Hap.py-Rob.ot/R/twitter-hourly-trends.R
echo $PATH
echo $(date)
/usr/local/bin/Rscript /Users/tim/code/Hap.py-Rob.ot/R/twitter-todays-trends.R
echo $(date)
/Users/tim/code/Hap.py-Rob.ot/python/post_daily_trend.py
echo $(date)
/usr/local/bin/Rscript /Users/tim/code/Hap.py-Rob.ot/R/twitter-follow.R
