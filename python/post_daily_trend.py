#! /usr/bin/python

#
# Import Packages
#
import pandas as pd
import twython as tw

working_dir = "/Users/tim/data/"
authKeys = pd.read_csv( "/Users/tim/data/auth/twitter-HRP.csv", header=0)
conkey = authKeys.iloc[0,0]
consec = authKeys.iloc[0,1]
acctok = authKeys.iloc[0,2]
accsec = authKeys.iloc[0,3]

twitter = tw.Twython(conkey,consec,acctok,accsec)
daily_trend = "/Users/tim/data/images/daily-trends.png"
print("Sending Photo")
f = open(daily_trend, 'rb')
twitter.update_status_with_media(status = "Enjoy this #DailyTwitterTrend infographic", media = f)