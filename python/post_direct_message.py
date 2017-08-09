#! /usr/bin/python

#
# Import Packages
#
import pandas as pd
import twython as tw

working_dir = "/Users/tim/data/"
authKeys = pd.read_csv( "/Users/tim/data/auth/twitter-HRP.csv", header=0)
authKeys = pd.read_csv( "/Users/tim/data/auth/rtweet-HRP.csv", header=0)
conkey = authKeys.iloc[0,0]
consec = authKeys.iloc[0,1]
acctok = authKeys.iloc[0,2]
accsec = authKeys.iloc[0,3]
#
#user Authentication
twitter = tw.Twython(conkey,consec, oauth_version=2)
#auth = twitter.get_authentication_tokens() 
ACCESS_TOKEN = twitter.obtain_access_token()
tw2 = tw.Twython(conkey, access_token=ACCESS_TOKEN)

OAUTH_TOKEN = auth['oauth_token']
OAUTH_TOKEN_SECRET = auth['oauth_token_secret']
auth['auth_url']


trends = pd.read_csv("/Users/tim/data/todays-trend.csv", header=0)
topTrend = trends.iloc[0,1]

#twitter = tw.Twython(conkey,consec,acctok,accsec, oauth_version=2)
twitter = tw.Twython(conkey,consec,acctok,accsec)
daily_trend = "/Users/tim/data/images/daily-trends.png"
print("Sending Direct Message")
f = open(daily_trend, 'rb')
url = "direct_messages/events/new"
ep = "https://api.twitter.com/1.1/direct_messages/events/new.json"
payattn_uid = 846168259355463680

#twitter.update_status_with_media(status = "Today's #DailyTwitterTrend is ", media = f)photo = open('/path/to/file/image.jpg', 'rb')
response = twitter.upload_media(media=f)

params = {
  "event": {
    "type": "message_create",
    "message_create": {
      "target": {
        "recipient_id": payattn_uid
      },
      "message_data": {
        "text": "Hello World!",
        "attachment": {
          "type": "media",
          "media": {
            "id": response['media_id']
          }
        }
      }
    }
  }
}

params1 = {
  "event": {
    "type": "message_create",
    "message_create": {
      "target": {
        "recipient_id": "846168259355463680"
      },
      "message_data": {
        "text": "Hello Pay_Attention20!"
        }
      }
    }
  }

resp2 = twitter.post(endpoint=ep, params=params1)
resp3 = tw2.get(endpoint="direct_messages")
resp3 = tw2.get_favorites()
#twitter.update_status(status="Today's #DailyTwitterTrend is " + topTrend + "!  Enjoy the infographic.", media_ids=[response['media_id']])