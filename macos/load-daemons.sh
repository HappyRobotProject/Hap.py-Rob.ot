#! /bin/sh
#
#Setup LaunchDaemons
#sudo launchctl stop /Library/LaunchDaemons/com.happyrobot.quarterly.plist
#sudo launchctl unload /Library/LaunchDaemons/com.happyrobot.quarterly.plist
sudo rm /Library/LaunchDaemons/com.happyrobot.quarterly.plist
sudo cp com.happyrobot.quarterly.plist /Library/LaunchDaemons/com.happyrobot.quarterly.plist
sudo chown root /Library/LaunchDaemons/com.happyrobot.quarterly.plist
sudo chgrp wheel /Library/LaunchDaemons/com.happyrobot.quarterly.plist
sudo launchctl load /Library/LaunchDaemons/com.happyrobot.quarterly.plist
sudo launchctl start /Library/LaunchDaemons/com.happyrobot.quarterly.plist
