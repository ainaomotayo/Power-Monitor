#!/bin/bash

while true
    do
    gpio wfi 4 rising
    python /opt/sendEmail.py
    sleep 300 #this will send an email every 5 mins
done
