import requests
import time
import RPi.GPIO as GPIO

GPIO.setmode(GPIO.BCM)
GPIO.setup(17, GPIO.OUT)

while True:
    # Perform the ping using the system ping command (one ping only)
    rawPingFile = os.popen('ping -c 1 %s' % ("8.8.8.8"))
    rawPingData = rawPingFile.readlines()
    rawPingFile.close()
    # Extract the ping time
    if len(rawPingData) < 2:
        # Failed to find a DNS resolution or route
        failed = True
        latency = 0
    else:
        index = rawPingData[1].find('time=')
        if index == -1:
            # Ping failed or timed-out
            failed = True
            latency = 0
        else:
            # We have a ping time, isolate it and convert to a number
            failed = False
            latency = rawPingData[1][index + 5:]
            latency = latency[:latency.find(' ')]
            latency = float(latency)
    # Set our outputs
    if failed:
        # Could not ping
        GPIO.output(17, GPIO.HIGH)
    else:
        # Ping stored in latency in milliseconds
        GPIO.output(17, GPIO.LOW)
        print '%f ms' % (latency)
