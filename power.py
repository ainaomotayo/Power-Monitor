import os
import time
import RPi.GPIO as GPIO
GPIO.setmode(GPIO.BCM)
GPIO.setup(5,GPIO.IN, pull_up_down = GPIO.PUD_DOWN)
GPIO.setup(6,GPIO.IN, pull_up_down = GPIO.PUD_DOWN)
from email.MIMEMultipart import MIMEMultipart
from email.MIMEBase import MIMEBase

#prev_input = 0
print "initialing"
while True:
	power_on = GPIO.input(5)
        power_off = GPIO.input(6)
	if (power_on == 1):
#	if ((prev_input) != input):
#		if (input == 0):
		print "POWER"
#		else:
#			print "hi"
		time.sleep(2)
	elif (power_off == 1):
		print "POWER OFF"
#		time
#		prev_input =input
		time.sleep(2)
	else:
		print "NO POWER"
		time.sleep(2)
GPIO.cleanup()
