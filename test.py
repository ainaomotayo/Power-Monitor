import time
import os
import time
import RPi.GPIO as GPIO
GPIO.setmode(GPIO.BCM)
GPIO.setup(4,GPIO.IN)
GPIO.input(4, False)
while True:
#	input = GPIO.IN(4)
	if GPIO.input(4) == True :
		print "Hello"
		time.sleep(2)
#	elif (input >= 1):
#		print "Hi"
#		time.sleep(2)
	else :
		print "Helloworld"
		time.sleep(2)
time.sleep(2)
