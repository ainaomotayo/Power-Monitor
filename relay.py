import os
import time
import RPi.GPIO as GPIO
GPIO.setmode(GPIO.BCM)
GPIO.setup(4,GPIO.IN)
from email.MIMEMultipart import MIMEMultipart
from email.MIMEBase import MIMEBase
from email.MIMEText import MIMEText
from email.Utils import COMMASPACE, formatdate
from email import Encoders
import smtplib

prev_input = 0
#data = time.strftime("%H:%M:%S")
#data_s = time.strftime("%c")
#data_f = time.strftime("%c")
while True:
  input = GPIO.input(4)
  if ((prev_input) != input):
    if (input==0):
        #create email
                message = """Power is Down at Wuse 2. Time is  """ + time.strftime("%c")
                msg = MIMEText(message)
                msg['subject'] = 'Power Outage - Stop'
                msg['from'] = 'ainaomotayo@gmail.com'
                msg['to'] = 'ainaomotayo@gmail.com'
                # send mail
                s = smtplib.SMTP_SSL('smtp.gmail.com',465)
                s.login('ainaomotayo@gmail.com' , 'Ol@sunk@nm190')
                s.sendmail(msg['From'], msg['To'], msg.as_string())
                s.quit
                print ("Power Outage - STOP")
                time.sleep(5)
    else:
        #create email
                message = """Power is Restored. Time is """ + time.strftime("%c")
                msg = MIMEText(message)
                msg['subject'] = 'Power Restored - Start'
                msg['from'] = 'ainaomotayo@gmail.com'
                msg['to'] = 'omotayo.aina@abujaelectricity.com,ainaomotayo@gmail.com'
                # send mail
                s = smtplib.SMTP_SSL('smtp.gmail.com',465)
                s.login('ainaomotayo@gmail.com' , 'Ol@sunk@nm190')
                s.sendmail(msg['From'], msg['To'], msg.as_string())
                s.quit
                print ("Power Restored START")
                time.sleep(5)
    prev_input = input
    #slight pause to debounce
    time.sleep(0.05)
