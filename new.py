import os
import time
import smtplib
import RPi.GPIO as GPIO
import mysql.connector
from datetime import datetime as dt
GPIO.setmode(GPIO.BCM)
GPIO.setup(5,GPIO.IN, pull_up_down = GPIO.PUD_DOWN)
GPIO.setup(6,GPIO.IN, pull_up_down = GPIO.PUD_DOWN)
GPIO.setup(21,GPIO.IN, pull_up_down = GPIO.PUD_DOWN)
GPIO.setup(20,GPIO.IN, pull_up_down = GPIO.PUD_DOWN)
from email.MIMEMultipart import MIMEMultipart
from email.MIMEBase import MIMEBase
from email.MIMEText import MIMEText
from email import Encoders

gmail_user = "power.monitor@abujaelectricity.com"
gmail_pwd = "WelcomeAedc!"

print "initialing"
while True:
        power_on = GPIO.input(5)
        power_off = GPIO.input(6)
        power_oninv = GPIO.input(20)
        power_offinv = GPIO.input(21)
        if ((power_on >= 1) & (power_oninv >=1)):
#               print 'Main 1 and Main 2 OK'
               def mail(to, subject, text):
                   msg = MIMEMultipart()

                   msg['From'] = gmail_user
                   msg['To'] = to
                   msg['Subject'] = subject

                   msg.attach(MIMEText(text))

                   mailServer = smtplib.SMTP("outlook.office365.com", 587)
                   mailServer.ehlo()
                   mailServer.starttls()
                   mailServer.ehlo()
                   mailServer.login(gmail_user, gmail_pwd)
                   mailServer.sendmail(gmail_user, to, msg.as_string())
                   mailServer.close()

               mail("facilsoftdev@abujaelectricity.com",
                   "Power is UP @ Jabi Area Office",
                """
                Power UP at Jabi Area Office.
		**************************
		MAINS & INVERTER IS UP
		**************************
                Event Time:
                """ + time.strftime("%c") +
                """
                """
               )
               mydb = mysql.connector.connect(
                   host="localhost",
                   user="root",
                   passwd="toor",
                   database="jabi_powermon"
                )
               mycursor = mydb.cursor()
	       sql = "INSERT INTO powmon (event_time, location, status, chart) VALUES (%s, %s, %s, %s)"
               val = (dt.now(), "Jabi", "UP", "100")
               mycursor.execute(sql, val)
               mydb.commit()
#               print(mycursor.rowcount, "record inserted")

        elif (power_on >= 1):
#               print 'Main 2 Not OK'
               def mail(to, subject, text):
                   msg = MIMEMultipart()

                   msg['From'] = gmail_user
                   msg['To'] = to
                   msg['Subject'] = subject

                   msg.attach(MIMEText(text))

                   mailServer = smtplib.SMTP("outlook.office365.com", 587)
                   mailServer.ehlo()
                   mailServer.starttls()
                   mailServer.ehlo()
                   mailServer.login(gmail_user, gmail_pwd)
                   mailServer.sendmail(gmail_user, to, msg.as_string())
                   mailServer.close()

               mail("facilsoftdev@abujaelectricity.com",
                    "CRITICAL  INVERTER: Power Outage @ Jabi Area Office",
                 """
                Power Outage has occurred at Jabi Area Office
		***********************   ****************
 		INVERTER OUT OF SERVICE - MAINS IS WORKING
		***********************   ****************
                Event Time:
                """ + time.strftime("%c") +
                """
                """
                )
               mydb = mysql.connector.connect(
                   host="localhost",
                   user="root",
                   passwd="toor",
                   database="jabi_powermon"
                )
               mycursor = mydb.cursor()
	       sql = "INSERT INTO powmon (event_time, location, status, chart) VALUES (%s, %s, %s, %s)"
               val = (dt.now(), "Jabi", "DOWN", "-100")
               mycursor.execute(sql, val)
               mydb.commit()
               print(mycursor.rowcount, "record inserted")
  
        elif (power_oninv >=  1):
            print 'Main 1 Not OK'
        if ((power_on >= 1) | (power_oninv >=1)):
            
            print "POWER"
            if (power_on >= 1):
                print "Main 1"
            else:
                print "Main 2 -- Power One is down running on Inverter"
                def mail(to, subject, text):
                   msg = MIMEMultipart()

                   msg['From'] = gmail_user
                   msg['To'] = to
                   msg['Subject'] = subject

                   msg.attach(MIMEText(text))

                   mailServer = smtplib.SMTP("outlook.office365.com", 587)
                   mailServer.ehlo()
                   mailServer.starttls()
                   mailServer.ehlo()
                   mailServer.login(gmail_user, gmail_pwd)
                   mailServer.sendmail(gmail_user, to, msg.as_string())
                   mailServer.close()

                mail("facilsoftdev@abujaelectricity.com",
                   "Power Outage @ Jabi Area Office",
                """
                Power Outage has occurred at Jabi Area Office.
		*************   *******************
		MAINS IS DOWN - RUNNING ON INVERTER
		*************   *******************
                Event Time:
                """ + time.strftime("%c") +
                """
                """
                )
                mydb = mysql.connector.connect(
                   host="localhost",
                   user="root",
                   passwd="toor",
                   database="jabi_powermon"
                )
                mycursor = mydb.cursor()
		sql = "INSERT INTO powmon (event_time, location, status, chart) VALUES (%s, %s, %s, %s)"
                val = (dt.now(), "Jabi", "MID", "0")
                mycursor.execute(sql, val)
                mydb.commit()
#                print(mycursor.rowcount, "record inserted")


            time.sleep(15)
        elif ((power_off >= 1) | (power_offinv >=1)):
            if (power_off >= 1):
#               print "Main 1 and Main 2 - Off"

	       def mail(to, subject, text):
		   msg = MIMEMultipart()

		   msg['From'] = gmail_user
		   msg['To'] = to
		   msg['Subject'] = subject

		   msg.attach(MIMEText(text))

		   mailServer = smtplib.SMTP("outlook.office365.com", 587)
		   mailServer.ehlo()
		   mailServer.starttls()
		   mailServer.ehlo()
		   mailServer.login(gmail_user, gmail_pwd)
		   mailServer.sendmail(gmail_user, to, msg.as_string())
		   mailServer.close()

	       mail("facilsoftdev@abujaelectricity.com",
		   "CRITICAL OUTAGE - MAINS & INVERTER @ Jabi Area Office",
		"""
		Power Outage has occurred at Jabi Area Office.
		************************   ****************
		MAINS & INVERTER IS DOWN - ATTENTION NEEDED
		************************   ****************
		Event Time:
		""" + time.strftime("%c") +
		"""
		"""
		)
	       mydb = mysql.connector.connect(
		   host="localhost",
		   user="root",
		   passwd="toor",
		   database="jabi_powermon"
		)
	       mycursor = mydb.cursor()

	       sql = "INSERT INTO powmon (event_time, location, status, chart) VALUES (%s, %s, %s, %s)"
	       val = (dt.now(), "Jabi", "DOWN", "-100")
	       mycursor.execute(sql, val)
	       mydb.commit()
#	       print(mycursor.rowcount, "record inserted")

            else:
#               print "Main 2 Off"
               def mail(to, subject, text):
                   msg = MIMEMultipart()

                   msg['From'] = gmail_user
                   msg['To'] = to
                   msg['Subject'] = subject

                   msg.attach(MIMEText(text))

                   mailServer = smtplib.SMTP("outlook.office365.com", 587)
                   mailServer.ehlo()
                   mailServer.starttls()
                   mailServer.ehlo()
                   mailServer.login(gmail_user, gmail_pwd)
                   mailServer.sendmail(gmail_user, to, msg.as_string())
                   mailServer.close()

               mail("facilsoftdev@abujaelectricity.com",
                   "Power Outage @ Jabi Area Office",
                """
                Power Outage has occurred at Jabi Area Office.
		**************** 
                INVERTER IS DOWN 
                **************** 
                Event Time:
                """ + time.strftime("%c") +
                """
                """
                )
               mydb = mysql.connector.connect(
                   host="localhost",
                   user="root",
                   passwd="toor",
                   database="jabi_powermon"
                )
               mycursor = mydb.cursor()
	       sql = "INSERT INTO powmon (event_time, location, status, chart) VALUES (%s, %s, %s, %s)"
               val = (dt.now(), "Jabi", "DOWN", "-100")
               mycursor.execute(sql, val)
               mydb.commit()
#               print(mycursor.rowcount, "record inserted")
	
               time.sleep(15)
time.sleep(5)
GPIO.cleanup()

