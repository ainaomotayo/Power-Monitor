#import mysql
import time
import mysql.connector
from datetime import datetime as dt
mydb = mysql.connector.connect(
   host="localhost",
   user="root",
   passwd="toor",
   database="jabi_powermon"
)
mycursor = mydb.cursor()
ttime = time.strftime("%c")
sql = "INSERT INTO powmon (event_time, location, status) VALUES (%s, %s, %s)"

val = (dt.now(), "Jabi", "OK")
mycursor.execute(sql, val)
mydb.commit()
print(mycursor.rowcount, "record inserted")
