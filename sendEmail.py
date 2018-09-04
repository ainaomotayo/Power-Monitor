#! /usr/bin/python

import smtplib
from email.mime.text import MIMEText

SERVER =    'smtp.gmail.com'
PORT =      587
SENDER =    'ainaomotayo@gmail.com'
PASSWORD =  'Ol@sunk@nm190'
RECIPIENTS =    ['ainaomotayo@gmail.com', 'omotayo.aina@abujaelectricity.com']
SUBJECT =   'Power Outage!'
BODY =      """
<html>
 <head></head>
  <body>

<style type="text/css">
.tg  {border-collapse:collapse;border-spacing:0;}
.tg td{font-family:Arial, sans-serif;font-size:14px;padding:10px 5px;border-style:solid;border-width:1px;overflow:hidden;word-break:normal;}
.tg th{font-family:Arial, sans-serif;font-size:14px;font-weight:normal;padding:10px 5px;border-style:solid;border-width:1px;overflow:hidden;word-break:normal;}
.tg .tg-0ord{text-align:right}
.tg .tg-qnmb{font-weight:bold;font-size:16px;text-align:center}
</style>
<table class="tg">
  <tr>
    <th class="tg-qnmb" colspan="2">POWER OUTAGE DETECTED!!!</th>
  </tr>
  <tr>
    <td class="tg-0ord">Location:</td>
    <td class="tg-031e">Wuse II Office</td>
  </tr>
  <tr>
    <td class="tg-0ord">Address:</td>
    <td class="tg-031e">1, Morija Street<br>City, State<br>00000</td>
  </tr>
  <tr>
    <td class="tg-0ord">Contact Info:</td>
    <td class="tg-031e">YOU - 07033697038</td>
  </tr>
</table>
  </body>
</html>
"""

session = smtplib.SMTP(SERVER, PORT)
session.set_debuglevel(1)
session.ehlo()
session.starttls()
session.ehlo
session.login(SENDER, PASSWORD)
msg = MIMEText(BODY, 'html')
msg['Subject'] = SUBJECT
msg['From'] = SENDER
msg['To'] = ", ".join(RECIPIENTS)
session.sendmail(SENDER, RECIPIENTS, msg.as_string())
session.quit()
