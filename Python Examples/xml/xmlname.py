import xml.etree.ElementTree as ET
from urllib.request import urlopen
#from bs4 import BeautifulSoup don't need this right???
import ssl

# Ignore SSL certificate errors
ctx = ssl.create_default_context()
ctx.check_hostname = False
ctx.verify_mode = ssl.CERT_NONE

#url = input('Enter - ') #have user enter webpage
url = " http://py4e-data.dr-chuck.net/comments_13646.xml" #kitty shortcut
html = urlopen(url, context=ctx).read() #open and read webpage
tree = ET.fromstring(html)


lst = tree.findall('comments/comment')
#print ('Comment list:', len(lst))  # check if reading correctly
sum = 0 
for item in lst:
	numbertxt = int(item.find('count').text)
	sum = sum + numbertxt
print (sum)

