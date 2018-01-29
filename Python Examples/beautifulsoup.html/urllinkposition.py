import urllib.request, urllib.parse, urllib.error
from bs4 import BeautifulSoup
import ssl

ctx = ssl.create_default_context()
ctx.check_hostname = False
ctx.verify_mode = ssl.CERT_NONE


url = "http://py4e-data.dr-chuck.net/known_by_Pawel.html"
html = urllib.request.urlopen(url, context = ctx).read()
soup = BeautifulSoup(html, 'html.parser')

count = 0
while count <7:	
	
	tags = soup('a')
	third = tags[17]
	link = third.get('href', None)
	#name = third.get('title', None)
	#print (name)
	html = urllib.request.urlopen(link, context = ctx).read()
	soup = BeautifulSoup(html, 'html.parser')
	count = count + 1

print (count)
print (link)
	