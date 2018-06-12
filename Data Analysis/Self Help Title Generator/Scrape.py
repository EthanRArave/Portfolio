#Bring in requisite libraries
import html5lib
import urllib3
from bs4 import BeautifulSoup
import requests

#Path aim to output
path = ("Where I wrote to")

#Storing some soup commands under simple monikers
http = urllib3.PoolManager()
bookname = list()
c = 0
for i in range(785):
	#With Beautiful Soup, there are much smarter methods of crawling these pages, but this was done quickly and worked in one
	url = "https://www.barnesandnoble.com/b/books/personal-growth/success-motivation-self-esteem/_/N-29Z8q8Z1c3b?Nrpp=40&page="+str(i)
	r = http.request('GET', url)
	#Getting each page into soup to parse and pull out what I wanted.
	soup = BeautifulSoup(r.data, 'html.parser')
	#This div storing the title of each book was my breakthrough. I had been unsucessful in extracting a JSON object embeded in each page
	#(though eventually did figure that out), but this turned out to be a perfectly workable solution. 
	for div in soup.findAll('div', attrs={"class":'product-shelf-image'}):
		c += 1
		bookname.append(div.find('a')['aria-describedby'])
		if c% 1000 == 0:
			print(c)
			
#Output the file
with open(path + "Motivation.txt","a") as Test:
			Test.write("\n".join(bookname))

#This succesfully results in a full, 40,000 book titles

