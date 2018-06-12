#Requisite libraries
import markovify
import random

#Data generated from Scraper.py (and provided)
rpath=("~Motivation.txt")
filen = "Motivation.txt"

#Read in motivation.txt and spit out chains. 
with open(rpath+filen,encoding="utf8") as f:
    text = f.read()
text_model = markovify.Text(text)
y = 0
l=list()

#Book limit sets the number of titles output from one iteration
book_limit = 100
for i in range(7):
    l.append((i+5)*10)
while y < book_limit:	
	x = text_model.make_short_sentence(random.choice(l))
	
#These are post-creation adjustments made to hastily assure that output is more believable
	if str(x) == "None" or str(x).count(":") > 1:
		continue
	else: 
		y += 1
	print(x)
