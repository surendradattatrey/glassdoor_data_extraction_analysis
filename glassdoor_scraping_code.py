# -*- coding: utf-8 -*-
"""
Created on Mon Nov 11 17:55:57 2019

@author: surendra_dattatrey
"""

import requests 
from bs4 import BeautifulSoup 
  
URL = "https://www.glassdoor.com/Reviews/Company-Reviews-E3096.htm?sort.sortType=RD&sort.descending=true"
r = requests.get(URL) 
  
soup = BeautifulSoup(r.content, 'html5lib') 
print(soup.prettify()) 
