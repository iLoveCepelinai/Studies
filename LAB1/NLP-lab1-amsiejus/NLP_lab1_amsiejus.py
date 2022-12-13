# 2022 10 22
# NLP
# TASK 1
# Matas Amsiejus

import time
import random
import numpy as np
from numpy import genfromtxt
import requests
from bs4 import BeautifulSoup
from fake_user_agent import user_agent
import pandas as pd
import itertools


######################################
# WEB SCRAPING
######################################

baseurl_houzz = 'https://www.houzz.com/'

headers = {
    'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; rv:91.0) Gecko/20100101 Firefox/91.0'
}


# HOUZZ

def scrapeCategory(category):
    # Links with all product pages
    gridList = [f'https://www.houzz.com/products/{category}?ls=2']

    # Create links for 1 + 11 pages
    for x in range(90, 1200, 90):
      gridList.append(f'https://www.houzz.com/products/{category}/p/{x}')

    productLinks = []

    # iterate through shop pages
    for gridLink in gridList:
      time.sleep(random.uniform(0,0.3))

      ua = user_agent()
      headers = {
        'User-Agent': ua
      }
      print("Link:\t{}".format(gridLink))

      r = None

      while r is None:
          try:
              r = requests.get(gridLink, headers = headers, timeout = 10)
          except requests.HTTPError as e:
              print(f"[!] Exception caught: {e}")
              time.sleep(5)

      # get code
      #r = requests.get('https://www.houzz.com/products/rugs?ls=2', headers = headers, timeout = 10)
      soup = BeautifulSoup(r.content, 'lxml')
      # get only product cards
      productList = soup.find_all('div', class_ = 'hz-product-card__meta')
      # iterate through the cards
      for item in productList:
        # get only links with referance (some cards don't have any)
        product = item.find('a', href = True)
        # append prouct link to all product link array (should be around 1000)
        productLinks.append(product['href'])
    return productLinks



######################################################################################
# This is depretiated, do not run

# Category coffee-and-accent-tables
sofasLinks = np.unique(scrapeCategory('sofas-and-sectionals'))
np.savetxt("sofasLinks.csv", sofasLinks, delimiter=",", fmt='%s')

# Category coffee-and-accent-tables
coffeeLinks = np.unique(scrapeCategory('coffee-and-accent-tables'))
np.savetxt("coffeeLinks.csv", coffeeLinks, delimiter=",", fmt='%s')

# Category side-tables-and-accent-tables
sideLinks = np.unique(scrapeCategory('side-tables-and-accent-tables'))
np.savetxt("sideLinks.csv", sideLinks, delimiter=",", fmt='%s')

# Category console-tables
consoleLinks = np.unique(scrapeCategory('console-tables'))
np.savetxt("consoleLinks.csv", consoleLinks, delimiter=",", fmt='%s')

# Category armchairs-and-accent-chairs
armchairLinks = np.unique(scrapeCategory('armchairs-and-accent-chairs'))
np.savetxt("armchairLinks.csv", armchairLinks, delimiter=",", fmt='%s')

# Category entertainment-centers-and-tv-stands
tvLinks = np.unique(scrapeCategory('entertainment-centers-and-tv-stands'))
np.savetxt("tvLinks.csv", tvLinks, delimiter=",", fmt='%s')

# Category chaise-lounge-chairs
chaiseLinks = np.unique(scrapeCategory('chaise-lounge-chairs'))
np.savetxt("chaiseLinks.csv", chaiseLinks, delimiter=",", fmt='%s')

# Category ottomans-and-cubes
cubesLinks = np.unique(scrapeCategory('ottomans-and-cubes'))
np.savetxt("cubesLinks.csv", cubesLinks, delimiter=",", fmt='%s')

#all_houzz_products = sofasLinks + coffeeLinks #+ sideLinks + consoleLinks + armchairLinks + tvLinks + chaiseLinks + cubesLinks
all_houzz_products = np.concatenate([sofasLinks, coffeeLinks, sideLinks, consoleLinks, armchairLinks, tvLinks, chaiseLinks, cubesLinks])
len(all_houzz_products)

all_houzz_products = np.unique(all_houzz_products)

np.savetxt("allhouzzlinks.csv", np.unique(all_houzz_products), delimiter=",", fmt='%s')

#####################################################################################
# RUN THIS INSTEAD

# Category sofas-and-sectionals
def writeLinks (category):
    categLinks = np.unique(scrapeCategory(category))
    #np.savetxt("sofasLinks.csv", sofasLinks, delimiter=",", fmt='%s')

    # With pandas and category column
    category_rep = list(itertools.repeat(category, len(categLinks)))
    d = {
        'Category' : category_rep,
        'Links' : categLinks
    }
    dataframe = pd.DataFrame(d)
    dataframe.to_csv(category+".csv", index = False)
    return dataframe


sofasLinks  = writeLinks('sofas-and-sectionals')
coffeeLinks = writeLinks('coffee-and-accent-tables')
sideLinks = writeLinks('side-tables-and-accent-tables')
consoleLinks = writeLinks('console-tables')
armchairLinks = writeLinks('armchairs-and-accent-chairs')
tvLinks = writeLinks('entertainment-centers-and-tv-stands')
chaiseLinks = writeLinks('chaise-lounge-chairs')
cubesLinks = writeLinks('ottomans-and-cubes')


all_houzz_products = pd.concat([sofasLinks, coffeeLinks, sideLinks, consoleLinks,
                                armchairLinks, tvLinks, chaiseLinks, cubesLinks], ignore_index = True)
# Remove possible duplicates from different categories
all_houzz_products = all_houzz_products.drop_duplicates(keep=False)
# Write csv file
all_houzz_products.to_csv("allhouzzproducts.csv", index = False)


#######################################

# PART 2. Getting product names, descriptions, prices.

# from numpy import genfromtxt
# Depreciated: all_houzz_products = genfromtxt('allhouzzlinks.csv', delimiter=',', filling_values = '0', skip_header = 0, dtype=str)
# New: all_houzz_products = pd.read_csv("allhouzzlinks.csv")
all_houzz_products = pd.read_csv("allhouzzproducts.csv")
houzz_product_list = []


for i in range(len(all_houzz_products)):
    time.sleep(random.uniform(0,0.1))
    ua = user_agent()
    headers = {
        'User-Agent': ua
    }

    r = None
    while r is None:
        try:
            r = requests.get(all_houzz_products.iloc[i,1], headers = headers)
        except requests.HTTPError as e:
            print(f"[!] Exception caught: {e}")
            time.sleep(30)

    soup = BeautifulSoup(r.content, 'lxml')
    try:
        product = {
            'title' : soup.find('span', class_ = 'view-product-title').text.strip(),
            'category' : all_houzz_products.iloc[i,0],
            'desription' : soup.find('div', class_ = 'vp-redesign-description').text.strip(),
            'price' : soup.find('span', class_ = 'pricing-info__price').text.strip()
        }
        print(f'{i} item done')
    except(RuntimeError, TypeError, NameError, BaseException):
        product = {
            'title' : 'ERROR',
            'category' : all_houzz_products.iloc[i,0],
            'desription' : 'Item not found',
            'price' : all_houzz_products.iloc[i,1]
        }
        print('ERROR IN GERRING ITEM')
    
    houzz_product_list.append(product)


df = pd.DataFrame(houzz_product_list)
print(df.head())

df.to_csv("houzz_final_data.csv", index = False)




#######################################################
# AMAZON WEB SCRAPING (DOES NOT WORK. GOT IP BANNED :( )
#######################################################

baseurl_amazon = 'https://www.amazon.com'

productLinksAmazon = []

linka = 'https://www.amazon.com/s?rh=n%3A3733551&fs=true&page=1'
linkb = 'https://www.amazon.com/s?rh=n%3A3733551&fs=true&page=2'

linkc = 'https://www.amazon.com/s?rh=n%3A3733491&fs=true&page=1'
linkd = 'https://www.amazon.com/s?k=sofas+and+sectionals&ref=nb_sb_noss_1'
# n%3A3733491

ua = user_agent()
headers = {
'User-Agent': ua
}
ua = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/74.0.3729.169 Safari/537.36'
r = None
while r is None:
    try:
        r = requests.get(linkd, headers = headers)
    except requests.HTTPError as e:
        print(f"[!] Exception caught: {e}")
        time.sleep(5)

soup = BeautifulSoup(r.content, 'lxml')
soup


def scrapeCategoryAmazon(category):
    # Links with all product pages
    gridList = []
    productLinks = []

    # Create links
    for x in range(1, 10):
      gridList.append(f'https://www.amazon.com/s?rh=n%{category}&fs=true&page={x}')

    # iterate through shop pages
    for gridLink in gridList:
      time.sleep(random.uniform(0,0.3))

      ua = user_agent()
      headers = {
        'User-Agent': ua
      }
      print("Link:\t{}".format(gridLink))

      r = None

      while r is None:
          try:
              r = requests.get(gridLink, headers = headers)
          except requests.HTTPError as e:
              print(f"[!] Exception caught: {e}")
              time.sleep(30)

      # get only product cards
      soup = BeautifulSoup(r.content, 'lxml')
      print(soup)
      productList = soup.find_all('a', class_ = "a-link-normal s-underline-text s-underline-link-text s-link-style a-text-normal")

      for item in productList:
        link = item['href']
        productLinks.append(link)

    return productLinks

productList = soup.find_all('a', class_ = "a-link-normal s-underline-text s-underline-link-text s-link-style a-text-normal")
len(productList)

sofasAmazonLinks = scrapeCategoryAmazon('3A3733551')
len(np.unique(sofasAmazonLinks))