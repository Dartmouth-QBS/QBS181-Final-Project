import requests
from bs4 import BeautifulSoup
import os

class Parser:
    """
    A class of useful tools for parsing Chicago PDF data.
    """

    def __init__(self):
        self.url = 'https://www.cmap.illinois.gov/data/community-snapshots'
        self.urlprefix = 'https://www.cmap.illinois.gov'
        self.soup = self.pageGetter()
        self.URLlist = self.urlList()
        self.outputdir = './pdfs/'

    def pageGetter(self):
        session = requests.Session()
        session.headers.update({'User-Agent': 'Custom user agent'})
        my_page = (session.get(self.url)).text
        soup = BeautifulSoup(my_page, "html.parser")
        return soup

    def urlList(self):
        ULs = self.soup.find_all('ul')[-1]
        URLlist = []

        for li in ULs.find_all('li'):
            a = li.find('a')
            URLlist.append(self.urlprefix + a['href'])

        return URLlist

class PDFPull(Parser):

    def __init__(self):
        super().__init__()

    def download(self):
        os.mkdir(self.outputdir)
        for url in self.URLlist:
            session = requests.Session()
            session.headers.update({'User-Agent': 'Custom user agent'})
            # print("Downloading", url)
            name = url.split("/")[-1]
            print("Downloading file: ", name)
            # Get response object for link
            response = session.get(url)

            # Write content in pdf file
            pdf = open(self.outputdir + name, 'wb')
            pdf.write(response.content)
            pdf.close()
        print("End download")

PDFPull().download()