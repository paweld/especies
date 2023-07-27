#!/usr/bin/env python
#================================================================================#
#     e-Species - A taxonomically intelligent species search engine.             #
#                  (C) 2008-2023 by Mauro J. Cavalcanti                          #
#                         <maurobio@gmail.com>                                   #
#                                                                                #
#  This program is free software; you can redistribute it and/or modify          #
#  it under the terms of the GNU General Public License as published by          #
#  the Free Software Foundation; either version 3 of the License, or             #
#  (at your option) any later version.                                           #
#                                                                                #
#  This program is distributed in the hope that it will be useful,               #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of                #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.See the                   # 
#  GNU General Public License for more details.                                  #
#                                                                                #
#  You should have received a copy of the GNU General Public License             #
#  along with this program. If not, see <http://www.gnu.org/licenses/>.          #
#                                                                                #       
#  Requirements:                                                                 #
#    Python version 2.6 or higher                                                #
#                                                                                #
#  REVISION HISTORY:                                                             #
#    Version 1.00, 29th Jun 08 - Initial public release                          #
#    Version 1.01,  6th Jul 08 - Added suggested spelling for search term        #
#    Version 1.02, 10th Jul 08 - Improved handling of synonym status and fixed   #
#                                a bug in spelling suggestion                    #
#    Version 1.03, 11th Jul 08 - Added a method to class COLSearch to            #
#                                check for the existence of a taxon name         #
#    Version 1.04, 31th Jul 08 - Added automated tagging for Wikipedia snippet   #
#    Version 1.05,  1st Aug 08 - Added a method to class NCBISearch to return    #
#                                a list of external information resources for    #
#                                search name                                     #
#    Version 1.06, 11th Aug 08 - Added a function to strip out markup tags       # 
#                                from Wikipedia snippet                          #
#    Version 1.07, 05th Sep 08 - Fixed a bug in handling Unicode characters in   #
#                                the author of a taxon name returned from CoL    #
#    Version 1.08, 09th Sep 08 - Renamed class SearchImage to YahooSearch        #
#                                and added functions spellingSuggestion          #
#                                (renamed to spellCheck) and termExtraction      #
#                                (renamed to termExtract) as new methods         # 
#    Version 1.09, 21th Oct 08 - Removed dependency of Set module, using tuple   # 
#                                instead, and fixed a problem with the display   #
#                                image thumbnails from Yahoo search              #
#    Version 1.10, 19th Mar 09 - Rewrote class GoogleScholarSearch to remove     #
#                                dependency of BeautifulSoup module, using a     #
#                                HTMLParser instead, and included a default      #
#                                value for class YahooSearch number of results   #
#    Version 1.11, 25th Mar 09 - Improved handling of returned references from   #
#                                Google Scholar and rewrote class                #
#                                WikipediaSearch to make use of Dapper           #
#    Version 1.12, 20th Jul 09 - Added some JavaScript for client-side search    #
#                                form validation                                 #
#    Version 1.13, 21th Jul 09 - Added stylesheet for better form display and    #
#                                minor fixes                                     #
#    Version 1.14, 14th Apr 10 - Adjusted for changes in CoL webservice calls    #
#    Version 1.15, 30th Jul 11 - Removed deprecated calls to Yahoo! Images and   #
#                                Yahoo! Spell Checker and substituted            #
#                                GoogleScholarSearch class for a more generic    #
#                                GoogleSearch class with methods to search from  #
#                                both Google Scholar and Google Images           #
#    Version 1.16,  2nd Aug 11 - Improved Google Images search                   #
#    Version 1.17, 14th Jul 12 - Fixed a bug query string variable in class      #
#                                CoLSearch and updated URL to the latest Annual  #
#                                Checklist version website                       #
#    Version 1.18, 20th Sep 13 - Added new routine for retrieving images from    #
#                                Google and other minor improvements             # 
#    Version 1.19, 16th Aug 14 - Substituted a FiveFilters proxy webservice for  #
#                                deprecated calls to Yahoo! Search               #
#    Version 1.20  22th Sep 14 - Fixed the FiveFilters webservice URL            #
#    Version 1.30  31th Jan 19 - Removed class GoogleSearch and replaced it for  #
#                                a PubMedSearch class to get bibliographic       #
#                                references from PubMed. Added a method for      # 
#                                fetching images from Wikimedia Commons to       # 
#                                class WikipediaSearch. Restored dependency to   #
#                                BeautifulSoup                                   #
#    Version 1.40   4th Jul 23 - Substituted a GBFI Taxobomic Backbone search    #
#                                for the deprecated CoL webservice               #
#                                Fixed duplicate entries in list of PubMed       #
#                                references                                      #
#    Version 1.50  19th Jul 23 - Added italics to scientific names in taxon      #
#                                classification output                           #
#================================================================================#

import cgi
import cgitb
import urllib
import urllib2
import urlparse
import httplib
import htmllib
import json
import os
import sys
import time
import string
import re
import xml.etree.ElementTree as ET
from xml.dom import minidom, Node
from bs4 import BeautifulSoup

def doc_order_iter(node):
    """ Iterates over each node in document order, returning each in turn """
    # Document order returns the current node,
    # then each of its children in turn
    yield node
    for child in node.childNodes:
        # Create a generator for each child,
        # Over which to iterate
        for cn in doc_order_iter(child):
            yield cn
    return

class AppURLopener(urllib.FancyURLopener):
    # trick Google into thinking I'm using Firefox
    version = "Mozilla/5.0 (Windows; U; Windows NT 5.1; it; rv:1.8.1.11) Gecko/20071127 Firefox/2.0.0.11"

urllib._urlopener = AppURLopener()

class GBIFSearch:
    """ Search GBIF (http://www.gbif.org) """
    def __init__(self):
        self.GBIF_URL = "http://api.gbif.org/v1"
        
    def search(self, searchStr):
        try:
            response = urllib.urlopen(self.GBIF_URL + "/species/?name=" + searchStr)
            result = json.load(response)
            # Get taxon id
            key = result['results'][0]['key']
            # Get name and status
            name = result['results'][0]['canonicalName']
            author = result['results'][0]['authorship']
            status = result['results'][0]['taxonomicStatus']
            status = status.lower()
            # If name is a synonym, get the accepted name
            if status != "accepted":
                valid_name = result['results'][0]['accepted']
            else:
                valid_name = name
            # Get higher taxa for this name
            taxon = []    
            kingdom = result['results'][0]['kingdom']
            phylum = result['results'][0]['phylum']
            classe = result['results'][0]['class']
            order = result['results'][0]['order']
            family = result['results'][0]['family']
            taxon.append(kingdom)
            taxon.append(phylum)
            taxon.append(classe)
            taxon.append(order)
            taxon.append(family)
        except:
            key = 0
            name = ""
            author = ''
            status = ""
            valid_name = ""
            taxon = []
        return (key, name, author, status, valid_name, taxon)
    
    def count(self, key):
        response = urllib.urlopen(self.GBIF_URL + "/occurrence/search?taxonKey=" + str(key))
        result = json.load(response)
        count = result['count']
        return count

class NCBISearch:
    """ Search NCBI's Entrez taxonomy database (http://www.ncbi.nlm.nih.gov/Entrez) """
    def __init__(self):
        self.NCBI_URL = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
        
    def search(self, searchStr):
        handle = urllib.urlopen(self.NCBI_URL + "esearch.fcgi?db=taxonomy&term=" + urllib.quote_plus(searchStr))
        xmldoc = minidom.parse(handle)
        handle.close()
        try:
            taxId = xmldoc.getElementsByTagName('Id')[0].firstChild.data
            handle = urllib.urlopen(self.NCBI_URL + "esummary.fcgi?db=taxonomy&id=" + taxId + "&retmode=xml")
            xmldoc = minidom.parse(handle)
            handle.close()
            division = xmldoc.getElementsByTagName('Item')[1].firstChild.data
            scientificName = xmldoc.getElementsByTagName('Item')[2].firstChild.data
            nucNum = xmldoc.getElementsByTagName('Item')[5].firstChild.data
            protNum = xmldoc.getElementsByTagName('Item')[6].firstChild.data
        except:
            taxId = "0"
            division = ""
            scientificName = ""
            nucNum = "0"
            protNum = "0"
        return (taxId, division, scientificName, nucNum, protNum)
    
    def links(self, id):
        handle = urllib.urlopen(self.NCBI_URL + "elink.fcgi?dbfrom=taxonomy&id=" + id + "&cmd=llinkslib")
        xmldoc = minidom.parse(handle)
        handle.close()
        keys = []
        vals = []
        results = {}
        try:	
            for node in doc_order_iter(xmldoc):
                if node.nodeType == Node.ELEMENT_NODE:
                    if node.localName == "ObjUrl":
                        name = node.getElementsByTagName('Name')[0].firstChild.data
                        url = node.getElementsByTagName('Url')[0].firstChild.data
                        keys.append(name)
                        vals.append(url)
            results = dict(zip(keys, vals))
        except:
            pass
        return results
    
class WikipediaSearch:
    """ Search Wikipedia (http://en.wikipedia.org) articles"""
    def __init__(self):
        self.WIKIPEDIA_URL = "https://en.wikipedia.org/wiki/"
            
    def snippet(self, searchStr):
        url = self.WIKIPEDIA_URL + searchStr.replace(' ', '_')
        try:
            html = urllib2.urlopen(url)
            soup = BeautifulSoup(html, 'html.parser')
            count = 0
            text = ''
            for para_tag in soup.find_all('p'):
                if count <= 2:
                    if len(para_tag.text) > 1:
                        if count == 2:
                            text += para_tag.text.strip('\n')
                    count += 1
                else:
                    break
            result = text.encode("ascii", "ignore")
        except:
            result = ""
        return result
    
    def images(self, searchStr, limit=10):
        """Search images from Wikimedia Commons"""
        candidates = []
        count = 0
        url = self.WIKIPEDIA_URL + searchStr.replace(' ', '_')
        try:
            html = urllib2.urlopen(url)
            soup = BeautifulSoup(html, 'html.parser')
            images = soup.find_all('img', {'src':re.compile('.jpg')})
            for image in images: 
                candidates.append('https:' + image['src'])
                count += 1
                if count >= limit:
                    break
        except:
            candidates = []
        return candidates

class FiveFiltersSearch:
    """ Search FiveFilters """
    def __init__(self):
        self.FF_URL = "http://termextract.fivefilters.org/"
        
    def __del__(self):
        if os.path.exists("temp.txt"):
            os.remove("temp.txt")    
  
    def termExtract(self, contextStr):
        """ Provides a list of significant words or phrases extracted from a larger content from FiveFilters Web service """
        usock = urllib.urlretrieve(self.FF_URL + "extract.php?text=" + urllib.quote_plus(contextStr) + "&output=txt&max=10", "temp.txt")
        try:
            infile = open("temp.txt", "r")
            lines = infile.readlines()
            infile.close()
            results = lines
        except:
            resulta = []
        return results
    
class PubMedSearch:
    """Search PubMed"""
    def __init__(self):
        self.PUBMED_URL = "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
        
    def search(self, searchStr, limit=10):
        """Search PubMed for articles and publications containing terms of interest"""
        res = {}
        url = self.PUBMED_URL + "esearch.fcgi?db=pubmed&retmode=json&retmax=" + str(limit) + "&sort=relevance&term=" + urllib.quote_plus(searchStr) 
        response = urllib.urlopen(url)
        result = json.load(response)
        idlist = result["esearchresult"]["idlist"]
        url = self.PUBMED_URL + "efetch.fcgi?db=pubmed&retmode=xml&id=" + ','.join(idlist)
        response = urllib.urlopen(url)
        xmldoc = ET.parse(response)
        response.close()
        root = xmldoc.getroot()
        for pubmedArticle in xmldoc.getiterator(tag='PubmedArticle'):  
            articleTitle = pubmedArticle.find('./MedlineCitation/Article/ArticleTitle')
            title = articleTitle.text
            for articleId in pubmedArticle.findall('.//ArticleId'):
                if articleId.attrib.get("IdType") == "doi":
                    doi = "http://dx.doi.org/doi:" + articleId.text
                    res[doi] = title
        temp = []
        results = {}
        for key, val in res.items():
            if val not in temp:
                temp.append(val)
                results[key] = val
        return results

if __name__ == '__main__':
    try:
        cgitb.enable()
        print "Content-type: text/html\n\n"
        form = cgi.FieldStorage()
        print "<html>"
        print "<body>"
        if form.has_key("name"):
            queryStr = form["name"].value
            queryStr = queryStr.capitalize()
        
            words = queryStr.split()
            if len(words) != 2:
                print "<title>e-Species</title>"
                print "<h3>Error filling out form</h3>"
                print "<p>Please enter a binomial specific epithet into the text box.</a>"
                print "<p>Remember: Only <a href=""http://en.wikipedia.org/wiki/Species"">species</a> are true natural entities!</p>"
                print "<a href=""index.htm"">Go back to the query form</a>"
            else:
                print "<html>"
                print "<head>"	
                print "<title>e-Species search results for " + queryStr + "</title>"
                print "<link rel=""stylesheet"" type=""text/css"" href=""../especies/stylesheet.css"">"
                print "</head>"
                print "<body bgcolor=""#ffffff"">"
                print "<h1><img src=""../especies/especies.png"" height=""73"" width=""385""></h1>"
                print "<h3>A taxonomically intelligent biodiversity search engine</h3>"
                print "<p>Search biological databases for a taxonomic name. The search is done ""on the fly"" using web services (JSON/XML) or URL API's. <a href=""../especies/about.htm"">Learn more about how it works.</a></p>"
                print "<form>"
                print "<input type=""button"" value=""Back"" onclick=""history.back()"">"
                print "</form>"
                
                searchGBIF = GBIFSearch()
                (key, name, author, status, valid_name, taxon) = searchGBIF.search(queryStr)
                if len(status) > 0: 
                    if status != "accepted":
                        status = " (" + status + " of <i>" + valid_name + "</i>" + ") " + author
                    else:
                        status = " (" + status + ")"
                print "<h2><i>" + queryStr + "</i>" + author + status + "</h2>"
                print "<h3>Classification from CoL</h3>"
                if len(name) == 0:
                    print "No names found"
                else:
                    print ';'.join(taxon)
                                
                searchWikipedia = WikipediaSearch()
                result = searchWikipedia.snippet(queryStr)
                searchFF = FiveFiltersSearch()
                tags = searchFF.termExtract(result)
                tagHTML = ""
                print "<h3>Text tags</h3>"
                for i in range(len(tags)):
                    tag = tags[i]
                    tag = tag.replace(" ", "&nbsp;")
                    tagHTML += "<span style='display:inline;border:1px solid blue; padding:1px;margin:2px;line-height:22px;background-color:rgb(181,213,255);'>" + tag + " " + "</span>"
                print tagHTML
                
                strWiki = queryStr
                urlWiki = "http://en.wikipedia.org/wiki/" + strWiki.replace(" ", "_")
                print "<h3>Wikipedia</h3>"
                if len(result) == 0:
                    print "No article title matches"
                else:	
                    print result
                    print "<p><a href=" + urlWiki + ">Original article<a></p>"
                
                print "<h3>Genomics from NCBI</h3>"
                searchNCBI = NCBISearch()
                (taxId, division, scientificName, nucNum, protNum) = searchNCBI.search(queryStr)
                urlId = "http://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?mode=Info&id=" + taxId
                urlNuc = "http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=Nucleotide&cmd=Search&dopt=DocSum&term=txid" + taxId + "[Organism:exp]"
                urlProt = "http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=Protein&cmd=Search&dopt=DocSum&term=txid" + taxId + "[Organism:exp]"
                print "TaxId: <a href=" + urlId + ">" + taxId + "</a>",
                if len(scientificName) == 0:
                    print "No items found for " + "<i>" + queryStr + "</i> ",
                else:
                    print "<i>" + scientificName + "</i>",
                    print "[" + division + "] ",
                print "Sequences: " + "<a href=" + urlNuc + ">" + nucNum + "</a> nucleotide, " + "<a href=" + urlProt + ">" + protNum + "</a> protein"
                linkOut = searchNCBI.links(taxId)
                print "<ul type='circle'>"
                for k, v in linkOut.items():
                    print "<li><a href=" + v + ">" + k + "</a></li>"
                print "</ul>"

                print "<h3>Map from GBIF</h3>"
                if key == 0:
                    print "No species found"
                else:
                    n = searchGBIF.count(key)
                    taxUrl = "<a href=""http://gbif.org/species/""" + str(key) + ">"
                    print "<p>" + taxUrl + str(n) + " record(s)</a></p>"
                    print taxUrl + "<iframe id=""mapByFrame"" name=""map"" src=""http://cdn.gbif.org/v1/map/index.html?type=TAXON&key=" + str(key) + "&resolution=2"" height=""56%"" width=""56%"" frameborder=""1""/></iframe></a>"
                    
                print "<h3>Images from Wikimedia Commons</h3>\n"
                imgs = searchWikipedia.images(queryStr, 5)
                if len(imgs) == 0:
                    print "No images found"
                else:
                    for img in imgs:
                        refUrl = "<a href='" + img + "'>"
                        print refUrl + "<img src='" + img + "' width=94 height=145 border=1></a>"
        
                print "<h3>Articles from PubMed</h3>"
                searchPubMed = PubMedSearch()
                pubs = searchPubMed.search(queryStr, 10)
                if len(pubs) == 0:
                    print "No articles found"
                else:	
                    for pub in pubs.keys():
                        print "<hr noshade>"
                        print "<b><a href='" + pub + "'>" + pubs[pub] + "</a></b><br>"
        else:
            print "<title>e-Species</title>"
            print "<h3>Error filling out form</h3>"
            print "<p>Please enter a binomial specific epithet into the text box.</a>"
            print "<p>Remember: Only <a href=""http://en.wikipedia.org/wiki/Species"">species</a> are true natural entities!</p>"
            print "<a href=""index.htm"">Go back to the query form</a>"
    
        print "<p align=""left""><small><small>&copy; 2008-2023 </small><a href=""http://github.com/maurobio/""><small>Mauro J. Cavalcanti</small></a></small></p>"
        print "</body>"
        print "</html>"
    except:
        cgi.print_exception()