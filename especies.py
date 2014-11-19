#!/usr/bin/python

#===============================================================================#
#     e-Species - A taxonomically intelligent species search engine.            #
#    (C) 2008, 2009, 2010, 2011, 2012, 2013, 2014 by Mauro J. Cavalcanti        #
#                         <maurobio@gmail.com>                                  #
#                                                                               #
#  This program is free software; you can redistribute it and/or modify         #
#  it under the terms of the GNU General Public License as published by         #
#  the Free Software Foundation; either version 3 of the License, or            #
#  (at your option) any later version.                                          #
#                                                                               #
#  This program is distributed in the hope that it will be useful,              #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of               #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                # 
#  GNU General Public License for more details.                                 #
#                                                                               #
#  You should have received a copy of the GNU General Public License            #
#  along with this program. If not, see <http://www.gnu.org/licenses/>.         #
#                                                                               #       
#  Requirements:                                                                #
#    Python version 2.3 or higher                                               #
#                                                                               #
#  REVISION HISTORY:                                                            #
#    Version 1.00, 29th Jun 08 - Initial public release                         #
#    Version 1.01,  6th Jul 08 - Added suggested spelling for search term       #
#    Version 1.02, 10th Jul 08 - Improved handling of synonym status and fixed  #
#                                a bug in spelling suggestion                   #
#    Version 1.03, 11th Jul 08 - Added a method to class COLSearch to           #
#                                check for the existence of a taxon name        #
#    Version 1.04, 31th Jul 08 - Added automated tagging for Wikipedia snippet  #
#    Version 1.05,  1st Aug 08 - Added a method to class NCBISearch to return   #
#                                a list of external information resources for   #
#                                search name                                    #
#    Version 1.06, 11th Aug 08 - Added a function to strip out markup tags      # 
#                                from Wikipedia snippet                         #
#    Version 1.07, 05th Sep 08 - Fixed a bug in handling Unicode characters in  #
#                                the author of a taxon name returned from CoL   #
#    Version 1.08, 09th Sep 08 - Renamed class SearchImage to YahooSearch       #
#                                and added functions spellingSuggestion         #
#                                (renamed to spellCheck) and termExtraction     #
#                                (renamed to termExtract) as new methods        # 
#    Version 1.09, 21th Oct 08 - Removed dependency of Set module, using tuple  # 
#                                instead, and fixed a problem with the display  #
#                                image thumbnails from Yahoo search             #
#    Version 1.10, 19th Mar 09 - Rewrote class GoogleScholarSearch to remove    #
#                                dependency of BeautifulSoup module, using a    #
#                                HTMLParser instead, and included a default     #
#                                value for class YahooSearch number of results  #
#    Version 1.11, 25th Mar 09 - Improved handling of returned references from  #
#                                Google Scholar and rewrote class               #
#                                WikipediaSearch to make use of Dapper          #
#    Version 1.12, 20th Jul 09 - Added some JavaScript for client-side search   #
#                                form validation                                #
#    Version 1.13, 21th Jul 09 - Added stylesheet for better form display and   #
#                                minor fixes                                    #
#    Version 1.14, 14th Apr 10 - Adjusted for changes in CoL webservice calls   #
#    Version 1.15, 30th Jul 11 - Removed deprecated calls to Yahoo! Images and  #
#                                Yahoo! Spell Checker and substituted           #
#                                GoogleScholarSearch class for a more generic   #
#                                GoogleSearch class with methods to search from #
#                                both Google Scholar and Google Images          #
#    Version 1.16,  2nd Aug 11 - Improved Google Images search                  #
#    Version 1.17, 14th Jul 12 - Fixed a bug query string variable in class     #
#                                CoLSearch and updated URL to the latest Annual #
#                                Checklist version website                      #
#    Version 1.18, 20th Sep 13 - Added new routine for retrieving images from   #
#                                Google and other minor improvements            # 
#    Version 1.19, 16th Aug 14 - Substituted a FiveFilters proxy webservice for #
#                                deprecated calls to Yahoo! Search              #
#===============================================================================#                                                    

import cgi
import cgitb
import urllib
import urllib2
import urlparse
import httplib
import htmllib
import os
import sys
import time
import string
import re
import formatter
from xml.dom import minidom
from xml.dom import Node

def doc_order_iter(node):
    """ Iterates over each node in document order, 	returning each in turn """
    # Document order returns the current node,
    # then each of its children in turn
    yield node
    for child in node.childNodes:
        # Create a generator for each child,
        # Over which to iterate
        for cn in doc_order_iter(child):
            yield cn
    return

class Parser(htmllib.HTMLParser):
    # build a list of tuples (anchor text, URL)

    def __init__(self, verbose=0):
        self.anchors = []
        f = formatter.NullFormatter()
        htmllib.HTMLParser.__init__(self, f, verbose)

    def anchor_bgn(self, href, name, type):
        self.save_bgn()
        self.href = href
        self.name = name
        self.type = type

    def anchor_end(self):
        text = string.strip(self.save_end())
        if self.href and text:
            self.anchors.append( (text, self.href) )

class AppURLopener(urllib.FancyURLopener):
    # trick Google into thinking I'm using Firefox
    version = "Mozilla/5.0 (Windows; U; Windows NT 5.1; it; rv:1.8.1.11) Gecko/20071127 Firefox/2.0.0.11"

urllib._urlopener = AppURLopener()

class COLSearch:
    """ Search Species2000/ITIS Catalogue of Life (http://www.catalogueoflife.org) """
    def __init__(self):
        self.SEARCH_HOST = "http://webservice.catalogueoflife.org"
        self.SEARCH_BASE_URL = "/annual-checklist/2012/"

    def search(self, searchStr):
        usock = urllib.urlopen(self.SEARCH_HOST + self.SEARCH_BASE_URL + "webservice?name=" + urllib.quote_plus(searchStr) + "&response=full")
        xmldoc = minidom.parse(usock)
        usock.close()
        try:
            # Get name and status
            name = xmldoc.getElementsByTagName('name')[0].firstChild.data
            author = xmldoc.getElementsByTagName('author')[0].firstChild.data
            status = xmldoc.getElementsByTagName('name_status')[0].firstChild.data
            # If name is a synonym, get the accepted name
            if status == "synonym" or status == "unambiguous synonym" or status == "ambiguous synonym" or status == "misapplied name":
                item_node = xmldoc.getElementsByTagName('accepted_name')[0]
                valid_name = item_node.getElementsByTagName('name')[0].firstChild.data
                valid_author = item_node.getElementsByTagName('author')[0].firstChild.data
            else:
                valid_name = name
                valid_author = author
            # Get higher taxa for this name
            taxon = []
            for i in range(5):
                item_node = xmldoc.getElementsByTagName('taxon')[i]
                item = item_node.getElementsByTagName('name')[0].firstChild.data
                taxon.append(item)
        except:
            name = ""
            author = ""
            status = ""
            valid_name = ""
            valid_author = ""
            taxon = []
        return (name, author, status, valid_name, valid_author, taxon)
    
    def check(self, searchStr):
        usock = urllib.urlopen(self.SEARCH_HOST + self.SEARCH_BASE_URL + "webservice?name=" + urllib.quote_plus(searchStr))
        xmldoc = minidom.parse(usock)
        usock.close()
        try:
            results = xmldoc.getElementsByTagName('results')
            attribute_list = results[0]
            errmsg = attribute_list.attributes['error_message'].value
            if len(errmsg) == 0:
                return True
            else:
                return False
        except:
            return False

class GBIFSearch:
    """ Search GBIF (http://www.gbif.org) """
    def __init__(self):
        self.SEARCH_HOST = "http://data.gbif.org"
        self.SEARCH_BASE_URL = "/species/taxonName/ajax/returnType/concept/view/ajaxMapUrls/provider/1/"
        
    def __del__(self):
        if os.path.exists("temp1.xml"):
            os.remove("temp1.xml")
        if os.path.exists("temp2.xml"):
            os.remove("temp2.xml")
        
    def search(self, searchStr):
        try:
            usock = urllib.urlopen(self.SEARCH_HOST + self.SEARCH_BASE_URL + "?query=" + urllib.quote_plus(searchStr))
            xmldoc = minidom.parse(usock)
            usock.close()
        except:
            # Handle diacritics in the retrived document
            usock = urllib.urlretrieve(self.SEARCH_HOST + self.SEARCH_BASE_URL + "?query=" + urllib.quote_plus(searchStr), "temp1.xml")
            inf = open("temp1.xml", "r")
            l = inf.readlines()
            inf.close()
            outf = open("temp2.xml", "w")
            for i in range(len(l)):
                try:
                    outf.write (l[i].decode("utf-8"))
                except:
                    continue
            outf.close()
            inf = open("temp2.xml", "r")
            s = inf.read()
            inf.close()
            xmldoc = minidom.parseString(s)
        try:
            # Get taxon id and image name
            taxId = xmldoc.getElementsByTagName('key')[0].firstChild.data
            imgName = xmldoc.getElementsByTagName('url')[0].firstChild.data
        except:
            taxId = ""
            imgName = ""
        return (taxId, imgName)

class NCBISearch:
    """ Search NCBI's Entrez taxonomy database (http://www.ncbi.nlm.nih.gov/Entrez) """
    def __init__(self):
        self.SEARCH_HOST = "http://eutils.ncbi.nlm.nih.gov"
        self.SEARCH_BASE_URL = "/entrez/eutils/"
        
    def search(self, searchStr):
        handle = urllib.urlopen(self.SEARCH_HOST + self.SEARCH_BASE_URL + "esearch.fcgi?db=taxonomy&term=" + urllib.quote_plus(searchStr))
        xmldoc = minidom.parse(handle)
        handle.close()
        try:
            taxId = xmldoc.getElementsByTagName('Id')[0].firstChild.data
            handle = urllib.urlopen(self.SEARCH_HOST + self.SEARCH_BASE_URL + "esummary.fcgi?db=taxonomy&id=" + taxId + "&retmode=xml")
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
        handle = urllib.urlopen(self.SEARCH_HOST + self.SEARCH_BASE_URL + "elink.fcgi?dbfrom=taxonomy&id=" + id + "&cmd=llinkslib")
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
    """ Search Wikipedia (http://en.wikipedia.org) using a transformer from Dapper (http://www.dapper.net)"""
    def __init__(self):
        self.SEARCH_HOST = "http://open.dapper.net"
        self.SEARCH_BASE_URL = "/transform.php?dappName=WikipediaSummary&transformer=CSV&extraArg_fields[]=Some_Content&applyToUrl=http://en.wikipedia.org/wiki/"
            
    def __del__(self):
        if os.path.exists("temp.txt"):
            os.remove("temp.txt")
    
    def search(self, searchStr):
        usock = urllib.urlretrieve(self.SEARCH_HOST + self.SEARCH_BASE_URL + searchStr.replace(' ', '_'), "temp.txt")
        try:
            infile = open("temp.txt", "r")
            lines = []
            while True:
                line = infile.readline()
                if not line: break
                lines.append(line)
            ret_val = lines
            infile.close()
        except:
            ret_val = []
        return ret_val
    
class FiveFiltersSearch:
    """ Search Yahoo! """
    def __init__(self):
        self.SEARCH_HOST = "http://term-extraction.aws.af.cm/"
  
    def __del__(self):
        if os.path.exists("temp.txt"):
            os.remove("temp.txt")    
  
    def termExtract(self, contextStr):
        """ Provides a list of significant words or phrases extracted from a larger content from FiveFilters Web service """
        usock = urllib.urlretrieve(self.SEARCH_HOST + "extract.php?text=" + urllib.quote_plus(contextStr) + "&output=txt&max=10", "temp.txt")
        try:
            infile = open("temp.txt", "r")
            lines = infile.readlines()
            infile.close()
            results = lines
        except:
            resulta = []
        return results
    
class GoogleSearch:
    """Search Google"""
    def __init__(self):
        self.SEARCH_SCHOLAR_HOST = "http://www.scholar.google.com"
        self.SEARCH_IMAGES_HOST = "http://www.images.google.com"
        self.SEARCH_IMAGES_URL = "/images"
        self.SEARCH_SCHOLAR_URL = "/scholar"
        self.SERVICE = None

    def searchScholar(self, searchStr, limit=10):
        """Search Google Scholar for articles and publications containing terms of interest"""
        file = urllib.urlopen(self.SEARCH_SCHOLAR_HOST + self.SEARCH_SCHOLAR_URL + "?q=%s&ie=UTF-8&oe=UTF-8&hl=en&btnG=Search" % urllib.quote_plus(searchStr))
        html = file.read()
        file.close()
        p = Parser()
        p.feed(html)
        p.close()
        candidates = {}
        count = 0
        for text, url in p.anchors:
            if count < limit:
                i = url.find("http")
                if i == 0:
                    j = url.find("google.com")
                    if j == -1:
                        k = url.find("answers.com")
                        if k == -1:
                            l = url.find("direct.bl.uk")
                            if l == -1:
                                candidates[url] = text
                                count += 1
        return candidates
    
    def searchImages(self, searchStr, limit=10):
        """Search Google Images"""
        try:
            import json
        except:
            return []
        # Replace spaces ' ' in search term for '%20' in order to comply with request
        searchStr = searchStr.replace(' ','%20')
        candidates = []
        count = 0
        for i in range(0,2):
            # Notice that the start changes for each iteration in order to request a new set of images for each loop
            url = ('https://ajax.googleapis.com/ajax/services/search/images?' + 'v=1.0&q='+searchStr+'&start='+str(i*4)+'&userip=MyIP')
            request = urllib2.Request(url, None, {'Referer': 'testing'})
            response = urllib2.urlopen(request)

            # Get results using JSON
            results = json.load(response)
            data = results['responseData']
            dataInfo = data['results']

            # Iterate for each result and get unescaped url
            for myUrl in dataInfo:
                count += 1
                candidates.append(myUrl['unescapedUrl'])

                urllib.urlopen(myUrl['unescapedUrl'],str(count)+'.jpg')
                if count >= limit:
                    break

        # Sleep for one second to prevent IP blocking from Google
        time.sleep(1)
        
        return candidates

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
                print "<a href=""/index.htm"">Go back to the query form</a>"
            else:
                print "<html>"
                print "<head>"	
                print "<title>e-Species search results for " + queryStr + "</title>"
                print "<link rel=""stylesheet"" type=""text/css"" href=""/stylesheet.css"">"
                print "</head>"
                print "<body bgcolor=""#ffffff"">"
                print "<h1><img src=""/especies.png"" height=""73"" width=""385""></h1>"
                print "<h3>A taxonomically intelligent biodiversity search engine</h3>"
                print "<p>Search biological databases for a taxonomic name. The search is done ""on the fly"" using web services (SOAP/XML) or URL API's. <a href=""/about.htm"">Learn more about how it works.</a></p>"
                print "<script language=""JavaScript"" type=""text/javascript"" src=""/valid.js"">"
                print "</script>"
                print "<form action=""/cgi-bin/especies.py"" method=""post"" name=""mainform"" id=""mainform"" onsubmit=""return validateTextSearch(this)"">"
                print "<table bgcolor=""#99cccc"" border=""0"">"
                print "<tbody>"
                print "<tr>"
                print "<td>Enter scientific name: <input size=""40"" name=""name"" type=""text""> <input name=""Submit"" value=""Search"" type=""submit""> <input name=""Reset"" value=""Clear"" type=""reset""></td>"
                print "</tr>"
                print "</tbody>"
                print "</table>"
                print "</form>"
                
                searchCOL = COLSearch()
                (name, author, status, valid_name, valid_author, taxon) = searchCOL.search(queryStr)
                if status == "synonym" or status == "unambiguous synonym" or status == "ambiguous synonym" or status == "misapplied name":
                    status = status + " of " + "<i>" + valid_name + "</i> " + valid_author
                print "<h2><i>" + queryStr + "</i> " + author.encode('ascii', 'ignore') + " (" + status + ")</h2>"
                print "<h3>Classification from CoL</h3>"
                if len(name) == 0:
                    print "No names found"
                else:
                    print '; '.join(taxon)
                                
                searchWikipedia = WikipediaSearch()
                result = searchWikipedia.search(queryStr)
                searchFF = FiveFiltersSearch()
                tags = searchFF.termExtract(result[0])
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
                    print result[0]
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
                searchGBIF = GBIFSearch()
                (taxId, imgName) = searchGBIF.search(queryStr)
                if len(taxId) == 0:
                    print "No species found"
                else:
                    urlImg = "http://data.gbif.org/" + imgName
                    taxUrl = "<a href='http://data.gbif.org/species/" + taxId + "'>"
                    print taxUrl + "<img src='" + urlImg + "' width=360 height=180 border=1/></a>"
                    
                searchGoogle = GoogleSearch()
                print "<h3>Images from Google</h3>"
                imgs = searchGoogle.searchImages(queryStr, 5)
                if len(imgs) == 0:
                    print "No images found"
                else:
                    for img in imgs:
                        refUrl = "<a href='" + img + "'>"
                        print refUrl + "<img src='" + img + "' width=94 height=145 border=1></a>"
        
                print "<h3>Articles from Google</h3>"
                pubs = searchGoogle.searchScholar(queryStr, 10)
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
            print "<a href=""/index.htm"">Go back to the query form</a>"
    
        print "<p align=""left""><small><small>&copy; 2008-2014 </small><a href=""http://sites.google.com/site/maurobio/""><small>Mauro J. Cavalcanti</small></a></small></p>"
        print "</body>"
        print "</html>"
    except:
        cgi.print_exception()