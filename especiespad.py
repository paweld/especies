#!/usr/bin/python
# -*- coding: utf-8 -*-

#===============================================================================#
#     e-Species Pad - A taxonomically intelligent species search engine         #
#               (C) 2008-2014 by Mauro J. Cavalcanti                            #
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
#    Python version 2.7 or higher                                               #
#    PyQt version 4.1 or higher                                                 #
#    BeautifulSoup version 4.3 or higher                                        #
#                                                                               #
#  REVISION HISTORY:                                                            #
#    Version 1.00, 27th Sep 14 - Initial public release                          #
#===============================================================================#                                                 

import urllib
import urllib2
import urlparse
import httplib
import htmllib
import json
import os
import sys
import platform
import time
import string
import re
import formatter
from xml.dom import minidom
from xml.dom import Node
from bs4 import BeautifulSoup
from PyQt4 import QtCore, QtGui, QtWebKit
from PyQt4.QtCore import Qt
from PyQt4.QtGui import QApplication, QCursor
from PyQt4.QtCore import QT_VERSION_STR
from PyQt4.Qt import PYQT_VERSION_STR

__version__ = "1.0.0"

def we_are_frozen():
    """Returns whether we are frozen via py2exe.
    This will affect how we find out where we are located."""
    return hasattr(sys, "frozen")

def module_path():
    """ This will get us the program's directory,
    even if we are frozen using py2exe"""
    if we_are_frozen():
        return os.path.dirname(unicode(sys.executable, sys.getfilesystemencoding( )))
    return os.path.dirname(unicode(__file__, sys.getfilesystemencoding( )))

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
        self.SEARCH_HOST = "http://termextract.fivefilters.org/"
        
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
        self.SEARCH_GOOGLE_HOST = "http://www.google.com"
        self.SEARCH_SCHOLAR_HOST = "http://www.scholar.google.com"
        self.SEARCH_IMAGES_HOST = "http://www.images.google.com"
        self.SEARCH_GOOGLE_URL = "/search"
        self.SEARCH_IMAGES_URL = "/images"
        self.SEARCH_SCHOLAR_URL = "/scholar"

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
        # Replace spaces ' ' in search term for '%20' in order to comply with request
        searchStr = searchStr.replace(' ','%20')
        candidates = []
        count = 0
        for i in range(0,2):
            # Notice that the start changes for each iteration in order to request a new set of images for each loop
            url = ('https://ajax.googleapis.com/ajax/services/search/images?' + 'v=1.0&q='+searchStr+'&start='+str(i*4)+'&userip=MyIP')
            request = urllib2.Request(url, None, {'Referer': 'testing'})
            response = urllib2.urlopen(request)

            try:
                # Get results using JSON
                results = json.load(response)
                data = results['responseData']
                dataInfo = data['results']
            except:
                break

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
    
class Browser(QtGui.QMainWindow):

    def __init__(self):
        """ Initialize the browser GUI and connect the events """

        QtGui.QMainWindow.__init__(self)
        self.name = ""
        self.centralwidget = QtGui.QWidget(self)
        
        self.mainLayout = QtGui.QHBoxLayout(self.centralwidget)
        self.mainLayout.setSpacing(0)
        self.mainLayout.setMargin(1)

        self.frame = QtGui.QFrame(self.centralwidget)

        self.gridLayout = QtGui.QVBoxLayout(self.frame)
        self.gridLayout.setMargin(0)
        self.gridLayout.setSpacing(0)

        self.horizontalLayout = QtGui.QHBoxLayout()
        self.edt_find = QtGui.QLineEdit(self.frame)
        self.btn_home = QtGui.QPushButton(self.frame)
        self.btn_load = QtGui.QPushButton(self.frame)
        self.btn_back = QtGui.QPushButton(self.frame)
        self.btn_ahead = QtGui.QPushButton(self.frame)
        self.btn_about = QtGui.QPushButton(self.frame)
        self.btn_exit = QtGui.QPushButton(self.frame)
        self.btn_home.setFlat(True)
        self.btn_load.setFlat(True)
        self.btn_back.setFlat(True)
        self.btn_ahead.setFlat(True)
        self.btn_about.setFlat(True)
        self.btn_exit.setFlat(True)
        self.btn_home.setToolTip("Home page")
        self.btn_load.setToolTip("Reload search results")
        self.btn_back.setToolTip("Return to previous page")
        self.btn_ahead.setToolTip("Advance to next page")
        self.btn_about.setToolTip("About this application")
        self.btn_exit.setToolTip("Exit application")

        self.btn_home.setIcon(QtGui.QIcon("static/home.png"))    
        self.btn_load.setIcon(QtGui.QIcon("static/reload.png"))
        self.btn_back.setIcon(QtGui.QIcon("static/back.png"))
        self.btn_ahead.setIcon(QtGui.QIcon("static/forward.png"))
        self.btn_about.setIcon(QtGui.QIcon("static/about.png"))
        self.btn_exit.setIcon(QtGui.QIcon("static/exit.png"))

        self.horizontalLayout.addWidget(self.btn_home)
        self.horizontalLayout.addWidget(self.btn_load)
        self.horizontalLayout.addWidget(self.btn_back)
        self.horizontalLayout.addWidget(self.btn_ahead)
        self.horizontalLayout.addWidget(self.btn_about)
        self.horizontalLayout.addWidget(self.btn_exit)
        self.horizontalLayout.addWidget(self.edt_find)
        self.gridLayout.addLayout(self.horizontalLayout)

        self.browser = QtWebKit.QWebView()
        self.browser.setContextMenuPolicy(Qt.NoContextMenu)
        self.home()
        
        self.gridLayout.addWidget(self.browser)
        self.mainLayout.addWidget(self.frame)
        self.setCentralWidget(self.centralwidget)
        self.setGeometry(192, 107, 766, 441)
        self.setWindowTitle("e-Species Pad")    
        self.setWindowIcon(QtGui.QIcon("static/icon.png"))

        self.connect(self.edt_find, QtCore.SIGNAL("returnPressed()"), self.find)
        self.connect(self.btn_home, QtCore.SIGNAL("clicked()"), self.home)
        self.connect(self.btn_load, QtCore.SIGNAL("clicked()"), self.load)
        self.connect(self.btn_back, QtCore.SIGNAL("clicked()"), self.browser.back)
        self.connect(self.btn_ahead, QtCore.SIGNAL("clicked()"), self.browser.forward)
        self.connect(self.btn_about, QtCore.SIGNAL("clicked()"), self.about)
        self.connect(self.btn_exit, QtCore.SIGNAL("clicked()"), self.close)
        self.show()
        
    def home(self):
        try:
            approot = os.path.abspath(os.path.dirname(__file__))
        except NameError:  # We are the main py2exe script, not a module
            approot = os.path.abspath(os.path.dirname(sys.argv[0]))
        baseUrl = QtCore.QUrl.fromLocalFile(os.path.join(approot, "static/"))
        html = """
        <html>
        <head>
        <link rel="stylesheet" type="text/css" href="stylesheet.css">
        </head>
        <body bgcolor="#FFFFFF">
        <h1><img src="especies.png" width="385" height="73"></h1>
        <h3>A taxonomically intelligent biodiversity search engine</h3>
        <p>Search biological databases for a taxonomic name. The search
        is done &quot;on the fly&quot; using web services (SOAP/XML) or
        URL API's.&nbsp;<a href="about.htm">Learn more about how it works</a></p>

        <p align="left">&copy; 2008-2014 Mauro J. Cavalcanti</p>

        <br><br><br><br><br><br><br>
        <p align="right"><br>
        <br>
        <font size="2"><span style="font-size: 9px; color: rgb(176, 176, 176); font-family: Arial;">Hosted by</span></font>&nbsp;<a href="http://biotupe.org"><img
        src="biotupe.gif" border="0" width="126" height="43"></a> <a
        href="http://sourceforge.net/projects/especies/"><img
        src="sf.jpg" border="0" width="105" height="31"></a></p>
        </body>
        </html>
        """
        self.browser.setHtml(html, baseUrl)
        
    def load(self):
        if len(self.name) == 0: return
        stream = QtCore.QFile("data/" + self.name)
        if stream.open(QtCore.QFile.ReadOnly):
           html = QtCore.QString.fromUtf8(stream.readAll())
        stream.close()
        try:
            approot = os.path.abspath(os.path.dirname(__file__))
        except NameError:  # We are the main py2exe script, not a module
            approot = os.path.abspath(os.path.dirname(sys.argv[0]))
        baseUrl = QtCore.QUrl.fromLocalFile(os.path.join(approot, "static/"))
        self.browser.setHtml(html, baseUrl)

    def find(self):
        if self.edt_find.text():
            text = str(self.edt_find.text()).capitalize()
            words = str(self.edt_find.text()).split()
        else:
            return
    
        if len(words) != 2:
            QtGui.QMessageBox.warning(self, "Error", 
            "Please enter a binomial specific epithet into the text box.\n\n Remember: Only *species* are true natural entities!",
            QtGui.QMessageBox.Ok, QtGui.QMessageBox.NoButton, QtGui.QMessageBox.NoButton)
        else:
            QApplication.setOverrideCursor(QCursor(Qt.WaitCursor))
            self.name = self.search(text)
            QApplication.restoreOverrideCursor()
            if self.name:
                self.load()
            else:    
                QtGui.QMessageBox.warning(self, "Error", 
                    "Name '" + text + "' not found!",
                QtGui.QMessageBox.Ok, QtGui.QMessageBox.NoButton, QtGui.QMessageBox.NoButton)
            return
        
    def about(self):
        QtGui.QMessageBox.about(self, "About e-Species Pad", 
        """<b>e-Species Pad</b> v %s
        <p>A taxonomically intelligent biodiversity search engine.
        <p>Search biological databases for a taxonomic name. The search is done ""on the fly"" using web services (SOAP/XML) or URL API's.
        <p>&copy; 2008-2014 Mauro J. Cavalcanti
        <p>Python %s - Qt %s - PyQt %s on %s %s""" % (__version__, platform.python_version(),
        QT_VERSION_STR, PYQT_VERSION_STR, platform.system(), platform.release()))
        
    def closeEvent(self, event):
        quit_msg = "Are you sure you want to exit the program?"
        reply = QtGui.QMessageBox.question(self, "Confirmation", quit_msg, QtGui.QMessageBox.Yes, QtGui.QMessageBox.No)

        if reply == QtGui.QMessageBox.Yes:
            event.accept()
        else:
            event.ignore()
            
    def search(self, queryStr):
        searchCOL = COLSearch()
        (name, author, status, valid_name, valid_author, taxon) = searchCOL.search(queryStr)
        if len(name) == 0:
            return
        
        if not os.path.exists("data/"):
            os.makedirs("data/")
        filename = queryStr.replace(" ", "_") + ".htm"
        outfile = open("data/" + filename, "wb")
        outfile.write("<html>\n")
        outfile.write("<head>\n")
        outfile.write("<link rel=""stylesheet"" type=""text/css"" href="'../static/stylesheet.css'">\n")
        outfile.write("</head>\n")
        outfile.write("<body bgcolor=""#ffffff"">\n")
        outfile.write("<h1><img src="'../static/especies.png'" height=""73"" width=""385""></h1>\n")
        outfile.write("<h3>A taxonomically intelligent biodiversity search engine</h3>\n")
        outfile.write("<p>Search biological databases for a taxonomic name. The search is done ""on the fly"" using web services (SOAP/XML) or URL API's.\n") 

        #searchCOL = COLSearch()
        #(name, author, status, valid_name, valid_author, taxon) = searchCOL.search(queryStr)
        if status == "synonym" or status == "unambiguous synonym" or status == "ambiguous synonym" or status == "misapplied name":
            status = status + " of " + "<i>" + valid_name + "</i> " + valid_author
        outfile.write("<h2><i>" + queryStr + "</i> " + author.encode('ascii', 'ignore') + " (" + status + ")</h2>\n")
        outfile.write("<h3>Classification from CoL</h3>\n")
        #if len(name) == 0:
        #    outfile.write("No names found\n")
        #else:
        outfile.write('; '.join(taxon))
        outfile.write("\n")
                                
        searchWikipedia = WikipediaSearch()
        result = searchWikipedia.search(queryStr)
        
        searchFF = FiveFiltersSearch()
        tags = searchFF.termExtract(result[0])
        tagHTML = ""
        outfile.write("<h3>Keywords</h3>\n")
        for i in range(len(tags)):
            tag = tags[i]
            tag = tag.replace(" ", "&nbsp;")
            tagHTML += "<span style='display:inline;border:1px solid blue; padding:1px;margin:2px;line-height:22px;background-color:rgb(181,213,255);'>" + tag + " " + "</span>"
        outfile.write(tagHTML)
        outfile.write("\n")

        strWiki = queryStr
        urlWiki = "http://en.wikipedia.org/wiki/" + strWiki.replace(" ", "_")
        outfile.write("<h3>Wikipedia</h3>\n")
        if len(result) == 0:
            outfile.write("No article title matches\n")
        else:	
            outfile.write(result[0])
            outfile.write("\n")
            outfile.write("<p><a href=" + urlWiki + ">Original article<a></p>\n")
    
        outfile.write("<h3>Genomics from NCBI</h3>\n")
        searchNCBI = NCBISearch()
        (taxId, division, scientificName, nucNum, protNum) = searchNCBI.search(queryStr)
        urlId = "http://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?mode=Info&id=" + taxId
        urlNuc = "http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=Nucleotide&cmd=Search&dopt=DocSum&term=txid" + taxId + "[Organism:exp]"
        urlProt = "http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=Protein&cmd=Search&dopt=DocSum&term=txid" + taxId + "[Organism:exp]"
        outfile.write("TaxId: <a href=" + urlId + ">" + taxId + "</a> ")
        if len(scientificName) == 0:
            outfile.write("No items found for " + "<i>" + queryStr + "</i> ")
        else:
            outfile.write("<i>" + scientificName + "</i>")
            outfile.write(" [" + division + "] ")
        outfile.write("Sequences: " + "<a href=" + urlNuc + ">" + nucNum + "</a> nucleotide, " + "<a href=" + urlProt + ">" + protNum + "</a> protein\n")
        linkOut = searchNCBI.links(taxId)
        outfile.write("<ul type='circle'>\n")
        for k, v in linkOut.items():
            outfile.write("<li><a href=" + v + ">" + k + "</a></li>\n")
        outfile.write("</ul>\n")

        outfile.write("<h3>Map from GBIF</h3>\n")
        searchGBIF = GBIFSearch()
        (taxId, imgName) = searchGBIF.search(queryStr)
        if len(taxId) == 0:
            outfile.write("No species found\n")
        else:
            urlImg = "http://data.gbif.org/" + imgName
            taxUrl = "<a href='http://data.gbif.org/species/" + taxId + "'>"
            outfile.write(taxUrl + "<img src='" + urlImg + "' width=360 height=180 border=1/></a>\n")
        
        searchGoogle = GoogleSearch()
        outfile.write("<h3>Images from Google</h3>\n")
        imgs = searchGoogle.searchImages(queryStr, 5)
        if len(imgs) == 0:
            outfile.write("No images found\n")
        else:
            for img in imgs:
                refUrl = "<a href='" + img + "'>"
                outfile.write(refUrl + "<img src='" + img + "' width=94 height=145 border=1></a>\n")
    
        outfile.write("<h3>Articles from Google</h3>\n")
        pubs = searchGoogle.searchScholar(queryStr, 10)
        if len(pubs) == 0:
            outfile.write("No articles found\n")
        else:	
            for pub in pubs.keys():
                outfile.write("<hr noshade>\n")
                outfile.write("<b><a href='" + pub + "'>" + pubs[pub] + "</a></b><br>\n")

        outfile.write("</body>\n")
        outfile.write("</html>\n")

        outfile.close()
        return filename    
    
if __name__ == "__main__":
    app = QtGui.QApplication(sys.argv)
    main = Browser()
    sys.exit(app.exec_())