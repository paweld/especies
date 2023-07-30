# <img src="static\icon.png" alt="e-Species"/> e-Species

A taxonomically intelligent biodiversity search engine

This is a pure Free Pascal/Python implementation of a taxonomically intelligent species search engine. It searches biological databases for a taxonomic name. The search is done "on the fly" using web services (JSON/XML) or URL API's.

e-Species aggregates information from multiple sources, as follows:

 - Gets nomenclatural information (higher taxon classification and synonyms, if any) for a given taxon name from the GBIF Backbone Taxonomy
 - Retrieves a short descriptive phrase for the taxon name from Wikipedia, to display on the e-Species page, handling redirections
 - Searches GenBank for protein and nucleotide sequences for the taxon name
 - Displays distribution maps from the Global Biodiversity Information Facility for the taxon name
 - Gets up to five images for the taxon name from Wikimedia Commons 
 - Extracts bibliographic references for the taxon from PubMed

There  are three different implementations of e-Species, all of them partially using the same codebase:

- A Web-based version, using the CGI protocol, which runs on the Apache web server
- A desktop-based version, which runs on MS-Windows and GNU/Linux personal computers
- A mobile-based app, which runs on Android cellphones and tablets (***under development***)

