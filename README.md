especies
========

A taxonomically intelligent biodiversity search engine

This is a pure Python CGI-based implementation of a taxonomically intelligent species search engine. It searches biological databases for a taxonomic name. The search is done "on the fly" using web services (SOAP/XML) or URL API's.

e-Species aggregates information from multiple sources, as follows:

 - Gets nomenclatural information (higher taxon classification and
   synonyms, if any) for a given taxon name from the Catalogue of Life
 - Retrieves a short descriptive phrase for the taxon name from Wikipedia,
   to display on the e-Species page, handling redirections
 - Searches GenBank for protein and nucleotide sequences for the taxon
   name
 - Displays distribution maps from the Global Biodiversity Information 
   Facility for the taxon name
 - Gets up to five images for the taxon name from Google Images 
 - Extracts bibliographic references for the taxon from Google Scholar

Please send bug reports, suggestions, and comments to Mauro J. Cavalcanti
<maurobio@gmail.com>
