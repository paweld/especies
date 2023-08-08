{================================================================================}
{     e-Species - A taxonomically intelligent species search engine.             }
{                  (C) 2008-2023 by Mauro J. Cavalcanti                          }
{                         <maurobio@gmail.com>                                   }
{                                                                                }
{  This program is free software; you can redistribute it and/or modify          }
{  it under the terms of the GNU General Public License as published by          }
{  the Free Software Foundation; either version 3 of the License, or             }
{  (at your option) any later version.                                           }
{                                                                                }
{  This program is distributed in the hope that it will be useful,               }
{  but WITHOUT ANY WARRANTY; without even the implied warranty of                }
{  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the                  }
{  GNU General Public License for more details.                                  }
{                                                                                }
{  You should have received a copy of the GNU General Public License             }
{  along with this program. If not, see <http://www.gnu.org/licenses/>.          }
{                                                                                }
{  Requirements:                                                                 }
{    Free Pascal version 3.2 or higher                                           }
{                                                                                }
{  REVISION HISTORY:                                                             }
{    Version 1.00, 29th Jun 2023 - Initial public release of the Free Pascal     }
{                                version (ported from an earlier Python version, }
{                                first released in June, 2008).                  }
{================================================================================}

program Especies;

{$APPTYPE CONSOLE}
{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  StrUtils,
  DOS,
  fphttpclient,
  fpjson,
  jsonparser,
  openssl,
  opensslsockets,
  DOM,
  XMLRead,
  XPath;

type
  TGBIFSearch = class(TObject) { Search GBIF (http://www.gbif.org) }
  public
    GBIF_URL: string;
    constructor Create;
    procedure Search(const searchStr: string; var key: integer;
      var scientificname, authorship, status, valid_name, kingdom,
      phylum, classe, order, family: string);
    function Count(key: integer): integer;
  end;

  TNCBISearch = class(TObject)
    { Search NCBI's Entrez taxonomy database (http://www.ncbi.nlm.nih.gov/Entrez) }
  public
    NCBI_URL: string;
    results: TStringList;
    constructor Create;
    destructor Destroy; override;
    procedure Summary(const searchStr: string; var id: integer;
      var division, scientificname, commonname: string; var nucNum, protNum: integer);
    function Links(id: integer): TStringList;
  end;

  TWikiSearch = class(TObject) { Search Wikipedia (http://en.wikipedia.org) articles }
  public
    WIKIPEDIA_URL: string;
    WIKIMEDIA_URL: string;
    candidates: TStringList;
    constructor Create;
    destructor Destroy; override;
    function Snippet(const searchStr: string): string;
    function Images(const searchStr: string; limit: integer = 10): TStringList;
  end;

  TFFSearch = class(TObject) { Search FiveFilters }
  public
    FF_URL: string;
    Lines: TStringList;
    constructor Create;
    destructor Destroy; override;
    function termExtract(const contextStr: string; limit: integer = 10): TStringList;
  end;

  TPubMedSearch = class(TObject) { Search PubMed }
  public
    PUBMED_URL: string;
    references: TStringList;
    constructor Create;
    destructor Destroy; override;
    function Search(const searchStr: string; limit: integer = 10): TStringList;
  end;

var
  queryStr: string;
  scientificname, authorship, status, valid_name, kingdom, phylum,
  classe, order, family, taxon_list: string;
  division, commonname, snippet: string;
  urlWiki, tag, tagHTML, refUrl, taxUrl, urlId, UrlNuc, urlProt, itemStr: string;
  key, taxId, nucNum, protNum: integer;
  i, words, nrecs: integer;
  linkOut, linkIn, imgs, tags, pubs: TStringList;
  GBIFSearch: TGBIFSearch;
  NCBISearch: TNCBISearch;
  WikiSearch: TWikiSearch;
  FFSearch: TFFSearch;
  PubMedSearch: TPubMedSearch;

  { TGBIFSearch methods }

  constructor TGBIFSearch.Create;
  begin
    GBIF_URL := 'http://api.gbif.org/v1';
  end;

  procedure TGBIFSearch.Search(const searchStr: string; var key: integer;
  var scientificname, authorship, status, valid_name, kingdom, phylum,
    classe, order, family: string);
  var
    JsonData: TJsonData;
  begin
    try
      try
        JsonData := GetJson(TFPHTTPClient.SimpleGet(GBIF_URL +
          '/species/?name=' + StringReplace(searchStr, ' ', '%20', [rfReplaceAll])));
        key := JsonData.FindPath('results[0].key').AsInteger;
        scientificname := JsonData.FindPath('results[0].canonicalName').AsString;
        authorship := JsonData.FindPath('results[0].authorship').AsString;
        status := JsonData.FindPath('results[0].taxonomicStatus').AsString;
        status := LowerCase(StringReplace(status, '_', ' ', [rfReplaceAll]));
        if status <> 'accepted' then
          valid_name := JsonData.FindPath('results[0].species').AsString;
        kingdom := JsonData.FindPath('results[0].kingdom').AsString;
        phylum := JsonData.FindPath('results[0].phylum').AsString;
        classe := JsonData.FindPath('results[0].class').AsString;
        order := JsonData.FindPath('results[0].order').AsString;
        family := JsonData.Findpath('results[0].family').AsString;
      except
        {on E: Exception do
          WriteLn('<h3><font color="red">Error fetching classification data from CoL: ',
            E.ClassName, #13#10, E.Message, '</font></h3>');}
        key := 0;
        scientificname := '';
        authorship := '';
        status := '';
        valid_name := '';
        kingdom := '';
        phylum := '';
        classe := '';
        order := '';
        family := '';
      end;
    finally
      JsonData.Free;
    end;
  end;

  function TGBIFSearch.Count(key: integer): integer;
  var
    JsonData: TJsonData;
    nrecs: integer;
  begin
    try
      try
        JsonData := GetJson(TFPHTTPClient.SimpleGet(GBIF_URL +
          '/occurrence/search?taxonKey=' + IntToStr(key)));
        nrecs := JsonData.FindPath('count').AsInteger;
        Result := nrecs;
      except
        {on E: Exception do
          WriteLn('<h3><font color="red">Error fetching number of records from GBIF: ',
            E.ClassName, #13#10, E.Message, '</font></h3>');}
		Result := 0;	
      end;
    finally
      JsonData.Free;
    end;
  end;

  { TNCBISearch methods }

  constructor TNCBISearch.Create;
  begin
    NCBI_URL := 'https://eutils.ncbi.nlm.nih.gov/entrez/eutils/';
    results := TStringList.Create;
    results.NameValueSeparator := '|';
  end;

  destructor TNCBISearch.Destroy;
  begin
    if FileExists('temp.xml') then
      DeleteFile('temp.xml');
    results.Free;
    inherited Destroy;
  end;

  procedure TNCBISearch.Summary(const searchStr: string; var id: integer;
  var division, scientificname, commonname: string; var nucNum, protNum: integer);
  var
    XmlData: ansistring;
    Doc: TXMLDocument;
    outfile: TextFile;
    Result: TXPathVariable;
  begin
    try
      try
        XmlData := TFPHTTPClient.SimpleGet(NCBI_URL + 'esearch.fcgi?db=taxonomy&term=' +
          StringReplace(searchStr, ' ', '+', [rfReplaceAll]));
        AssignFile(outfile, 'temp.xml');
        Rewrite(outfile);
        WriteLn(outfile, XmlData);
        CloseFile(outfile);
        ReadXMLFile(Doc, 'temp.xml');

        { Get taxon id }
        Result := EvaluateXPathExpression('/eSearchResult/IdList/Id',
          Doc.DocumentElement);
        id := StrToInt(string(Result.AsText));

        XmlData := TFPHTTPClient.SimpleGet(NCBI_URL + 'esummary.fcgi?db=taxonomy&id=' +
          IntToStr(Id) + '&retmode=xml');
        AssignFile(outfile, 'temp.xml');
        Rewrite(outfile);
        WriteLn(outfile, XmlData);
        CloseFile(outfile);
        ReadXMLFile(Doc, 'temp.xml');

        { Get summary data }
        Result := EvaluateXPathExpression(
          '/eSummaryResult/DocSum/Item[@Name="Division"]', Doc.DocumentElement);
        division := string(Result.AsText);

        Result := EvaluateXPathExpression(
          '/eSummaryResult/DocSum/Item[@Name="ScientificName"]', Doc.DocumentElement);
        scientificname := string(Result.AsText);

        Result := EvaluateXPathExpression(
          '/eSummaryResult/DocSum/Item[@Name="CommonName"]', Doc.DocumentElement);
        commonname := string(Result.AsText);

        { Get nucleotide sequences }
        XmlData := TFPHTTPClient.SimpleGet(NCBI_URL +
          'esearch.fcgi?db=nucleotide&term=' +
          StringReplace(searchStr, ' ', '+', [rfReplaceAll]));
        AssignFile(outfile, 'temp.xml');
        Rewrite(outfile);
        WriteLn(outfile, XmlData);
        CloseFile(outfile);
        ReadXMLFile(Doc, 'temp.xml');
        nucNum := StrToInt(string(EvaluateXPathExpression('/eSearchResult/Count',
          Doc.DocumentElement).AsText));

        { Get protein sequences }
        XmlData := TFPHTTPClient.SimpleGet(NCBI_URL + 'esearch.fcgi?db=protein&term=' +
          StringReplace(searchStr, ' ', '+', [rfReplaceAll]));
        AssignFile(outfile, 'temp.xml');
        Rewrite(outfile);
        WriteLn(outfile, XmlData);
        CloseFile(outfile);
        ReadXMLFile(Doc, 'temp.xml');
        protNum := StrToInt(string(EvaluateXPathExpression('/eSearchResult/Count',
          Doc.DocumentElement).AsText));
      except
        {on E: Exception do
          WriteLn('<h3><font color="red">Error fetching biomolecular data from NCBI: ',
            E.ClassName, #13#10, E.Message, '</font></h3>');}
        id := 0;
        division := '';
        scientificName := '';
        nucNum := 0;
        protNum := 0;
      end;
    finally
      Result.Free;
      Doc.Free;
    end;
  end;

  function TNCBISearch.Links(id: integer): TStringList;
  var
    XmlData: ansistring;
    Doc: TXMLDocument;
    outfile: TextFile;
    Result1, Result2: TXPathVariable;
    NodeSet1, NodeSet2: TNodeSet;
    i: integer;
  begin
    { Get list of links }
    try
      try
        XmlData := TFPHTTPClient.SimpleGet(NCBI_URL + 'elink.fcgi?dbfrom=taxonomy&id=' +
          IntToStr(id) + '&cmd=llinkslib');
        AssignFile(outfile, 'temp.xml');
        Rewrite(outfile);
        WriteLn(outfile, XmlData);
        CloseFile(outfile);
        ReadXMLFile(Doc, 'temp.xml');
        Result1 := EvaluateXPathExpression('//ObjUrl/Url', Doc.DocumentElement);
        Result2 := EvaluateXPathExpression('//ObjUrl/Provider/Name',
          Doc.DocumentElement);
        NodeSet1 := Result1.AsNodeSet;
        NodeSet2 := Result2.AsNodeSet;
        if NodeSet1.Count > 0 then
          for i := 0 to NodeSet1.Count - 1 do
            results.Add(string(TDomElement(NodeSet1.Items[i]).TextContent) +
              '|' + string(TDomElement(NodeSet2.Items[i]).TextContent));
        Result := results;
      except
        {on E: Exception do
          WriteLn('<h3><font color="red">Error fetching links from NCBI: ',
            E.ClassName, #13#10, E.Message, '</font></h3>');}
        Result := nil;
      end;
    finally
      Result1.Free;
      Result2.Free;
    end;
  end;

  { TWikiSearch methods }

  constructor TWikiSearch.Create;
  begin
    WIKIPEDIA_URL := 'https://en.wikipedia.org/api/rest_v1/page/summary/';
    WIKIMEDIA_URL := 'https://en.wikipedia.org/api/rest_v1/page/media-list/';
	{WIKIPEDIA_REDIRECT_URL := 'https://en.wikipedia.org/w/api.php?action=query&titles=';}
    candidates := TStringList.Create;
  end;

  destructor TWikiSearch.Destroy;
  begin
    candidates.Free;
    inherited Destroy;
  end;

  function TWikiSearch.Snippet(const searchStr: string): string;
  var
    JsonData: TJsonData;
    Client: TFPHttpClient;
  begin
    Client := TFPHttpClient.Create(nil);
    try
      try
        { Allow redirections }
        Client.AllowRedirect := True;
        JsonData := GetJson(Client.Get(WIKIPEDIA_URL +
          StringReplace(searchStr, ' ', '_', [rfReplaceAll])));
        Result := JsonData.FindPath('extract').AsString;
      except
        {on E: Exception do
          WriteLn('<h3><font color="red">Error fetching text snippet from Wikipedia: ',
            E.ClassName, #13#10, E.Message, '</font></h3>');}
        Result := '';
      end;
    finally
      JsonData.Free;
      Client.Free;
    end;
  end;
  
(* function TWikiSearch.Snippet(const searchStr: string): string;
var
  JsonData: TJsonData;
begin
  try
    try
      { Allow redirections }
      JsonData := GetJSON(TFPHTTPClient.SimpleGet(WIKIPEDIA_REDIRECT_URL +
        StringReplace(queryStr, ' ', '+', [rfReplaceAll]) + '&redirects&format=json'));
      if JsonData.FindPath('query.redirects[0].to') <> nil then
        queryStr := JsonData.FindPath('query.redirects[0].to').AsString;
      JsonData := GetJson(TFPHTTPClient.SimpleGet(WIKIPEDIA_URL +
        StringReplace(searchStr, ' ', '_', [rfReplaceAll])));
      Result := JsonData.FindPath('extract').AsString;
    except
      Result := '';
    end;
  finally
    JsonData.Free;
  end;
end; *)

  { Search images from Wikimedia Commons }
  function TWikiSearch.Images(const searchStr: string; limit: integer = 10): TStringList;
  var
    JsonData, JsonItem, JsonItems: TJsonData;
    Client: TFPHttpClient;
    i, Count: integer;
    ext: string;
  begin
    try
      try
        Client := TFPHttpClient.Create(nil);
        Client.AllowRedirect := True;
        JsonData := GetJson(Client.Get(WIKIMEDIA_URL +
          StringReplace(searchStr, ' ', '_', [rfReplaceAll])));
        JsonItems := JsonData.FindPath('items');
        Count := 0;
        for i := 0 to JsonItems.Count - 1 do
        begin
          JsonItem := JsonItems.Items[i];
          ext := ExtractFileExt(JsonItem.FindPath('title').AsString);
          if (ext = '.jpg') then
          begin
            candidates.Add(JsonItem.FindPath('title').AsString);
            Inc(Count);
            if Count >= limit then
              break;
          end;
        end;
        Result := candidates;
      except
        {on E: Exception do
          WriteLn('<h3><font color="red">Error fetching images from Wikimedia: ',
            E.ClassName, #13#10, E.Message, '</font></h3>');}
        candidates := nil;
      end;
    finally
      JsonData.Free;
      Client.Free;
    end;
  end;
  
(*function TWikiSearch.Images(const searchStr: string; limit: integer = 10): TStringList;
var
  JsonData, JsonItem, JsonItems: TJsonData;
  i, Count: integer;
  ext: string;
begin
  try
    try
      JsonData := GetJSON(TFPHTTPClient.SimpleGet(WIKIPEDIA_REDIRECT_URL +
        StringReplace(queryStr, ' ', '+', [rfReplaceAll]) + '&redirects&format=json'));
      if JsonData.FindPath('query.redirects[0].to') <> nil then
        queryStr := JsonData.FindPath('query.redirects[0].to').AsString;
      JsonData := GetJson(TFHTTPClient.SimpleGet(WIKIMEDIA_URL +
        StringReplace(searchStr, ' ', '_', [rfReplaceAll])));
      JsonItems := JsonData.FindPath('items');
      Count := 0;
      for i := 0 to JsonItems.Count - 1 do
      begin
        JsonItem := JsonItems.Items[i];
        ext := ExtractFileExt(JsonItem.FindPath('title').AsString);
        if (ext = '.jpg') then
        begin
          candidates.Add(JsonItem.FindPath('title').AsString);
          Inc(Count);
          if Count >= limit then
            break;
        end;
      end;
      Result := candidates;
    except
      candidates := nil;
    end;
  finally
    JsonData.Free;
  end;
end;*)  

  { TFFSearch methods }

  constructor TFFSearch.Create;
  begin
    FF_URL := 'http://termextract.fivefilters.org/';
    Lines := TStringList.Create;
  end;

  destructor TFFSearch.Destroy;
  begin
    if FileExists('temp.txt') then
      DeleteFile('temp.txt');
    Lines.Free;
    inherited Destroy;
  end;

  { Provides a list of significant words or phrases extracted from a larger content from FiveFilters Web service }

  function TFFSearch.termExtract(const contextStr: string;
    limit: integer = 10): TStringList;
  var
    XmlData: ansistring;
    outfile: TextFile;
  begin
    try
      try
        XmlData := TFPHTTPClient.SimpleGet(FF_URL + 'extract.php?text=' +
          StringReplace(contextStr, ' ', '+', [rfReplaceAll]) +
          '&output=txt&max=' + IntToStr(limit));
        AssignFile(outfile, 'temp.txt');
        Rewrite(outfile);
        WriteLn(outfile, XmlData);
        CloseFile(outfile);
        Lines.LoadFromFile('temp.txt');
        Result := Lines;
      except
        {on E: Exception do
          WriteLn('<h3><font color="red">Error fetching keywords from FiveFilters: ',
            E.ClassName, #13#10, E.Message, '</font></h3>');}
        Result := nil;
      end;
    finally
    end;
  end;

  { TPubMedSearch methods }

  constructor TPubMedSearch.Create;
  begin
    PUBMED_URL := 'https://eutils.ncbi.nlm.nih.gov/entrez/eutils/';
    references := TStringList.Create;
  end;

  destructor TPubMedSearch.Destroy;
  begin
    if FileExists('temp.xml') then
      DeleteFile('temp.xml');
    references.Free;
    inherited Destroy;
  end;

  function TPubMedSearch.Search(const searchStr: string;
    limit: integer = 10): TStringList;
  var
    XmlData: ansistring;
    Doc: TXMLDocument;
    outfile: TextFile;
    Result1, Result2: TXPathVariable;
    NodeSet1, NodeSet2, Ids: TNodeSet;
    id: string;
	i: integer;
  begin
    try
      try
        XmlData := TFPHTTPClient.SimpleGet(PUBMED_URL +
          'esearch.fcgi?db=pubmed&retmax=' + IntToStr(limit) +
          '&sort=relevance&term=' + StringReplace(searchStr, ' ', '+', [rfReplaceAll]));
        AssignFile(outfile, 'temp.xml');
        Rewrite(outfile);
        WriteLn(outfile, XmlData);
        CloseFile(outfile);
        ReadXMLFile(Doc, 'temp.xml');

        { Get reference ids }
        Result1 := EvaluateXPathExpression('/eSearchResult/IdList/Id',
          Doc.DocumentElement);
        Ids := Result1.AsNodeSet;
        id := '';
        if Ids.Count > 0 then
          for i := 0 to Ids.Count - 1 do
            id := id + string(TDomElement(Ids.Items[i]).TextContent) + ',';

        XmlData := TFPHTTPClient.SimpleGet(PUBMED_URL + 'efetch.fcgi?db=pubmed&id=' +
          id + '&retmode=xml');
        AssignFile(outfile, 'temp.xml');
        Rewrite(outfile);
        WriteLn(outfile, XmlData);
        CloseFile(outfile);
        ReadXMLFile(Doc, 'temp.xml');

        { Get list of references }
        Result1 := EvaluateXPathExpression('//Article/ArticleTitle',
          Doc.DocumentElement);
        Result2 := EvaluateXPathExpression(
          '//PubmedData/ArticleIdList/ArticleId[@IdType="doi"]', Doc.DocumentElement);
        NodeSet1 := Result1.AsNodeSet;
        NodeSet2 := Result2.AsNodeSet;
        if NodeSet1.Count > 0 then
        begin
          for i := 0 to NodeSet1.Count - 1 do
            try
              references.Add(string(TDomElement(NodeSet1.Items[i]).TextContent) +
                '=' + string(TDomElement(NodeSet2.Items[i]).TextContent));
            except
              continue;
            end;
        end;
        Result := references;
      except
        {on E: Exception do
          WriteLn('<h3><font color="red">Error fetching references from PubMed: ',
            E.ClassName, #13#10, E.Message, '</font></h3>');}
        Result := nil;
      end;
    finally
      Result1.Free;
      Result2.Free;
      Doc.Free;
    end;
  end;

begin
  InitSSLInterface;
  WriteLn('Content-Type: text/html', #10#13);
  WriteLn('<html>');
  WriteLn('<title>e-Species</title>');
  WriteLn('<body>');

  queryStr := '';
  queryStr := GetEnv('QUERY_STRING');
  queryStr := StringReplace(ExtractDelimited(2, queryStr, ['=', '&']),
    '+', ' ', [rfReplaceAll]);

  words := WordCount(queryStr, [' ']);
  if words <> 2 then
  begin
    WriteLn('<title>e-Species</title>');
    WriteLn('<h3>Error filling out form</h3>');
    WriteLn('<p>Please enter a binomial specific epithet into the text box.</a>');
    WriteLn('<p>Remember: Only <a href="http://en.wikipedia.org/wiki/Species">species</a> are true natural entities!</p>');
    WriteLn('<a href="../especies/index.htm">Go back to the query form</a>');
  end
  else
  begin
    WriteLn('<html>');
    WriteLn('<head>');
    WriteLn('<title>e-Species search results for ' + queryStr + '</title>');
    WriteLn('<link rel="stylesheet" type="text/css" href="../especies/stylesheet.css">');
    WriteLn('</head>');
    WriteLn('<body bgcolor="#ffffff">');
    WriteLn('<h1><img src="../especies/especies.png" height="73" width="385"></h1>');
    WriteLn('<h3>A taxonomically intelligent biodiversity search engine</h3>');
    WriteLn('<p>Search biological databases for a taxonomic name. The search is done "on the fly" using web services (JSON/XML) or URL API''s. <a href="../especies/about.htm">Learn more about how it works.</a></p>');
    WriteLn('<form>');
    WriteLn('<input type="button" value="Back" onclick="history.back()">');
    WriteLn('</form>');
  end;

  GBIFSearch := TGBIFSearch.Create;
  GBIFSearch.Search(queryStr, key, scientificname, authorship, status, valid_name,
    kingdom, phylum, classe, order, family);
  if (Length(status) > 0) then
  begin
    if status <> 'accepted' then
      status := ' (' + status + ' of <i>' + valid_name + '</i>' + ' ' + authorship + ')'
    else
      status := ' (' + status + ')';
  end;
  taxon_list := kingdom + '; ' + phylum + '; ' + classe + '; ' + order + '; ' + family;
  WriteLn('<h2><i>' + queryStr + '</i>' + ' ' + authorship + status + '</h2>');
  WriteLn('<h3>Classification from CoL</h3>');
  if Length(scientificname) = 0 then
    WriteLn('No names found')
  else
    WriteLn(taxon_list);
  nrecs := GBIFSearch.Count(key);
  GBIFSearch.Destroy;

  WikiSearch := TWikiSearch.Create;
  snippet := WikiSearch.Snippet(queryStr);
  FFSearch := TFFSearch.Create;
  tags := FFSearch.termExtract(snippet, 10);
  tagHTML := '';
  WriteLn('<h3>Text tags</h3>');
  for i := 0 to tags.Count - 1 do
  begin
    tag := tags[i];
    tag := StringReplace(tag, ' ', '&nbsp;', [rfReplaceAll]);
    tagHTML := tagHTML +
      '<span style=''display:inline;border:1px solid blue; padding:1px;margin:2px;line-height:22px;background-color:rgb(181,213,255);''>'
      + tag + ' ' + '</span>';
  end;
  WriteLn(tagHTML);
  FFSearch.Destroy;

  urlWiki := 'http://en.wikipedia.org/wiki/' + StringReplace(queryStr,
    ' ', '_', [rfReplaceAll]);
  WriteLn('<h3>Wikipedia</h3>');
  if Length(snippet) = 0 then
    WriteLn('No article title matches')
  else
  begin
    WriteLn(snippet);
    WriteLn('<p><a href="' + urlWiki + '">Original article</a></p>');
  end;

  WriteLn('<h3>Genomics from NCBI</h3>');
  NCBISearch := TNCBISearch.Create;
  NCBISearch.Summary(queryStr, taxId, division, scientificname,
    commonname, nucNum, protNum);
  urlId := 'http://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?mode=Info&id=' +
    IntToStr(taxId);
  urlNuc :=
    'http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=Nucleotide&cmd=Search&dopt=DocSum&term=txid'
    + IntToStr(taxId) + '[Organism:exp]';
  urlProt :=
    'http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=Protein&cmd=Search&dopt=DocSum&term=txid'
    + IntToStr(taxId) + '[Organism:exp]';
  Write('TaxId: <a href="' + urlId + '">' + IntToStr(taxId) + '</a>&nbsp;');
  if Length(scientificName) = 0 then
    Write('No items found for ' + '<i>' + queryStr + '</i> ')
  else
  begin
    Write('<i>' + scientificName + '</i>');
    Write('[' + division + '] ');
    WriteLn('Sequences: ' + '<a href="' + urlNuc + '">' + IntToStr(nucNum) +
      '</a> nucleotide, ' + '<a href="' + urlProt + '">' + IntToStr(protNum) +
      '</a> protein');
  end;
  linkOut := NCBISearch.Links(taxId);
  linkIn := TStringList.Create;
  WriteLn('<ul type="circle">');
  for i := 0 to linkOut.Count - 1 do
  begin
    itemStr := linkOut.ValueFromIndex[i];
    if linkIn.IndexOf(itemStr) < 0 then
    begin
      WriteLn('<li><a href="' + linkOut.Names[i] + '">' +
        linkOut.ValueFromIndex[i] + '</a></li>');
      linkIn.Append(itemStr);
    end;
  end;
  linkIn.Free;
  WriteLn('</ul>');
  NCBISearch.Destroy;

  WriteLn('<h3>Map from GBIF</h3>');
  if key = 0 then
    WriteLn('No species found')
  else
  begin
    taxUrl := '<a href=http://gbif.org/species/' + IntToStr(key) + '>';
    WriteLn('<p>' + taxUrl + IntToStr(nrecs) + ' record(s)</a></p>');
    WriteLn(taxUrl +
      '<iframe id="mapByFrame" name="map" src="http://cdn.gbif.org/v1/map/index.html?type=TAXON&key='
      + IntToStr(key) +
      '&resolution=2" height="56%" width="56%" frameborder="1"/></iframe></a>');
  end;

  WriteLn('<h3>Images from Wikimedia Commons</h3>');
  imgs := WikiSearch.Images(queryStr, 5);
  if imgs.Count = 0 then
    WriteLn('No images found')
  else
  begin
    for i := 0 to imgs.Count - 1 do
    begin
      refUrl := '<a href="http://en.wikipedia.org/wiki/' + imgs[i] + '">';
      WriteLn(refUrl + '<img src="http://commons.wikimedia.org/wiki/Special:Filepath/'
        + ExtractDelimited(2, imgs[i], [':']) + '" width=94 height=145 border=1></a>');
    end;
  end;
  WikiSearch.Destroy;

  WriteLn('<h3>Articles from PubMed</h3>');
  PubMedSearch := TPubMedSearch.Create;
  pubs := PubMedSearch.Search(queryStr);
  if (pubs = nil) or (pubs.Count = 0) then
    WriteLn('No articles found')
  else
  begin
    for i := 0 to pubs.Count - 1 do
    begin
      WriteLn('<hr noshade>');
      WriteLn('<b><a href="http://dx.doi.org/doi:' + pubs.ValueFromIndex[i] +
        '">' + pubs.Names[i] + '</a></b><br>');
    end;
  end;
  PubMedSearch.Destroy;

  WriteLn('<p align="left"><small><small>&copy; 2008-2023 </small><a href="http://github.com/maurobio/"><small>Mauro J. Cavalcanti</small></a></small></p>');
  WriteLn('</body>');
  WriteLn('</html>');
end.
