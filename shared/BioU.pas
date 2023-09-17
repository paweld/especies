{===============================================================================}
{                             B i o U   Library                                 }
{                                                                               }
{       A General-Purpose Library of Routines for Fetching Data From Several    }
{                       Online Biodiversity Databases                           }
{                                                                               }
{                            Version 3.0, September 2023                        }
{                                                                               }
{             Authors: * Mauro J. Cavalcanti, Rio de Janeiro, BRASIL            }
{                          E-mail: <maurobio@gmail.com>                         }
{                      * Paweł Dmitruk (paweld), https://github.com/paweld      }
{                                                                               }
{  This program is free software; you can redistribute it and/or modify         }
{  it under the terms of the GNU General Public License as published by         }
{  the Free Software Foundation; either version 3 of the License, or            }
{  (at your option) any later version.                                          }
{                                                                               }
{  This program is distributed in the hope that it will be useful,              }
{  but WITHOUT ANY WARRANTY; without even the implied warranty of               }
{  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the                 }
{  GNU General Public License for more details.                                 }
{                                                                               }
{  You should have received a copy of the GNU General Public License            }
{  along with this program. If not, see <http://www.gnu.org/licenses/>.         }
{===============================================================================}
unit BioU;

{$mode objfpc}{$H+}
{$warn 6058 off}{$warn 3124 off}{$warn 3123 off}

interface

uses
  Classes, SysUtils, StrUtils, fgl,
  LPDNetU,
  fpjson, jsonparser, fpjsonrtti,
  DOM, XMLRead, XPath,
  base64,
  BGRABitmap, BGRABitmapTypes, BGRAWriteJpeg;

type

  { TTimeList }

  TTimeList = class(specialize TFPGList<Int64>)
  public
    constructor Create;
    function Sum(aTopX: Integer): Int64;
    procedure LogTime;
  end;

  //download thread type
  TType = (ttGBIF, ttNCBI, ttWiki);
  //TBio status
  TStatus = (tsNOK, tsOK, tsEmpty);

  { TKVItem }

  TKVItem = class(TCollectionItem)
  private
    FKey: String;
    FValue: String;
  published
    property Key: String read FKey write FKey;
    property Value: String read FValue write FValue;
  end;

  { TKVList }

  TKVList = class(TCollection)
  private
    function GetItems(Index: Integer): TKVItem;
    procedure SetItems(Index: Integer; AValue: TKVItem);
  public
    constructor Create;
    function Add: TKVItem;
    procedure Add(aKey, aValue: String);
    property Items[Index: Integer]: TKVItem read GetItems write SetItems;
    procedure CopyFrom(aSource: TKVList; withClear: Boolean = True);
  end;

  TBio = class;

  { TEndThread }

  TEndThread = class(TThread)
  private
    FAwait: Boolean;
    FParentBio: TBio;
  protected
    procedure Execute; override;
  public
    constructor Create(aParentBio: TBio; aAwait: Boolean);
  end;

  { TDownImgThread }

  TDownImgThread = class(TThread)
  private
    FParentList: TKVList;
    FLimit: Integer;
    FRunning: Boolean;
    function GetImage: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(aParentList: TKVList; aImgLimit: Integer);
    property Running: Boolean read FRunning;
  end;

  { TDownThread }

  TDownThread = class(TThread)
  private
    FAwait: Boolean;
    FBio: TBio;
    FName: String;
    FParentBio: TBio;
    FType: TType;
    Fhc: THTTPClient;
    procedure GetGBIF;
    procedure GetNCBI;
    procedure GetWiki;
  protected
    procedure Execute; override;
  public
    constructor Create(aParentBio: TBio; aType: TType; aAwait: Boolean);
    destructor Destroy; override;
  end;

  { TBio }

  TBio = class
  private
    FAfterGet: TNotifyEvent;
    FDownThreadCount: Integer;
    FDTarr: Array [0..2] of TDownThread;
    FRunning: Boolean;
    FFFtags: TStringList;
    FGBIFauthorship: String;
    FGBIFclasse: String;
    FGBIFcount: Integer;
    FGBIFfamily: String;
    FGBIFkey: Integer;
    FGBIFkingdom: String;
    FGBIFmapImage: String;
    FGBIForder: String;
    FGBIFphylum: String;
    FGBIFscientificname: String;
    FGBIFstatus: String;
    FGBIFvalid_name: String;
    FErrorMessage: String;
    FName: String;
    FNCBIcommonname: String;
    FNCBIdivision: String;
    FNCBIid: Integer;
    FNCBIlinks: TKVList;
    FNCBImap: String;
    FNCBInucNum: Integer;
    FNCBIprotNum: Integer;
    FNCBIreferences: TKVList;
    FNCBIscientificname: String;
    FStatus: TStatus;
    FWIKIimages: TKVList;
    FWIKIsnippet: String;
    procedure EndGet(aAwait: Boolean);
    procedure Run(aName: String; aAwait: Boolean);
    function GetRunning: Boolean;
    procedure SetRunning(aValue: Boolean);
    function GetGBIFSummary: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function GetAndWait(aName: String): Boolean;
    procedure Get(aName: String);
    procedure Stop;
    function LoadFromJson(json: String): Boolean;
    function SaveToJson: String;
    property ErrorMessage: String read FErrorMessage;
    property Status: TStatus read FStatus;
    property AfterGet: TNotifyEvent read FAfterGet write FAfterGet;
    property Running: Boolean read GetRunning;
  published
    property Name: String read FName;
    //gbif
    property GBIFkey: Integer read FGBIFkey write FGBIFkey;
    property GBIFscientificname: String read FGBIFscientificname write FGBIFscientificname;
    property GBIFauthorship: String read FGBIFauthorship write FGBIFauthorship;
    property GBIFstatus: String read FGBIFstatus write FGBIFstatus;
    property GBIFvalid_name: String read FGBIFvalid_name write FGBIFvalid_name;
    property GBIFkingdom: String read FGBIFkingdom write FGBIFkingdom;
    property GBIFphylum: String read FGBIFphylum write FGBIFphylum;
    property GBIFclasse: String read FGBIFclasse write FGBIFclasse;
    property GBIForder: String read FGBIForder write FGBIForder;
    property GBIFfamily: String read FGBIFfamily write FGBIFfamily;
    property GBIFcount: Integer read FGBIFcount write FGBIFcount;
    property GBIFmapImage: String read FGBIFmapImage write FGBIFmapImage;
    //ncbi
    property NCBIid: Integer read FNCBIid write FNCBIid;
    property NCBIdivision: String read FNCBIdivision write FNCBIdivision;
    property NCBIscientificname: String read FNCBIscientificname write FNCBIscientificname;
    property NCBIcommonname: String read FNCBIcommonname write FNCBIcommonname;
    property NCBInucNum: Integer read FNCBInucNum write FNCBInucNum;
    property NCBIprotNum: Integer read FNCBIprotNum write FNCBIprotNum;
    property NCBIlinks: TKVList read FNCBIlinks write FNCBIlinks;
    property NCBImap: String read FNCBImap write FNCBImap;  //map (image) in base64
    //ncbi pubs
    property NCBIreferences: TKVList read FNCBIreferences write FNCBIreferences;
    //wiki
    property WIKIsnippet: String read FWIKIsnippet write FWIKIsnippet;
    property WIKIimages: TKVList read FWIKIimages write FWIKIimages;
    //ff
    property FFtags: TStringList read FFFtags write FFFtags;
  end;

  function sReplace(aText, aFrom: String; aTo: String = ''; aCaseSensitive: Boolean = False): String;
  function GetUrlToFile(Url: String; AsName: String): Boolean;
  function GetImgUrlToBase64Jpeg(Url: String; InlineImg: Boolean; newWidth: Integer = 0; newHeight: Integer = 0; Proportional: Boolean = False): String;
  function GetAndMergeImages(baseUrl, pointsUrl: String; resultFile: String; newWidth: Integer = 0; newHeight: Integer = 0; Proportional: Boolean = False): String;
  function OnlineStatus: Byte;

var
  bioCS: TRTLCriticalSection;
  imgCS: TRTLCriticalSection;

implementation

function sReplace(aText, aFrom: String; aTo: String; aCaseSensitive: Boolean): String;
var
  replaceFlags: TReplaceFlags = [rfReplaceAll, rfIgnoreCase];
begin
  Result := aText;
  if pos(aFrom, aText) > 0 then
  begin
    if aCaseSensitive then
      replaceFlags := replaceFlags - [rfIgnoreCase];
    Result := StringReplace(Result, aFrom, aTo, replaceFlags);
  end;
end;

function GetUrlToFile(Url: String; AsName: String): Boolean;
var
  ms: TMemoryStream;
  Client: THTTPClient;
begin
  Result := False;
  if (ExtractFilePath(AsName) <> '') and not DirectoryExists(ExtractFilePath(AsName)) and not ForceDirectories(ExtractFilePath(AsName)) then
    Exit;     
  Client := THTTPClient.Create;
  ms := TMemoryStream.Create;
  if Client.Get(Url, ms) and (ms.Size > 0) then
  begin
    ms.SaveToFile(AsName);
    Result := True;
  end;
  ms.Free;
  Client.Free;
end;

function GetImgUrlToBase64Jpeg(Url: String; InlineImg: Boolean; newWidth: Integer; newHeight: Integer; Proportional: Boolean): String;
var
  Client: THTTPClient;
  ss: TStringStream;
  w, h: Integer;
  scale: Double;
  bmp: TBGRABitmap;
  jpgw: TBGRAWriterJpeg;
begin
  Result := '';
  Client := THTTPClient.Create;
  ss := TStringStream.Create;
  if Client.Get(Url, ss) and (ss.Size > 0) then
  begin
    ss.Position := 0;
    bmp := TBGRABitmap.Create(ss);
    if (newWidth > 0) and (newHeight > 0) then
    begin
      if Proportional then
      begin
        scale := newWidth / bmp.Width;
        if scale > (newHeight / bmp.Height) then
          scale := newHeight / bmp.Height;
        w := trunc(bmp.Width * scale);
        h := trunc(bmp.Height * scale);
      end
      else
      begin
        w := newWidth;
        h := newHeight;
      end;                                             
      BGRAReplace(bmp, bmp.Resample(w, h, rmFineResample));
    end;
    ss.Clear;
    jpgw := TBGRAWriterJpeg.Create;
    jpgw.CompressionQuality := 85;
    bmp.SaveToStream(ss, jpgw);
    jpgw.Free;
    bmp.Free;
    Result := EncodeStringBase64(ss.DataString);
  end;
  ss.Free;
  if InlineImg and (Result <> '') then
    Result := 'data:image/jpeg;base64,' + Result;
  Client.Free;
end;

function GetAndMergeImages(baseUrl, pointsUrl: String; resultFile: String; newWidth: Integer; newHeight: Integer; Proportional: Boolean): String;
var
  baseImg, ovrImg: TBGRABitmap;
  bms, pms: TMemoryStream;
  ss: TStringStream;
  x, y: Integer;
  px1, px2, px: TBGRAPixel;
  jpgw: TBGRAWriterJpeg;
  w, h: Integer;
  scale: Double;
  Client: THttpClient;
  b: Boolean;
begin
  Result := '';
  bms := TMemoryStream.Create;
  pms := TMemoryStream.Create;
  Client := THTTPClient.Create;
  if Client.Get(baseUrl, bms) and (bms.Size > 0) then
  begin
    bms.Position := 0;
    baseImg := TBGRABitmap.Create(bms);
  end;
  Client.Clear;
  if Client.Get(pointsUrl, pms) and (pms.Size > 0) then
  begin
    pms.Position := 0;
    ovrImg := TBGRABitmap.Create(pms);
  end;
  Client.Free;
  b := (bms.Size = 0) or (pms.Size = 0);
  bms.Free;
  pms.Free;
  if b then
    exit;
  if (baseImg.Width <> ovrImg.Width) or (baseImg.Height <> ovrImg.Height) then
    exit;
  for y := 0 to baseImg.Height - 1 do
  begin
    for x := 0 to baseImg.Width - 1 do
    begin
      px1 := baseImg.GetPixel(x, y);
      px2 := ovrImg.GetPixel(x, y);
      if px2.alpha = 0 then
        px := px1
      else
        px := px2;
      baseImg.SetPixel(x, y, px);
    end;
  end;
  if (newWidth > 0) and (newHeight > 0) then
  begin
    if Proportional then
    begin
      scale := newWidth / baseImg.Width;
      if scale > (newHeight / baseImg.Height) then
        scale := newHeight / baseImg.Height;
      w := trunc(baseImg.Width * scale);
      h := trunc(baseImg.Height * scale);
    end
    else
    begin
      w := newWidth;
      h := newHeight;
    end;
    BGRAReplace(baseImg, baseImg.Resample(w, h, rmFineResample));
  end;
  if resultFile <> '' then
  begin
    baseImg.SaveToFile(resultFile);
    Result := 'OK';
  end
  else
  begin
    ss := TStringStream.Create;
    jpgw := TBGRAWriterJpeg.Create;
    jpgw.CompressionQuality := 85;
    baseImg.SaveToStream(ss, jpgw);
    jpgw.Free;
    Result := 'data:image/jpeg;base64,' + EncodeStringBase64(ss.DataString);
    ss.Free;
  end;
  ovrImg.Free;
  baseImg.Free;
end;

function OnlineStatus: Byte;
begin
  Result := LPDNetU.OnlineStatus;
end;

{ TTimeList }

constructor TTimeList.Create;
begin
  inherited Create;
  inherited Add(GetTickCount64);
end;

function TTimeList.Sum(aTopX: Integer): Int64;
var
  i, c: Integer;
begin
  Result := 0;
  if aTopX = 0 then
    c := Count - 1
  else if aTopX > Count - 1 then
    exit
  else
    c := aTopX;
  for i := 0 to c - 1 do
    Result := Result + Items[i];
end;

procedure TTimeList.LogTime;
var
  ts: Int64 = 0;
begin
  if Count > 0 then
    ts := Items[Count - 1];
  Insert(0, GetTickCount64 - ts);
end;

{ TKVList }

function TKVList.GetItems(Index: Integer): TKVItem;
begin
  Result := TKVItem(inherited Items[Index]);
end;

procedure TKVList.SetItems(Index: Integer; AValue: TKVItem);
begin
  Items[Index].Assign(AValue);
end;

constructor TKVList.Create;
begin
  inherited Create(TKVItem);
end;

function TKVList.Add: TKVItem;
begin
  Result := inherited Add as TKVItem;
end;

procedure TKVList.Add(aKey, aValue: String);
var
  uti: TKVItem;
begin
  uti := Add;
  uti.Key := aKey;
  uti.Value := aValue;
end;

procedure TKVList.CopyFrom(aSource: TKVList; withClear: Boolean);
var
  i: Integer;
begin
  if withClear then
    Clear;
  for i := 0 to aSource.Count - 1 do
    Add(aSource.Items[i].Key, aSource.Items[i].Value);
end;

{ TEndThread }

procedure TEndThread.Execute;
begin
  EnterCriticalSection(bioCS);
  try
    FParentBio.EndGet(FAwait);
  finally
    LeaveCriticalSection(bioCS);
  end;
end;

constructor TEndThread.Create(aParentBio: TBio; aAwait: Boolean);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  EnterCriticalSection(bioCS);
  try
    FParentBio := aParentBio;
  finally
    LeaveCriticalSection(bioCS);
  end;
  FAwait := aAwait;
  Start;
end;

{ TDownImgThread }

function TDownImgThread.GetImage: Boolean;
var
  i, idx: Integer;
  url, img: String;
begin
  Result := False;
  if Terminated then
    exit;
  idx := -1;
  i := 0;
  url := '';
  img := '';
  EnterCriticalSection(imgCS);
  try
    while not Terminated and (i < FLimit) and (i < FParentList.Count) do
    begin
      if (FParentList.Items[i].Value <> '') and (FParentList.Items[i].Value[1] = '@') then
      begin
        url := Copy(FParentList.Items[i].Value, 2, Length(FParentList.Items[i].Value));
        FParentList.Items[i].Value := '%' + url;
        idx := i;
        break;
      end
      else
      begin
        if FParentList.Items[i].Value <> '' then
          Inc(i);
      end;
    end;
  finally
    LeaveCriticalSection(imgCS);
  end;
  if idx >= 0 then
  begin
    img := GetImgUrlToBase64Jpeg(url, True, 300, 300, True);
    i := 0;
    while not Terminated and (img = '') and (i < 2) do
    begin
      img := GetImgUrlToBase64Jpeg(url, True, 300, 300, True);
      Inc(i);
    end;
    if img = '' then
      Result := GetImage
    else
      Result := True;
    EnterCriticalSection(imgCS);
    try
      FParentList.Items[idx].Value := img;
    finally
      LeaveCriticalSection(imgCS);
    end;
  end;
end;

procedure TDownImgThread.Execute;
var
  b: Boolean;
begin
  b := GetImage;
  while not Terminated and b do
    b := GetImage;
  EnterCriticalSection(imgCS);
  try
    FRunning := False;
  finally
    LeaveCriticalSection(imgCS);
  end;
end;

constructor TDownImgThread.Create(aParentList: TKVList; aImgLimit: Integer);
begin
  inherited Create(True);
  FreeOnTerminate := False; 
  FLimit := aImgLimit;
  EnterCriticalSection(imgCS);
  try
    FParentList := aParentList; 
    FRunning := True;
  finally
    LeaveCriticalSection(imgCS);
  end;
  Start;
end;

{ TDownThread }

procedure TDownThread.GetGBIF;
const
  url = 'http://api.gbif.org/v1';
  basemapurl = 'https://tile.gbif.org/3857/omt/0/0/0@1x.png?style=gbif-classic';
  pointsurl = 'https://api.gbif.org/v2/map/occurrence/density/0/0/0@1x.png?style=classic.point&taxonKey=';
var
  jd: TJSONData;
  json: String;
begin
  //get count
  if not Terminated and (FBio.Status = tsOK) then
  begin
    Fhc.Clear;
    json := '';
    if Fhc.Get(url + '/occurrence/search?taxonKey=' + IntToStr(FBio.GBIFkey), json) and (json <> '') then
    begin
      jd := GetJson(json);
      if jd.FindPath('count') <> nil then
      begin
        FBio.GBIFcount := jd.FindPath('count').AsInteger;
      end
      else
      begin
        FBio.FStatus := tsNOK;
        FBio.FErrorMessage := 'No "count" info';
      end;
      jd.Free;
    end
    else
    begin
      FBio.FStatus := tsNOK;
      FBio.FErrorMessage := '#1 - ' + Fhc.ResultMessage;
    end;
  end;
  //get map image
  if not Terminated and (FBio.Status = tsOK) then
    FBio.GBIFmapImage := GetAndMergeImages(basemapurl, pointsurl + IntToStr(FBio.GBIFkey), '', 500, 500, True);
  //update parent data
  if not Terminated then
  begin
    EnterCriticalSection(bioCS);
    try
      if FParentBio.Status in [tsEmpty, tsOK] then
        FParentBio.FStatus := FBio.FStatus;
      if FBio.Status = tsOK then
      begin
        FParentBio.GBIFcount := FBio.GBIFcount;
        FParentBio.GBIFmapImage := FBio.GBIFmapImage;
      end
      else
        FParentBio.FErrorMessage := FParentBio.ErrorMessage + 'GBIF: ' + FBio.ErrorMessage;
    finally
      LeaveCriticalSection(bioCS);
    end;
  end;
end;

procedure TDownThread.GetNCBI;
const
  url = 'https://eutils.ncbi.nlm.nih.gov/entrez/eutils/';
  reflimit = 15;
var
  Doc: TXMLDocument;
  xpv, xpv1: TXPathVariable;
  ns, ns1: TNodeSet;
  ms: TMemoryStream;
  s: String;
  i: Integer;
  timelist: TTimeList;

  procedure CheckReqTime; //only 3 requests per second for ip address
  var
    ts: Int64;
  begin
    ts := timelist.Sum(3);
    while (ts > 0) and (ts < 1050) and not Terminated do
    begin
      sleep(10);
      ts := timelist.Sum(3);
    end;
  end;

begin
  ms := TMemoryStream.Create;   
  timelist := TTimeList.Create;
  //Get taxon id
  if not Terminated and (FBio.Status = tsOK) then
  begin
    Fhc.Clear;
    if Fhc.Get(url + 'esearch.fcgi?db=taxonomy&term=' + sReplace(FName, ' ', '+'), ms) and (ms.Size > 0) then
    begin
      ms.Position := 0;
      ReadXMLFile(Doc, ms);
      xpv := EvaluateXPathExpression('/eSearchResult/IdList/Id', Doc.DocumentElement);
      if xpv <> nil then
      begin
        if not TryStrToInt(String(xpv.AsText), FBio.FNCBIid) then
        begin
          FBio.FStatus := tsNOK;
          FBio.FErrorMessage := 'Incorrect "ID" value';
        end;
        xpv.Free;
      end
      else
      begin
        FBio.FStatus := tsNOK;
        FBio.FErrorMessage := 'No "ID" value';
      end;
      Doc.Free;
    end
    else
    begin
      FBio.FStatus := tsNOK;
      FBio.FErrorMessage := '#1 - ' + Fhc.ResultMessage;
    end;
    timelist.LogTime;
  end;
  //Get summary data
  ms.Clear;
  Fhc.Clear;
  if not Terminated and (FBio.Status = tsOK) then
  begin
    if Fhc.Get(url + 'esummary.fcgi?db=taxonomy&id=' + IntToStr(FBio.FNCBIid) + '&retmode=xml', ms) and (ms.Size > 0) then
    begin
      ms.Position := 0;
      ReadXMLFile(Doc, ms);
      xpv := EvaluateXPathExpression('/eSummaryResult/DocSum/Item[@Name="Division"]', Doc.DocumentElement);
      if xpv <> nil then
      begin
        FBio.NCBIdivision := String(xpv.AsText);
        xpv.Free;
      end
      else
      begin
        FBio.FStatus := tsNOK;
        FBio.FErrorMessage := 'No "Division" value';
      end;
      xpv := EvaluateXPathExpression('/eSummaryResult/DocSum/Item[@Name="ScientificName"]', Doc.DocumentElement);
      if xpv <> nil then
      begin
        FBio.NCBIscientificname := String(xpv.AsText);
        xpv.Free;
      end
      else
      begin
        FBio.FStatus := tsNOK;
        FBio.FErrorMessage := 'No "ScientificName" value';
      end;
      xpv := EvaluateXPathExpression('/eSummaryResult/DocSum/Item[@Name="CommonName"]', Doc.DocumentElement);
      if xpv <> nil then
      begin
        FBio.NCBIcommonname := String(xpv.AsText);
        xpv.Free;
      end
      else
      begin
        FBio.FStatus := tsNOK;
        FBio.FErrorMessage := 'No "CommonName" value';
      end;
      Doc.Free;
    end
    else
    begin
      FBio.FStatus := tsNOK;
      FBio.FErrorMessage := '#2 - ' + Fhc.ResultMessage;
    end;      
    timelist.LogTime;
  end;
  //get nucleotide sequences
  ms.Clear;
  Fhc.Clear;
  if not Terminated and (FBio.Status = tsOK) then
  begin
    if Fhc.Get(url + 'esearch.fcgi?db=nucleotide&term=' + sReplace(FName, ' ', '+'), ms) and (ms.Size > 0) then
    begin
      ms.Position := 0;
      ReadXMLFile(Doc, ms);
      xpv := EvaluateXPathExpression('/eSearchResult/Count', Doc.DocumentElement);
      if xpv <> nil then
      begin
        if not TryStrToInt(String(xpv.AsText), i) then
        begin
          FBio.FStatus := tsNOK;
          FBio.FErrorMessage := 'Incorrect "nucNum" value';
        end
        else
          FBio.NCBInucNum := i;
        xpv.Free;
      end
      else
      begin
        FBio.FStatus := tsNOK;
        FBio.FErrorMessage := 'No "nucNum" value';
      end;
      Doc.Free;
    end
    else
    begin
      FBio.FStatus := tsNOK;
      FBio.FErrorMessage := '#3 - ' + Fhc.ResultMessage;
    end;     
    timelist.LogTime;
  end;
  //get protein sequences
  CheckReqTime;
  ms.Clear;
  Fhc.Clear;
  if not Terminated and (FBio.Status = tsOK) then
  begin
    if Fhc.Get(url + 'esearch.fcgi?db=protein&term=' + sReplace(FName, ' ', '+'), ms) and (ms.Size > 0) then
    begin
      ms.Position := 0;
      ReadXMLFile(Doc, ms);
      xpv := EvaluateXPathExpression('/eSearchResult/Count', Doc.DocumentElement);
      if xpv <> nil then
      begin
        if not TryStrToInt(String(xpv.AsText), i) then
        begin
          FBio.FStatus := tsNOK;
          FBio.FErrorMessage := 'Incorrect "protcNum" value';
        end
        else
          FBio.NCBIprotNum := i;
        xpv.Free;
      end
      else
      begin
        FBio.FStatus := tsNOK;
        FBio.FErrorMessage := 'No "protNum" value';
      end;
      Doc.Free;
    end
    else
    begin
      FBio.FStatus := tsNOK;
      FBio.FErrorMessage := '#4 - ' + Fhc.ResultMessage;
    end;    
    timelist.LogTime;
  end;
  //get list of links
  CheckReqTime;
  ms.Clear;
  Fhc.Clear;
  if not Terminated and (FBio.Status = tsOK) then
  begin
    if Fhc.Get(url + 'elink.fcgi?dbfrom=taxonomy&id=' + IntToStr(FBio.FNCBIid) + '&cmd=llinkslib', ms) and (ms.Size > 0) then
    begin
      ms.Position := 0;
      ReadXMLFile(Doc, ms);
      xpv := EvaluateXPathExpression('//ObjUrl/Url', Doc.DocumentElement);
      if xpv <> nil then
      begin
        ns := xpv.AsNodeSet;
        xpv1 := EvaluateXPathExpression('//ObjUrl/Provider/Name', Doc.DocumentElement);
        if xpv1 <> nil then
        begin
          ns1 := xpv1.AsNodeSet;
          for i := 0 to ns.Count - 1 do
          begin
            if ns1.Count > i then
              FBio.NCBIlinks.Add(String(TDomElement(ns.Items[i]).TextContent), String(TDomElement(ns1.Items[i]).TextContent))
            else
              FBio.NCBIlinks.Add(String(TDomElement(ns.Items[i]).TextContent), '');
          end;
          xpv1.Free;
        end
        else
        begin
          for i := 0 to ns.Count - 1 do
            FBio.NCBIlinks.Add(String(TDomElement(ns.Items[i]).TextContent), '');
        end;
        xpv.Free;
      end
      else
      begin
        FBio.FStatus := tsNOK;
        FBio.FErrorMessage := 'No "links"';
      end;
      Doc.Free;
    end
    else
    begin
      FBio.FStatus := tsNOK;
      FBio.FErrorMessage := '#5 - ' + Fhc.ResultMessage;
    end;      
    timelist.LogTime;
  end;
  //get reference ids
  CheckReqTime;
  s := '';
  ms.Clear;
  Fhc.Clear;
  if not Terminated and (FBio.Status = tsOK) then
  begin
    if Fhc.Get(url + 'esearch.fcgi?db=pubmed&retmax=' + IntToStr(reflimit) + '&sort=relevance&term=' + sReplace(FName, ' ', '+'), ms) and (ms.Size > 0) then
    begin
      ms.Position := 0;
      ReadXMLFile(Doc, ms);
      xpv := EvaluateXPathExpression('/eSearchResult/IdList/Id', Doc.DocumentElement);
      if xpv <> nil then
      begin
        ns := xpv.AsNodeSet;
        for i := 0 to ns.Count - 1 do
          s := s + String(TDomElement(ns.Items[i]).TextContent) + ',';
        xpv.Free;
      end
      else
      begin
        FBio.FStatus := tsNOK;
        FBio.FErrorMessage := 'No "reference ids"';
      end;
      Doc.Free;
    end
    else
    begin
      FBio.FStatus := tsNOK;
      FBio.FErrorMessage := '#6 - ' + Fhc.ResultMessage;
    end;    
    timelist.LogTime;
  end;
  //get references
  CheckReqTime;
  ms.Clear;
  Fhc.Clear;
  if not Terminated and (FBio.Status = tsOK) then
  begin
    if Fhc.Get(url + 'efetch.fcgi?db=pubmed&id=' + s + '&retmode=xml', ms) and (ms.Size > 0) then
    begin
      ms.Position := 0;
      ReadXMLFile(Doc, ms);
      xpv := EvaluateXPathExpression('//PubmedData/ArticleIdList/ArticleId[@IdType="doi"]', Doc.DocumentElement);
      if xpv <> nil then
      begin
        ns := xpv.AsNodeSet;
        xpv1 := EvaluateXPathExpression('//Article/ArticleTitle', Doc.DocumentElement);
        if xpv1 <> nil then
        begin
          ns1 := xpv1.AsNodeSet;
          for i := 0 to ns.Count - 1 do
          begin
            if ns1.Count > i then
              FBio.NCBIreferences.Add('http://dx.doi.org/doi:' + String(TDomElement(ns.Items[i]).TextContent), String(TDomElement(ns1.Items[i]).TextContent))
            else
              FBio.NCBIreferences.Add('http://dx.doi.org/doi:' + String(TDomElement(ns.Items[i]).TextContent), '');
          end;
          xpv1.Free;
        end
        else
        begin
          for i := 0 to ns.Count - 1 do
            FBio.NCBIreferences.Add('http://dx.doi.org/doi:' + String(TDomElement(ns.Items[i]).TextContent), '');
        end;
        xpv.Free;
      end
      else
      begin
        FBio.FStatus := tsNOK;
        FBio.FErrorMessage := 'No "references"';
      end;
      Doc.Free;
    end
    else
    begin
      FBio.FStatus := tsNOK;
      FBio.FErrorMessage := '#7 - ' + Fhc.ResultMessage;
    end;
  end;
  ms.Free;
  timelist.Free;
  //update parent data
  if not Terminated then
  begin
    EnterCriticalSection(bioCS);
    try
      if FParentBio.Status in [tsEmpty, tsOK] then
        FParentBio.FStatus := FBio.FStatus;
      if FBio.Status = tsOK then
      begin
        FParentBio.NCBIcommonname := FBio.NCBIcommonname;
        FParentBio.NCBIdivision := FBio.NCBIdivision;
        FParentBio.NCBIid := FBio.NCBIid;
        FParentBio.NCBIlinks.CopyFrom(FBio.NCBIlinks);
        FParentBio.NCBImap := FBio.NCBImap;
        FParentBio.NCBInucNum := FBio.NCBInucNum;
        FParentBio.NCBIprotNum := FBio.NCBIprotNum;
        FParentBio.NCBIreferences.CopyFrom(FBio.NCBIreferences);
        FParentBio.NCBIscientificname := FBio.NCBIscientificname;
      end
      else
        FParentBio.FErrorMessage := FParentBio.ErrorMessage + 'NCBI: ' + FBio.ErrorMessage;
    finally
      LeaveCriticalSection(bioCS);
    end;
  end;
end;

procedure TDownThread.GetWiki;
const
  url = 'https://en.wikipedia.org/';
  api = 'https://en.wikipedia.org/api/rest_v1/page/';
  tagurl = 'http://termextract.fivefilters.org/';
  taglimit = 15;
  imglimit = 5;
var
  json, redirected, txt, ext, s: String;
  jd: TJSONData;
  ja: TJSONArray;
  jo: TJSONObject;
  i: Integer;
  ditarr: Array[0..2] of TDownImgThread;
  ditstatus: Boolean;
begin
  //allow redirections
  if not Terminated and (FBio.Status = tsOK) then
  begin
    json := '';
    Fhc.Clear;
    redirected := FName;
    if Fhc.Get(url + 'w/api.php?action=query&titles=' + sReplace(FName, ' ', '+') + '&redirects&format=json', json) and (json <> '') then
    begin
      jd := GetJson(json);
      if jd.FindPath('query.redirects[0].to') <> nil then
        redirected := jd.FindPath('query.redirects[0].to').AsString;
      jd.Free;
    end
    else
    begin
      FBio.FStatus := tsNOK;
      FBio.FErrorMessage := '#1 - ' + Fhc.ResultMessage;
    end;
  end;   
  //get images
  if not Terminated and (FBio.Status = tsOK) then
  begin
    json := '';
    Fhc.Clear;
    if Fhc.Get(api + 'media-list/' + sReplace(redirected, ' ', '_'), json) and (json <> '') then
    begin
      jd := GetJson(json);
      if jd.FindPath('items') <> nil then
      begin
        ja := TJSONArray(jd.FindPath('items'));
        for i := 0 to ja.Count - 1 do
        begin
          jo := TJSONObject(ja.Items[i]);
          if jo.FindPath('title') <> nil then
          begin
            s := jo.FindPath('title').AsString;
            ext := LowerCase(ExtractFileExt(s));
            if ext = '.jpg' then
              FBio.WIKIimages.Add('http://en.wikipedia.org/wiki/' + s, '@http://commons.wikimedia.org/wiki/Special:Filepath/' + ExtractDelimited(2, s, [':']));
          end;
        end;
      end;
      jd.Free;
      if FBio.WIKIimages.Count > 0 then
      begin
        for i := Low(ditarr) to High(ditarr) do
          ditarr[i] := TDownImgThread.Create(FBio.WIKIimages, imglimit);
      end;
    end
    else
    begin
      FBio.FStatus := tsNOK;
      FBio.FErrorMessage := '#4 - ' + Fhc.ResultMessage;
    end;
  end;
  //get extract
  if not Terminated and (FBio.Status = tsOK) then
  begin
    json := '';
    Fhc.Clear;                           
    if Fhc.Get(api + 'summary/' + sReplace(redirected, ' ', '_'), json) and (json <> '') then
    begin
      jd := GetJson(json);
      if jd.FindPath('extract') <> nil then
        FBio.WIKIsnippet := jd.FindPath('extract').AsString;
      jd.Free;
    end
    else
    begin
      FBio.FStatus := tsNOK;
      FBio.FErrorMessage := '#2 - ' + Fhc.ResultMessage;
    end;
  end;
  //get tags
  if not Terminated and (FBio.Status = tsOK) and (FBio.WIKIsnippet <> '') then
  begin
    txt := '';
    Fhc.Clear;
    if Fhc.Get(tagurl + 'extract.php?text=' + sReplace(FBio.WIKIsnippet, ' ', '+') + '&output=txt&max=' + IntToStr(taglimit), txt) and (txt <> '') then
    begin
      FBio.FFtags.Text := sReplace(txt, '\n', sLineBreak);
      for i := FBio.FFtags.Count - 1 downto 0 do
      begin
        if Trim(FBio.FFtags[i]) = '' then
          FBio.FFtags.Delete(i)
        else
          FBio.FFtags[i] := Trim(FBio.FFtags[i]);
      end;
    end
    else
    begin
      FBio.FStatus := tsNOK;
      FBio.FErrorMessage := '#3 - ' + Fhc.ResultMessage;
    end;
  end;
  //check for images
  if FBio.WIKIimages.Count > 0 then
  begin
    ditstatus := False;
    for i := Low(ditarr) to High(ditarr) do
    begin
      EnterCriticalSection(imgCS);
      try
        ditstatus := ditstatus or ditarr[i].Running;
      finally
        LeaveCriticalSection(imgCS);
      end;
    end;
    while not Terminated and ditstatus do
    begin
      sleep(100);
      ditstatus := False;
      for i := Low(ditarr) to High(ditarr) do
      begin
        EnterCriticalSection(imgCS);
        try
          ditstatus := ditstatus or ditarr[i].Running;
        finally
          LeaveCriticalSection(imgCS);
        end;
      end;
    end;
    for i := Low(ditarr) to High(ditarr) do
    begin
      if ditarr[i] <> nil then
      begin
        ditarr[i].Terminate;
        ditarr[i].WaitFor;
        ditarr[i].Free;
        ditarr[i] := nil;
      end;
    end;
    for i := FBio.WIKIimages.Count - 1 downto 0 do
    begin
      if (FBio.WIKIimages.Items[i].Value = '') or (FBio.WIKIimages.Items[i].Value[1] in ['@', '%']) then
        FBio.WIKIimages.Delete(i);
    end;
  end;
  //update parent data
  if not Terminated then
  begin
    EnterCriticalSection(bioCS);
    try
      if FParentBio.Status in [tsEmpty, tsOK] then
        FParentBio.FStatus := FBio.FStatus;
      if FBio.Status = tsOK then
      begin
        FParentBio.WIKIsnippet := FBio.WIKIsnippet;
        FParentBio.WIKIimages.CopyFrom(FBio.WIKIimages);
        FParentBio.FFtags.Text := FBio.FFtags.Text;
      end
      else
        FParentBio.FErrorMessage := FParentBio.ErrorMessage + 'WIKI: ' + FBio.ErrorMessage;
    finally
      LeaveCriticalSection(bioCS);
    end;
  end;
end;

procedure TDownThread.Execute;
var
  ndtc: Integer;
begin
  if not Terminated then
  begin
    case FType of
      ttGBIF: GetGBIF;
      ttNCBI: GetNCBI;
      ttWiki: GetWiki;
    end;
  end;
  EnterCriticalSection(bioCS);
  try
    ndtc := FParentBio.FDownThreadCount - 1;
    FParentBio.FDownThreadCount := ndtc;
  finally
    LeaveCriticalSection(bioCS);
  end;
  if ndtc < 1 then
    TEndThread.Create(FParentBio, FAwait);
end;

constructor TDownThread.Create(aParentBio: TBio; aType: TType; aAwait: Boolean);
begin
  inherited Create(True);
  FreeOnTerminate := False;   
  FBio := TBio.Create;
  EnterCriticalSection(bioCS);
  try                    
    FParentBio := aParentBio;
    FName := FParentBio.Name;
    FBio.FStatus := FParentBio.Status;
    FBio.FGBIFauthorship := FParentBio.GBIFauthorship;
    FBio.FGBIFclasse := FParentBio.GBIFclasse;
    FBio.FGBIFfamily := FParentBio.GBIFfamily;
    FBio.FGBIFkey := FParentBio.GBIFkey;
    FBio.FGBIFkingdom := FParentBio.GBIFkingdom;
    FBio.FGBIForder := FParentBio.GBIForder;
    FBio.FGBIFphylum := FParentBio.GBIFphylum;
    FBio.FGBIFscientificname := FParentBio.GBIFscientificname;
    FBio.FGBIFstatus := FParentBio.GBIFstatus;
    FBio.FGBIFvalid_name := FParentBio.GBIFvalid_name;
  finally
    LeaveCriticalSection(bioCS);
  end;
  FAwait := aAwait;
  if FBio.FStatus = tsEmpty then;
    FBio.FStatus := tsOK;
  FType := aType;
  Fhc := THTTPClient.Create;
  Start;
end;

destructor TDownThread.Destroy;
begin
  FBio.Free;
  Fhc.Free;
  inherited Destroy;
end;

{ TBio }

procedure TBio.EndGet(aAwait: Boolean);
var
  i: Integer;
begin
  for i := Low(FDTarr) to High(FDTarr) do
  begin
    if FDTarr[i] <> nil then
    begin
      FDTarr[i].Terminate; 
      FDTarr[i].WaitFor;  
      FDTarr[i].Free;  
      FDTarr[i] := nil;  
    end;
  end;
  if FDownThreadCount > 0 then
  begin
    Clear;
    FDownThreadCount := 0;
  end;
  SetRunning(False);
  if not aAwait and Assigned(FAfterGet) then
    FAfterGet(Self);
end;

procedure TBio.Run(aName: String; aAwait: Boolean);
begin
  SetRunning(True);
  FName := aName;
  if (FName <> '') and GetGBIFSummary then
  begin
    FDownThreadCount := 3;
    FDTarr[0] := TDownThread.Create(Self, ttGBIF, aAwait);
    FDTarr[1] := TDownThread.Create(Self, ttNCBI, aAwait);
    FDTarr[2] := TDownThread.Create(Self, ttWiki, aAwait);
  end
  else
  begin
    if FName = '' then
      FErrorMessage := 'Name cannot be empty!';
    FName := '';
    FStatus := tsNOK;
    SetRunning(False);
    if not aAwait and Assigned(FAfterGet) then
      FAfterGet(Self);
  end;
end;

function TBio.GetRunning: Boolean;
begin
  EnterCriticalSection(bioCS);
  try
    Result := FRunning;
  finally
    LeaveCriticalSection(bioCS);
  end;
end;

procedure TBio.SetRunning(aValue: Boolean);
begin
  EnterCriticalSection(bioCS);
  try
    if FRunning <> aValue then
      FRunning := aValue;
  finally
    LeaveCriticalSection(bioCS);
  end;
end;

function TBio.GetGBIFSummary: Boolean;
const
  url = 'http://api.gbif.org/v1';
var
  jd: TJSONData;
  jo: TJSONObject;
  json: String;
  i: Integer;
  hc: THTTPClient;
begin
  Result := False;
  //get summary data
  json := '';
  hc := THTTPClient.Create;
  if hc.Get(url + '/species/?name=' + sReplace(FName, ' ', '%20'), json) and (json <> '') then
  begin
    jd := GetJson(json);
    if (jd.FindPath('results') <> nil) and (TJSONArray(jd.FindPath('results')).Count > 0) then
    begin
      jo := TJSONObject(TJSONArray(jd.FindPath('results')).Items[0]);
      for i := 0 to jo.Count - 1 do
      begin
        case jo.Names[i] of
          'key': FGBIFkey := jo.Items[i].AsInteger;
          'canonicalName': FGBIFscientificname := jo.Items[i].AsString;
          'authorship': FGBIFauthorship := jo.Items[i].AsString;
          'taxonomicStatus':
            begin
              FGBIFstatus := LowerCase(StringReplace(jo.Items[i].AsString, '_', ' ', [rfReplaceAll]));
              if FGBIFstatus <> 'accepted' then
                FGBIFvalid_name := jo.Items[jo.IndexOfName('species')].AsString;
            end;
          'kingdom': FGBIFkingdom := jo.Items[i].AsString;
          'phylum': FGBIFphylum := jo.Items[i].AsString;
          'class': FGBIFclasse := jo.Items[i].AsString;
          'order': FGBIForder := jo.Items[i].AsString;
          'family': FGBIFfamily := jo.Items[i].AsString;
        end;
      end;
      Result := True;
    end
    else
      FErrorMessage := 'Species not exists';
    jd.Free;
  end
  else
    FErrorMessage := '#' + hc.ResultMessage;
  hc.Free;
  if not Result then
    FStatus := tsNOK;
end;

constructor TBio.Create;
begin
  FDownThreadCount := 0;  
  FErrorMessage := '';
  FStatus := tsEmpty;
  FRunning := False;
  FName := '';
  FFFtags := TStringList.Create;
  FNCBIlinks := TKVList.Create;
  FNCBIreferences := TKVList.Create;
  FWIKIimages := TKVList.Create;
  Clear;
end;

destructor TBio.Destroy;
begin
  Stop;
  FFFtags.Free;
  FNCBIlinks.Free;
  FNCBIreferences.Free;
  FWIKIimages.Free;
  inherited Destroy;
end;

procedure TBio.Clear;
begin
  FName := '';
  FFFtags.Clear;
  FGBIFauthorship := '';
  FGBIFclasse := '';
  FGBIFcount := 0;
  FGBIFfamily := '';
  FGBIFkey := 0;
  FGBIFkingdom := '';
  FGBIFmapImage := '';
  FGBIForder := '';
  FGBIFphylum := '';
  FGBIFscientificname := '';
  FGBIFstatus := '';
  FGBIFvalid_name := '';
  FNCBIcommonname := '';
  FNCBIdivision := '';
  FNCBIid := 0;
  FNCBIlinks.Clear;
  FNCBImap := '';
  FNCBInucNum := 0;
  FNCBIprotNum := 0;
  FNCBIreferences.Clear;
  FNCBIscientificname := '';
  FWIKIimages.Clear;
  FWIKIsnippet := '';
end;

function TBio.GetAndWait(aName: String): Boolean;
begin
  Result := False;
  Run(aName, True);
  while Running do
    sleep(25);
  Result := FStatus = tsOK;
end;

procedure TBio.Get(aName: String);
begin
  if not Running then
    Run(aName, False);
end;

procedure TBio.Stop;
begin
  if Running then
    EndGet(False);
end;

function TBio.LoadFromJson(json: String): Boolean;
var
  jds: TJSONDeStreamer;
begin     
  Result := False;
  if Running then
    exit;
  if json = '' then
    exit; 
  Clear;
  jds := TJSONDeStreamer.Create(nil);
  try
    jds.JSONToObject(json, Self);
    Result := True;
  finally
    jds.Free;
  end;
end;

function TBio.SaveToJson: String;
var
  js: TJSONStreamer;
begin
  js := TJSONStreamer.Create(nil);
  js.Options := js.Options + [jsoTStringsAsArray];
  Result := js.ObjectToJSONString(Self);
  js.Free;
end;

initialization
  InitCriticalSection(bioCS);
  InitCriticalSection(imgCS);
finalization
  DoneCriticalSection(bioCS);
  DoneCriticalSection(imgCS);

end.
