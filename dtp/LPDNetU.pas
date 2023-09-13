{==============================================================================

MIT License

Copyright (c) 2023 paweld, https://github.com/paweld

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

==============================================================================}

unit LPDNetU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, URIParser,
  httpsend, ssl_openssl, ssl_openssl_lib, synautil, blcksock, synachar;

type

  { THTTPClient }

  THTTPClient = class(THTTPSend)
  private
    function mHTTPMethod(const Method, URL: String): Boolean;
  public
    constructor Create;
    function Get(aURL: String; aResponse: TStream): Boolean;
    function Get(aURL: String; var aResponse: String): Boolean;
    function Post(aURL: String; aRequest, aResponse: TStream): Boolean;
    function Post(aURL, aRequest: String; var aResponse: String): Boolean;
    function Put(aURL: String; aRequest, aResponse: TStream): Boolean;
    function Put(aURL, aRequest: String; var aResponse: String): Boolean;
    function Delete(aURL: String; aResponse: TStream): Boolean;
    function Delete(aURL: String; var aResponse: String): Boolean;
    function Head(aURL: String): Boolean;
  end;

implementation

{ THTTPClient }

function THTTPClient.mHTTPMethod(const Method, URL: String): Boolean;
var
  newURL: String;
  uri: TURI;
  i: Integer;
  tmpheaders: TStringList;
begin
  tmpheaders := TStringList.Create;
  tmpheaders.Assign(Self.Headers);
  Result := Self.HTTPMethod(Method, URL);
  if Result and ((Self.ResultCode = 301) or (Self.ResultCode = 302) or (Self.ResultCode = 303) or (Self.ResultCode = 307) or (Self.ResultCode = 308)) then
  begin
    newURL := '';
    for i := 0 to Self.Headers.Count - 1 do
    begin
      if LowerCase(Copy(Self.Headers[i], 1, 8)) = 'location' then
      begin
        newURL := Copy(Self.Headers[i], 11, Length(Self.Headers[i]) - 10);
        if (LowerCase(Copy(newURL, 1, 8)) <> 'https://') and (LowerCase(Copy(newURL, 1, 7)) <> 'http://') and (LowerCase(Copy(newURL, 1, 4)) <> 'self') then
        begin
          uri := ParseURI(URL);
          if Copy(newURL, 1, 1) = '/' then
            newURL := uri.Protocol + '://' + uri.Host + newURL
          else
            newURL := uri.Protocol + '://' + uri.Host + '/' + newURL;
        end;
      end;
    end;
    if newURL <> '' then
    begin
      Self.Headers.Clear;
      Self.Headers.Assign(tmpheaders);
      Result := mHTTPMethod(Method, newURL);
    end;
  end;
  tmpheaders.Free
end;

constructor THTTPClient.Create;
begin
  inherited Create;
end;

function THTTPClient.Get(aURL: String; aResponse: TStream): Boolean;
begin      
  Result := False;
  try
    Self.mHTTPMethod('GET', aURL);
    Result := (Self.ResultCode = 200);
  except

  end;
  aResponse.CopyFrom(Self.Document, 0);
  aResponse.Position := 0;
end;

function THTTPClient.Get(aURL: String; var aResponse: String): Boolean;
var
  wyjscie: TStringStream;
begin
  wyjscie := TStringStream.Create('');
  Result := Self.Get(aURL, wyjscie);
  aResponse := wyjscie.DataString;
  wyjscie.Free;
end;

function THTTPClient.Post(aURL: String; aRequest, aResponse: TStream): Boolean;
begin         
  Result := False;
  try
    aRequest.Position := 0;
    Self.Document.CopyFrom(aRequest, 0);
    Self.mHTTPMethod('POST', aURL);
    Result := (Self.ResultCode = 200);
  except

  end;
  aResponse.CopyFrom(Self.Document, 0);
  aResponse.Position := 0;
end;

function THTTPClient.Post(aURL, aRequest: String; var aResponse: String): Boolean;
var
  wejscie, wyjscie: TStringStream;
begin
  wejscie := TStringStream.Create(aRequest);
  wyjscie := TStringStream.Create('');
  Result := Post(aURL, wejscie, wyjscie);
  aResponse := wyjscie.DataString;
  wejscie.Free;
  wyjscie.Free;
end;

function THTTPClient.Put(aURL: String; aRequest, aResponse: TStream): Boolean;
begin
  Result := False;
  try
    aRequest.Position := 0;
    Self.Document.CopyFrom(aRequest, 0);
    Self.mHTTPMethod('PUT', aURL);
    Result := (Self.ResultCode = 200);
  except

  end;
  aResponse.CopyFrom(Self.Document, 0);
  aResponse.Position := 0;
end;

function THTTPClient.Put(aURL, aRequest: String; var aResponse: String): Boolean;
var
  wejscie, wyjscie: TStringStream;
begin
  wejscie := TStringStream.Create(aRequest);
  wyjscie := TStringStream.Create('');
  Result := Put(aURL, wejscie, wyjscie);
  aResponse := wyjscie.DataString;
  wejscie.Free;
  wyjscie.Free;
end;

function THTTPClient.Delete(aURL: String; aResponse: TStream): Boolean;
begin
  Result := False;
  try
    Self.mHTTPMethod('DELETE', aURL);
    Result := (Self.ResultCode = 200);
  except

  end;
  aResponse.CopyFrom(Self.Document, 0);
  aResponse.Position := 0;
end;

function THTTPClient.Delete(aURL: String; var aResponse: String): Boolean;
var
  wyjscie: TStringStream;
begin
  wyjscie := TStringStream.Create('');
  Result := Delete(aURL, wyjscie);
  aResponse := wyjscie.DataString;
  wyjscie.Free;
end;

function THTTPClient.Head(aURL: String): Boolean;
begin
  Result := False;
  try
    Self.mHTTPMethod('HEAD', aURL);
    Result := (Self.ResultCode = 200);
  except

  end;
end;

end.
