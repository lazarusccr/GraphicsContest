unit Utilitaire_chaine_utf8;

{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils, lazutf8;

type

// Tools for UTF8 string


TArrayOfUTF8 = array of string;  // container for each code point of an utf8 string ( utf8 character may have 1, 2, 3 or 4 bytes )


{ TS8 }     // <= permet d'accéder à une UTF8 comme avant : machaine[i]...
            // mais pas optimisé pour de très grandes chaines de caractères

TS8 = class     // TStringUTF8
private
  FArrayChar: TArrayOfUTF8 ;
  function GetCharacter(i: integer): string;
  function GetCharCount: integer;
  procedure SetCharacter(i: integer; AValue: string);
  procedure NotifyIndexError( Index: integer);
public
  constructor Create;
  constructor Create( aUTF8String: string );
  procedure InitWith( aUTF8String: string ); // initialize with a string
  function All: string; // give the entire string
  property Char[i: integer]: string read GetCharacter write SetCharacter; default;  // 1 based as string
  property CharCount: integer read GetCharCount;
end;


operator = (const s1: TS8; const s2: string ): boolean; inline;
operator > (const s1: TS8; const s2: string ): boolean; inline;
operator >= (const s1: TS8; const s2: string ): boolean; inline;
operator < (const s1: TS8; const s2: string ): boolean; inline;
operator <= (const s1: TS8; const s2: string ): boolean; inline;
operator <> (const s1: TS8; const s2: string ): boolean; inline;
operator + (const s1: TS8; const s2: string ): string ; inline;



// utilisez les functions de LazUTF8  suivantes:

//  UTF8Pos ( txt_a_trouver, txt_où_rechercher, pos de début de recherche en UTF8 code point );
//  UTF8Length (txt ); renvoi la longueur du texte en UTF8 code point
//  UTF8Copy ( const s: string; StartCharIndex, CharCount: PtrInt ): string;
//  UTF8Delete ( var s: String; StartCharIndex, CharCount: PtrInt );


// renvoi le codepoint 32 bits du caractère UTF8
function UTF8CharToCode(const anUTF8Char: string ): DWord;
function CodeToUTF8Char( aCode:DWord ): string;


//  ----------------------
//  ///   CARACTERES   ///
//  ----------------------
//
// sépare les caractères d'une chaine UTF8
function UTF8ToCharArray( anUTF8String: string ): TArrayOfUTF8;  // ne rien libérer
function UTF8ToCharStringList( anUTF8String: string ): TStringList; // penser à liberer le TStringList
// reconstruit une chaine dont les caractères ont été précédemment séparés
function ArrayCharToUTF8( anArray: TArrayOfUTF8 ): string;
function TStringListCharToUTF8( anStringList: TStringList ): string;



//  ----------------
//  ///   MOTS   ///
//  ----------------
// sépare les mots d'une chaine UTF8
function UTF8ToWordArray(anUTF8String: string; aSeparator: TArrayOfUTF8 ): TArrayOfUTF8;
// reconstruit une chaine dont les mots ont été précédemment séparés
function WordArrayToUTF8String( anArray: TArrayOfUTF8 ): string;




implementation

procedure ErrorUTF8(const aMsg: string );
begin
 raise exception.create(aMsg) at
     get_caller_addr(ExceptAddr),
     get_caller_frame(ExceptFrames);
end;

function UTF8CharToCode(const anUTF8Char: string): DWord;  // un caractère UTF8 peut comporter jusqu'à 4 octets
var p: PByte;
begin
 p := PByte(PChar( anUTF8Char ));
 if Length(anUTF8Char)=0
   then Result :=$00000000
   else if (p^ and $F0) = $F0
          then Result := (p^ shl 24) or ((p+1)^ shl 16) or ((p+2)^ shl 8) or (p+3)^
          else if (p^ and $E0) = $E0
                 then Result := (p^ shl 16) or ((p+1)^ shl 8) or (p+2)^
                 else if (p^ and $C0) = $C0
                        then Result := (p^ shl 8) or (p+1)^
                        else Result := p^;
end;

function CodeToUTF8Char(aCode: DWord): string;
var b1, b2,b3,b4: byte;
begin
 b1 := (aCode and $FF000000) shr 24 ;
 b2 := (aCode and $00FF0000) shr 16 ;
 b3 := (aCode and $0000FF00) shr 8 ;
 b4 := aCode and $000000FF;
 Result := '';
 if b1<>0
   then Result += chr(b1);
 if (b1<>0) or (b2<>0)
   then Result += chr(b2);
 if (b1<>0) or (b2<>0) or (b3<>0)
   then Result += chr(b3);
 if (b1<>0) or (b2<>0) or (b3<>0) or (b4<>0)
   then Result += chr(b4);
end;

function UTF8ToCharArray(anUTF8String: string): TArrayOfUTF8;
var
  p: PChar;
  CharLen: integer;
  i: integer;
begin
 SetLength( Result, 0 );
 if Length( anUTF8String )=0 then exit;
 p:=PChar(anUTF8String);
 repeat
   i :=  Length( Result );
   SetLength( Result, i+1 );
   Result[i] :='';
   CharLen := UTF8CharacterLength(p);
   if CharLen >= 1 then Result[i] += P[0] ; //FirstByte := P[0];
   if CharLen >= 2 then Result[i] += P[1] ; //SecondByte := P[1];
   if CharLen >= 3 then Result[i] += P[2] ; //ThirdByte := P[2];
   if CharLen = 4 then Result[i] += P[3] ; //FourthByte := P[3];
   inc(p,CharLen);
 until (CharLen=0) or (p^ = #0);
end;

function UTF8ToCharStringList(anUTF8String: string): TStringList;
var
  p: PChar;
  CharLen: integer;
  s: string;
begin
 Result := TStringList.Create;
 p:=PChar(anUTF8String);
 repeat
   CharLen := UTF8CharacterLength(p);
   s :='';
   if CharLen >= 1 then s += P[0] ; //FirstByte := P[0];
   if CharLen >= 2 then s += P[1] ; //SecondByte := P[1];
   if CharLen >= 3 then s += P[2] ; //ThirdByte := P[2];
   if CharLen = 4 then s += P[3] ; //FourthByte := P[3];
   if s<>'' then Result.Add( s );
   inc(p,CharLen);
 until (CharLen=0) or (p^ = #0);
end;

function ArrayCharToUTF8(anArray: TArrayOfUTF8): string;
var i: integer;
begin
 Result := '';
 for i:=0 to Length( anArray )-1 do
   Result += anArray[i] ;
end;

function TStringListCharToUTF8(anStringList: TStringList): string;
var i: integer;
begin
 Result := '';
 for i:=0 to anStringList.Count-1 do
   Result += anStringList.Strings[i];
end;

function UTF8ToWordArray(anUTF8String: string; aSeparator: TArrayOfUTF8 ): TArrayOfUTF8;
var
 i : integer ;
 t : string ;
 arraylettre: TArrayOfUTF8;
  function IsSeparator( aChar: string ): boolean;
  var j:integer;
  begin
   Result := FALSE;
   for j:=0 to Length(aSeparator)-1 do
     if aChar=aSeparator[j]
       then begin Result := TRUE; exit; end;
  end;
  procedure AddWordToResult( aWord: string);
  var i: integer;
  begin
   i := Length(Result);
   SetLength( Result, i+1 );
   Result[i] := aWord;
  end;

begin
 SetLength( Result, 0 );
 if length ( anUTF8String ) = 0 then exit ;
 arraylettre := UTF8ToCharArray( anUTF8String );
 i := 1 ;
 t := '' ;
 repeat
  if not IsSeparator( arraylettre[i] )
    then t += arraylettre[i]
    else if length ( t ) > 0
    then begin
          AddWordToResult( t );
          t := '' ;
         end;
  inc (i) ;
 until i > length ( arraylettre ) ;
 if t <> '' then AddWordToResult( t );
end;

function WordArrayToUTF8String(anArray: TArrayOfUTF8): string;
begin
 // not implemented
 Result :='';
end;

{ TS8 }

function TS8.GetCharacter(i: integer): string;
begin
 if (i<1) or (i>Length( FArrayChar ))
   then NotifyIndexError( i );
 Result := FArrayChar[i-1];
end;

function TS8.GetCharCount: integer;
begin
 Result := Length( FArrayChar );
end;

procedure TS8.SetCharacter(i: integer; AValue: string);
begin
 if (i<1) or (i>Length( FArrayChar ))
   then NotifyIndexError( i );
  FArrayChar[i-1] := AValue ;
end;

procedure TS8.NotifyIndexError( Index: integer);
begin
 ErrorUTF8('Index error, ['+inttostr( Index )+'] is out of bound ! (1 based)');
end;

constructor TS8.Create;
begin
  inherited Create;
  InitWith( '' );
end;

constructor TS8.Create(aUTF8String: string);
begin
  inherited Create;
  InitWith( aUtf8String );
end;

procedure TS8.InitWith(aUTF8String: string);
begin
  SetLength( FArrayChar, 0 );
  FArrayChar := UTF8ToCharArray( aUTF8String );
end;

function TS8.All: string;
var i: integer;
begin
 Result :='';
 for i:=0 to Length( FArrayChar )-1 do
  Result += FArrayChar[i];
end;




operator = (const s1: TS8; const s2: string ): boolean;
begin
 Result := ( s1.All = s2 );
end;

operator>(const s1: TS8; const s2: string): boolean;
begin
  Result := UTF8CompareStr( s1.All, s2 ) > 0;
end;

operator>=(const s1: TS8; const s2: string): boolean;
begin
 Result := UTF8CompareStr( s1.All, s2 ) >= 0;
end;

operator<(const s1: TS8; const s2: string): boolean;
begin
 Result := UTF8CompareStr( s1.All, s2 ) < 0;
end;

operator<=(const s1: TS8; const s2: string): boolean;
begin
 Result := UTF8CompareStr( s1.All, s2 ) <= 0;
end;

operator<>(const s1: TS8; const s2: string): boolean;
begin
 Result := s1.All <> s2;
end;

operator + (const s1: TS8; const s2: string ): string;
begin
 Result :=  S1.All + s2 ;
end;

end.

