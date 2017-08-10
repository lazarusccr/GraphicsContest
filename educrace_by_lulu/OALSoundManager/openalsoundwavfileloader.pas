unit OpenALSoundWavFileLoader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  OpenAL;

type

TArrayOfByte = array of byte;

{ TSoundLoader }

TSoundLoader = class
protected
  FStream: TFileStream;
  procedure DoOpenStream;
public
  procedure OpenFile( const AFilename: string ); virtual;
  function GetAllData: TArrayOfByte; virtual; abstract;
  function GetChannelCount: integer; virtual; abstract;
  function GetFormat: word; virtual; abstract;
  function GetFrequency: integer; virtual; abstract;
  function GetSampleCount: QWord; virtual; abstract;
  function GetBytePerSample: integer; virtual; abstract;
  function GetDataSizeInByte: QWord; virtual; abstract;
end;


type

TWavHeader = packed record
    RIFFHeader       : array[0..3] of char;
    FileSize         : integer;
    WAVEHeader       : array[0..3] of char;

    FormatHeader     : array[0..3] of char;
    FormatHeaderSize : integer;
    FormatCode       : word;
    ChannelNumber    : word;
    SampleRate       : longword;
    BytesPerSecond   : longword;
    BytesPerSample   : word;
    BitsPerSample    : word;
 end;

TWavChunk = packed record
    ChunkName        : array[0..3] of char;
    ChunkSize        : integer;
end;


{ TWavLoader }

TWavLoader = class( TSoundLoader )
private
  FWavHeader: TWavHeader;
  FWavDataStreamPos : int64;
  FWavDataByteLength : integer;
  procedure InitFromHeader;
public
  constructor Create;
  destructor Destroy; override;
  procedure OpenFile( const AFilename: string ); override;
  function GetAllData: TArrayOfByte; override;
  function GetChannelCount: integer; override;
  function GetFormat: word; override;
  function GetFrequency: integer; override;
  function GetSampleCount: QWord; override;
  function GetBytePerSample: integer; override;
  function GetDataSizeInByte: QWord; override;
end;

implementation

{ TSoundLoader }

procedure TSoundLoader.DoOpenStream;
begin

end;

procedure TSoundLoader.OpenFile(const AFilename: string);
begin
 if FStream <> NIL then FStream.Free;

 FStream := TFileStream.Create( AFilename, fmOpenRead );
end;

{ TWavLoader }

procedure TWavLoader.InitFromHeader;
var wc: TWavChunk;// :D
begin
 if FStream = NIL then exit;

 FStream.Position := 0;
 FStream.ReadBuffer( FWavHeader, sizeof ( TWavHeader ));

 FWavHeader.FormatHeaderSize := LEtoN( FWavHeader.FormatHeaderSize );
 FWavHeader.FormatCode := LEtoN( FWavHeader.FormatCode );
 FWavHeader.ChannelNumber := LEtoN( FWavHeader.ChannelNumber );
 FWavHeader.SampleRate := LEtoN( FWavHeader.SampleRate );
 FWavHeader.BytesPerSecond := LEtoN( FWavHeader.BytesPerSecond );
 FWavHeader.BytesPerSample := LEtoN( FWavHeader.BytesPerSample );
 FWavHeader.BitsPerSample := LEtoN( FWavHeader.BitsPerSample );

 repeat
  FStream.ReadBuffer( wc, sizeof ( wc ));  // read chunk header
  wc.ChunkSize := LEtoN( wc.ChunkSize );
  if wc.ChunkName = 'data' then begin
    FWavDataByteLength := wc.ChunkSize;
    FWavDataStreamPos := FStream.Position;
    exit;
  end else FStream.Seek( wc.ChunkSize ,soCurrent );
 until FStream.Position >= FStream.Size;

end;

constructor TWavLoader.Create;
begin
 FStream := NIL;
end;

destructor TWavLoader.Destroy;
begin
 if FStream <> NIL then FStream.Free;
 inherited Destroy;
end;

procedure TWavLoader.OpenFile(const AFilename: string);
begin
 inherited OpenFile( AFilename );
 InitFromHeader;
end;

function TWavLoader.GetAllData: TArrayOfByte;
begin
 if FStream = NIL
   then SetLength( Result, 0 )
   else begin
     SetLength( Result, FWavDataByteLength );
     FStream.Position := FWavDataStreamPos;
     FWavDataByteLength := FStream.Read( Result[0], FWavDataByteLength );
   end;
end;

function TWavLoader.GetChannelCount: integer;
begin
 if FStream = NIL
   then Result := 0
   else Result := FWavHeader.ChannelNumber;
end;

function TWavLoader.GetFormat: word;
begin
 Result := $0000;
 if FStream = NIL then exit;
 case FWavHeader.ChannelNumber of
         1: case FWavHeader.BitsPerSample of
                8: Result := AL_FORMAT_MONO8;
               16: Result := AL_FORMAT_MONO16;
         end;
         2: case FWavHeader.BitsPerSample of
                8: Result := AL_FORMAT_STEREO8;
               16: Result := AL_FORMAT_STEREO16;
         end;
   end;
end;

function TWavLoader.GetFrequency: integer;
begin
 if FStream = NIL
   then Result := 0
   else Result := FWavHeader.SampleRate;
end;

function TWavLoader.GetSampleCount: QWord;
begin
 if FStream = NIL
   then Result := 0
   else Result := QWord( FWavDataByteLength div GetBytePerSample );
end;

function TWavLoader.GetBytePerSample: integer;
begin
 if FStream = NIL
   then Result := 0
   else Result := FWavHeader.BytesPerSample;
end;

function TWavLoader.GetDataSizeInByte: QWord;
begin
 Result := FWavDataByteLength;
end;

end.

