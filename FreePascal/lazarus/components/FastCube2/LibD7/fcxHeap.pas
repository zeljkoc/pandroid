{*******************************************************}
{                                                       }
{            FastCube 2 heap manager unit               }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

//VCL uses section
{$IFNDEF FMX}
unit fcxHeap;
{$include fcx.inc}

interface
//FMX uses
{$ELSE FMX}
{$include fcx.inc}

interface
{$ENDIF FMX}

type
  PHeapChunk = ^THeapChunk;
  THeapChunk = record
    Allocation: Pointer;
    Next: PHeapChunk;
  end;

  TfcxHeap = class
  private
    FChunkSize: Cardinal;
    FBaseSize: Cardinal;
    FFirstChunk: PHeapChunk;
    FCurrentChunk: PHeapChunk;
    FCurrent: PAnsiChar; // to enable pointer ariphmetic
    FAvailSize: Cardinal; // to speed up (?) allocation
  protected
    function AddChunk(RequestedSize: Cardinal): PHeapChunk;
  public
    constructor Create(BaseSize: Cardinal = 0);
    destructor Destroy; override;

    procedure GetMem(var P: Pointer; Size: Cardinal); overload;
    function AllocMem(Size: Cardinal): Pointer; overload;

    procedure GetMem(var P: Pointer); overload;
    function AllocMem: Pointer; overload;
  end;

implementation

{ TfcxHeap }

function TfcxHeap.AddChunk(RequestedSize: Cardinal): PHeapChunk;
begin
  New(Result);

  if FChunkSize < RequestedSize then
    FChunkSize := RequestedSize;
  
  System.GetMem(Result.Allocation, FChunkSize);
  Result.Next := nil;
end;

function TfcxHeap.AllocMem(Size: Cardinal): Pointer;
begin
  GetMem(Result, Size);
  FillChar(Result^, Size, 0);
end;

function TfcxHeap.AllocMem: Pointer;
begin
  GetMem(Result, FBaseSize);
  FillChar(Result^, FBaseSize, 0);
end;

constructor TfcxHeap.Create(BaseSize: Cardinal = 0);
begin
  if BaseSize <> 0 then
  begin
    if BaseSize < 1024 then
      FChunkSize := (1024 div BaseSize) * BaseSize * 1024
    else
      FChunkSize := BaseSize * 1024;
    FBaseSize := BaseSize;
  end
  else
  begin
    FChunkSize := 1024 * 1024;
    FBaseSize := 0;
  end;

  FFirstChunk := AddChunk(FChunkSize);
  FCurrentChunk := FFirstChunk;
  FCurrent := FFirstChunk.Allocation;
  FAvailSize := FChunkSize;
end;

destructor TfcxHeap.Destroy;
var
  Chunk, Next: PHeapChunk;
begin
  Chunk := FFirstChunk;
  while Chunk <> nil do
  begin
    Next := Chunk.Next;
    FreeMem(Chunk.Allocation);
    Dispose(Chunk);
    Chunk := Next;
  end;
  inherited;
end;

procedure TfcxHeap.GetMem(var P: Pointer; Size: Cardinal);
begin
  if FAvailSize < Size then
  begin
    FCurrentChunk.Next := AddChunk(Size);
    FCurrentChunk := FCurrentChunk.Next;
    FCurrent := FCurrentChunk.Allocation;
    FAvailSize := FChunkSize;
  end;
  P := FCurrent;
  inc(FCurrent, Size);
  dec(FAvailSize, Size);
end;

procedure TfcxHeap.GetMem(var P: Pointer);
begin
  if FAvailSize < FBaseSize then
  begin
    FCurrentChunk.Next := AddChunk(FBaseSize);
    FCurrentChunk := FCurrentChunk.Next;
    FCurrent := FCurrentChunk.Allocation;
    FAvailSize := FChunkSize;
  end;
  P := FCurrent;
  inc(FCurrent, FBaseSize);
  dec(FAvailSize, FBaseSize);
end;

end.
