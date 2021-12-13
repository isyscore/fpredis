{$I fpredis.inc}

unit fpredis;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF WINDOWS} windows {$ELSE} Unix {$ENDIF},
  Classes, sysutils,
  fpredis_api, fpredis_read, fgl, Variants, LConvEncoding;

type

  TFPRedisMap = specialize TFPGMap<String, String>;
  TFPZScoreMap = specialize TFPGMap<String, Double>;

  TFPRedisExeception = class(Exception)
  end;

  { TFPRedis }

  TFPRedis = class(TObject)
  private
    FConnected: Boolean;
    FHost: string;
    FPort: Integer;
    FAuth: string;
    FDatabase: Integer;
    FCtx: PRedisContext;
    FLastError: string;
    procedure processError(const reply: PRedisReply);
    function replyOnlyStatus(const reply: PRedisReply): Boolean;
    function replyStatusOK(const reply: PRedisReply): Boolean;
    function replyStatusString(const reply: PRedisReply): string;
    function replyString(const reply: PRedisReply): string;
    function replyInteger1(const reply: PRedisReply): Boolean;
    function replyIntegerV(const reply: PRedisReply): Int64;
    function replyDoubleV(const reply: PRedisReply): Double;
    function replyArrayString(const reply: PRedisReply): TStringArray;

  public
    constructor Create;
    constructor Create(host: string; port: Integer; auth: string = ''; database: Integer = 0);
    destructor Destroy; override;
    function Connect(): Boolean;
    function Connect(host: string; port: Integer; auth: string = ''; database: Integer = 0): Boolean;
    procedure Disconnect();
    function Info(AKey: string): string;
    function Select(ADatabase: Integer): Boolean;
    function Get(AKey: String): String;
    function &Set(AKey: String; AValue: String): Boolean;
    function Exists(AKey: string): Boolean;
    function Delete(AKey: string): Boolean;
    function &Type(AKey: String): String;
    function Echo(AStr: string): string;
    function Keys(APattern: string): TStringArray;
    function Expire(AKey: string; ASeconds: Integer): Int64;
    function ExpireAt(AKey: string; AUnixTime: Int64): Int64;
    function Pexpire(AKey: string; AMilliSeconds: Integer): Int64;
    function PexpireAt(AKey: string; AMilliSecondsTimeStamp: Int64): Int64;
    function Ttl(AKey: string): Int64;
    function Pttl(AKey: string): Int64;
    function SetBit(AKey: string; AOffset: Int64; AValue: Boolean): Boolean;
    function GetBit(AKey: string; AOffset: Int64): Boolean;
    function GetAllBits(AKey: string): TBytes;
    procedure GetStringOrBytes(AKey: String; out Str: String; out Bytes: TBytes);
    function Unlink(AKey: string): Int64;
    function Move(AKey: string; ADBIndex: Integer): Int64;
    function GetSet(AKey: String; AValue: string): String;
    function SetNx(AKey: string; AVAlue: string): Int64;
    function SetEx(AKey: string; ASeconds: Integer; AValue: string): String;
    function Decr(AKey: string): Int64;
    function DecrBy(AKey: string; ADecrement: Int64): Int64;
    function Incr(AKey: string): Int64;
    function IncrBy(AKey: string; ADecrement: Int64): Int64;
    function IncrByFloat(AKey: string; ADecrement: Double): Double;
    function Append(AKey: string; AValue: string): Int64;
    function SubStr(AKey: string; AStart: Integer; AEnd: Integer): string;
    function StrLen(AKey: string): Int64;
    function SetRange(AKey: string; AOffset: Int64; AValue: string): Int64;
    function GetRange(AKey: string; AStartOffset: Int64; AEndOffset: Int64): String;
    function PsetEx(AKey: string; AMilliSeconds: Int64; AValue: string): String;
    function Persist(AKey: string): Int64;
    function Touch(AKey: string): Int64;

    function HSet(AKey: string; AField: String; AValue: String): Int64;
    function HGet(AKey: string; AField: string): string;
    function HSetNx(AKey: string; AField: String; AValue: string): Int64;
    function HExists(AKey: string; AField: string): Boolean;
    function HDel(AKey: string; AField: string): Int64;
    function HLen(AKey: string): Int64;
    function HKeys(AKey: string): TStringArray;
    function HVals(AKey: string): TStringArray;
    function HMSet(AKey: string; AMap: TFPRedisMap): Boolean;
    function HMGet(AKey: string; AFields: TStringArray): TStringArray;
    function HGetAll(AKey: String): TFPRedisMap;
    function HIncrBy(AKey: string; AField: string; AValue: Int64): Int64;
    function HIncrByFloat(AKey: string; AField: string; AValue: Double): Double;

    function LPush(AKey: string; AValue: TStringArray): Int64;
    function RPush(AKey: string; AValue: TStringArray): Int64;
    function LPushX(AKey: string; AValue: TStringArray): Int64;
    function RPushX(AKey: string; AValue: TStringArray): Int64;
    function LLen(AKey: string): Int64;
    function LRange(Akey: string; AStart: Int64; AStop: Int64): TStringArray;
    function LTrim(AKey: string; AStart: Int64; AStop: Int64): string;
    function LIndex(AKey: string; AIndex: Int64): String;
    function LSet(AKey: string; AIndex: Int64; AValue: string): string;
    function LRem(AKey: string; ACount: Int64; AValue: string): Int64;
    function LPop(AKey: string): string;
    function RPop(AKey: string): string;
    function BLPop(AArgs: TStringArray): TStringArray;
    function BLPop(ATimeout: Integer; AKey: String): TStringArray;
    function BRPop(AArgs: TStringArray): TStringArray;
    function BRPop(ATimeout: Integer; AKey: String): TStringArray;

    function SAdd(AKey: string; AMembers: TStringArray): Int64;
    function SMembers(AKey: string): TStringArray;
    function SRem(AKey: string; AMembers: TStringArray): Int64;
    function SPop(AKey: string): string;
    function SPop(AKey: string; ACount: Int64): TStringArray;
    function SMove(ASrcKey: string; ADestKey: string; AMember: string): Int64;
    function SCard(AKey: string): Int64;
    function SIsMember(AKey: string; AMember: string): Boolean;
    function SInter(AKeys: TStringArray): TStringArray;
    function SUnion(AKeys: TStringArray): TStringArray;
    function SDiff(AKeys: TStringArray): TStringArray;
    function SRandMember(AKey: string): string;
    function SRandMember(AKey: string; ACount: Int64): TStringArray;

    function ZAdd(AKey: string; AScore: Double; AMember: string): Int64;
    function ZAdd(AKey: string; AMap: TFPZScoreMap): Int64;
    function ZRange(AKey: string; AStart: Int64; AStop: Int64): TStringArray;
    function ZRangeWithScore(AKey: string; AStart: Int64; AStop: Int64): TFPZScoreMap;
    function ZRem(AKey: string; AMembers: TStringArray): Int64;
    function ZCount(AKey: string; AMin: Double; AMax: Double): Int64;
    function ZCount(AKey: string; AMin: String; AMax: String): Int64;
    function ZCard(AKey: string): Int64;
    function ZIncrBy(AKey: string; AIncrement: Double; AMember: String): Double;
    function ZRank(AKey: string; AMember: string): Int64;
    function ZRevRank(AKey: string; AMember: string): Int64;
    function ZRevRange(AKey: string; AMember: string): TStringArray;
    function ZScore(AKey: string; AMember: string): Double;

    function Scan(ACursor: Int64): TStringArray;

  public
    property Connected: Boolean read FConnected;
    property LastError: string read FLastError;
  end;

implementation

{ TFPRedis }

procedure TFPRedis.processError(const reply: PRedisReply);
begin
  FLastError:= string(reply^.str);
  {$IFNDEF WINDOWS}
  WriteLn(FLastError);
  {$ENDIF}
end;

function TFPRedis.replyOnlyStatus(const reply: PRedisReply): Boolean;
begin
  Result := False;
  if (reply^._type = REDIS_REPLY_STATUS) then begin
    Result := True;
  end else begin
    processError(reply);
  end;
  freeReplyObject(reply);
end;

function TFPRedis.replyStatusOK(const reply: PRedisReply): Boolean;
begin
  Result := False;
  if (reply^._type = REDIS_REPLY_STATUS) then begin
    Result := reply^.str = 'OK';
  end else begin
    processError(reply);
  end;
  freeReplyObject(reply);
end;

function TFPRedis.replyStatusString(const reply: PRedisReply): string;
begin
  if (reply^._type = REDIS_REPLY_STATUS) then begin
    Result := string(reply^.str);
  end else begin
    processError(reply);
  end;
  freeReplyObject(reply);
end;

function TFPRedis.replyString(const reply: PRedisReply): string;
begin
  Result := '';
  if (reply^._type = REDIS_REPLY_STRING) then begin
    Result := string(reply^.str);
  end else begin
    processError(reply);
  end;
  freeReplyObject(reply);
end;

function TFPRedis.replyInteger1(const reply: PRedisReply): Boolean;
begin
  Result := False;
  if (reply^._type = REDIS_REPLY_INTEGER) then begin
    Result := reply^._integer = 1;
  end else begin
    processError(reply);
  end;
  freeReplyObject(reply);
end;

function TFPRedis.replyIntegerV(const reply: PRedisReply): Int64;
begin
  Result := 0;
  if (reply^._type = REDIS_REPLY_INTEGER) then begin
    Result := reply^._integer;
  end else begin
    processError(reply);
  end;
  freeReplyObject(reply);
end;

function TFPRedis.replyDoubleV(const reply: PRedisReply): Double;
begin
  Result := 0.0;
  if (reply^._type = REDIS_REPLY_DOUBLE) then begin
    Result := reply^.dval;
  end else begin
    processError(reply);
  end;
  freeReplyObject(reply);
end;

function TFPRedis.replyArrayString(const reply: PRedisReply): TStringArray;
var
  i: Integer;
begin
  Result := nil;
  if (reply^._type = REDIS_REPLY_ARRAY) then begin
    SetLength(Result, reply^.elements);
    for i:= 0 to Length(Result) - 1 do begin
      Result[i] := string(reply^.element[i]^.str);
    end;
  end else begin
    processError(reply);
  end;
  freeReplyObject(reply);
end;

constructor TFPRedis.Create;
begin
  FHost:= '127.0.0.1';
  FPort:= 6379;
  FAuth:= '';
  FDatabase:= 0;
  FLastError:= '';
  FCtx:= nil;
end;

constructor TFPRedis.Create(host: string; port: Integer; auth: string;
  database: Integer);
begin
  FHost:= host;
  FPort:= port;
  FAuth:= auth;
  FDatabase:= database;
  FLastError:= '';
  FCtx:= nil;
end;

destructor TFPRedis.Destroy;
begin
  if (FCtx <> nil) then begin
    redisFree(FCtx);
  end;
  inherited Destroy;
end;

function TFPRedis.Connect(): Boolean;
var
  reply: PRedisReply;
begin
  if (FCtx <> nil) then begin
    redisFree(FCtx);
    FCtx:= nil;
  end;
  FCtx:= redisConnect(PChar(FHost), 26379);
  if (FCtx^.err <> REDIS_OK) then begin
    FLastError:= 'Connect Error: %s'.format([string(FCtx^.errstr)]);
    FConnected:= False;
    redisFree(FCtx);
    FCtx:= nil;
    Exit(False);
  end;
  if (FAuth <> '') then begin
    reply:= redisCommand(FCtx, 'AUTH %s', [PChar(FAuth)]);
    Result := reply^._type <> REDIS_REPLY_ERROR;
    if (not Result) then begin
      FLastError:= 'Auth Failed: %s'.Format([string(reply^.str)]);
    end else begin
      Result := Select(FDatabase);
    end;
    freeReplyObject(reply);
  end;
  FConnected:= Result;
end;

function TFPRedis.Connect(host: string; port: Integer; auth: string;
  database: Integer): Boolean;
begin
  FHost:= host;
  FPort:= port;
  FAuth:= auth;
  FDatabase:= database;
  Result := Connect();
end;

procedure TFPRedis.Disconnect();
begin
  FConnected := False;
  if (FCtx <> nil) then begin
    redisFree(FCtx);
    FCtx:= nil;
  end;
end;

function TFPRedis.Info(AKey: string): string;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'INFO %s', [PChar(AKey)]);
  Result := replyString(reply);
end;

function TFPRedis.Select(ADatabase: Integer): Boolean;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'SELECT %d', [ADatabase]);
  Result := replyOnlyStatus(reply);
end;

function TFPRedis.Get(AKey: String): String;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'GET %s', [PChar(AKey)]);
  Result := replyString(reply);
end;

function TFPRedis.&Set(AKey: String; AValue: String): Boolean;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply := redisCommand(FCtx, 'SET %s %s', [PChar(AKey), PChar(AValue)]);
  Result := replyStatusOK(reply);
end;

function TFPRedis.Exists(AKey: string): Boolean;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'EXISTS %s', [PChar(AKey)]);
  Result := replyInteger1(reply);
end;

function TFPRedis.Delete(AKey: string): Boolean;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'DEL %s', [PChar(AKey)]);
  Result := replyInteger1(reply);
end;

function TFPRedis.&Type(AKey: String): String;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'TYPE %s', [PChar(AKey)]);
  Result := replyStatusString(reply);
end;

function TFPRedis.Echo(AStr: string): string;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'ECHO %s', [PChar(AStr)]);
  Result := replyString(reply);
end;

function TFPRedis.Keys(APattern: string): TStringArray;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'KEYS %s', [PChar(APattern)]);
  Result := replyArrayString(reply);
end;

function TFPRedis.Expire(AKey: string; ASeconds: Integer): Int64;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'EXPIRE %s %d', [PChar(AKey), ASeconds]);
  Result := replyIntegerV(reply);
end;

function TFPRedis.ExpireAt(AKey: string; AUnixTime: Int64): Int64;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'EXPIREAT %s %d', [PChar(AKey), AUnixTime]);
  Result := replyIntegerV(reply);
end;

function TFPRedis.Pexpire(AKey: string; AMilliSeconds: Integer): Int64;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'PEXPIRE %s %d', [PChar(AKey), AMilliSeconds]);
  Result := replyIntegerV(reply);
end;

function TFPRedis.PexpireAt(AKey: string; AMilliSecondsTimeStamp: Int64): Int64;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'PEXPIREAT %s %d', [PChar(AKey), AMilliSecondsTimeStamp]);
  Result := replyIntegerV(reply);
end;

function TFPRedis.Ttl(AKey: string): Int64;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'TTL %s', [PChar(AKey)]);
  Result := replyIntegerV(reply);
end;

function TFPRedis.Pttl(AKey: string): Int64;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'PTTL %s', [PChar(AKey)]);
  Result := replyIntegerV(reply);
end;

function TFPRedis.SetBit(AKey: string; AOffset: Int64; AValue: Boolean): Boolean;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'SETBIT %s %d %d', [PChar(AKey), AOffset, specialize IfThen<Integer>(AValue, 1, 0)]);
  Result := replyInteger1(reply);
end;

function TFPRedis.GetBit(AKey: string; AOffset: Int64): Boolean;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'GETBIT %s %d', [PChar(AKey), AOffset]);
  Result := replyInteger1(reply);
end;

function TFPRedis.GetAllBits(AKey: string): TBytes;
var
  reply: PRedisReply;
  i: Integer;
begin
  reply:= redisCommand(FCtx, 'GET %s', [PChar(AKey)]);
  SetLength(Result, reply^.len);
  for i :=0 to reply^.len - 1 do begin
    Result[i] := Integer(reply^.str[i]);
  end;
  freeReplyObject(reply);
end;

procedure TFPRedis.GetStringOrBytes(AKey: String; out Str: String; out Bytes: TBytes);
var
  reply: PRedisReply;
  i: Integer;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'GET %s', [PChar(AKey)]);

  if (reply^._type = REDIS_REPLY_STRING) then begin
    SetLength(Bytes, reply^.len);
    for i:= 0 to reply^.len - 1 do begin
      Bytes[i] := Integer(reply^.str[i]);
    end;
    str := '';
    for i:= 0 to reply^.len - 1 do begin
      str += Char(Bytes[i]);
    end;
  end else begin
    processError(reply);
  end;
  freeReplyObject(reply);
end;

function TFPRedis.Unlink(AKey: string): Int64;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'UNLIKE %s', [PChar(AKey)]);
  Result := replyIntegerV(reply);
end;

function TFPRedis.Move(AKey: string; ADBIndex: Integer): Int64;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'MOVE %s %d', [PChar(AKey), ADBIndex]);
  Result := replyIntegerV(reply);
end;

function TFPRedis.GetSet(AKey: String; AValue: string): String;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply := redisCommand(FCtx, 'GETSET %s %s', [PChar(AKey), PChar(AValue)]);
  Result := replyString(reply);
end;

function TFPRedis.SetNx(AKey: string; AVAlue: string): Int64;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'SETNX %s %s', [PChar(AKey), PChar(AValue)]);
  Result := replyIntegerV(reply);
end;

function TFPRedis.SetEx(AKey: string; ASeconds: Integer; AValue: string): String;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'SETEX %s %d %s', [PChar(AKey), ASeconds, PChar(AValue)]);
  Result := replyString(reply);
end;

function TFPRedis.Decr(AKey: string): Int64;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'DECR %s', [PChar(AKey)]);
  Result := replyIntegerV(reply);
end;

function TFPRedis.DecrBy(AKey: string; ADecrement: Int64): Int64;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'DECRBY %s %d', [PChar(AKey), ADecrement]);
  Result := replyIntegerV(reply);
end;

function TFPRedis.Incr(AKey: string): Int64;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'INCR %s', [PChar(AKey)]);
  Result := replyIntegerV(reply);
end;

function TFPRedis.IncrBy(AKey: string; ADecrement: Int64): Int64;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'INCRBY %s %d', [PChar(AKey), ADecrement]);
  Result := replyIntegerV(reply);
end;

function TFPRedis.IncrByFloat(AKey: string; ADecrement: Double): Double;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'INCRBYFLOAT %s %f', [PChar(AKey), ADecrement]);
  Result := replyDoubleV(reply);
end;

function TFPRedis.Append(AKey: string; AValue: string): Int64;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'APPEND %s %s', [PChar(AKey), PChar(AValue)]);
  Result := replyIntegerV(reply);
end;

function TFPRedis.SubStr(AKey: string; AStart: Integer; AEnd: Integer): string;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'SUBSTR %s %d %d', [PChar(AKey), AStart, AEnd]);
  Result := replyString(reply);
end;

function TFPRedis.StrLen(AKey: string): Int64;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'STRLEN %s', [PChar(AKey)]);
  Result := replyIntegerV(reply);
end;

function TFPRedis.SetRange(AKey: string; AOffset: Int64; AValue: string): Int64;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'SETRANGE %s %d %s', [PChar(AKey), AOffset, PChar(AValue)]);
  Result := replyIntegerV(reply);
end;

function TFPRedis.GetRange(AKey: string; AStartOffset: Int64; AEndOffset: Int64): String;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'GETRANGE %s %d %d', [PChar(AKey), AStartOffset, AEndOffset]);
  Result := replyString(reply);
end;

function TFPRedis.PsetEx(AKey: string; AMilliSeconds: Int64; AValue: string): String;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'PSETEX %s %d %s', [PChar(AKey), AMilliSeconds, PChar(AValue)]);
  Result := replyString(reply);
end;

function TFPRedis.Persist(AKey: string): Int64;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'PERSIST %s', [PChar(AKey)]);
  Result := replyIntegerV(reply);
end;

function TFPRedis.Touch(AKey: string): Int64;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'TOUCH %s', [PChar(AKey)]);
  Result := replyIntegerV(reply);
end;

function TFPRedis.HSet(AKey: string; AField: String; AValue: String): Int64;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'HSET %s %s %s', [PChar(AKey), PChar(AField), PChar(AValue)]);
  Result := replyIntegerV(reply);
end;

function TFPRedis.HGet(AKey: string; AField: string): string;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'HGET %s %s', [PChar(AKey), PChar(AField)]);
  Result := replyString(reply);
end;

function TFPRedis.HSetNx(AKey: string; AField: String; AValue: string): Int64;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'HSETEX %s %s %s', [PChar(AKey), PChar(AField), PChar(AValue)]);
  Result := replyIntegerV(reply);
end;

function TFPRedis.HExists(AKey: string; AField: string): Boolean;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'HEXISTS %s %s', [PChar(AKey), PChar(AField)]);
  Result := replyInteger1(reply);
end;

function TFPRedis.HDel(AKey: string; AField: string): Int64;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'HDEL %s %s', [PChar(AKey), PChar(AField)]);
  Result := replyIntegerV(reply);
end;

function TFPRedis.HLen(AKey: string): Int64;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'HLEN %s', [PChar(AKey)]);
  Result := replyIntegerV(reply);
end;

function TFPRedis.HKeys(AKey: string): TStringArray;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'HKEYS %s', [PChar(AKey)]);
  Result := replyArrayString(reply);
end;

function TFPRedis.HVals(AKey: string): TStringArray;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'HVALS %s', [PChar(AKey)]);
  Result := replyArrayString(reply);
end;

function TFPRedis.HMSet(AKey: string; AMap: TFPRedisMap): Boolean;
var
  i: Integer;
  argv: array of PChar = nil;
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  SetLength(argv, 2 + AMap.Count * 2);
  argv[0] := 'HMSET';
  argv[1] := PChar(AKey);
  for i:= 0 to AMap.Count - 1 do begin
    argv[2 + i * 2] := PChar(AMap.Keys[i]);
    argv[2 + i * 2 + 1] := PChar(AMap.Data[i]);
  end;
  reply := redisCommandArgv(FCtx, Length(argv), PPChar(argv), nil);
  Result := replyStatusOK(reply);
end;

function TFPRedis.HMGet(AKey: string; AFields: TStringArray): TStringArray;
var
  i: Integer;
  argv: array of PChar = nil;
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  SetLength(argv, 2 + Length(AFields));
  argv[0] := 'HMGET';
  argv[1] := PChar(AKey);
  for i := 0 to Length(AFields) - 1 do begin
    argv[2 + i] := PChar(AFields[i]);
  end;
  reply:= redisCommandArgv(FCtx, Length(argv), PPChar(argv), nil);
  Result := replyArrayString(reply);
end;

function TFPRedis.HGetAll(AKey: String): TFPRedisMap;
var
  reply: PRedisReply;
  arr: TStringArray;
  i: Integer;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'HGETALL %s', [PChar(AKey)]);
  arr := replyArrayString(reply);
  Result := TFPRedisMap.Create;
  for i := 0 to (Length(arr) div 2 - 1) do begin
    Result.Add(arr[i * 2], arr[i * 2 + 1]);
  end;
end;

function TFPRedis.HIncrBy(AKey: string; AField: string; AValue: Int64): Int64;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'HINCRBY %s %s %d', [PChar(AKey), PChar(AField), AValue]);
  Result := replyIntegerV(reply);
end;

function TFPRedis.HIncrByFloat(AKey: string; AField: string; AValue: Double): Double;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'HINCRBYFLOAT %s %s %f', [PChar(AKey), PChar(AField), AValue]);
  Result := replyDoubleV(reply);
end;

function TFPRedis.LPush(AKey: string; AValue: TStringArray): Int64;
var
  reply: PRedisReply;
  argv: array of PChar = nil;
  i: Integer;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  SetLength(argv, 2 + Length(AValue));
  argv[0] := 'LPUSH';
  argv[1] := PChar(AKey);
  for i:=  0 to Length(AValue) - 1 do begin
    argv[2 + i] := PChar(AValue[i]);
  end;
  reply := redisCommandArgv(FCtx, Length(argv), PPChar(argv), nil);
  Result := replyIntegerV(reply);
end;

function TFPRedis.RPush(AKey: string; AValue: TStringArray): Int64;
var
  reply: PRedisReply;
  argv: array of PChar = nil;
  i: Integer;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  SetLength(argv, 2 + Length(AValue));
  argv[0] := 'RPUSH';
  argv[1] := PChar(AKey);
  for i:=  0 to Length(AValue) - 1 do begin
    argv[2 + i] := PChar(AValue[i]);
  end;
  reply := redisCommandArgv(FCtx, Length(argv), PPChar(argv), nil);
  Result := replyIntegerV(reply);
end;

function TFPRedis.LPushX(AKey: string; AValue: TStringArray): Int64;
var
  reply: PRedisReply;
  argv: array of PChar = nil;
  i: Integer;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  SetLength(argv, 2 + Length(AValue));
  argv[0] := 'LPUSHX';
  argv[1] := PChar(AKey);
  for i:=  0 to Length(AValue) - 1 do begin
    argv[2 + i] := PChar(AValue[i]);
  end;
  reply := redisCommandArgv(FCtx, Length(argv), PPChar(argv), nil);
  Result := replyIntegerV(reply);
end;

function TFPRedis.RPushX(AKey: string; AValue: TStringArray): Int64;
var
  reply: PRedisReply;
  argv: array of PChar = nil;
  i: Integer;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  SetLength(argv, 2 + Length(AValue));
  argv[0] := 'RPUSHX';
  argv[1] := PChar(AKey);
  for i:=  0 to Length(AValue) - 1 do begin
    argv[2 + i] := PChar(AValue[i]);
  end;
  reply := redisCommandArgv(FCtx, Length(argv), PPChar(argv), nil);
  Result := replyIntegerV(reply);
end;

function TFPRedis.LLen(AKey: string): Int64;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'LLEN %s', [PChar(AKey)]);
  Result := replyIntegerV(reply);
end;

function TFPRedis.LRange(Akey: string; AStart: Int64; AStop: Int64): TStringArray;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'LRANGE %s %d %d', [PChar(AKey), AStart, AStop]);
  Result := replyArrayString(reply);
end;

function TFPRedis.LTrim(AKey: string; AStart: Int64; AStop: Int64): string;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'LTRIM %s %d %d', [PChar(AKey), AStart, AStop]);
  Result := replyString(reply);
end;

function TFPRedis.LIndex(AKey: string; AIndex: Int64): String;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'LINDEX %s %d', [PChar(AKey), AIndex]);
  Result := replyString(reply);
end;

function TFPRedis.LSet(AKey: string; AIndex: Int64; AValue: string): string;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'LSET %s %d %s', [PChar(AKey), AIndex, PChar(AValue)]);
  Result := replyString(reply);
end;

function TFPRedis.LRem(AKey: string; ACount: Int64; AValue: string): Int64;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'LREM %s %d %s', [PChar(AKey), ACount, PChar(AValue)]);
  Result := replyIntegerV(reply);
end;

function TFPRedis.LPop(AKey: string): string;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'LPOP %s', [PChar(AKey)]);
  Result := replyString(reply);
end;

function TFPRedis.RPop(AKey: string): string;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'RPOP %s', [PChar(AKey)]);
  Result := replyString(reply);
end;

function TFPRedis.BLPop(AArgs: TStringArray): TStringArray;
var
  reply: PRedisReply;
  argv: array of PChar = nil;
  i: Integer;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  SetLength(argv, 1 + Length(AArgs));
  argv[0] := 'BLPOP';
  for i :=0 to Length(AArgs) - 1 do begin
    argv[1 + i] := PChar(AArgs[i]);
  end;
  reply:= redisCommandArgv(FCtx, Length(argv), PPChar(argv), nil);
  Result := replyArrayString(reply);
end;

function TFPRedis.BLPop(ATimeout: Integer; AKey: String): TStringArray;
begin
  Result := BLPop([AKey, ATimeout.ToString]);
end;

function TFPRedis.BRPop(AArgs: TStringArray): TStringArray;
var
  reply: PRedisReply;
  argv: array of PChar = nil;
  i: Integer;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  SetLength(argv, 1 + Length(AArgs));
  argv[0] := 'BRPOP';
  for i :=0 to Length(AArgs) - 1 do begin
    argv[1 + i] := PChar(AArgs[i]);
  end;
  reply:= redisCommandArgv(FCtx, Length(argv), PPChar(argv), nil);
  Result := replyArrayString(reply);
end;

function TFPRedis.BRPop(ATimeout: Integer; AKey: String): TStringArray;
begin
  Result := BRPop([AKey, ATimeout.ToString]);
end;

function TFPRedis.SAdd(AKey: string; AMembers: TStringArray): Int64;
var
  reply: PRedisReply;
  argv: array of PChar = nil;
  i: Integer;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  SetLength(argv, 2 + Length(AMembers));
  argv[0] := 'SADD';
  argv[1] := PChar(AKey);
  for i:= 0 to Length(AMembers) - 1 do begin
    argv[2 + i] := PChar(AMembers[i]);
  end;
  reply:= redisCommandArgv(FCtx, Length(argv), PPChar(argv), nil);
  Result := replyIntegerV(reply);
end;

function TFPRedis.SMembers(AKey: string): TStringArray;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'SMEMBERS %s', [PChar(AKey)]);
  Result := replyArrayString(reply);
end;

function TFPRedis.SRem(AKey: string; AMembers: TStringArray): Int64;
var
  reply: PRedisReply;
  argv: array of PChar = nil;
  i: Integer;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  SetLength(argv, 2 + Length(AMembers));
  argv[0] := 'SREM';
  argv[1] := PChar(AKey);
  for i:= 0 to Length(AMembers) - 1 do begin
    argv[2 + i] := PChar(AMembers[i]);
  end;
  reply:= redisCommandArgv(FCtx, Length(argv), PPChar(argv), nil);
  Result := replyIntegerV(reply);
end;

function TFPRedis.SPop(AKey: string): string;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'SPOP %s', [PChar(AKey)]);
  Result := replyString(reply);
end;

function TFPRedis.SPop(AKey: string; ACount: Int64): TStringArray;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'SPOP %s %d', [PChar(AKey), ACount]);
  Result := replyArrayString(reply);
end;

function TFPRedis.SMove(ASrcKey: string; ADestKey: string; AMember: string): Int64;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'SMOVE %s %s %s', [PChar(ASrcKey), PChar(ADestKey), PChar(AMember)]);
  Result := replyIntegerV(reply);
end;

function TFPRedis.SCard(AKey: string): Int64;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'SCARD %s', [PChar(AKey)]);
  Result := replyIntegerV(reply);
end;

function TFPRedis.SIsMember(AKey: string; AMember: string): Boolean;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'SISMEMBER %s %s', [PChar(AKey), PChar(AMember)]);
  Result := replyInteger1(reply);
end;

function TFPRedis.SInter(AKeys: TStringArray): TStringArray;
var
  reply: PRedisReply;
  argv: array of PChar = nil;
  i: Integer;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  SetLength(argv, 1 + Length(AKeys));
  argv[0] := 'SINTER';
  for i:= 0 to Length(AKeys) - 1 do begin
    argv[1 + i] := PChar(AKeys[i]);
  end;
  reply:= redisCommandArgv(FCtx, Length(argv), PPChar(argv), nil);
  Result := replyArrayString(reply);
end;

function TFPRedis.SUnion(AKeys: TStringArray): TStringArray;
var
  reply: PRedisReply;
  argv: array of PChar = nil;
  i: Integer;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  SetLength(argv, 1 + Length(AKeys));
  argv[0] := 'SUNION';
  for i:= 0 to Length(AKeys) - 1 do begin
    argv[1 + i] := PChar(AKeys[i]);
  end;
  reply:= redisCommandArgv(FCtx, Length(argv), PPChar(argv), nil);
  Result := replyArrayString(reply);
end;

function TFPRedis.SDiff(AKeys: TStringArray): TStringArray;
var
  reply: PRedisReply;
  argv: array of PChar = nil;
  i: Integer;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  SetLength(argv, 1 + Length(AKeys));
  argv[0] := 'SDIFF';
  for i:= 0 to Length(AKeys) - 1 do begin
    argv[1 + i] := PChar(AKeys[i]);
  end;
  reply:= redisCommandArgv(FCtx, Length(argv), PPChar(argv), nil);
  Result := replyArrayString(reply);
end;

function TFPRedis.SRandMember(AKey: string): string;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'SRANDMEMBER %s', [PChar(AKey)]);
  Result := replyString(reply);
end;

function TFPRedis.SRandMember(AKey: string; ACount: Int64): TStringArray;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'SRANDMEMBER %s %d', [PChar(AKey), ACount]);
  Result := replyArrayString(reply);
end;

function TFPRedis.ZAdd(AKey: string; AScore: Double; AMember: string): Int64;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'ZADD %s %f %s', [PChar(AKey), AScore, PChar(AMember)]);
  Result := replyIntegerV(reply);
end;

function TFPRedis.ZAdd(AKey: string; AMap: TFPZScoreMap): Int64;
var
  reply: PRedisReply;
  arcv: array of PChar = nil;
  i: Integer;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  SetLength(arcv, 2 + AMap.Count * 2);
  arcv[0] := 'ZADD';
  arcv[1] := PChar(AKey);
  for i := 0 to AMap.Count - 1 do begin
    arcv[2 + i * 2] := PChar(AMap.Data[i].ToString);
    arcv[2 + i * 2 + 1] := PChar(AMap.Keys[i]);
  end;
  reply:= redisCommandArgv(FCtx, Length(arcv), PPChar(arcv), nil);
  Result := replyIntegerV(reply);
end;

function TFPRedis.ZRange(AKey: string; AStart: Int64; AStop: Int64): TStringArray;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'ZRANGE %s %d %d', [PChar(AKey), AStart, AStop]);
  Result := replyArrayString(reply);
end;

function TFPRedis.ZRangeWithScore(AKey: string; AStart: Int64; AStop: Int64
  ): TFPZScoreMap;
var
  reply: PRedisReply;
  len: Int64;
  i: Int64;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'ZRANGE %s %d %d WITHSCORES', [PChar(AKey), AStart, AStop]);
  Result := TFPZScoreMap.Create;
  len := reply^.elements;
  for i:= 0 to (len div 2) - 1 do begin
    Result.Add(string(reply^.element[i * 2]^.str), string(reply^.element[i * 2 + 1]^.str).ToDouble);
  end;
  freeReplyObject(reply);
end;

function TFPRedis.ZRem(AKey: string; AMembers: TStringArray): Int64;
var
  reply: PRedisReply;
  arcv: array of PChar;
  i: Integer;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  SetLength(arcv, 2 + Length(AMembers));
  arcv[0] := 'ZREM';
  arcv[1] := PChar(AKey);
  for i := 0 to Length(AMembers) - 1 do begin
    arcv[2 + i] := PChar(AMembers[i]);
  end;
  reply:= redisCommandArgv(FCtx, Length(arcv), PPChar(arcv), nil);
  Result := replyIntegerV(reply);
end;

function TFPRedis.ZCount(AKey: string; AMin: Double; AMax: Double): Int64;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'ZCOUNT %s %f %f', [PChar(AKey), AMin, AMax]);
  Result := replyIntegerV(reply);
end;

function TFPRedis.ZCount(AKey: string; AMin: String; AMax: String): Int64;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'ZCOUNT %s %s %s', [PChar(AKey), PChar(AMin), PChar(AMax)]);
  Result := replyIntegerV(reply);
end;

function TFPRedis.ZCard(AKey: string): Int64;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'ZCARD %s', [PChar(AKey)]);
  Result := replyIntegerV(reply);
end;

function TFPRedis.ZIncrBy(AKey: string; AIncrement: Double; AMember: String): Double;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'ZINCRBY %s %f %s', [PChar(AKey), AIncrement, PChar(AMember)]);
  Result := replyDoubleV(reply);
end;

function TFPRedis.ZRank(AKey: string; AMember: string): Int64;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'ZRANK %s %s', [PChar(AKey), PChar(AMember)]);
  Result := replyIntegerV(reply);
end;

function TFPRedis.ZRevRank(AKey: string; AMember: string): Int64;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'ZREVRANK %s %s', [PChar(AKey), PChar(AMember)]);
  Result := replyIntegerV(reply);
end;

function TFPRedis.ZRevRange(AKey: string; AMember: string): TStringArray;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'ZREVRANGE %s %s', [PChar(AKey), PChar(AMember)]);
  Result := replyArrayString(reply);
end;

function TFPRedis.ZScore(AKey: string; AMember: string): Double;
var
  reply: PRedisReply;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply:= redisCommand(FCtx, 'ZSCORE %s %s', [PChar(AKey), PChar(AMember)]);
  Result := replyDoubleV(reply);
end;

function TFPRedis.Scan(ACursor: Int64): TStringArray;
var
  reply: PRedisReply;
  i: Integer;
begin
  if (FCtx = nil) then raise TFPRedisExeception.Create('Not connected to Redis Server.');
  reply := redisCommand(FCtx, 'SCAN %d MATCH * COUNT 10000', [ACursor]);
  SetLength(Result, reply^.element[1]^.elements);
  for i:= 0 to Length(Result) - 1 do begin
    Result[i] := string(reply^.element[1]^.element[i]^.str);
  end;
  freeReplyObject(reply);
end;

end.

