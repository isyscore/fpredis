unit nativetest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpredis_api, fpredis_read;

procedure doTest();

implementation

procedure doTest();
var
  c: PRedisContext;
  reply: PRedisReply;
  sampleJson: String = '{"code": 200, "message":"OK"}';
begin
  c := redisConnect('10.30.30.78', 26379);
  if (c^.err <> REDIS_OK) then begin
    WriteLn('Connect Error: ', c^.errstr);
    Exit;
  end;
  WriteLn('Connected.');
  reply:= redisCommand(c, 'AUTH %s', ['ZljIsysc0re123']);

  if (reply^._type = REDIS_REPLY_ERROR) then begin
    WriteLn('Auth Failed.');
  end else begin
    WriteLn('Auth Passed.');
  end;

  freeReplyObject(reply);

  reply := redisCommand(c, 'SELECT 7', []);
  if (reply^._type <> REDIS_REPLY_STATUS) then begin
    WriteLn(reply^.str);
  end else begin
    WriteLn('Select 7 OK.');
  end;

  freeReplyObject(reply);

  reply := redisCommand(c, 'SET sample:test_redis %s', [PChar(sampleJson)]);
  WriteLn(reply^.str);
  if (reply^._type <> REDIS_REPLY_STATUS) then begin
    WriteLn(reply^.str);
  end else begin
    WriteLn('SET asmple:test_redis OK.');
  end;

  freeReplyObject(reply);

  reply := redisCommand(c, 'GET sample:test_redis', []);
  WriteLn(reply^._type);
  WriteLn(reply^.str);
  freeReplyObject(reply);

  reply:= redisCommand(c, 'EXISTS sample:test_redis1', []);
    WriteLn(reply^._type);
  WriteLn(reply^._integer);
  freeReplyObject(reply);


  reply := redisCommand(c, 'SET sample:test_redis1 0', [PChar(sampleJson)]);
  if (reply^._type <> REDIS_REPLY_STATUS) then begin
    WriteLn(reply^.str);
  end else begin
    WriteLn('SET asmple:test_redis1 OK.');
  end;
  freeReplyObject(reply);

    reply := redisCommand(c, 'GET sample:test_redis1', []);
  WriteLn(reply^._type);
  WriteLn(reply^.str);
  freeReplyObject(reply);


  redisFree(c);

end;

end.

