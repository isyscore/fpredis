unit fpredis_read;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF WINDOWS} windows {$ELSE} Unix {$ENDIF};

const
  REDIS_ERR = -1;
  REDIS_OK = 0;

  REDIS_ERR_IO = 1; { Error in read or write }
  REDIS_ERR_EOF = 3; { End of file }
  REDIS_ERR_PROTOCOL = 4; { Protocol error }
  REDIS_ERR_OOM = 5; { Out of memory }
  REDIS_ERR_TIMEOUT = 6; { Timed out }
  REDIS_ERR_OTHER = 2; { Everything else... }

  REDIS_REPLY_STRING = 1;
  REDIS_REPLY_ARRAY = 2;
  REDIS_REPLY_INTEGER = 3;
  REDIS_REPLY_NIL = 4;
  REDIS_REPLY_STATUS = 5;
  REDIS_REPLY_ERROR = 6;
  REDIS_REPLY_DOUBLE = 7;
  REDIS_REPLY_BOOL = 8;
  REDIS_REPLY_MAP = 9;
  REDIS_REPLY_SET = 10;
  REDIS_REPLY_ATTR = 11;
  REDIS_REPLY_PUSH = 12;
  REDIS_REPLY_BIGNUM = 13;
  REDIS_REPLY_VERB = 14;

type

  {$I type_win.inc}

  PPRedisReadTask = ^PRedisReadTask;
  PRedisReadTask = ^TRedisReadTask;
  TRedisReadTask = record
    _type: cint;
    elements: clonglong; { number of elements in multibulk container }
    idx: cint; { index in parent (array) object }
    obj: Pointer; { holds user-generated value for a read task }
    parent: PRedisReadTask; { parent task }
    privdata: Pointer; { user-settable arbitrary field }
  end;

  TCreateString = function (const task: PRedisReadTask; c: PChar; s: size_t): Pointer; cdecl;
  TCreateArray = function (const task: PRedisReadTask; s: size_t): Pointer; cdecl;
  TCreateInteger = function (const task: PRedisReadTask; s: clonglong): Pointer; cdecl;
  TCreateDouble = function (const task: PRedisReadTask; d: cdouble; c: PChar; s: size_t): Pointer; cdecl;
  TCreateNil = function (const task: PRedisReadTask): Pointer; cdecl;
  TCreateBool = function (const task: PRedisReadTask; i: cint): Pointer; cdecl;
  TFreeObject = procedure (p: Pointer); cdecl;

  PRedisReplyObjectFunctions = ^TRedisReplyObjectFunctions;
  TRedisReplyObjectFunctions = record
    createString: TCreateString;
    createArray: TCreateArray;
    createInteger: TCreateInteger;
    createDouble: TCreateDouble;
    createNil: TCreateNil;
    createBool: TCreateBool;
    freeObject: TFreeObject;
  end;

  PRedisReader = ^TRedisReader;
  TRedisReader = record
    err: cint;  { Error flags, 0 when there is no error }
    errstr: array[0..127] of Char; { String representation of error when applicable }
    buf: PChar; { Read buffer }
    pos: size_t; { Buffer cursor }
    len: size_t; { Buffer length  }
    maxbuf: size_t; { Max length of unused buffer }
    maxelements: clonglong; { Max multi-bulk elements }
    task: PPRedisReadTask;
    tasks: cint;
    ridx: cint; { Index of current read task }
    reply: Pointer; { Temporary reply pointer }
    fn: PRedisReplyObjectFunctions;
    privdata: Pointer;
  end;

function redisReaderCreateWithFunctions(fn: PRedisReplyObjectFunctions): PRedisReader; cdecl; external;
procedure redisReaderFree(r: PRedisReader); cdecl; external;
function redisReaderFeed(r: PRedisReader; const buf: PChar; len: size_t): cint; cdecl; external;
function redisReaderGetReply(r: PRedisReader; reply: PPointer): cint; cdecl; external;

implementation

end.

