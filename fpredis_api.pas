unit fpredis_api;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF WINDOWS} windows {$ELSE} Unix {$ENDIF},
  fpredis_read, fpredis_dict, Sockets, fpredis_sds;


const
(* Connection type can be blocking or non-blocking and is set in the
 * least significant bit of the flags field in redisContext. *)
  REDIS_BLOCK = $1;

(* Connection may be disconnected before being free'd. The second bit
 * in the flags field is set when the context is connected. *)
  REDIS_CONNECTED = $2;

(* The async API might try to disconnect cleanly and flush the output
 * buffer and read all subsequent replies before disconnecting.
 * This flag means no new commands can come in and the connection
 * should be terminated once all replies have been read. *)
  REDIS_DISCONNECTING = $4;

(* Flag specific to the async API which means that the context should be clean
 * up as soon as possible. *)
  REDIS_FREEING = $8;

(* Flag that is set when an async callback is executed. *)
  REDIS_IN_CALLBACK = $10;

(* Flag that is set when the async context has one or more subscriptions. *)
  REDIS_SUBSCRIBED = $20;

(* Flag that is set when monitor mode is active *)
  REDIS_MONITORING = $40;

(* Flag that is set when we should set SO_REUSEADDR before calling bind() *)
  REDIS_REUSEADDR = $80;

(*
 * Flag that indicates the user does not want the context to
 * be automatically freed upon error
 *)
  REDIS_NO_AUTO_FREE = $200;

(* Flag that indicates the user does not want replies to be automatically freed *)
  REDIS_NO_AUTO_FREE_REPLIES = $400;

  REDIS_KEEPALIVE_INTERVAL = 15; (* seconds *)

(* number of times we retry to connect in the case of EADDRNOTAVAIL and
 * SO_REUSEADDR is being used. *)
  REDIS_CONNECT_RETRIES = 10;

  REDIS_OPT_NONBLOCK = $01;
  REDIS_OPT_REUSEADDR = $02;

(*
 * Don't automatically free the async object on a connection failure,
 * or other implicit conditions. Only free on an explicit call to disconnect() or free()
 *)
  REDIS_OPT_NOAUTOFREE = $04;

(* Don't automatically intercept and free RESP3 PUSH replies. *)
  REDIS_OPT_NO_PUSH_AUTOFREE = $08;

(*
 * Don't automatically free replies
 *)
  REDIS_OPT_NOAUTOFREEREPLIES = $10;


type
  {$I type_win.inc}

  TRedisFD = {$IFDEF win32}cint{$ELSE} {$IFDEF win64}culonglong{$ELSE}culong{$ENDIF} {$ENDIF};

  PRedisContextFuncs = ^TRedisContextFuncs;

  TRedisConnectionType = (
    REDIS_CONN_TCP,
    REDIS_CONN_UNIX,
    REDIS_CONN_USERFD);

  TTop = record
    host: PChar;
    source_addr: PChar;
    port: cint;
  end;

  TUnixSock = record
    path: PChar;
  end;

  PRedisAsyncContext = ^TRedisAsyncContext;

  TFreePrivdata = procedure (p: Pointer); cdecl;
  TRedisPushFn = procedure (p1: Pointer; p2: Pointer); cdecl;
  TRedisAsyncPushFn = procedure (c: PRedisAsyncContext; p: Pointer); cdecl;

  PRedisContext = ^TRedisContext;
  TRedisContext = record
    funcs: PRedisContextFuncs;  { Function table }
    err: cint; { Error flags, 0 when there is no error }
    errstr: array[0..127] of Char; { String representation of error when applicable }
    fd: TRedisFD;
    flags: cint;
    obuf: PChar; { Write buffer }
    reader: PRedisReader; { Protocol reader }
    connection_type: TRedisConnectionType;
    connect_timeout: ptimeval;
    command_timeout: ptimeval;
    top: TTop;
    unix_sock: TUnixSock;

    { For non-blocking connect }
    saddr: psockaddr;
    addrlen: size_t;

    (* Optional data and corresponding destructor users can use to provide
     * context to a given redisContext.  Not used by hiredis. *)
    privdata: Pointer;
    free_privdata: TFreePrivdata;

    (* Internal context pointer presently used by hiredis to manage
     * SSL connections. *)
    privctx: Pointer;

    (* An optional RESP3 PUSH handler *)
    push_cb: TRedisPushFn;
  end;

  TFreePrivctx = procedure(p: Pointer); cdecl;
  TAsyncRead = procedure (ctx: PRedisAsyncContext); cdecl;
  TAsyncWrite = procedure (ctx: PRedisAsyncContext); cdecl;
  TRead = function (ctx: PRedisContext; c: PChar; s: size_t): ssize_t; cdecl;
  TWrite = function (ctx: PRedisContext): ssize_t; cdecl;

  TRedisContextFuncs = record
    free_privctx: TFreePrivctx;
    async_read: TAsyncRead;
    async_write: TAsyncWrite;
    read: TRead;
    write: TWrite;
  end;


  PPRedisReply = ^PRedisReply;
  PRedisReply = ^TRedisReply;
  TRedisReply = record
    _type: cint; { REDIS_REPLY_* }
    _integer: clonglong; { The integer when type is REDIS_REPLY_INTEGER }
    dval: cdouble; { The double when type is REDIS_REPLY_DOUBLE }
    len: size_t; { Length of string }
    str: PChar; (* Used for REDIS_REPLY_ERROR, REDIS_REPLY_STRING
                   REDIS_REPLY_VERB, REDIS_REPLY_DOUBLE (in additional to dval),
                   and REDIS_REPLY_BIGNUM. *)
    vtype: array[0..3] of Char; (* Used for REDIS_REPLY_VERB, contains the null
                                   terminated 3 character content type, such as "txt". *)
    elements: size_t; { number of elements, for REDIS_REPLY_ARRAY }
    element: PPRedisReply;
  end;


  TEndpointTop = record
    source_addr: PChar;
    ip: PChar;
    port: cint;
  end;

  TEndpoint = record
    top: TEndpointTop;
    unix_socket: PChar;
    fs: TRedisFD;
  end;

  PRedisOptions = ^TRedisOptions;
  TRedisOptions = record
    _type: cint;
    options: cint;
    connect_timeout: ptimeval;
    command_timeout: ptimeval;
    endpoint: TEndpoint;
    privdata: Pointer;
    free_privdata: TFreePrivdata;
    push_cb: TRedisPushFn;
    async_push_cb: TRedisAsyncPushFn;
  end;

  TRedisCallbackFn = procedure (ctx: PRedisAsyncContext; p1: Pointer; p2: Pointer); cdecl;

  PRedisCallback = ^TRedisCallback;
  TRedisCallback = record
    next: PRedisCallback;  { simple singly linked list }
    fn: TRedisCallbackFn;
    pending_subs: cint;
    privdata: Pointer;
  end;

  TRedisCallbackList = record
    head: PRedisCallback;
    tail: PRedisCallback;
  end;

  TRedisDisconnectCallback = procedure (const ctx: PRedisAsyncContext; status: cint); cdecl;
  TRedisConnectCallback = procedure (const ctx: PRedisAsyncContext; status: cint); cdecl;
  TRedisTimerCallback = procedure (timer: Pointer; privdata: Pointer); cdecl;
  TDataCleanup = procedure (privdata: Pointer); cdecl;
  TAddRead = procedure (privdata: Pointer); cdecl;
  TDelRead = procedure (privdata: Pointer); cdecl;
  TAddWrite = procedure (privdata: Pointer); cdecl;
  TDelWrite = procedure (privdata: Pointer); cdecl;
  TCleanup = procedure (privdata: Pointer); cdecl;
  TScheduleTimer = procedure (privdata: Pointer; tv: timeval); cdecl;

  TAsyncEv = record
    data: Pointer;
    addRead: TAddRead;
    delRead: TDelRead;
    addWrite: TAddWrite;
    delWrite: TDelWrite;
    cleanup: TCleanup;
    scheduleTimer: TScheduleTimer;
  end;

  TAsyncSub = record
    invalid: TRedisCallbackList;
    channels: PDict;
    patterns: PDict;
  end;

  TRedisAsyncContext = Record
    c: TRedisContext;
    err: cint;
    errstr: PChar;
    data: Pointer;
    dataCleanup: TDataCleanup;
    ev: TAsyncEv;
    onDisconnect: TRedisDisconnectCallback;
    onConnect: TRedisConnectCallback;
    replies: TRedisCallbackList;
    saddr: psockaddr;
    addrlen: size_t;
    sub: TAsyncSub;
    push_cb: TRedisAsyncPushFn;
  end;

  PRedisSSLContextError = ^TRedisSSLContextError;
  TRedisSSLContextError = (
    REDIS_SSL_CTX_NONE = 0,                     { No Error }
    REDIS_SSL_CTX_CREATE_FAILED,                { Failed to create OpenSSL SSL_CTX }
    REDIS_SSL_CTX_CERT_KEY_REQUIRED,            { Client cert and key must both be specified or skipped }
    REDIS_SSL_CTX_CA_CERT_LOAD_FAILED,          { Failed to load CA Certificate or CA Path }
    REDIS_SSL_CTX_CLIENT_CERT_LOAD_FAILED,      { Failed to load client certificate }
    REDIS_SSL_CTX_PRIVATE_KEY_LOAD_FAILED,      { Failed to load private key }
    REDIS_SSL_CTX_OS_CERTSTORE_OPEN_FAILED,     { Failed to open system certifcate store }
    REDIS_SSL_CTX_OS_CERT_ADD_FAILED            { Failed to add CA certificates obtained from system to the SSL context }
  );

  PSslSt = ^TSslSt;
  TSslSt = record

  end;

  PRedisSSLContext = ^TRedisSSLContext;
  TRedisSSLContext = record

  end;

const
  {$IFDEF win32}
  REDIS_INVALID_FD = -1;
  {$ELSE}
  REDIS_INVALID_FD = TRedisFD(not 0);
  {$ENDIF}

function redisConnect(const ip: PChar; port: cint): PRedisContext; cdecl; external;
function redisConnectWithOptions(const options: PRedisOptions): PRedisContext; cdecl; external;
function redisConnectUnix(const path: PChar): PRedisContext; cdecl; external;
function redisConnectUnixWithTimeout(const path: PChar; const tv: timeval): PRedisContext; cdecl; external;
function redisConnectUnixNonBlock(const path: PChar): PRedisContext; cdecl; external;
function redisConnectFd(fd: TRedisFD): PRedisContext; cdecl; external;
function redisConnectWithTimeout(const ip: PChar; port: cint; const tv: timeval): PRedisContext; cdecl; external;
function redisConnectNonBlock(const ip: PChar; port: cint): PRedisContext; cdecl; external;
function redisConnectBindNonBlock(const ip: PChar; port: cint; const source_addr: PChar): PRedisContext; cdecl; external;
function redisConnectBindNonBlockWithReuse(const ip: PChar; port: cint; const source_addr: PChar): PRedisContext; cdecl; external;
function redisReconnect(c: PRedisContext): cint; cdecl; external;
function redisSetPushCallback(c: PRedisContext; fn: TRedisPushFn): TRedisPushFn; cdecl; external;
function redisSetTimeout(c: PRedisContext; const tv: timeval): cint; cdecl; external;
function redisEnableKeepAlive(c: PRedisContext): cint; cdecl; external;
procedure redisFree(c: PRedisContext); cdecl; external;
function redisFreeKeepFd(c: PRedisContext): TRedisFD; cdecl; external;
function redisBufferRead(c: PRedisContext): cint; cdecl; external;
function redisBufferWrite(c: PRedisContext; done: pcint): cint; cdecl; external;
function redisGetReply(c: PRedisContext; reply: PPointer): cint; cdecl; external;
function redisGetReplyFromReader(c: PRedisContext; reply: PPointer): cint; cdecl; external;
function redisAppendFormattedCommand(c: PRedisContext; const cmd: PChar; len: size_t): cint; cdecl; external;
function redisvAppendCommand(c: PRedisContext; const format: PChar): cint; cdecl; varargs; external;
function redisAppendCommand(c: PRedisContext; const format: PChar; args: array of const): cint; cdecl; external;
function redisAppendCommandArgv(c: PRedisContext; argc: cint; const argv: PPChar; const argvlen: pSize_t): cint; cdecl; external;
function redisvCommand(c: PRedisContext; const format: PChar): Pointer; cdecl; varargs; external;
function redisCommand(c: PRedisContext; const format: PChar; args: array of const): Pointer; cdecl; external;
function redisCommandArgv(c: PRedisContext; argc: cint; const argv: PPChar; const argvlen: pSize_t): Pointer; cdecl; external;

function redisReaderCreate(): PRedisReader; cdecl; external;
procedure freeReplyObject(reply: Pointer); cdecl; external;
function redisvFormatCommand(target: PPChar; const format: PChar): cint; cdecl; varargs; external;
function redisFormatCommand(target: PPChar; const format: PChar; args: array of const): cint; cdecl; external;
function redisFormatCommandArgv(target: PPChar; argc: cint; const argv: PPChar; const argvlen: pSize_t): cint; cdecl; external;
function redisFormatSdsCommandArgv(target: PSds; argc: cint; const argv: PPChar; const argvlen: pSize_t): cint; cdecl; external;
procedure redisFreeCommand(cmd: PChar); cdecl; external;
procedure redisFreeSdsCommand(cmd: TSds); cdecl; external;

function redisAsyncConnectWithOptions(const options: PRedisOptions): PRedisAsyncContext; cdecl; external;
function redisAsyncConnect(const ip: PChar; port: cint): PRedisAsyncContext; cdecl; external;
function redisAsyncConnectBind(const ip: PChar; port: cint; const source_addr: PChar): PRedisAsyncContext; cdecl; external;
function redisAsyncConnectBindWithReuse(const ip: PChar; port: cint; const source_addr: PChar): PRedisAsyncContext; cdecl; external;
function redisAsyncConnectUnix(const path: PChar): PRedisAsyncContext; cdecl; external;
function redisAsyncSetConnectCallback(ac: PRedisAsyncContext; fn: TRedisConnectCallback): cint; cdecl; external;
function redisAsyncSetDisconnectCallback(ac: PRedisAsyncContext; fn: TRedisDisconnectCallback): cint; cdecl; external;
function redisAsyncSetPushCallback(ac: PRedisAsyncContext; fn: TRedisAsyncPushFn): TRedisAsyncPushFn; cdecl; external;
function redisAsyncSetTimeout(ac: PRedisAsyncContext; tv: timeval): cint; cdecl; external;
procedure redisAsyncDisconnect(ac: PRedisAsyncContext); cdecl; external;
procedure redisAsyncFree(ac: PRedisAsyncContext); cdecl; external;
procedure redisAsyncHandleRead(ac: PRedisAsyncContext); cdecl; external;
procedure redisAsyncHandleWrite(ac: PRedisAsyncContext); cdecl; external;
procedure redisAsyncHandleTimeout(ac: PRedisAsyncContext); cdecl; external;
procedure redisAsyncRead(ac: PRedisAsyncContext); cdecl; external;
procedure redisAsyncWrite(ac: PRedisAsyncContext); cdecl; external;
function redisvAsyncCommand(ac: PRedisAsyncContext; fn: TRedisCallbackFn; privdata: Pointer; const format: PChar): cint; cdecl; varargs; external;
function redisAsyncCommand(ac: PRedisAsyncContext; fn: TRedisCallbackFn; privdata: Pointer; const format: PChar; args: array of const): cint; cdecl; external;
function redisAsyncCommandArgv(ac: PRedisAsyncContext; fn: TRedisCallbackFn; privdata: Pointer; argc: cint; const argv: PPChar; const argvlen: pSize_t): cint; cdecl; external;
function redisAsyncFormattedCommand(ac: PRedisAsyncContext; fn: TRedisCallbackFn; privdata: Pointer; const cmd: PChar; len: size_t): cint; cdecl; external;

function redisSSLContextGetError(error: TRedisSSLContextError): PChar; cdecl; external;
function redisInitOpenSSL(): cint; cdecl; external;

function redisCreateSSLContext(
  const cacert_filename: PChar;
  const capath: PChar;
  const cert_filename: PChar;
  const private_key_filename: PChar;
  const server_name: PChar;
  error: PRedisSSLContextError): PRedisSSLContext; cdecl; external;

procedure redisFreeSSLContext(ctx: PRedisSSLContext); cdecl; external;
function redisInitiateSSLWithContext(c: PRedisContext; sslc: PRedisSSLContext): cint; cdecl; external;
function redisInitiateSSL(c: PRedisContext; ssl: PSslSt): cint; cdecl; external;

procedure redisNetClose(c: PRedisContext); cdecl; external;
function redisNetRead(c: PRedisContext; buf: PChar; bufcap: size_t): ssize_t; cdecl; external;
function redisNetWrite(c: PRedisContext): ssize_t; cdecl; external;
function redisCheckSocketError(c: PRedisContext): cint; cdecl; external;
function redisContextSetTimeout(c: PRedisContext; const tv: timeval): cint; cdecl; external;
function redisContextConnectTcp(c: PRedisContext; const addr: PChar; port: cint; const timeout: ptimeval): cint; cdecl; external;
function redisContextConnectBindTcp(c: PRedisContext; const addr: PChar; port: cint; const timeout: ptimeval; const source_addr: PChar): cint; cdecl; external;
function redisContextConnectUnix(c: PRedisContext; const path: PChar; const timeout: ptimeval): cint; cdecl; external;
function redisKeepAlive(c: PRedisContext; interval: cint): cint; cdecl; external;
function redisCheckConnectDone(c: PRedisContext; completed: pcint): cint; cdecl; external;
function redisSetTcpNoDelay(c: PRedisContext): cint; cdecl; external;


implementation

end.

