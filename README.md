[![Build Status](https://github.com/redis/hiredis/actions/workflows/build.yml/badge.svg)](https://github.com/redis/hiredis/actions/workflows/build.yml)

# FPREDIS

FPREDIS is a FPC client library for the [Redis](https://redis.io/) database.

It's a wrapper for [HIREDIS](https://github.com/redis/hiredis) and provides the full feature.

### Basic Usage

```pascal
var
  api: TFPRedis;
  key: String = 'mykey';
  b: Boolean;
  s: String;
begin
  api := TFPRedis.Create('127.0.0.1', 6379, 'mypassword');
  b := api.Connect();
  WriteLn('Connected: ', b);

  b := api.&Set(key, 'myvalue');
  WriteLn('Set: ', b);

  b := api.Exists(key);
  WriteLn('Exists: ', b);

  s := api.Get(key);
  WriteLn('Get: ', s);

  api.Free;
end.
```

### OS Support

All Unix-bases OS are supported.




