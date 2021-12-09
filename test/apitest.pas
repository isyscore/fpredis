unit apitest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpredis;

procedure doTest();

implementation

procedure doTest();
var
  api: TFPRedis;
  b: Boolean;
  keyls: string = 'sample:ls';
  keyhs: string = 'sample:hs';
  key: string = 'sample:test_redis';
  keybit: string = 'sample:test_bit';
  keyint: String = 'sample:test_int';
  sampleJson: String = '{"code": 200, "message":"OK"}';
  s: string;
  i: Int64;
  arr: TStringArray;
  m: TFPRedisMap;
begin
  api := TFPRedis.Create('10.30.30.78', 26379, 'ZljIsysc0re123', 7);
  b := api.Connect();
  WriteLn('Connected: ', b);

  b := api.Exists(key);
  WriteLn('Exists: ', b);

  if (b) then begin
    b := api.Delete(key);
    WriteLn('Delete: ', b);
  end;

  b := api.&Set(key, sampleJson);
  WriteLn('Set: ', b);

  s := api.Get(key);
  WriteLn('Get: ', s);

  s := api.&Type(key);
  WriteLn('Type: ', s);

  s := api.Echo('2333');
  WriteLn('Echo: ', s);

  i := api.Expire(key, 80);
  WriteLn(i);

  i := api.Pttl(key);
  WriteLn(i);

  b := api.SetBit(keybit, 10086, True);
  WriteLn(b);
  b := api.GetBit(keybit, 10086);
  WriteLn(b);
  b := api.GetBit(keybit, 100);
  WriteLn(b);

  arr := api.Keys('sample:test*');
  for i := 0 to Length(arr) - 1 do begin
    WriteLn(arr[i]);
  end;

  api.&Set(keyint, '10');
  i := api.Incr(keyint);
  WriteLn(i);
  s := api.SubStr(key, 1, 2);
  WriteLn(s);

  i := api.StrLen(key);
  WriteLn(i);

  api.HSet(keyhs, 'k2', 'v');

  s := api.HGet(keyhs, 'key1');
  WriteLn(s);

  arr := api.HKeys(keyhs);
  for i:= 0 to Length(arr) - 1 do begin
    WriteLn(arr[i]);
  end;

   m := TFPRedisMap.Create;
   m.Add('k_0', 'v_0');
   m.Add('k_1', 'v_1');
   m.Add('k_2', 'v_2');
   b := api.HMSet(keyhs, m);
   m.Free;
   WriteLn('HMSet:', b);

  arr := api.HMGet(keyhs, ['k_0', 'k_1', 'k_2']);
  for i:= 0 to Length(arr) - 1 do begin
     WriteLn(arr[i]);
  end;

  m := api.HGetAll(keyhs);
  for i:= 0 to m.Count - 1 do begin
    WriteLn('key:', m.Keys[i], ', val:', m.Data[i]);
  end;
  m.Free;

  i := api.RPush(keyls, ['a', 'b', 'c']);
  WriteLn(i);

  i := api.LLen(keyls);
  WriteLn(i);

  api.Free;
end;

end.

