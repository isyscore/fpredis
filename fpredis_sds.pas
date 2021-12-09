unit fpredis_sds;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF WINDOWS} windows {$ELSE} Unix {$ENDIF};

const
  SDS_MAX_PREALLOC = 1024 * 1024;

  SDS_TYPE_5 = 0;
  SDS_TYPE_8 = 1;
  SDS_TYPE_16 = 2;
  SDS_TYPE_32 = 3;
  SDS_TYPE_64 = 4;
  SDS_TYPE_MASK = 7;
  SDS_TYPE_BITS = 3;

type
  {$I type_win.inc}

  PSds = ^TSds;
  TSds = PChar;

  uint8_t = byte;
  uint16_t = word;
  uint32_t = dword;
  uint64_t = qword;

  TSdshdr5 = packed record
    flags: cuchar;
    buf: array of Char;
  end;

  TSdshdr8 = packed record
    len: uint8_t; { used }
    alloc: uint8_t; { excluding the header and null terminator }
    flags: cuchar; { 3 lsb of type, 5 unused bits }
    buf: array of Char;
  end;

  TSdshdr16 = packed record
    len: uint16_t;  { used }
    alloc: uint16_t; { excluding the header and null terminator }
    flags: cuchar; { 3 lsb of type, 5 unused bits }
    buf: array of Char;
  end;

  TSdshdr32 = packed record
    len: uint32_t;  { used }
    alloc: uint32_t; { excluding the header and null terminator }
    flags: cuchar; { 3 lsb of type, 5 unused bits }
    buf: array of Char;
  end;

  TSdshdr64 = packed record
    len: uint64_t;  { used }
    alloc: uint64_t; { excluding the header and null terminator }
    flags: cuchar; { 3 lsb of type, 5 unused bits }
    buf: array of Char;
  end;

implementation

end.

