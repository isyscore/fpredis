{$IFDEF WINDOWS}
  cint = integer;
  clonglong = longlong;
  cdouble = double;
  cuint = UINT;
  culong = ULONG;
  cuchar = UCHAR;
  pcuchar = PUCHAR;
  culonglong = ULONGLONG;
  pcint = PINT;
  pSize_t = ^SIZE_T;

  time_t = int64;
  suseconds_t = longint;

  ptimeval = ^timeval;
  timeval = record
    tv_sec: time_t;
    tv_usec: suseconds_t;
  end;

{$ENDIF}
