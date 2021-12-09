{$IFDEF WINDOWS}
  {$LINKLIB libhiredis_x86-64_win64.a}
{$ELSE}
  {$IFDEF DARWIN}
    {$LINKLIB libhiredis_x86-64_darwin.a}
  {$ELSE}
    {$LINKLIB libhiredis_x86-64_linux.a}
  {$ENDIF}
{$ENDIF}
