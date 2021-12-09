unit fpredis_dict;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF WINDOWS} windows {$ELSE} Unix {$ENDIF};

type

  {$I type_win.inc}

  PPDictEntry = ^PDictEntry;
  PDictEntry = ^TDictEntry;
  TDictEntry = record
    key: Pointer;
    val: Pointer;
    next: PDictEntry;
  end;

  THashFunction = function (const key: Pointer): cuint; cdecl;
  TKeyDup = function (privdata: Pointer; const key: Pointer): Pointer; cdecl;
  TValDup = function (privdata: Pointer; const obj: Pointer): Pointer; cdecl;
  TKeyCompare = function (privdata: Pointer; const key1: Pointer; const key2: Pointer): cint; cdecl;
  TKeyDestructor = procedure (privdata: Pointer; key: Pointer); cdecl;
  TValDestructor = procedure (privdata: Pointer; obj: Pointer); cdecl;

  PDictType = ^TDictType;
  TDictType = record
    hashFunction: THashFunction;
    keyDup: TKeyDup;
    valDup: TValDup;
    keyCompare: TKeyCompare;
    keyDestructor: TKeyDestructor;
    valDestructor: TValDestructor;
  end;

  PDict = ^TDict;
  TDict = record
    table: PPDictEntry;
    &type: PDictType;
    size: culong;
    sizemask: culong;
    used: culong;
    privdata: Pointer;
  end;

  PDictIterator = ^TDictIterator;
  TDictIterator = record
    ht: PDict;
    index:cint;
    entry: PDictEntry;
    nextEntry: PDictEntry;
  end;


function dictGenHashFunction(const bug: pcuchar; len: cint): cuint; cdecl; external;
function dictCreate(&type: PDictType; privDataPtr: Pointer): PDict; cdecl; external;
function dictExpand(ht: PDict; size: culong): cint; cdecl; external;
function dictAdd(ht: PDict; key: Pointer; val: Pointer): cint; cdecl; external;
function dictReplace(ht: PDict; key: Pointer; val: Pointer): cint; cdecl; external;
function dictDelete(ht: PDict; const key: Pointer): cint; cdecl; external;
procedure dictRelease(ht: PDict); cdecl; external;
function dictFind(ht: PDict; const key: Pointer): PDictEntry; cdecl; external;
procedure dictInitIterator(iter: PDictIterator; ht: PDict); cdecl; external;
function dictNext(iter: PDictIterator): PDictEntry; cdecl; external;

implementation

end.

