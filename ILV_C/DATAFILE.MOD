IMPLEMENTATION MODULE DataFilesInOut;
FROM RealInOut IMPORT WriteReal,ReadReal;
FROM InOut IMPORT OpenInput,OpenOutput,CloseInput,CloseOutput,
                  ReadInt,WriteInt,WriteString,WriteLn,ReadString;
FROM LogiFile IMPORT WriteNBytes,Create,File,Close,Open,ReadNBytes,
                     OpenMode,EndFile,Reset,GetPos,SetPos;
FROM SYSTEM IMPORT ADDRESS,ADR;
FROM BinFiles IMPORT BinFileActions,ReadBinFileHeader,WriteBinFileHeader;
FROM Graphics IMPORT ScreenMode;

(***************************************************************************)
(***    About      : INPUT / OUTPUT OF 1D ASCII (SGPLOT)  DATA FILES     ***)
(***    Author     : H.S.                                                ***)
(***    Date       : 20 - 03 -1992                                       ***)
(***    System     : LOGITECH MODULA2                                    ***)
(***    Last change: 22 - 03 -1992  BY H.S                               ***)
(***************************************************************************)    

PROCEDURE RealArrayOut(ap:INTEGER; x:ARRAY OF REAL);
VAR format,xaxis : INTEGER;
    xmin,xinc    : REAL;
BEGIN
  WriteString('Write real-array to ASCII-file :');WriteLn;
  WriteString('      1. Normal ASCII file');WriteLn;
  WriteString('      2. SGplot file format');WriteLn;
  WriteString('      3. Cancel file output');WriteLn;
  WriteString('Your choice [1..3] : ');ReadInt(format);WriteLn;
  IF format=1 THEN
    AsciiOut(ap,x);
  END;
  IF format=2 THEN
    WriteString('Define x-axis [1=yes] : ');ReadInt(xaxis);WriteLn;
    IF xaxis#1 THEN  
      SGFileOut(ap,x);
    ELSE
      WriteString('Minimum x-value   : ');ReadReal(xmin);WriteLn;
      WriteString('Increment x-value : ');ReadReal(xinc);WriteLn;
      SGXYFileOut(ap,x,xmin,xinc);
    END;     
  END;  
END RealArrayOut;
  
PROCEDURE RealArrayIn(ap:INTEGER;VAR x:ARRAY OF REAL);
VAR format,xaxis,i : INTEGER;
    xmin,xinc      : REAL;
BEGIN
  WriteString('Read ASCII-file containing reals :');WriteLn;
  WriteString('      1. Normal ASCII file');WriteLn;
  WriteString('      2. SGplot file format');WriteLn;
  WriteString('      3. Cancel file input');WriteLn;
  WriteString('Your choice [1,2,3] : ');ReadInt(format);WriteLn;
  IF format=1 THEN
    AsciiIn(ap,x);
  END;
  IF format=2 THEN
    SGFileIn(ap,x);
  END;
  IF format=3 THEN
    FOR i:=0 TO ap-1 DO
      x[i]:=0.0;
    END;
  END;      
END RealArrayIn;  

PROCEDURE Create_IDL_File(fnm:ARRAY OF CHAR);
VAR ftype,i,j,nbytes,pp,fndw,PVW,dummy:INTEGER;
    in,out:File;
    address:ADDRESS;
    done:BOOLEAN;
    fnm1:ARRAY[0..15] OF CHAR;
    hiarr:ARRAY[0..14] OF INTEGER;
    realdummy:REAL;
    written,read:CARDINAL;
BEGIN
  ScreenMode(3);
  WriteString('File name PV-Wave file : ');ReadString(fnm1);
  Open(in,fnm,ReadOnly,done);
  ReadBinFileHeader(in,ftype,pp,fndw);
  hiarr[0]:=3;(* in fact it is a 2D simulated measurement (time domain) *) 
  hiarr[1]:=0;(* no fake measurements *)
  hiarr[2]:=0;(* no counter number *)
  hiarr[3]:=2;(* number of channals *)
  hiarr[4]:=pp;(* number of points in acquisition direction *)
  hiarr[5]:=fndw;(* number of points in first evolution direction *)
  hiarr[6]:=0;(* number of points in second evolution direction *)
  hiarr[7]:=0;(* number of points in third evolution direction *)
  hiarr[8]:=1;(* number of additions *)
  hiarr[9]:=0; (* of no importance *)
  hiarr[10]:=0;(* of no importance *)
  hiarr[11]:=0;(* of no importance *)
  hiarr[12]:=0;(* of no importance *)
  hiarr[13]:=0;(* of no importance *)
  hiarr[14]:=2;(* data type : floating points; single precision ; complex *)
  Create(out,fnm,done);
  address:=ADR(hiarr[0]);
  nbytes:=15*4;
  WriteNBytes(out,address,nbytes,written);
  dummy:=0; 
  address:=ADR(dummy);
  nbytes:=4;
  FOR i:=1 TO (964 DIV 4) DO
    WriteNBytes(out,address,nbytes,written);
  END;
  nbytes:=8;
  address:=ADR(realdummy); 
  FOR i:=0 TO 2*pp-1 DO
    FOR j:= 0 TO fndw-1 DO
      ReadNBytes(in,address,nbytes,read);
      WriteNBytes(out,address,nbytes,written);
    END;
  END;
  Close(in,done);Close(out,done);
END Create_IDL_File;


PROCEDURE SGFileOut(ap:INTEGER;x:ARRAY OF REAL);
VAR i:INTEGER;
BEGIN
  WriteString('SG-Plot File Out >> ');WriteLn;
  OpenOutput('DAT');
  FOR i:=0 TO ap-1 DO
    WriteInt(1,5);WriteString('   ');WriteReal(FLOAT(i),12);WriteString('   ');
    WriteReal(x[i],15);WriteLn;
  END;
  CloseOutput;
END SGFileOut;

PROCEDURE AsciiOut(ap:INTEGER; x:ARRAY OF REAL);
VAR i:INTEGER;
BEGIN
  WriteString('Ascii File Out >> ');WriteLn;
  OpenOutput('DAT');
  FOR i:=0 TO ap-1 DO
    WriteReal(x[i],15);WriteLn;
  END;
  CloseOutput;
END AsciiOut;

PROCEDURE SGXYFileOut(ap:INTEGER; x:ARRAY OF REAL; xmin,xinc:REAL);
VAR i:INTEGER;
    y:REAL;
BEGIN
  WriteString('SG-Plot File Out >> ');WriteLn;
  OpenOutput('DAT');
  FOR i:=0 TO ap-1 DO
    y:=FLOAT(i)*xinc+xmin;
    WriteInt(1,5);WriteString('   ');WriteReal(x[i],12);WriteString('   ');
    WriteReal(y,15);WriteLn;
  END;
  CloseOutput;
END SGXYFileOut;

PROCEDURE SGFileIn(ap:INTEGER;VAR x:ARRAY OF REAL);
VAR i,dummy:INTEGER;
    dummy1:REAL;
BEGIN
  WriteString('SG-Plot File In >> ');WriteLn;
  OpenInput('DAT');
  FOR i:=0 TO ap-1 DO
    ReadInt(dummy);ReadReal(dummy1);
    ReadReal(x[i]);
  END;
  CloseInput;
END SGFileIn;

PROCEDURE AsciiIn(ap:INTEGER;VAR x:ARRAY OF REAL);
VAR i,dummy:INTEGER;
    dummy1:REAL;
BEGIN
  WriteString('SG-Plot File In >> ');WriteLn;
  OpenInput('DAT');
  FOR i:=0 TO ap-1 DO
    ReadReal(x[i]);
  END;
  CloseInput;
END AsciiIn;


END DataFilesInOut.
