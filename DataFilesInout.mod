IMPLEMENTATION MODULE DataFilesInout;

FROM InOut IMPORT ReadInt,WriteInt,WriteString,WriteLn,ReadString;

IMPORT FIO;
FROM FpuIO IMPORT RealToStr, StrToReal, WriteReal, ReadReal;
FROM WholeStr IMPORT IntToStr,ConvResults, StrToInt;
FROM Strings IMPORT Assign, Insert, Delete, Pos, Copy, ConCat, Length,
                    CompareStr;

FROM SYSTEM IMPORT ADDRESS,ADR;
FROM BinaryFilesInOut IMPORT BinFileActions,ReadBinFileHeader,WriteBinFileHeader;


(***************************************************************************)
(***    About      : INPUT / OUTPUT OF 1D ASCII (SGPLOT)  DATA FILES     ***)
(***    Author     : H.S.                                                ***)
(***    Date       : 20 - 03 -1992                                       ***)
(***    System     : LOGITECH MODULA2                                    ***)
(***    Change     : 22 - 03 - 1992  BY H.S                              ***)
(***    Last change: 14 - 01 - 2020  BY H.S                              ***)
(***************************************************************************)    

PROCEDURE RealArrayOut(ap:INTEGER; x:ARRAY OF REAL; xmin_arg:REAL; xinc_arg:REAL);
VAR format,xaxis : INTEGER;
    xmin,xinc    : REAL;
BEGIN
  WriteLn;
  WriteString('Write real-array to ASCII-file :');WriteLn;
  WriteString('      1. Normal ASCII file');WriteLn;
  WriteString('      2. XYPlot file format');WriteLn;
  WriteString('      3. SGplot file format');WriteLn;
  WriteString('      4. Cancel file output');WriteLn;
  WriteString('Your choice [1..4] : ');ReadInt(format);WriteLn;
  IF format=1 THEN
    AsciiOut(ap,x);
  END;
  IF format=2 THEN
    SimpleXYPlotFileOut(ap,x,xmin_arg,xinc_arg);
  END;
  IF format=3 THEN
    WriteString('Define x-axis [1=yes] : ');ReadInt(xaxis);WriteLn;
    IF xaxis#1 THEN  
      SGPlotFileOut(ap,x,0.0,1.0);
    ELSE
      WriteString('Minimum x-value   : ');ReadReal(xmin);WriteLn;
      WriteString('Increment x-value : ');ReadReal(xinc);WriteLn;
      SGPlotFileOut(ap,x,xmin,xinc);
    END;     
  END;  
END RealArrayOut;
  
PROCEDURE RealArrayIn( VAR ap:INTEGER; VAR x:ARRAY OF REAL; 
                       VAR xmin_arg:REAL; xinc_arg:REAL);
VAR format,xaxis,i : INTEGER;
    xmin,xinc      : REAL;
BEGIN
  WriteLn;
  WriteString('Read ASCII-file containing reals :');WriteLn;
  WriteString('      1. Normal ASCII file');WriteLn;
  WriteString('      2. XYplot file format');WriteLn;
  WriteString('      3. SGplot file format');WriteLn;
  WriteString('      4. Cancel file input');WriteLn;
  WriteString('Your choice [1,2,3,4] : ');ReadInt(format);WriteLn;
  IF format=1 THEN
    SimpleTextArrayFileIn(ap,x);
  END;
  IF format=2 THEN
    SimpleXYPlotFileIn(ap,x);
  END;
  IF format=2 THEN
    SimpleXYPlotFileIn(ap,x);
  END;
  IF format=3 THEN
    FOR i:=0 TO ap-1 DO
      x[i]:=0.0;
    END;
  END;      
END RealArrayIn;  

PROCEDURE Create_IDL_File( fnm:ARRAY OF CHAR );
VAR ftype,i,j,nbytes,pp,fndw,PVW,dummy:INTEGER;
    in,out:FIO.File;
    address:ADDRESS;
    done:BOOLEAN;
    fnm1:ARRAY[0..128] OF CHAR;
    hiarr:ARRAY[0..14] OF INTEGER;
    realdummy:REAL;
    written,read:CARDINAL;
BEGIN
  WriteString('File name PV-Wave file : ');ReadString(fnm1);
  
  in := FIO.OpenToRead( fnm );

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
  
  out := FIO.OpenToWrite( fnm );

  address:=ADR(hiarr[0]);
  nbytes:=15*4;
  
  written := FIO.WriteNBytes( out, nbytes, address);
  
  dummy:=0; 
  address:=ADR(dummy);
  nbytes:=4;
  FOR i:=1 TO (964 DIV 4) DO
    written := FIO.WriteNBytes( out, nbytes, address);
  END;
  nbytes:=8;
  address:=ADR(realdummy); 
  FOR i:=0 TO 2*pp-1 DO
    FOR j:= 0 TO fndw-1 DO
      read := FIO.ReadNBytes(in,nbytes,address);
      written := FIO.WriteNBytes( out, nbytes, address);
    END;
  END;

  FIO.Close(in);
  FIO.Close(out);

END Create_IDL_File;

PROCEDURE AsciiOut(ap:INTEGER; x:ARRAY OF REAL);
VAR i:INTEGER;
    fnm:ARRAY[0..128] OF CHAR;
    outValAsStr, outIntConvStr: ARRAY[0..15] OF CHAR;
    out:FIO.File;
BEGIN
  
  WriteString('Ascii File Out >> ');ReadString(fnm);WriteLn();
  out := FIO.OpenToWrite(fnm);
  IntToStr( ap, outIntConvStr );
  (* OpenOutput('DAT'); *)
  FIO.WriteString(out, outIntConvStr);
  FIO.WriteLine(out);
  FOR i:=0 TO ap-1 DO
    RealToStr( x[i], 12, 11, outValAsStr );
    FIO.WriteString(out, outValAsStr);
    FIO.WriteLine(out);
  END;
  FIO.Close(out);
END AsciiOut;



(* ********************************************************************* *)
(* First line states the number of point pairs in the file.              *)
(* Simple list of (group_id,x,y) values are written in a file.           *)
(* ********************************************************************* *)
PROCEDURE SGPlotFileOut(ap:INTEGER; x:ARRAY OF REAL; xmin, xinc:REAL);
VAR i: INTEGER;
    fnm: ARRAY[0..128] OF CHAR;
    outStrXval, outStrYval: ARRAY[0..15] OF CHAR;
    y: REAL;
    group_id_str: ARRAY[0..1] OF CHAR;
    group_id: CARDINAL;
    out:FIO.File;
BEGIN
  group_id := 1;
  IntToStr(group_id,group_id_str); 
  WriteString('SG-Plot File Out >> ');ReadString(fnm);WriteLn();
  out := FIO.OpenToWrite(fnm);

  FOR i:=0 TO ap-1 DO
    y:=FLOAT(i)*xinc+xmin;
    RealToStr( y, 12, 11, outStrXval );
    RealToStr( x[i], 12, 11, outStrYval );
    FIO.WriteString(out, group_id_str);
    FIO.WriteString(out,'   ');
    FIO.WriteString(out,outStrXval);FIO.WriteString(out,'   ');
    FIO.WriteString(out,outStrYval);FIO.WriteLine(out);
  END;

  FIO.Close(out);

END SGPlotFileOut;


(* ********************************************************************* *)
(* Simple list of (x,y) values header value number of points.            *)
(* First line states the number of point pairs in the file.              *)
(* ********************************************************************* *)
PROCEDURE SimpleXYPlotFileOut(ap:INTEGER; x:ARRAY OF REAL; xmin, xinc:REAL);
VAR i: INTEGER;
    fnm: ARRAY[0..128] OF CHAR;
    outStrXval, outStrYval: ARRAY[0..15] OF CHAR;
    y: REAL;
    ap_AsString: ARRAY[0..3] OF CHAR;
    out:FIO.File;
BEGIN 
  WriteString('Simple XY-Plot File Out >> ');ReadString(fnm);WriteLn( );
  out := FIO.OpenToWrite(fnm);
  IntToStr(ap,ap_AsString);
  FIO.WriteString(out,ap_AsString);FIO.WriteLine(out);
  FOR i:=0 TO ap-1 DO
    y:=FLOAT(i)*xinc+xmin;
    RealToStr( y, 12, 11, outStrXval );
    RealToStr( x[i], 12, 11, outStrYval );
    (* FIO.WriteString(out, group_id_str);
       FIO.WriteString(out,'   '); *)
    FIO.WriteString(out,outStrXval);FIO.WriteString(out,'   ');
    FIO.WriteString(out,outStrYval);FIO.WriteLine(out);
  END;
  FIO.Close(out);
END SimpleXYPlotFileOut;



PROCEDURE SimpleXYPlotFileIn(VAR ap:INTEGER;VAR x:ARRAY OF REAL);
VAR i,dummy:INTEGER;
    dummy1:REAL;
    in:FIO.File;
    fnm: ARRAY[0..128] OF CHAR;
    lineAsString: ARRAY[0..33] OF CHAR;
    xValStr,yValStr: ARRAY[0..15] OF CHAR;
    result: ARRAY[0..12] OF CHAR;
    convRes:ConvResults;
BEGIN
  WriteString('XY-Plot File In >> ');ReadString(fnm);WriteLn();

  in := FIO.OpenToRead(fnm);

  FIO.ReadString(in,lineAsString);
  WriteString( lineAsString ); WriteLn();
  StrToInt(lineAsString, ap,convRes);
  WriteInt( ap, 4 );WriteLn();

  FOR i:=0 TO ap-1 DO
    FIO.ReadString(in,lineAsString);
    Copy( lineAsString, 1, 12, xValStr );
    Copy( lineAsString, 15, 26, yValStr );
    StrToReal(yValStr,x[i]);
    WriteString("Reading value : ");WriteReal( x[i], 12, 11 ); WriteLn( );
  END;
  FIO.Close(in);
END SimpleXYPlotFileIn;


(* *********************************************************** *)
(* Reads a simple array of "ap" real values from textfile fnm  *)
(* *********************************************************** *)
PROCEDURE SimpleTextArrayFileIn( VAR ap:INTEGER; VAR x:ARRAY OF REAL );
VAR i,dummy: INTEGER;
    dummy1: REAL;
    in: FIO.File;
    fnm: ARRAY[0..128] OF CHAR;
    stringIn: ARRAY[0..18] OF CHAR;
    convRes: ConvResults;
BEGIN
  WriteString('Simple text value file In >> ');ReadString(fnm);WriteLn();
 
  in := FIO.OpenToRead(fnm);
  (* The first line states the number of simple points that follow. *)
  FIO.ReadString(in,stringIn);
  WriteString('Number of points to read : ');WriteString( stringIn ); WriteLn();
  StrToInt(stringIn, ap, convRes);

  FOR i:=0 TO ap-1 DO
    FIO.ReadString(in,stringIn);
    StrToReal(stringIn,x[i]);
    WriteString("Value as string : "); WriteString(stringIn); WriteLn( );
    WriteString("* Reading value : "); WriteReal( x[i], 12,11 ); WriteLn( );
  END;

  FIO.Close(in);

END SimpleTextArrayFileIn;


END DataFilesInout.
