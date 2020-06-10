IMPLEMENTATION MODULE plot386;
(*uitbreiding 2d color image (12 colorlevels; 64x64 pixels 12-4-92 *)
                    (*5-3-'91 uitgebreid tot y en z vs. x*)
			      (*rev. 16-3-91*)
(*Deze implementation-module maakt een 2-D plot van een data groep
 (x[0..n-1],y[0..n-1]), met een schaling. Alleen de minimale en maximale 
 plotwaarden worden afgedrukt in twee cijfers nauwkeurig
 het verkregen plotje geeft dus alleen een aardige indruk van de resulta-
 ten. Voor een fraaiere plot gebruike men  SGPLOT*)

IMPORT DebugPMD;
FROM Delay IMPORT Delay;
FROM RealInOut IMPORT WriteReal;
FROM MathLib0 IMPORT sqrt;
FROM Graphics IMPORT ForegroundColor,ClearWindow,ScreenMode,Palette,
                     Text,Line,Dot,ColorTable,BackgroundColor;
FROM NumberConversion IMPORT LongIntToString,IntToString;
FROM FloatingUtilities IMPORT Int;
FROM RealConversions IMPORT RealToString;
FROM LogiFile IMPORT WriteNBytes,Create,File,Close,Open,ReadNBytes,
                     OpenMode,EndFile,Reset,GetPos,SetPos,ReadChar,WriteChar;
FROM SYSTEM IMPORT ADDRESS,ADR;
FROM InOut IMPORT Read,WriteString,ReadInt,WriteLn,WriteInt;
VAR p,q,r,s,schaalx,schaaly,bereikx,bereiky:REAL;
        ok:BOOLEAN;
    
PROCEDURE minmax(VAR x:ARRAY OF REAL;n:CARDINAL;VAR max,min:REAL);
VAR j:CARDINAL;
BEGIN
        max:=x[0]; min:=x[0];
        FOR j:=1 TO n-1 DO
          IF x[j]> max THEN max:=x[j] END;
          IF x[j]<=min THEN min:=x[j] END;
        END;
END minmax;


PROCEDURE schermterug;
VAR c:CHAR;
BEGIN
  Read(c); 
  ScreenMode(3);
END schermterug;

PROCEDURE schermklaar(tc1,backgrkleur:CARDINAL);
VAR j,u,v,w,z:INTEGER;
BEGIN
  ScreenMode(6);
  ClearWindow(backgrkleur);
  ForegroundColor(tc1);
  Line(100,0,600,0,tc1);
  Line(600,0,600,179,tc1);
  Line(600,179,100,179,tc1);
  Line(100,179,100,0,tc1);
  FOR j:=1 TO 9  DO
    u:= j*50+100;
    v:= j*50+125;
    w:= j*18-1;
    z:= j*18-9;
    Line(u,0,u,5,tc1);
    Line(u,179,u,174,tc1);
    Line(v,0,v,3,tc1);
    Line(v,179,v,177,tc1);
    Line(100,w,110,w,tc1);
    Line(600,w,590,w,tc1);
    Line(100,z,105,z,tc1);
    Line(600,z,595,z,tc1);
  END;
END schermklaar;

PROCEDURE schaalverd(p,q,r,s:REAL;tc1:CARDINAL);
VAR  pstr,qstr,rstr,sstr:ARRAY[0..20] OF CHAR;
BEGIN
  RealToString(r,-1,9,rstr,ok);
  Text(26,10,rstr,tc1);
  RealToString(s,-1,9,sstr,ok);
  Text(26,178,sstr,tc1);
  RealToString(p,-1,9,pstr,ok);
  Text(530,188,pstr,tc1);
  RealToString(q,-1,9,qstr,ok);
  Text(100,188,qstr,tc1)
END schaalverd;

PROCEDURE offset(VAR x:ARRAY OF REAL;n:CARDINAL;p,q:REAL);
VAR j:CARDINAL;
BEGIN
    FOR j:= 0 TO n-1 DO
      x[j]:=x[j]-q;
    END;
END offset;

PROCEDURE curve(VAR x,y:ARRAY OF REAL;n,tc:CARDINAL);
VAR j:CARDINAL;
    u,v,w,z:INTEGER;
BEGIN
  FOR j:=0 TO n-2 DO
    u:=100+TRUNC(x[j]);
    v:=179-TRUNC(y[j]);
    w:=100+TRUNC(x[j+1]);
    z:=179-TRUNC(y[j+1]);
    Line(u,v,w,z,tc);
  END;
END curve;

PROCEDURE punt(VAR x,y:ARRAY OF REAL;n,tc:CARDINAL);
VAR j:CARDINAL;
    u,v:INTEGER;
BEGIN
  FOR j:=0 TO n-1 DO
    u:=100+TRUNC(x[j]);
    v:=179-TRUNC(y[j]);
    Dot(u,v,tc);
  END;
END punt;

PROCEDURE tdplot1(x,y:ARRAY OF REAL;n,tc1,tcb:CARDINAL);
VAR j:CARDINAL;
    noplot:BOOLEAN;
    dummy:CHAR;
BEGIN
  noplot:=TRUE;
  minmax(x,n,p,q); minmax(y,n,r,s);
  IF r=s THEN 
    WriteString('Ymin=Ymax, no plot');
    Read(dummy);
    noplot:=FALSE;
  END;
  IF noplot THEN
    bereikx:= ABS(p-q); bereiky:=ABS(r-s);
    schaalx:=bereikx/500.0;schaaly:=bereiky/179.0;
    offset(x,n,p,q);
    offset(y,n,r,s);
    FOR j:=0 TO n-1 DO
      x[j]:=x[j]/schaalx;
      y[j]:=y[j]/schaaly;
    END;
    schermklaar(tc1,tcb);
    schaalverd(p,q,r,s,tc1);
    curve(x,y,n,tc1);
    schermterug;
  END;  
END tdplot1;

PROCEDURE tdplot2( x,y,z:ARRAY OF REAL;n,tc1,tcb,tc3:CARDINAL);
VAR j:CARDINAL;
    maz,miz:REAL;
    dummy:CHAR;
    noplot:BOOLEAN;
BEGIN
  noplot:=TRUE;
  minmax(x,n,p,q);minmax(y,n,r,s);minmax(z,n,maz,miz);
  IF maz>r THEN r:=maz END;
  IF miz<s THEN s:=miz END;
  IF r=s THEN 
    WriteString('Ymin=Ymax, no plot');
    Read(dummy);
    noplot:=FALSE;
  END;
  IF noplot THEN
    bereikx:= ABS(p-q); bereiky:=ABS(r-s);
    schaalx:=bereikx/500.0;schaaly:=bereiky/179.0;
    offset(x,n,p,q);
    offset(y,n,r,s);
    offset(z,n,maz,miz);
    FOR j:=0 TO n-1 DO
      x[j]:=x[j]/schaalx;
      y[j]:=y[j]/schaaly;
      z[j]:=z[j]/schaaly
    END;
    schermklaar(tc1,tcb);
    schaalverd(p,q,r,s,tc1);
    curve(x,y,n,tc1);
    punt(x,z,n,tc3);
    schermterug;
  END;  
END tdplot2;

PROCEDURE Image(nc,ndx,ndy,ndz:INTEGER);
VAR string:ARRAY[0..15] OF CHAR;
    magtype,n1,n2,i,j:INTEGER;
    read,written:CARDINAL;
    adrmx,adrmy,adrmz,adrval:ADDRESS;
    mx,my,mtr,mz,value,min,max:REAL;
    done:BOOLEAN;
    in,out:File;
    chr:CHAR;
BEGIN
  ScreenMode(3);
  WriteString('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~');WriteLn;
  WriteString('~     Image routine   :   10 colorlevels         ~');WriteLn;
  WriteString('~             Version 1 (april 1992);            ~');WriteLn;
  WriteString('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~');WriteLn;
  WriteLn;
  WriteString('Image of : 1. Mx ');WriteLn;
  WriteString('           2. My ');WriteLn;
  WriteString('           3. Mtr');WriteLn;
  WriteString('           4. Mz ');WriteLn;
  WriteString('           5. no image and quit');WriteLn;
  WriteString('Your choice [1..5] : ');ReadInt(magtype);
  IF magtype<5 THEN
    adrmx:=ADR(mx);adrmy:=ADR(my);adrmz:=ADR(mz);adrval:=ADR(value);
    IF (nc=1) AND (ndx=1) THEN n1:=ndy;n2:=ndz END;
    IF (nc=1) AND (ndy=1) THEN n1:=ndx;n2:=ndz END;
    IF (nc=1) AND (ndz=1) THEN n1:=ndx;n2:=ndz END;
    IF (ndx=1) AND (ndy=1) THEN n1:=nc;n2:=ndz END;
    IF (ndx=1) AND (ndz=1) THEN n1:=nc;n2:=ndy END;
    IF (ndy=1) AND (ndz=1) THEN n1:=nc;n2:=ndx END;
    Open(in,'mstat.bin',ReadOnly,done);
    min:=0.0;max:=0.0;
    FOR i:=0 TO n1-1 DO
      FOR j:=0 TO n2-1 DO
        ReadNBytes(in,adrmx,8,read);
        ReadNBytes(in,adrmy,8,read);
        ReadNBytes(in,adrmz,8,read);
        mtr:=sqrt(mx*my+mz*mz);
        CASE magtype OF
          1:value:=mx;|
          2:value:=my;|
          3:value:=mtr;|
          4:value:=mz;
        END;
        IF value>max THEN
          max:=value;
        END;
        IF value<min THEN
          min:=value;
        END;              
      END;     
    END;  
    Close(in,done);  
    Open(in,'mstat.bin',ReadOnly,done);
    Create(out,'colormap.bin',done);
    FOR i:=0 TO n1-1 DO
      FOR j:=0 TO n2-1 DO
        ReadNBytes(in,adrmx,8,read);
        ReadNBytes(in,adrmy,8,read);
        ReadNBytes(in,adrmz,8,read);
        mtr:=sqrt(mx*mx+my*my);
        CASE magtype OF
          1:value:=Scale(mx,min,max);|
          2:value:=Scale(my,min,max);|
          3:value:=Scale(mtr,min,max);|
          4:value:=Scale(mz,min,max);
        END;
        value:=FLOAT(TRUNC(9.0*value+0.5)); (*10 color values*)     
        WriteNBytes(out,adrval,8,written);
      END;
    END;  
    Close(in,done);  
    Close(out,done);
    DoColorImage(n1,n2);
  END;  
END Image;

PROCEDURE Index1;
VAR i,j,k,x,y:INTEGER;
BEGIN
  FOR j:=0 TO 2 DO
    FOR k:=0 TO 2 DO
      x:=300+k*3;
      y:=30+j*3;
      SetPixel1(x,y,0,1);
    END;
  END;    
  FOR j:=0 TO 2 DO
    FOR k:=0 TO 2 DO
      x:=300+k*3;
      y:=45+j*3;
      SetPixel2(x,y,0,1);
    END;
  END;    
  FOR j:=0 TO 2 DO
    FOR k:=0 TO 2 DO
      x:=300+k*3;
      y:=60+j*3;
      SetPixel3(x,y,0,1);
    END;
  END;    
END Index1;

PROCEDURE Index2;
VAR i,j,k,x,y:INTEGER;
BEGIN
  FOR j:=0 TO 2 DO
    FOR k:=0 TO 2 DO
      x:=300+k*3;
      y:=75+j*3;
      SetPixel1(x,y,1,2);
    END;
  END;    
  FOR j:=0 TO 2 DO
    FOR k:=0 TO 2 DO
      x:=300+k*3;
      y:=90+j*3;
      SetPixel2(x,y,1,2);
    END;
  END;    
  FOR j:=0 TO 2 DO
    FOR k:=0 TO 2 DO
      x:=300+k*3;
      y:=105+j*3;
      SetPixel3(x,y,1,2);
    END;
  END;    
END Index2;

PROCEDURE Index3;
VAR i,j,k,x,y:INTEGER;
BEGIN
  FOR j:=0 TO 2 DO
    FOR k:=0 TO 2 DO
      x:=300+k*3;
      y:=120+j*3;
      SetPixel1(x,y,2,3);
    END;
  END;    
  FOR j:=0 TO 2 DO
    FOR k:=0 TO 2 DO
      x:=300+k*3;
      y:=135+j*3;
      SetPixel2(x,y,2,3);
    END;
  END;    
  FOR j:=0 TO 2 DO
    FOR k:=0 TO 2 DO
      x:=300+k*3;
      y:=150+j*3;
      SetPixel3(x,y,2,3);
    END;
  END;    
  FOR j:=0 TO 2 DO
    FOR k:=0 TO 2 DO
      x:=300+k*3;
      y:=165+j*3;
      SetPixel4(x,y,2,3);
    END;
  END;    
END Index3;

PROCEDURE DoColorImage(n1,n2:INTEGER);
VAR x,y,i,j,offsetx,offsety:INTEGER;
    colortype:REAL;
    chr,char:CHAR;
    read:CARDINAL;
    adrcoltyp:ADDRESS;
    in:File;
    done:BOOLEAN;
BEGIN
  offsetx:=5;offsety:=5;
  ScreenMode(4);
  Palette(2);
  BackgroundColor(15);
  Open(in,'colormap.bin',ReadOnly,done);
  adrcoltyp:=ADR(colortype);
  FOR i:=0 TO n1-1 DO
    FOR j:=0 TO n2-1 DO
      x:=offsetx+i*3;
      y:=offsety+j*3;
      ReadNBytes(in,adrcoltyp,8,read);
      ColoredPixel(x,y,TRUNC(colortype));
    END;
  END;
  Close(in,done);
  Index1;Index2;Index3;  
  ForegroundColor(8);
  Read(chr);
  ScreenMode(3);
END DoColorImage;      
  

PROCEDURE Scale(in,min,max:REAL):REAL;
VAR out:REAL;
BEGIN
  IF min<=0.0 THEN
    out:=(in+ABS(min))/ABS(max-min);
  ELSE
    out:=(in-min)/ABS(max-min);
  END;
  RETURN out;
END Scale;    

PROCEDURE ColoredPixel(x,y,colorvalue:INTEGER);
VAR 
BEGIN
  CASE colorvalue OF
      0:SetPixel1(x,y,0,1);|
      1:SetPixel2(x,y,0,1);|
      2:SetPixel3(x,y,0,1);|
      3:SetPixel1(x,y,1,2);|
      4:SetPixel2(x,y,1,2);|
      5:SetPixel3(x,y,1,2);|
      6:SetPixel1(x,y,2,3);|
      7:SetPixel2(x,y,2,3);|
      8:SetPixel3(x,y,2,3);|
      9:SetPixel4(x,y,2,3);|
  END;
END ColoredPixel;

PROCEDURE ColorTablePixel(x,y,colorvalue:INTEGER);
VAR 
BEGIN
  SetPixel1(x,y,-1,-1);
END ColorTablePixel;

PROCEDURE SetPixel1(x,y,tc1,tc2:INTEGER);
VAR i,j:INTEGER;
BEGIN
  FOR i:=0 TO 2 DO
    FOR j:=0 TO 2 DO
      Dot(x+i,y+j,tc1);
    END;
  END;
END SetPixel1;  

PROCEDURE SetPixel2(x,y,tc1,tc2:INTEGER);
VAR i,j:INTEGER;
BEGIN
  FOR i:=0 TO 2 DO
    FOR j:=0 TO 2 DO
      Dot(x+i,y+j,tc1);
    END;
  END;
  Dot(x+1,y+1,tc2);
END SetPixel2;  

PROCEDURE SetPixel3(x,y,tc1,tc2:INTEGER);
VAR i,j,a:INTEGER;
BEGIN
  Dot(x,y,tc2);
  Dot(x+1,y,tc1);
  Dot(x+2,y,tc2);
  Dot(x,y+1,tc1);
  Dot(x+1,y+1,tc2);
  Dot(x+2,y+1,tc1);
  Dot(x,y+2,tc2);
  Dot(x+1,y+2,tc1);
  Dot(x+2,y+2,tc2);
END SetPixel3;
              
PROCEDURE SetPixel4(x,y,tc1,tc2:INTEGER);
VAR i,j:INTEGER;
BEGIN
  FOR i:=0 TO 2 DO
    FOR j:=0 TO 2 DO
      Dot(x+i,y+j,tc2);
    END;
  END;
END SetPixel4;  

END plot386.     
