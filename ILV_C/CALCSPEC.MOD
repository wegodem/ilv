IMPLEMENTATION MODULE CalcSpec;

FROM Delay IMPORT Delay;
FROM MathLib0 IMPORT sqrt,sin,cos,exp,arctan,ln,entier;
FROM MatrixMathLib IMPORT CMatCommutator,CMatAdd,CMatMultAdd,CMatRMultAdd,
                          CMatMult,CMatZero,ShowCMatElements,CopyCMatrix,
                          RMultCMat,CMultCMat,ExpectationValue,CalcTrace,
                          ExpMatrix,DoRotation,NormCMatrix,CMatrix,
                          COMPLEXLONGREAL,DoSpectrum,TwoDiagonal;
FROM InOut IMPORT Read,Write,ReadInt,ReadCard,ReadString,WriteInt,WriteCard, 
     WriteString,WriteLn,OpenInput,CloseInput,OpenOutput,CloseOutput,Done;
FROM LogiFile IMPORT WriteNBytes,Create,File,Close,Open,ReadNBytes,
                     OpenMode,EndFile,Reset,GetPos,SetPos,Delete;
FROM SYSTEM IMPORT ADDRESS,ADR;
FROM Graphics IMPORT ScreenMode;
FROM RealInOut IMPORT WriteReal,ReadReal;
FROM plot386 IMPORT tdplot1;  
FROM BinFiles IMPORT BinFileActions,ReadBinFileHeader,WriteBinFileHeader;
FROM FOUR3 IMPORT four2,fft;
FROM SpinOperatorLib IMPORT MakeKets,MakeMatrixIplus,MakeMatrixImin,
                            MakeMatrixIx,MakeMatrixIy,MakeMatrixIz,
                            Iarray,produktket,MakeTotalIx,MakeTotalIy,
                            MakeTotalIz;
                            
CONST  pi = 3.1415926535897932; 
TYPE String = ARRAY[0..15] OF CHAR;

PROCEDURE SeparateSpectra(n,ap:INTEGER;
                          VAR densmat,Istat,Ixtot,Iytot,Iztot:CMatrix;
                          ket:produktket;Ilist:Iarray);
VAR store:CMatrix;
    dw,time,h,abs,disp:REAL;
    k,l,m,dummy,i,j,ftype,fn,fndw,Ns1,Nc1,Ns2,Nc2,choice:INTEGER;
    nbytes,nbytes1,written,read:CARDINAL;
    f,in:File;
    address,address1,address2:ADDRESS;
    done:BOOLEAN;
    lr:COMPLEXLONGREAL;
    ssp1,csp1,ssp2,csp2:TwoDiagonal;
BEGIN
  ScreenMode(3);
  WriteString('----------------------------------------------------------');WriteLn;
  WriteString('-    Calculation spectra of density matrix / matrices    -');WriteLn;
  WriteString('----------------------------------------------------------');WriteLn;
  WriteLn;
  address1:=ADR(abs);address2:=ADR(disp);
  CMatZero(n,densmat,densmat);
  Open(in,'dm.bin',ReadOnly,done);
  ReadBinFileHeader(in,ftype,fn,fndw);
  IF (ftype#1) AND (n=fn) THEN  
    nbytes:=2*8*n;
    nbytes1:=8;  (* A REAL IS REPRESENTED IN LOGITECH MODULA_2 AS 8 bytes *)
    Create(f,'ilvsign.bin',done);
    WriteString("How many time-points (power of 2): ");ReadInt(ap);WriteLn;
    WriteString("Acquisition time [seconds] : ");ReadReal(time);
    WriteLn;
    h:=time/FLOAT(ap-1);
    WriteString('Offset frequency [Hz] = ');ReadReal(dw);WriteLn;
    dw:=dw*2.0*pi;
    RMultCMat(n,dw,Iztot,store);
    CMatAdd(n,Istat,store,store);
    RMultCMat(n,-1.0,store,store);
    WriteBinFileHeader(f,1,ap,fndw);   
    FOR i:= 1 TO fndw DO
      FOR j:=1 TO fn DO
        address:=ADR(densmat[j,1,1]);
        ReadNBytes(in,address,nbytes,read);
        IF read # nbytes THEN
          WriteString('>>> READ BINARY FILE ERROR <<<');Delay(1000);WriteLn;
        END;      
      END;
      DoSpectrum(n,densmat,store,Ixtot,csp1,ssp1,Nc1,Ns1);
      DoSpectrum(n,densmat,store,Iytot,csp2,ssp2,Nc2,Ns2);
      FOR m:=0 TO ap-1 DO
        abs:=0.0;disp:=0.0;
        FOR k:=1 TO Ns1 DO
          disp:=disp+ssp1[1,k]*sin(2.0*pi*ssp1[2,k]*FLOAT(m)*h);
        END;
        FOR k:=1 TO Nc1 DO
          abs:=abs+csp1[1,k]*cos(2.0*pi*csp1[2,k]*FLOAT(m)*h);
        END;
        abs:=abs+disp;
        WriteNBytes(f,address1,nbytes1,written);
        abs:=0.0;disp:=0.0;
        FOR k:=1 TO Ns2 DO
          disp:=disp+ssp2[1,k]*sin(2.0*pi*ssp2[2,k]*FLOAT(m)*h);
        END;
        FOR k:=1 TO Nc2 DO
          abs:=abs+csp2[1,k]*cos(2.0*pi*csp2[2,k]*FLOAT(m)*h);
        END;
        disp:=disp+abs;
        WriteNBytes(f,address2,nbytes1,written);
      END;    
    END;
    Close(f,done);Close(in,done);
  END;
  Create_PV_WAVE_File;
END SeparateSpectra;

PROCEDURE Create_PV_WAVE_File;
VAR ftype,i,j,nbytes,pp,fndw,PVW,dummy:INTEGER;
    in,out:File;
    address:ADDRESS;
    done:BOOLEAN;
    fnm:ARRAY[0..15] OF CHAR;
    hiarr:ARRAY[0..14] OF INTEGER;
    realdummy:REAL;
    written,read:CARDINAL;
BEGIN
  ScreenMode(3);
  WriteString('File name PV-Wave file : ');ReadString(fnm);
  Open(in,'ilvsign.bin',ReadOnly,done);
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
END Create_PV_WAVE_File;

PROCEDURE AverageSpectrum(n,ap:INTEGER;
                          VAR densmat,Istat,Ixtot,Iytot,Iztot:CMatrix;
                          ket:produktket;Ilist:Iarray);
VAR store:CMatrix;
    dw,time,h,abs,disp:REAL;
    k,l,m,dummy,i,j,ftype,fn,fndw,Ns1,Nc1,Ns2,Nc2:INTEGER;
    nbytes,nbytes1,written,read:CARDINAL;
    f,in:File;
    address,address1,address2:ADDRESS;
    done:BOOLEAN;
    lr:COMPLEXLONGREAL;
    ssp1,csp1,ssp2,csp2:TwoDiagonal;
BEGIN
  ScreenMode(3);
  WriteString('**********************************************************');WriteLn;
  WriteString('*** Calculation of average spectrum of densty matrices ***');WriteLn;
  WriteString('**********************************************************');WriteLn;
  WriteLn;
  address1:=ADR(abs);address2:=ADR(disp);
  CMatZero(n,densmat,densmat);
  Open(in,'dm.bin',ReadOnly,done);
  ReadBinFileHeader(in,ftype,fn,fndw);
  IF (ftype#1) AND (n=fn) THEN  
    nbytes:=2*8*n;
    nbytes1:=8;  (* A REAL IS REPRESENTED IN LOGITECH MODULA_2 AS 8 bytes *)
    Create(f,'ilvsign.bin',done);
    WriteString("How many time-points (power of 2): ");ReadInt(ap);WriteLn;
    WriteString("Acquisition time [seconds] : ");ReadReal(time);
    WriteLn;
    h:=time/FLOAT(ap-1);
    WriteString('Offset frequency [Hz] = ');ReadReal(dw);WriteLn;
    dw:=dw*2.0*pi;
    WriteBinFileHeader(f,1,ap,1);   
    FOR i:= 1 TO fndw DO
      FOR j:=1 TO fn DO
        address:=ADR(store[j,1,1]);
        ReadNBytes(in,address,nbytes,read);
        IF read # nbytes THEN
          WriteString('>>> READ BINARY FILE ERROR <<<');Delay(1000);WriteLn;
        END;      
      END;
      CMatAdd(n,store,densmat,densmat);
    END;
    Close(in,done);  
    RMultCMat(n,dw,Iztot,store);
    CMatAdd(n,Istat,store,store);
    RMultCMat(n,-1.0,store,store);
    DoSpectrum(n,densmat,store,Ixtot,csp1,ssp1,Nc1,Ns1);
    DoSpectrum(n,densmat,store,Iytot,csp2,ssp2,Nc2,Ns2);
    FOR m:=0 TO ap-1 DO
      abs:=0.0;disp:=0.0;
      FOR k:=1 TO Ns1 DO
        disp:=disp+ssp1[1,k]*sin(2.0*pi*ssp1[2,k]*FLOAT(m)*h);
      END;
      FOR k:=1 TO Nc1 DO
        abs:=abs+csp1[1,k]*cos(2.0*pi*csp1[2,k]*FLOAT(m)*h);
      END;
      abs:=abs+disp;
      WriteNBytes(f,address1,nbytes1,written);
      abs:=0.0;disp:=0.0;
      FOR k:=1 TO Ns2 DO
        disp:=disp+ssp2[1,k]*sin(2.0*pi*ssp2[2,k]*FLOAT(m)*h);
      END;
      FOR k:=1 TO Nc2 DO
        abs:=abs+csp2[1,k]*cos(2.0*pi*csp2[2,k]*FLOAT(m)*h);
      END;
      disp:=disp+abs;
      WriteNBytes(f,address2,nbytes1,written);
    END;    
  END;
  Close(f,done);
  Create_PV_WAVE_File;
END AverageSpectrum;

PROCEDURE Determine2DSpectrum(n,ap:INTEGER;
                              VAR densmat,Istat,Ixtot,Iytot,Iztot:CMatrix;
                              ket:produktket;Ilist:Iarray);
VAR store:CMatrix;
    dw,time,h,abs,disp:REAL;
    k,l,m,dummy,i,j,ftype,fn,fndw,Ns1,Nc1,Ns2,Nc2,choice:INTEGER;
    nbytes,nbytes1,written,read:CARDINAL;
    f,in:File;
    address,address1,address2:ADDRESS;
    done:BOOLEAN;
    lr:COMPLEXLONGREAL;
    ssp1,csp1,ssp2,csp2:TwoDiagonal;
BEGIN
  ScreenMode(3);
  WriteString('----------------------------------------------------------');WriteLn;
  WriteString('-              Calculation of a 2D-spectrum              -');WriteLn;
  WriteString('----------------------------------------------------------');WriteLn;
  WriteLn;
  address1:=ADR(abs);address2:=ADR(disp);
  CMatZero(n,densmat,densmat);
  Open(in,'dm.bin',ReadOnly,done);
  ReadBinFileHeader(in,ftype,fn,fndw);
  IF (ftype#1) AND (n=fn) THEN  
    nbytes:=2*8*n;
    nbytes1:=8;  (* A REAL IS REPRESENTED IN LOGITECH MODULA_2 AS 8 bytes *)
    Create(f,'ilvsign.bin',done);
    WriteString("How many time-points (power of 2): ");ReadInt(ap);WriteLn;
    WriteString("Acquisition time [seconds] : ");ReadReal(time);
    WriteLn;
    h:=time/FLOAT(ap-1);
    WriteString('Offset frequency [Hz] = ');ReadReal(dw);WriteLn;
    dw:=dw*2.0*pi;
    RMultCMat(n,dw,Iztot,store);
    CMatAdd(n,Istat,store,store);
    RMultCMat(n,-1.0,store,store);
    WriteBinFileHeader(f,1,ap,fndw);   
    FOR i:= 1 TO fndw DO
      FOR j:=1 TO fn DO
        address:=ADR(densmat[j,1,1]);
        ReadNBytes(in,address,nbytes,read);
        IF read # nbytes THEN
          WriteString('>>> READ BINARY FILE ERROR <<<');Delay(1000);WriteLn;
        END;      
      END;
      DoSpectrum(n,densmat,store,Ixtot,csp1,ssp1,Nc1,Ns1);
      DoSpectrum(n,densmat,store,Iytot,csp2,ssp2,Nc2,Ns2);
      FOR m:=0 TO ap-1 DO
        abs:=0.0;disp:=0.0;
        FOR k:=1 TO Ns1 DO
          disp:=disp+ssp1[1,k]*sin(2.0*pi*ssp1[2,k]*FLOAT(m)*h);
        END;
        FOR k:=1 TO Nc1 DO
          abs:=abs+csp1[1,k]*cos(2.0*pi*csp1[2,k]*FLOAT(m)*h);
        END;
        abs:=abs+disp;
        WriteNBytes(f,address1,nbytes1,written);
        abs:=0.0;disp:=0.0;
        FOR k:=1 TO Ns2 DO
          disp:=disp+ssp2[1,k]*sin(2.0*pi*ssp2[2,k]*FLOAT(m)*h);
        END;
        FOR k:=1 TO Nc2 DO
          abs:=abs+csp2[1,k]*cos(2.0*pi*csp2[2,k]*FLOAT(m)*h);
        END;
        disp:=disp+abs;
        WriteNBytes(f,address2,nbytes1,written);
      END;    
    END;
    Close(f,done);Close(in,done);
  END;
  ScreenMode(3);
  WriteString('This program has the ability to calculate the 2D ');WriteLn;
  WriteString('FFT of the 2D- timedomain signal. This operation ');WriteLn;
  WriteString('however, is extremely slow. It is recommanded to ');WriteLn;
  WriteString('use PV-wave to perform the 2D FFT on the time do-');WriteLn;
  WriteString("main signal (option 1). If you aren't in a hurry ");WriteLn;
  WriteString('use option 2.');WriteLn;WriteLn;
  WriteString('Thus the options are : ');WriteLn;
  WriteString('  1. Write a PV-Wave processable 2D time domain signal');WriteLn;
  WriteString('  2. Perform FFT using this programs slow algorithm');WriteLn;
  WriteString('     and then write a PV-Wave Processable spectrum');WriteLn;WriteLn;
  WriteString('Your choice [1,2] : ');ReadInt(choice);WriteLn;
  IF choice=2 THEN       
    Calc2Dfft;
    Create_PV_WAVE_File;
  ELSE  
    Create_PV_WAVE_File;
  END;  
END Determine2DSpectrum;

PROCEDURE Calc2Dfft;
VAR func:ARRAY[0..2048] OF REAL;
    f,g,h:File;
    i:INTEGER;
    written,nbytes1,nbytes2,nbytes,read:CARDINAL;
    ftype,fndw,z,l,n,k,dummy,pp:INTEGER;
    addr1,addr2,strtaddr:ADDRESS;
    done:BOOLEAN;
BEGIN
  
  Open(f,'ilvsign.bin',ReadOnly,done);
  Create(g,'spectra.bin',done);
  ReadBinFileHeader(f,ftype,pp,fndw);
  WriteBinFileHeader(g,ftype,pp,fndw);
  nbytes:=8;
  nbytes1:=2*pp*8;
  strtaddr:=ADR(func[0]);
  FOR z:=1 TO fndw DO
    ReadNBytes(f,strtaddr,nbytes1,read);
    IF nbytes1 # read THEN
      WriteString('>>> Read Binary File Error   <<<');Delay(1000);WriteLn;
    END;     
    func[0]:=func[0]/2.0;
    func[1]:=func[1]/2.0;
    FOR l:=0 TO pp-1 DO
      func[2*l]:=func[2*l]*exp(-FLOAT(l)*6.0/FLOAT(pp));
      func[2*l+1]:=func[2*l+1]*exp(-FLOAT(l)*6.0/FLOAT(pp));
    END;  
    four2(func,pp,FALSE);
    WriteString('ft  ');
    WriteNBytes(g,strtaddr,nbytes1,read);
  END;  
  WriteLn;
  Close(g,done);Close(f,done);
  
  Create(h,'sort.bin',done);
  Open(f,'spectra.bin',ReadOnly,done);
  ReadBinFileHeader(f,ftype,pp,fndw);
  WriteBinFileHeader(h,ftype,pp,fndw);
  addr1:=ADR(func[0]);
  addr2:=ADR(func[pp]);
  nbytes1:=2*pp*8;
  nbytes2:=pp*8;
  FOR k:=0 TO fndw-1 DO
    ReadNBytes(f,addr1,nbytes1,read);
    WriteNBytes(h,addr2,nbytes2,written);
    WriteNBytes(h,addr1,nbytes2,written);
  END;  
  Close(f,done);Close(h,done);

  TransposeDataMatrix('sort.bin','spectra.bin');

  Open(f,'spectra.bin',ReadOnly,done);
  Create(g,'sort.bin',done);
  ReadBinFileHeader(f,ftype,pp,fndw);
  WriteBinFileHeader(g,ftype,pp,fndw);
  nbytes:=8;
  nbytes1:=2*pp*8;
  strtaddr:=ADR(func[0]);
  FOR z:=1 TO fndw DO
    ReadNBytes(f,strtaddr,nbytes1,read);
    IF nbytes1 # read THEN
      WriteString('>>> Read Binary File Error   <<<');Delay(1000);WriteLn;
    END;     
    func[0]:=func[0]/2.0;
    func[1]:=func[1]/2.0;
    FOR l:=0 TO pp-1 DO
      func[2*l]:=func[2*l]*exp(-FLOAT(l)*6.0/FLOAT(pp));
      func[2*l+1]:=func[2*l+1]*exp(-FLOAT(l)*6.0/FLOAT(pp));
    END;  
    four2(func,pp,FALSE);
    WriteNBytes(g,strtaddr,nbytes,read);
    WriteString('ft  ');
  END;  
  Close(g,done);Close(f,done);
  WriteLn;
  Create(h,'sort.bin',done);
  Open(f,'spectra.bin',ReadOnly,done);
  ReadBinFileHeader(f,ftype,pp,fndw);
  WriteBinFileHeader(h,ftype,pp,fndw);
  addr1:=ADR(func[0]);
  addr2:=ADR(func[pp]);
  nbytes1:=2*pp*8;
  nbytes2:=pp*8;
  FOR k:=0 TO fndw-1 DO
    ReadNBytes(f,addr1,nbytes1,read);
    WriteNBytes(h,addr2,nbytes2,written);
    WriteNBytes(h,addr1,nbytes2,written);
  END;  
  Close(f,done);Close(h,done);
  (* zero of spectrum in evolution direction now in the middle *)

  TransposeDataMatrix('sort.bin','spectra.bin');
  (* Data Matrix has its original dimensions again *)

END Calc2Dfft;  

PROCEDURE TransposeDataMatrix(Inmatname,Trmatname:ARRAY OF CHAR);
VAR k,l,i,counter,pp,fndw,ftype:INTEGER;
    dummy,okdata:REAL;
    read:CARDINAL;
    h,f:File;
    dumaddr,address:ADDRESS;
    done:BOOLEAN; 
BEGIN
  WriteString('Transposing matrix');WriteLn;
  Open(h,Inmatname,ReadOnly,done);
  
  IF done THEN
    WriteString('Opened file : ');WriteString(Inmatname);
  END;
  Create(f,Trmatname,done);
  IF done THEN
    WriteString('Created file : ');WriteString(Trmatname);
  END;
          
  ReadBinFileHeader(h,ftype,pp,fndw);
  WriteBinFileHeader(f,ftype,fndw,pp);
  dumaddr:=ADR(dummy);
  address:=ADR(okdata);
  FOR i:=0 TO pp-1 DO
    WriteInt(i,5);WriteLn;
    Reset(h);      
    counter:=0;
    LOOP
      (* read until correct matrix point has been found *)
      IF counter=i THEN EXIT END;
      ReadNBytes(h,dumaddr,8,read);
      ReadNBytes(h,dumaddr,8,read);
      counter:=counter+1;
    END;
    FOR k:=0 TO fndw-1 DO 
      ReadNBytes(h,address,8,read);  
      WriteNBytes(f,address,8,read);
      ReadNBytes(h,address,8,read);
      WriteNBytes(f,address,8,read);      
      FOR l:=1 TO 2*pp-2  DO
        ReadNBytes(h,dumaddr,8,read);
      END;      
    END;
  END;
  Close(f,done);Delete(h,done);
END TransposeDataMatrix;  

PROCEDURE DispTransvSign;
VAR func:ARRAY[0..2048] OF REAL;
    f,g:File;
    nbytes,read:CARDINAL;
    ftype,fndw,z,l,n,dummy,pp:INTEGER;
    strtaddr:ADDRESS;
    done:BOOLEAN;
BEGIN
  strtaddr:=ADR(func[0]);
  Open(f,'ilvsign.bin',ReadOnly,done);
  Create(g,'spectra.bin',done);
  ReadBinFileHeader(f,ftype,pp,fndw);
  WriteBinFileHeader(g,ftype,pp,fndw);
  nbytes:=pp*8*2;
  FOR z:=1 TO fndw DO
    ReadNBytes(f,strtaddr,nbytes,read);
    IF nbytes # read THEN
      WriteString('>>> Read Binary File Error   <<<');Delay(1000);WriteLn;
    END;     
    func[0]:=func[0]/2.0;
    func[1]:=func[1]/2.0;
    FOR l:=0 TO pp-1 DO
      func[2*l]:=func[2*l]*exp(-FLOAT(l)*6.0/FLOAT(pp));
      func[2*l+1]:=func[2*l+1]*exp(-FLOAT(l)*6.0/FLOAT(pp));
    END;  
    four2(func,pp,FALSE);
    WriteNBytes(g,strtaddr,nbytes,read);
  END;  
  Close(g,done);Close(f,done);  
  ScreenMode(3);      
END DispTransvSign;  

PROCEDURE DisplaySignals(ap:INTEGER);
VAR plx,ply,xaxis:ARRAY[0..1024] OF REAL;
    f,g:File;
    nbytes,read:CARDINAL;
    i,pp,ftype,fndw,z,l,n,dummy:INTEGER;
    addr1,addr2:ADDRESS;
    done:BOOLEAN;
BEGIN
  nbytes:=8;
  Open(g,'spectra.bin',ReadOnly,done);
  Open(f,'ilvsign.bin',ReadOnly,done);
  OpenOutput('DAT');
  ReadBinFileHeader(f,ftype,pp,fndw);
  ReadBinFileHeader(g,ftype,pp,fndw);
  FOR z:=1 TO fndw DO
    FOR l:=0 TO pp-1 DO
      addr1:=ADR(plx[l]);
      addr2:=ADR(ply[l]);
      ReadNBytes(f,addr1,nbytes,read);
      ReadNBytes(f,addr2,nbytes,read);
      xaxis[l]:=FLOAT(l);         
    END;
    FOR l:=0 TO pp-1 DO
      WriteCard(5*z-4,4);WriteString("   ");WriteReal(FLOAT(l),12);
      WriteString("   ");WriteReal(plx[l],12);WriteLn; 
      WriteCard(5*z-3,4);WriteString("   ");WriteReal(FLOAT(l),12);
      WriteString("   ");WriteReal(ply[l],12);WriteLn;
    END;
    tdplot1(xaxis,plx,pp,1,2);
    tdplot1(xaxis,ply,pp,1,2);    
    FOR l:=0 TO pp-1 DO
      addr1:=ADR(plx[l]);
      addr2:=ADR(ply[l]);
      ReadNBytes(g,addr1,nbytes,read);
      ReadNBytes(g,addr2,nbytes,read);         
    END;
    FOR i:=0 TO pp-1 DO
      xaxis[i]:=plx[i];
    END;
    FOR i:=0 TO (pp DIV 2)-1 DO
      plx[i]:=xaxis[(pp DIV 2)+i-1];
      plx[i+(ap DIV 2)]:=xaxis[i];  
    END;      
    FOR i:=0 TO pp-1 DO
      xaxis[i]:=ply[i];
    END;
    FOR i:=0 TO (pp DIV 2)-1 DO
      ply[i]:=xaxis[(pp DIV 2)+i-1];
      ply[i+(ap DIV 2)]:=xaxis[i];  
    END; 
    FOR i:=0 TO pp-1 DO
      xaxis[i]:=FLOAT(i);
    END;       
    IF pp>1 THEN
      tdplot1(xaxis,plx,pp,1,2);
      tdplot1(xaxis,ply,pp,1,2);
      FOR l:=0 TO pp-1 DO
        WriteCard(5*z-2,4);WriteString("   ");WriteReal(FLOAT(l),12);
        WriteString("   ");WriteReal(plx[l],12);WriteLn; 
        WriteCard(5*z-1,4);WriteString("   ");WriteReal(FLOAT(l),12);
        WriteString("   ");WriteReal(ply[l],12);WriteLn;          
        plx[l]:=sqrt(plx[l]*plx[l]+ply[l]*ply[l]);
        WriteCard(5*z,4);WriteString("   ");WriteReal(FLOAT(l),12);
        WriteString("   ");WriteReal(plx[l],12);WriteLn; 
      END; 
      tdplot1(xaxis,plx,pp,1,2);  
    ELSE
      WriteString('no xyplots of one point ...');ReadInt(dummy);WriteLn;
    END;
  END;  
  CloseOutput;
END DisplaySignals;

END CalcSpec.
