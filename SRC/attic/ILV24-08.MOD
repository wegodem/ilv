MODULE ilv;
IMPORT DebugPMD;
FROM Keyboard IMPORT KeyPressed;
FROM LogiFile IMPORT WriteNBytes,Create,File,Close,Open,ReadNBytes,
                     OpenMode,EndFile,Reset,GetPos,SetPos;
FROM Exec IMPORT Run,DosCommand;
FROM Strings IMPORT Concat;
FROM SYSTEM IMPORT ADDRESS,ADR;
FROM Sounds IMPORT Sound,NoSound;
FROM Delay IMPORT Delay;
FROM MathLib0 IMPORT sqrt,sin,cos,exp,arctan,ln,entier;
FROM RealInOut IMPORT WriteReal,ReadReal;
FROM Graphics IMPORT ScreenMode;
FROM InOut IMPORT Read, Write,
     ReadInt, ReadCard, ReadString,
     WriteInt, WriteCard, WriteString, WriteLn,
     EOL,OpenInput,CloseInput,OpenOutput,CloseOutput,Done;
FROM Directories IMPORT Delete,Rename,DirResult,DirQuery,DirQueryProc;
FROM plot386 IMPORT tdplot1;  
FROM PulseLib IMPORT AMPulseDefinition,FMPulseDefinition,
                     AMHardPulseDefinition;
FROM MatrixMathLib IMPORT CMatCommutator,CMatAdd,CMatMultAdd,CMatRMultAdd,
                          CMatMult,CMatZero,ShowCMatElements,CopyCMatrix,
                          RMultCMat,CMultCMat,ExpectationValue,CalcTrace,
                          ExpMatrix,DoRotation,NormCMatrix,CMatrix,
                          COMPLEXLONGREAL,DoSpectrum,TwoDiagonal;
FROM SpinOperatorLib IMPORT MakeKets,MakeMatrixIplus,MakeMatrixImin,
                            MakeMatrixIx,MakeMatrixIy,MakeMatrixIz,
                            Iarray,produktket;                          
FROM BinFiles IMPORT BinFileActions,ReadBinFileHeader,WriteBinFileHeader;
FROM FOUR3 IMPORT four2,fft;
FROM DataFilesInOut IMPORT RealArrayOut,RealArrayIn;

CONST
     Nstmax=16;
     Nspmax=4;
     pi = 3.1415926535897932; 
     version = "            386SX-TEST VERSION 11 : july  20, 1992        ";
TYPE 
     RArray = ARRAY[0..256] OF REAL;
     RMat = ARRAY[1..Nspmax],[1..Nspmax] OF REAL;
     String= ARRAY[0..80] OF CHAR;
     glnarray = ARRAY[0..134] OF REAL;  

VAR  
     ja:BOOLEAN; 
     rotframe,batch,pulseflag,ap,ndw,i,Nspins,n,calc,npar,ncom:INTEGER;
     h,ti,pt,x0,dx,gs:REAL;
     cn,cn1,cn2,cn3,cn4,cn5,Eval,x:COMPLEXLONGREAL;
     densmat,Izee,Izgrad,Ixtot,Iytot,
     Iztot,Icoup,optr:CMatrix;
     Ilist: Iarray;
     ket: produktket;
     xb,yb,offset:RArray;
     jc:RMat;
     par,pcom,xicom:glnarray;
     
PROCEDURE showket(ketnr: INTEGER); 
VAR 
    i: INTEGER; 
BEGIN 
  FOR i := 1 TO Nspins DO 
    Write( CHR(179)); 
    Write( " "); 
    WriteInt(ket[ketnr,i],3); 
    WriteString( "/2  "); 
  END; 
  WriteString( " >"); 
  WriteLn; 
END showket; 

PROCEDURE Showallkets; 

VAR 
    i: INTEGER; 
BEGIN 
  ScreenMode(3); 
  FOR i := 1 TO n DO 
    showket(i)
  END; 
  WriteLn; 
END Showallkets; 

PROCEDURE Ispins; 

VAR 
    k, Inumber: INTEGER; 
    klaar: BOOLEAN; 

BEGIN 
  klaar := FALSE; 
  WHILE ( NOT klaar) DO 
    klaar := TRUE; 
    FOR k := 1 TO Nspmax DO 
      Ilist[k] := 0
    END; 
    REPEAT 
      WriteLn;
      WriteString( "Number of Spins = "); 
      ReadInt(Nspins); 
    UNTIL (Nspins <= Nspmax); 
    FOR k := 1 TO Nspins DO 
      WriteLn;
      WriteString( "I quantum number of spin("); 
      WriteInt( k,3); 
      WriteString( ") = x/2  with   x = "); 
      ReadInt(Inumber); 
      Ilist[k] := Inumber; 
    END; 
    n := 1; 
    FOR k := 1 TO Nspins DO 
      n := (Ilist[k]+1)*n
    END; 
    WriteLn; 
    IF (n > Nstmax) THEN 
      klaar := FALSE
    END; 
  END; 
  WriteString("n = ");
  WriteInt(n,8);
  WriteLn;
END Ispins; 

PROCEDURE MakeBoltzmann; 

  VAR 
    i: INTEGER; 
    gamma: COMPLEXLONGREAL; 
    store: CMatrix; 
    okay:BOOLEAN;

BEGIN 
  CMatZero(n,densmat,densmat); 
  FOR i := 1 TO Nspins DO 
    CMatZero(n,store,store); 
    WriteLn;
    WriteString( "Relative Intensity of Spin ("); 
    WriteInt( i,3); 
    WriteString( ") = "); 
    ReadReal(gamma[1]);gamma[2]:=0.0;
    MakeMatrixIz(Ilist,ket,n,i,store); 
    NormCMatrix(n,store); 
    CMultCMat(n,gamma,store,store); 
    CMatMultAdd(n,store,cn1,densmat,cn1,densmat); 
  END;
END MakeBoltzmann; 

PROCEDURE MakeZeeman; 
  VAR 
    i: INTEGER; 
    gamma: COMPLEXLONGREAL; 
    store: CMatrix; 
    okay:BOOLEAN;
BEGIN 
  CMatZero(n,Izee,Izee); 
  FOR i := 1 TO Nspins DO 
    CMatZero(n,store,store); 
    WriteLn;
    WriteString( "Offset-frequency [Hz] of Spin ("); 
    WriteInt( i,3); 
    WriteString( ") = "); 
    ReadReal(gamma[1]);gamma[2]:=0.0;
    gamma[1]:=gamma[1]*2.0*pi;
    MakeMatrixIz(Ilist,ket,n,i,store); 
    CMultCMat(n,gamma,store,store); 
    CMatMultAdd(n,store,cn1,Izee,cn1,Izee); 
  END;
END MakeZeeman; 

PROCEDURE SpinSpinCoupling;
VAR mat,mat1,mat2:CMatrix;
	k,l,exact:INTEGER;
	j:REAL;
BEGIN
  CMatZero(n,mat,mat);
  ScreenMode(3);
  IF Nspins > 1 THEN
    WriteString('1. Weak-Coupling approximation. ');WriteLn;
    WriteString('2. Exact calculation. ');WriteLn;
    WriteString(' Your choice [1,2]');ReadInt(exact);WriteLn;
    WriteString('Give all Scalar coupling constants J(i,j)');WriteLn;
    WriteString('from the term J(i,j).I(i).S(j)');WriteLn;WriteLn;
    FOR k:=1 TO Nspins DO
      FOR l:= 1 TO Nspins DO
        IF l>k THEN
          WriteString('Scalar Coupling Constant J(');WriteInt(k,1);
          WriteString(',');WriteInt(l,1);WriteString(')  = ');  
          ReadReal(jc[k,l]);WriteLn;WriteLn;
          j:=2.0*jc[k,l]*pi; (* ! *)
          IF j # 0.0 THEN
            WriteString("Calculating J.I(n).I(m)-TERM");WriteLn;
            IF exact#1 THEN
              (* *)
              MakeMatrixIx(Ilist,ket,n,k,mat1);
              MakeMatrixIx(Ilist,ket,n,l,mat2);
              CMatMult(n,mat1,mat2,mat1);
              RMultCMat(n,j,mat1,mat1);
              CMatAdd(n,mat1,mat,mat); 
              (* *)
              MakeMatrixIy(Ilist,ket,n,k,mat1);
              MakeMatrixIy(Ilist,ket,n,l,mat2);
              CMatMult(n,mat1,mat2,mat1);
              RMultCMat(n,j,mat1,mat1);
              CMatAdd(n,mat1,mat,mat); 
            END;       
            MakeMatrixIz(Ilist,ket,n,k,mat1);
            MakeMatrixIz(Ilist,ket,n,l,mat2);
            CMatMult(n,mat1,mat2,mat1);
            RMultCMat(n,j,mat1,mat1);
            CMatAdd(n,mat1,mat,mat);
          END;
        END;
      END;
    END;
    CopyCMatrix(n,mat,Icoup);
  END;
END SpinSpinCoupling;


PROCEDURE MakeGradientOperator; 
  VAR 
    i: INTEGER; 
    gamma: COMPLEXLONGREAL; 
    store: CMatrix; 
    okay:BOOLEAN;
BEGIN 
  CMatZero(n,Izgrad,Izgrad); 
  FOR i := 1 TO Nspins DO 
    CMatZero(n,store,store); 
    WriteLn;
    MakeMatrixIz(Ilist,ket,n,i,store); 
    CMatMultAdd(n,store,cn1,Izgrad,cn1,Izgrad); 
  END;
END MakeGradientOperator; 

PROCEDURE MakeTotalIx(Norm:BOOLEAN); 
     
VAR 
    i: INTEGER; 
    gamma: COMPLEXLONGREAL; 
    store: CMatrix; 
    okay:BOOLEAN;

BEGIN 
  CMatZero(n,Ixtot,Ixtot); 
  FOR i := 1 TO Nspins DO 
    CMatZero(n,store,store); 
    MakeMatrixIx(Ilist,ket,n,i,store); 
    IF Norm THEN
      NormCMatrix(n,store);
    END;
    CMatMultAdd(n,store,cn1,Ixtot,cn1,Ixtot); 
  END;
END MakeTotalIx; 

PROCEDURE MakeTotalIy(Norm:BOOLEAN); 
VAR 
    i: INTEGER; 
    gamma: COMPLEXLONGREAL; 
    store: CMatrix; 
    okay:BOOLEAN;
BEGIN 
  CMatZero(n,Iytot,Iytot); 
  FOR i := 1 TO Nspins DO 
    CMatZero(n,store,store); 
    MakeMatrixIy(Ilist,ket,n,i,store); 
    IF Norm THEN
      NormCMatrix(n,store);
    END;
    CMatMultAdd(n,store,cn1,Iytot,cn1,Iytot); 
  END;
END MakeTotalIy; 

PROCEDURE MakeTotalIz(Norm:BOOLEAN); 
     
VAR 
    i: INTEGER; 
    gamma: COMPLEXLONGREAL; 
    store: CMatrix; 
    okay:BOOLEAN;

BEGIN 
  CMatZero(n,Iztot,Iztot); 
  FOR i := 1 TO Nspins DO 
    CMatZero(n,store,store); 
    MakeMatrixIz(Ilist,ket,n,i,store);
    IF Norm THEN 
      NormCMatrix(n,store);
    END;
    CMatMultAdd(n,store,cn1,Iztot,cn1,Iztot); 
  END;
END MakeTotalIz;

PROCEDURE SetupSpins; 
BEGIN 
  ScreenMode(3);WriteLn;WriteLn;WriteLn; 
  WriteString('Definition of the spinsystem: ');
  WriteLn;
  Ispins; 
  MakeKets(ket, Ilist); 
  MakeBoltzmann; 
END SetupSpins; 

PROCEDURE init ;
BEGIN
  rotframe:=0;
  batch:=0;
  x0:=0.0;dx:=0.0;gs:=0.0;ndw:=1;(* default gradient setting *)
  calc:=0;
  pulseflag:=2;
  cn[1]:=0.0;cn[2]:=1.0;
  cn1[1]:=1.0;cn1[2]:=0.0;
  cn2[1]:=0.5;cn2[2]:=0.0;
  cn3[1]:=2.0;cn3[2]:=0.0;
  cn4[1]:=1.0/6.0;cn4[2]:=0.0;
  cn5[1]:=-1.0;cn5[2]:=0.0;
  CMatZero(n,densmat,densmat);
  CMatZero(n,Izee,Izee);
  CMatZero(n,Icoup,Icoup);
  CMatZero(n,Izgrad,Izgrad);
  CMatZero(n,Ixtot,Ixtot);
  CMatZero(n,Iytot,Iytot);
  CMatZero(n,Iztot,Iztot);
END init;

PROCEDURE DefinegradientOffset;
VAR
BEGIN
  ScreenMode(3);WriteLn;WriteLn;WriteLn;
  WriteString('*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*');WriteLn;
  WriteString('*_* GRADIENT INITIALISATION *_*');WriteLn;
  WriteString('*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*');WriteLn;WriteLn;
  MakeGradientOperator;
  WriteString('Give the number of steps in the offset-');WriteLn;
  WriteString('frequency direction [-] : ');ReadInt(ndw);
  WriteLn;
  WriteString('Give the gradient strength [Hz/cm] : ');
  ReadReal(gs);WriteLn;
  gs:=gs*2.0*pi;
  WriteString('Give the stepsize dx [cm] : ');
  ReadReal(dx);WriteLn;
  WriteString('Give the spatial offset x(0) [cm] : ');ReadReal(x0);
  WriteLn;
END DefinegradientOffset;

PROCEDURE DefinePulseHamiltonian;
VAR answer,hard:INTEGER;
BEGIN 
  ScreenMode(3);WriteLn;WriteLn;WriteLn;
  WriteString('*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*');WriteLn;
  WriteString('     DEFINITION OF PULSE HAMILTONIAN     ');WriteLn;
  WriteString('*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*');WriteLn;
  WriteLn;WriteLn;
  WriteString('Apply a standard hardpulse 90 or 180 [1 =yes]');ReadInt(hard);
  WriteLn;
  IF hard=1 THEN
    pulseflag:=0;
    ap:=1;ti:=1.0e-5;h:=1.0e-5;(*both h and ti must have the same value!!*)
    AMHardPulseDefinition(ap,ti,xb,yb);
  ELSE  
    WriteString('What is the pulse time [sec]  :');ReadReal(pt);
    WriteLn;WriteLn;
    WriteString(' How many pulse-time intervals [-] : '); 
    ReadInt(ap);WriteLn;WriteLn;
    h:=pt/FLOAT(ap);
    ti:=pt/FLOAT(ap);
    WriteString('Which form has the Pulse Hamiltonian ?');
    WriteLn;
    WriteString('1. Hp(t) = f(t).Ix   (AM modulated) ');WriteLn;
    WriteString('2. Hp(t) = f(t).Ix + g(t).Iy (FM or adiabatic)');WriteLn;
    WriteString('3. Pulse file (Bx,By) from disk');WriteLn;
    WriteString('4. Pulse file (Bx,Bz) from disk');WriteLn;
    WriteLn;
    WriteString(' Your choice is [1,2,3] : ');
    ReadInt(answer);WriteLn;
    IF answer=1 THEN pulseflag:=0;END;
    IF answer=2 THEN pulseflag:=1;END;
    IF answer=3 THEN pulseflag:=1;END;
    IF answer=4 THEN pulseflag:=1;END;
    CASE answer OF 
      1:AMPulseDefinition(ap,ti,xb,yb);
        FOR i:=0 TO ap-1 DO
          offset[i]:=0.0;
        END;|
      2:FMPulseDefinition(ap,ti,xb,yb);            
        FOR i:=0 TO ap-1 DO
          offset[i]:=0.0;
        END;|
      3:ReadBxByPulse(ap,xb,yb,offset);|
      4:ReadBxBzPulse(ap,xb,yb,offset);
        rotframe:=1;
    END;
  END;  
END DefinePulseHamiltonian;

PROCEDURE ReadBxByPulse(ap:INTEGER;VAR xb,yb,offset:RArray);
VAR i:INTEGER;
BEGIN
  WriteLn;WriteLn;
  WriteString('File name Bx(t)');
  RealArrayIn(ap,xb);
  WriteString('File name By(t)');
  RealArrayIn(ap,yb);
  FOR i:=0 TO ap-1 DO
    offset[i]:=0.0;
  END;  
END ReadBxByPulse;

PROCEDURE ReadBxBzPulse(ap:INTEGER;VAR xb,yb,offset:RArray);
VAR i:INTEGER;
BEGIN
  WriteLn;WriteLn;
  WriteString('Name RF amplitude file Bx(t)');WriteLn;
  RealArrayIn(ap,xb);
  WriteString('Name of RF offset frequency file dW(t)');WriteLn;
  RealArrayIn(ap,offset);
  FOR i:=0 TO ap-1 DO
    yb[i]:=0.0;
  END;  
END ReadBxBzPulse;


PROCEDURE OutputPulse;
VAR
BEGIN
  WriteString('Output x-component RF-pulse ');WriteLn;
  RealArrayOut(ap,xb);
  WriteString('Output y-component RF-pulse ');WriteLn;
  RealArrayOut(ap,yb);
  WriteString('Output offset sweep of RF-pulse ');WriteLn;
  RealArrayOut(ap,offset);
END OutputPulse;  

PROCEDURE Rotate;
VAR hc,hb,hs,ht,r:CMatrix;
    dw:REAL;
    k,l,dummy,j,ftype,fn,fndw:INTEGER;
    written,read,nbytes,highpos,lowpos:CARDINAL;
    inout,out:File;
    result:DirResult;
    address1,address2:ADDRESS;
    done:BOOLEAN;
BEGIN
  IF pulseflag = 2 THEN
    WriteString('>>>> NO RF-PULSE DEFINED ERROR <<<<');WriteLn;
  ELSE
    IF calc = 0 THEN 
      Create(inout,'dm.bin',done);
      WriteBinFileHeader(inout,0,n,ndw); 
      nbytes:=n*8*2;
      FOR k:=1 TO ndw DO
        FOR j:= 1 TO n DO        
          address1:=ADR(densmat[j,1,1]);
          WriteNBytes(inout,address1,nbytes,written);          
          IF written # nbytes THEN
            WriteString('>>> WRITE BINARY-FILE ERROR <<<');Delay(1000);WriteLn;  
          END;
        END;     
      END;
      Close(inout,done);
      CMatZero(n,densmat,densmat);
    END;  
    Open(inout,'dm.bin',ReadOnly,done); 
    ReadBinFileHeader(inout,ftype,fn,fndw);
    IF (ftype=0) AND (fn=n) AND (fndw=ndw) THEN 
      CMatAdd(n,Izee,Icoup,hc);    
      Create(out,'store',done);
      WriteBinFileHeader(out,0,n,ndw); 
      nbytes:=n*8*2;
      FOR k:=1 TO ndw DO
        CMatZero(n,r,r);
        FOR j:=1 TO n DO
          r[j,j,1]:=1.0;
        END;           
        FOR j:= 1 TO n DO
          address1:=ADR(densmat[j,1,1]);
          ReadNBytes(inout,address1,nbytes,read);        
          IF read # nbytes THEN
            WriteString('>>> READ BINARY FILE ERROR <<<');Delay(1000);WriteLn;
          END;
        END;
        FOR l:=0 TO ap-1 DO
          dw:=(x0+FLOAT(k-1)*dx)*gs+offset[l];
          RMultCMat(n,dw,Izgrad,hs);
          CMatAdd(n,hc,hs,hs);
          RMultCMat(n,xb[l],Ixtot,ht);
          CMatAdd(n,hs,ht,hb);
          RMultCMat(n,yb[l],Iytot,ht);
          CMatAdd(n,ht,hb,hb);
          RMultCMat(n,-1.0,hb,hb);
          ExpMatrix(n,hb,h,hb);
          CMatMult(n,r,hb,r);      
        END;
        DoRotation(n,r,densmat);
        FOR j:=1 TO n DO
          address1:=ADR(densmat[j,1,1]);       
          WriteNBytes(out,address1,nbytes,written);
          IF written # nbytes THEN
            WriteString('>>> WRITE TO BINARY FILE ERROR <<<');Delay(1000);WriteLn;
          END;
        END;
        WriteString('*');WriteLn;          
      END;
      Close(inout,done);Close(out,done);
      Delete('dm.bin',result);
      Rename('store','dm.bin',result);
      calc:=calc+1;
    ELSE 
      Close(inout,done);
      WriteString('Dimensions of dm.bin-file are : ');WriteLn;
      WriteInt(ftype,5);WriteInt(fn,5);WriteInt(fndw,5);WriteLn;
      WriteString('Expected dimensions are : ');WriteLn;
      WriteInt(0,5);WriteInt(n,5);WriteInt(ndw,5);WriteLn;
      WriteString('>>> NO CALCULATION <<<');ReadInt(dummy);      
    END;
  END;  
  ScreenMode(3);    
END Rotate;

PROCEDURE B1Inhomogeneity;
(***************************************************************)
(***  This procedure calculates the effects of B1-variation  ***)
(***  for a single offset frequency.                         ***)
(***************************************************************)
VAR hc,hs,ht,r:CMatrix;
    factor,dw,minfac,maxfac:REAL;
    k,l,dummy,j,ftype,fn,fndw:INTEGER;
    written,read,nbytes,highpos,lowpos:CARDINAL;
    inout,out:File;
    address1,address2:ADDRESS;
    result:DirResult;
    done:BOOLEAN;
    stx,sty:ARRAY[0..128] OF REAL;
BEGIN
  MakeGradientOperator;
  IF pulseflag = 2 THEN
    WriteString('>>>> NO RF-PULSE DEFINED ERROR <<<<');WriteLn;    
  ELSE
    FOR i:=0 TO ap-1 DO 
      stx[i]:=xb[i];
      sty[i]:=yb[i];
    END;
    ScreenMode(3);
    WriteString('********************************************************************');
    WriteLn; 
    WriteString('***        This part of the program integrates the equations     ***'); 
    WriteLn;
    WriteString('***  for a number of RF-amplitudes ,but single offset frequency: ***'); 
    WriteLn;
    WriteString('***                B1[n] := B1[n]*multiplicationfactor           ***'); 
    WriteLn;
    WriteString('********************************************************************');
    WriteLn;WriteLn;
    WriteString('Number of RF amplitudes is : ');ReadInt(ndw); 
    WriteString('Minimal multiplication factor :');ReadReal(minfac); 
    WriteLn;
    WriteString('Maximal multiplication factor :');ReadReal(maxfac);
    WriteLn;  
    IF calc = 0 THEN 
      Create(inout,'dm.bin',done);
      WriteBinFileHeader(inout,0,n,ndw); 
      nbytes:=n*8*2;
      FOR k:=1 TO ndw DO
        FOR j:= 1 TO n DO        
          address1:=ADR(densmat[j,1,1]);
          WriteNBytes(inout,address1,nbytes,written);          
          IF nbytes # written THEN
            WriteString('>>> WRITE BINARY-FILE ERROR <<<');WriteLn;  
          END;
        END;     
      END;
      Close(inout,done);
      CMatZero(n,densmat,densmat);
    END;  
    Open(inout,'dm.bin',ReadOnly,done); 
    ReadBinFileHeader(inout,ftype,fn,fndw);
    IF (ftype=0) AND (fn=n) AND (fndw=ndw) THEN 
      CMatAdd(n,Izee,Icoup,hc);    
      Create(out,'store',done);
      WriteBinFileHeader(out,0,n,ndw); 
      nbytes:=n*8*2;
      FOR k:=0 TO ndw-1 DO
        factor:=minfac+FLOAT(k)*(maxfac-minfac)/(FLOAT(ndw-1)); 
        FOR i:=0 TO ap-1 DO
          xb[i]:=stx[i]*factor;
          yb[i]:=sty[i]*factor;
        END;
        CMatZero(n,r,r);
        FOR j:=1 TO n DO
          r[j,j,1]:=1.0;
        END;           
        FOR j:= 1 TO n DO
          address1:=ADR(densmat[j,1,1]);
          ReadNBytes(inout,address1,nbytes,read);        
          IF read # nbytes THEN
            WriteString('>>> READ BINARY FILE ERROR <<<');WriteLn;
          END;
        END;
        FOR l:=0 TO ap-1 DO
          dw:=x0*gs+offset[l];
          RMultCMat(n,dw,Izgrad,hs);
          CMatAdd(n,hc,hs,hs);
          RMultCMat(n,xb[l],Ixtot,ht);
          CMatAdd(n,hs,ht,hs);
          RMultCMat(n,yb[l],Iytot,ht);
          CMatAdd(n,ht,hs,hs);
          RMultCMat(n,-1.0,hs,hs);
          ExpMatrix(n,hs,h,hs);
          CMatMult(n,r,hs,r);      
        END;
        DoRotation(n,r,densmat);
        FOR j:=1 TO n DO
          address1:=ADR(densmat[j,1,1]);       
          WriteNBytes(out,address1,nbytes,written);
          IF nbytes # written  THEN
            WriteString('>>> WRITE TO BINARY FILE ERROR <<<');WriteLn;
          END;
        END;
        WriteString('*');          
      END;
      Close(inout,done);Close(out,done);
      Delete('dm.bin',result);
      Rename('store','dm.bin',result);
      calc:=calc+1;
    ELSE 
      Close(inout,done);
      WriteString('Dimensions of dm.bin-file are : ');WriteLn;
      WriteInt(ftype,5);WriteInt(fn,5);WriteInt(fndw,5);WriteLn;
      WriteString('Expected dimensions are : ');WriteLn;
      WriteInt(0,5);WriteInt(n,5);WriteInt(ndw,5);WriteLn;
      WriteString('>>> NO CALCULATION <<<');ReadInt(dummy);      
    END;
  END; 
  FOR i:=0 TO ap-1 DO
    xb[i]:=stx[i];
    yb[i]:=sty[i]; 
  END; 
END B1Inhomogeneity;


PROCEDURE PulseToPulseDensity;

(* **************************************************************************
   This procedure is meant for a free spin with s=1/2 although more comple-
   cated systems can also be examined. The procedure needs:{{n = dimension of 
   the square densmat; ap number of points the RF-pulse consists; ndw = num-
   ber of steps in offset direction; h*ap = Total RF-pulse time; gx = gra-
   dient strength in Hz/cm; x0 offset in cm; dx is stepsize in cm ; the 
   matrices Izee (Zeeman operator matrix); Icoup J-coupling matrix; the 
   operator Ixtot , Iytot which are the total transverse operator matrices
   Izgrad the gradient operator; calc an integer: calc= 1 means dm.bin cannot
   be used as ininial state file calc#1 dm.bin = initial state file; densmat 
   a variable matrix: this matrix contains allways the latest result of a 
   calculation.}}     as input
   For an array of initial states the digitized pulse xb[n] is converted to
   pulse density pulse, and its effect on the density matrix is calculated.
   ************************************************************************ *)
   
VAR hc,hb,hs,ht,r: CMatrix;
    alfa,dw,strength,b1mod,modon,modoff,b1modx,b1mody,time,sort: REAL;
    k,l,dummy,j,ftype,fn,fndw,on,off: INTEGER;
    written,read,nbytes,highpos,lowpos: CARDINAL;
    inout,out: File;
    result: DirResult;
    address1,address2: ADDRESS;
    done: BOOLEAN;
BEGIN
  IF pulseflag = 2 THEN
    WriteString('>>>> NO RF-PULSE DEFINED ERROR <<<<');WriteLn;
  ELSE
    IF calc = 0 THEN 
      Create(inout,'dm.bin',done);
      WriteBinFileHeader(inout,0,n,ndw); 
      nbytes:=n*8*2;
      FOR k:=1 TO ndw DO
        FOR j:= 1 TO n DO        
          address1:=ADR(densmat[j,1,1]);
          WriteNBytes(inout,address1,nbytes,written);          
          IF written # nbytes THEN
            WriteString('>>> WRITE BINARY-FILE ERROR <<<');Delay(1000);WriteLn;  
          END;
        END;     
      END;
      Close(inout,done);
      CMatZero(n,densmat,densmat);
    END;  
    Open(inout,'dm.bin',ReadOnly,done); 
    ReadBinFileHeader(inout,ftype,fn,fndw);
    IF (ftype=0) AND (fn=n) AND (fndw=ndw) THEN 
      CMatAdd(n,Izee,Icoup,hc);    
      Create(out,'store',done);
      WriteBinFileHeader(out,0,n,ndw);
      nbytes:=n*8*2;
      b1mod:=0.0;
      FOR l:= 0 TO ap-1 DO
        sort:=sqrt(xb[l]*xb[l]+yb[l]*yb[l]);
        IF sort > b1mod THEN
          b1mod:=sort;          (*  100% modulation if B1 = B1(max) *)
        END;
      END;
      OutputPWMPulse; 
   (* The amplitude of the B1-pwm field is ABS(B1(max)) sample & hold *)
      FOR k:=1 TO ndw DO
        CMatZero(n,r,r);
        FOR j:=1 TO n DO
          r[j,j,1]:=1.0;
        END;           
        FOR j:= 1 TO n DO
          address1:=ADR(densmat[j,1,1]);
          ReadNBytes(inout,address1,nbytes,read);        
          IF read # nbytes THEN
            WriteString('>>> READ BINARY FILE ERROR <<<');Delay(1000);WriteLn;
          END;
        END;
        dw:=(x0+FLOAT(k-1)*dx)*gs;
        RMultCMat(n,dw,Izgrad,hs);
        CMatAdd(n,hc,hs,hs);
        FOR l:=0 TO ap-1 DO
          time:=FLOAT(l)*h;
          strength:=sqrt(xb[l]*xb[l]+yb[l]*yb[l]);
          alfa:=strength/b1mod;
          b1modx:=xb[l]*b1mod/strength;
          b1mody:=yb[l]*b1mod/strength;
          modon:=alfa*h;
          modoff:=(1.0-alfa)*h;   
          
      (* make times discrete 7t simulation *)

(*   
          on:=TRUNC((modon/1.0e-6)+0.5);  
          modon:=FLOAT(on)*1.0e-6;
          off:=TRUNC((modoff/1.0e-6)+0.5);
          modoff:=FLOAT(off)*1.0e-6;
*)  
         
          RMultCMat(n,b1modx,Ixtot,ht);
          CMatAdd(n,hs,ht,hb);
          RMultCMat(n,b1mody,Iytot,ht);
          CMatAdd(n,ht,hb,hb);
          RMultCMat(n,-1.0,hb,hb);
          ExpMatrix(n,hb,modon,hb);
          CMatMult(n,r,hb,r); 
          RMultCMat(n,-1.0,hs,hb);
          ExpMatrix(n,hb,modoff,hb);
          CMatMult(n,r,hb,r);     
        END;
        DoRotation(n,r,densmat);
        FOR j:=1 TO n DO
          address1:=ADR(densmat[j,1,1]);       
          WriteNBytes(out,address1,nbytes,written);
          IF written # nbytes THEN
            WriteString('>>> WRITE TO BINARY FILE ERROR <<<');Delay(1000);WriteLn;
          END;
        END;
        WriteString('*');          
      END;
      WriteLn;
      Close(inout,done);Close(out,done);
      Delete('dm.bin',result);
      Rename('store','dm.bin',result);
      calc:=calc+1;
    ELSE 
      Close(inout,done);
      WriteString('Dimensions of dm.bin-file are : ');WriteLn;
      WriteInt(ftype,5);WriteInt(fn,5);WriteInt(fndw,5);WriteLn;
      WriteString('Expected dimensions are : ');WriteLn;
      WriteInt(0,5);WriteInt(n,5);WriteInt(ndw,5);WriteLn;
      WriteString('>>> NO CALCULATION <<<');ReadInt(dummy);      
    END;
  END;  
  ScreenMode(3);    
END PulseToPulseDensity;

PROCEDURE OutputPWMPulse;
VAR phase,strength,b1modx,b1mody,sort,b1mod,time,modon,modoff,alfa:REAL;
    l,on,off:INTEGER;
BEGIN
  FOR l:= 0 TO ap-1 DO
    sort:=sqrt(xb[l]*xb[l]+yb[l]*yb[l]);
    IF sort > b1mod THEN
      b1mod:=sort;
    END;
  END; 
  WriteString('File name PWM pulse = ');OpenOutput('DAT'); 

  WriteCard(1,5);WriteString("   ");WriteReal(0.0,18);
  WriteString("   ");WriteReal(0.0,18);WriteLn;  
  WriteCard(2,5);WriteString("   ");WriteReal(0.0,18);
  WriteString("   ");WriteReal(0.0,18);WriteLn;
  
  FOR l:=0 TO ap-1 DO
    time:=FLOAT(l)*h;
    strength:=sqrt(xb[l]*xb[l]+yb[l]*yb[l]);
    alfa:=strength/b1mod;
    b1modx:=xb[l]*b1mod/strength;
    b1mody:=yb[l]*b1mod/strength;
    modon:=alfa*h;
    
(* 
    on:=TRUNC((modon/1.0e-6)+0.5);
    WriteInt(on,7);
*) 
    modoff:=(1.0-alfa)*h;
(*  
    off:=TRUNC((modoff/1.0e-6)+0.5);
    WriteInt(off,7);
    phase:=arctan(yb[l]/(xb[l]+1.0e-12))*180.0/pi;
    IF (xb[l]>=0.0) AND (yb[l]>=0.0) THEN
      WriteInt(TRUNC(phase+0.5),8);
    END;
    IF (xb[l]>0.0) AND (yb[l]<0.0) THEN
      WriteInt(TRUNC(phase+360.0+0.5),8);
    END;
    IF (xb[l]<0.0) AND (yb[l]>0.0) THEN
      WriteInt(TRUNC(phase+180.0+0.5),8);
    END;
    IF (xb[l]<0.0) AND (yb[l]<0.0) THEN
      WriteInt(TRUNC(phase+180.0+0.5),8);
    END;        
    WriteLn;
*)  
    WriteCard(1,5);WriteString("   ");WriteReal(time,18);
    WriteString("   ");WriteReal(b1modx/(2.0*pi),18);WriteLn;  
    WriteCard(1,5);WriteString("   ");WriteReal(time+modon,18);
    WriteString("   ");WriteReal(b1modx/(2.0*pi),18);WriteLn;
    WriteCard(1,5);WriteString("   ");WriteReal(time+modon,18);
    WriteString("   ");WriteReal(0.0,18);WriteLn;  
    WriteCard(1,5);WriteString("   ");WriteReal(time+modon+modoff,18);
    WriteString("   ");WriteReal(0.0,18);WriteLn;
    WriteCard(2,5);WriteString("   ");WriteReal(time,18);
    WriteString("   ");WriteReal(b1mody/(2.0*pi),18);WriteLn;  
    WriteCard(2,5);WriteString("   ");WriteReal(time+modon,18);
    WriteString("   ");WriteReal(b1mody/(2.0*pi),18);WriteLn;
    WriteCard(2,5);WriteString("   ");WriteReal(time+modon,18);
    WriteString("   ");WriteReal(0.0,18);WriteLn;  
    WriteCard(2,5);WriteString("   ");WriteReal(time+modon+modoff,18);
    WriteString("   ");WriteReal(0.0,18);WriteLn;

  END;
  CloseOutput;    
END OutputPWMPulse;

PROCEDURE SearchRotation;

(*  This procedure carries out an integration over an ap-time-points       *
 *  Hamiltonian of 'sample and hold'-type; this integration is performed   *
 *  for ndw-equidistant offset frequency points. ndw density matrices are  *
 *  stored on the hard disc. During the integration the offset frequency   *
 *  and time points are written to screen at which the expectation value   *
 *  of some previous defined operator has his maximumum value              *)
                               
VAR hc,hb,hs,ht,oper:CMatrix;
    dw,maxexpval:REAL;
    lr:COMPLEXLONGREAL;
    k,l,dummy,j,fn,ftype,fndw:INTEGER;
    written,read,nbytes,highpos,lowpos:CARDINAL;
    inout,out:File;
    result:DirResult;
    address1,address2:ADDRESS;
    done:BOOLEAN;
BEGIN
  IF pulseflag = 2 THEN
    WriteString('>>>> NO RF-PULSE DEFINED ERROR <<<<');WriteLn;
  ELSE
    maxexpval:=0.0;
    MakeExpectationValueOperator(n,oper);
    IF calc = 0 THEN
      WriteString('Creating new dm.bin  ...');Delay(1000);WriteLn; 
      Create(inout,'dm.bin',done);
      WriteBinFileHeader(inout,0,n,ndw); 
      nbytes:=n*8*2;
      FOR k:=1 TO ndw DO
        FOR j:= 1 TO n DO        
          address1:=ADR(densmat[j,1,1]);
          WriteNBytes(inout,address1,nbytes,written);          
          IF written # nbytes THEN
            WriteString('>>> WRITE BINARY-FILE ERROR <<<');Delay(1000);WriteLn;  
          END;
        END;     
      END;
      Close(inout,done);
    END;  
    Open(inout,'dm.bin',ReadOnly,done); 
    ReadBinFileHeader(inout,ftype,fn,fndw);
    IF (ftype=0) AND (fn=n) AND (fndw=ndw) THEN
      CMatAdd(n,Izee,Icoup,hc);    
      Create(out,'store',done);
      WriteBinFileHeader(out,0,n,ndw);
      nbytes:=n*8*2;
      FOR k:=1 TO ndw DO
        FOR j:= 1 TO n DO
          address1:=ADR(densmat[j,1,1]);
          ReadNBytes(inout,address1,nbytes,read);        
          IF read # nbytes THEN
            WriteString('>>> READ BINARY FILE ERROR <<<');Delay(1000);WriteLn;
          END;
        END;    
        dw:=(x0+FLOAT(k-1)*dx)*gs;
        RMultCMat(n,dw,Izgrad,hs);
        CMatAdd(n,hc,hs,hs);
        FOR l:=0 TO ap-1 DO
          RMultCMat(n,xb[l],Ixtot,ht);
          CMatAdd(n,hs,ht,hb);
          RMultCMat(n,yb[l],Iytot,ht);
          CMatAdd(n,ht,hb,hb);
          RMultCMat(n,-1.0,hb,hb);
          ExpMatrix(n,hb,h,hb);
          DoRotation(n,hb,densmat);
          ExpectationValue(n,densmat,oper,lr);
          IF ABS(lr[1]) > maxexpval THEN
            maxexpval:=ABS(lr[1]);
            WriteString('expectation value = ');
            WriteReal(lr[1],18);WriteString("   Offset : ");WriteReal(dw/(2.0*pi),10);
            WriteString("  time : ");WriteReal((FLOAT(l)*h),10);WriteLn;
          END;  
        END;
        FOR j:=1 TO n DO
          address1:=ADR(densmat[j,1,1]);       
          WriteNBytes(out,address1,nbytes,written);
          IF written # nbytes THEN
            WriteString('>>> WRITE TO BINARY FILE ERROR <<<');Delay(1000);WriteLn;
          END;
        END;  
      END;
      Close(inout,done);Close(out,done);
      Delete('dm.bin',result);
      Rename('store','dm.bin',result);
      calc:=calc+1;
      WriteString('Calculation Ready');ReadInt(dummy);
    ELSE 
      Close(inout,done);
      WriteString('Dimensions of dm.bin-file are : ');WriteLn;
      WriteInt(ftype,5);WriteInt(fn,5);WriteInt(fndw,5);WriteLn;
      WriteString('Expected dimensions are : ');WriteLn;
      WriteInt(0,5);WriteInt(n,5);WriteInt(ndw,5);WriteLn;
      WriteString('>>> NO CALCULATION <<<');ReadInt(dummy);      
    END;
  END;
  ScreenMode(3);
END SearchRotation;

PROCEDURE SubMenuIntegrate;
VAR dummy:INTEGER;
BEGIN
  WriteString('Sub menu integrate - version February 5 1990 :');WriteLn;
  WriteString('Possible actions :');WriteLn;
  WriteString('          1. Determine expectation values of arbitrary operators'); 
  WriteLn;
  WriteString('          2. Continue that is return to main menu');WriteLn;
  WriteLn;WriteString('Your choice [1,2]  :');ReadInt(dummy);WriteLn;
  IF dummy#2 THEN
    ExpValNDensMat;
    SubMenuIntegrate;
  END;
END SubMenuIntegrate;

PROCEDURE ExpValNDensMat;
VAR store1,store2:CMatrix;
    address:ADDRESS;
    in:File;
    read,nbytes,t:CARDINAL;
    i,j,dummy,ftype,fn,fndw:INTEGER;
    x,sig:ARRAY[1..512] OF REAL;
    done:BOOLEAN;
    lr:COMPLEXLONGREAL;
BEGIN
  Open(in,'dm.bin',ReadOnly,done);
  ReadBinFileHeader(in,ftype,fn,fndw);
  nbytes:=fn*8*2;
  MakeExpectationValueOperator(fn,store1);
  OpenOutput('DAT');
  t:=0;  
  FOR i:=1 TO fndw DO
    FOR j:=1 TO fn DO
      address:=ADR(store2[j,1,1]);
      ReadNBytes(in,address,nbytes,read);
      IF read # nbytes THEN
        WriteString('>>> READ BINARY FILE ERROR <<<');Delay(1000);WriteLn;            
      END;
    END;  
    ExpectationValue(fn,store1,store2,lr); 
    sig[i]:=lr[1];
    WriteCard(1,5);WriteString("   ");WriteReal(FLOAT(i),10);
    WriteString("   ");WriteReal(lr[1],18);WriteLn; 
    x[i]:=FLOAT(t);
    t:=t+1;
  END;
  Close(in,done);CloseOutput;  
  WriteString('Last Expectation value : ');
  WriteReal(lr[1],18);WriteLn;
  IF batch#1 THEN
    IF fndw > 1 THEN
      tdplot1(x,sig,fndw,1,2); 
    ELSE 
      WriteString('no xyplot of one point ...');ReadInt(dummy);WriteLn;
    END;
  END;  
END ExpValNDensMat;
    
PROCEDURE ExpectationValueRotation;
VAR hc,hb,hs,ht,oper:CMatrix;
    dw:REAL;
    f,inout,out:File;
    strtaddr1,strtaddr2,address1:ADDRESS;
    k,l,dummy,storedensmat,j,ftype,fn,fndw:INTEGER;
    written,nbytes,nbytes1,read:CARDINAL;
    lr:COMPLEXLONGREAL;
    done:BOOLEAN;
    result:DirResult;
BEGIN
  IF pulseflag = 2 THEN
    WriteString('>>>> NO RF-PULSE DEFINED ERROR <<<<');WriteLn;
  ELSE
    MakeExpectationValueOperator(n,oper);
    strtaddr1:=ADR(lr[1]);
    nbytes1:=8;
    Create(f,'expval.bin',done);
    WriteBinFileHeader(f,1,ap,ndw);
    nbytes:=n*2*8; 
    IF calc = 0 THEN
      WriteString('Creating new dm.bin  ...');Delay(1000);WriteLn; 
      Create(inout,'dm.bin',done);
      WriteBinFileHeader(inout,0,n,ndw); 
      FOR k:=1 TO ndw DO
        FOR j:= 1 TO n DO        
          address1:=ADR(densmat[j,1,1]);
          WriteNBytes(inout,address1,nbytes,written);          
          IF written # nbytes THEN
            WriteString('>>> WRITE BINARY-FILE ERROR <<<');Delay(1000);WriteLn;  
          END;
        END;     
      END;
      Close(inout,done);
    END;      
    CMatAdd(n,Izee,Icoup,hc);
    OpenOutput('DAT');
    Open(inout,'dm.bin',ReadOnly,done);
    ReadBinFileHeader(inout,ftype,fn,fndw);
    IF (fn=n) THEN 
      Create(out,'store',done); 
      IF ndw=1 THEN 
        WriteBinFileHeader(out,2,n,ap);
      ELSE
        WriteBinFileHeader(out,0,n,ndw);
      END;       
      FOR k:=1 TO ndw DO
        IF ndw#1 THEN
          FOR j:= 1 TO n DO
            address1:=ADR(densmat[j,1,1]);
            ReadNBytes(inout,address1,nbytes,read);        
            IF read # nbytes THEN
              WriteString('>>> READ BINARY FILE ERROR <<<');Delay(1000);WriteLn;
            END;
          END;
        END;            
        dw:=(x0+FLOAT(k-1)*dx)*gs;
        RMultCMat(n,dw,Izgrad,hs);
        CMatAdd(n,hc,hs,hs);
        FOR l:=0 TO ap-1 DO
          RMultCMat(n,xb[l],Ixtot,ht);
          CMatAdd(n,hs,ht,hb);
          RMultCMat(n,yb[l],Iytot,ht);
          CMatAdd(n,ht,hb,hb);
          RMultCMat(n,-1.0,hb,hb);
          ExpMatrix(n,hb,h,hb);
          DoRotation(n,hb,densmat);
          IF ndw=1 THEN
            FOR i:=1 TO n DO
              strtaddr2:=ADR(densmat[i,1,1]);
              WriteNBytes(out,strtaddr2,nbytes,written);
              IF written # nbytes THEN 
                WriteString('Write to Binary File Error');
              END;   
            END;
          END;    
          ExpectationValue(n,oper,densmat,lr);
          WriteNBytes(f,strtaddr1,nbytes1,written);
          IF written # nbytes1 THEN 
            WriteString('Write to Binary File Error');
          END;   
          WriteCard(k,5);WriteString("   ");WriteReal(FLOAT(l),10);
          WriteString("   ");WriteReal(lr[1],18);WriteLn; 
        END;
        IF ndw#1 THEN 
          FOR i:=1 TO n DO
            strtaddr2:=ADR(densmat[i,1,1]);
            WriteNBytes(out,strtaddr2,nbytes,written);
            IF written # nbytes THEN 
              WriteString('Write to Binary File Error');
            END;   
          END;
        END;  
      END;    
      Close(f,done);
      Close(inout,done);
      Close(out,done);
      CloseOutput;
      calc:=calc+1;
      Delete('dm.bin',result);
      Rename('store','dm.bin',result);       
    ELSE
      CloseOutput;
      Close(f,done);
      Close(inout,done);
      WriteString('Dimensions of dm.bin-file are : ');WriteLn;
      WriteInt(ftype,5);WriteInt(fn,5);WriteInt(fndw,5);WriteLn;
      WriteString('Expected dimensions are : ');WriteLn;
      WriteInt(0,5);WriteInt(n,5);WriteInt(ndw,5);WriteLn;
      WriteString('>>> NO EXPECTATION VALUES  <<<');ReadInt(dummy);      
    END;  
  END; 
END ExpectationValueRotation;


PROCEDURE DetermineSpectrum;
VAR hc,store:CMatrix;
    dw,time,h,abs,disp:REAL;
    k,l,m,dummy,j,ftype,fn,fndw,Ns1,Nc1,Ns2,Nc2,choice:INTEGER;
    nbytes,nbytes1,written,read:CARDINAL;
    f,in:File;
    address,address1,address2:ADDRESS;
    done:BOOLEAN;
    lr:COMPLEXLONGREAL;
    ssp1,csp1,ssp2,csp2:TwoDiagonal;
BEGIN
  ScreenMode(3);
  WriteString('**********************************************************');WriteLn;
  WriteString('***  Calculation spectra of density matrix / matrices  ***');WriteLn;
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
    CMatAdd(n,Izee,Icoup,hc);
    MakeTotalIx(TRUE);
    MakeTotalIy(TRUE);
    WriteString("How many time-points (power of 2): ");ReadInt(ap);WriteLn;
    WriteString("Acquisition time [seconds] : ");ReadReal(time);
    WriteLn;
    h:=time/FLOAT(ap-1);
    WriteString('Offset frequency [Hz] = ');ReadReal(dw);WriteLn;
    dw:=dw*2.0*pi;
    RMultCMat(n,dw,Iztot,store);
    CMatAdd(n,hc,store,store);
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
    MakeTotalIx(FALSE);
    MakeTotalIy(FALSE);
  END;            
END DetermineSpectrum;

PROCEDURE CalculateAverageSpectrum;
VAR hc,store:CMatrix;
    dw,time,h,abs,disp:REAL;
    k,l,m,dummy,j,ftype,fn,fndw,Ns1,Nc1,Ns2,Nc2:INTEGER;
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
    CMatAdd(n,Izee,Icoup,hc);
    MakeTotalIx(TRUE);
    MakeTotalIy(TRUE);
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
    CMatAdd(n,hc,store,store);
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
  MakeTotalIx(FALSE);
  MakeTotalIy(FALSE);
END CalculateAverageSpectrum;

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

PROCEDURE DisplaySignals;
VAR plx,ply,xaxis:ARRAY[0..1024] OF REAL;
    f,g:File;
    nbytes,read:CARDINAL;
    pp,ftype,fndw,z,l,n,dummy:INTEGER;
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
        
PROCEDURE ExpectationRotation;
VAR hc,hb,hs,ht,store:CMatrix;
    dw:REAL;
    k,l,dummy:INTEGER;
BEGIN
  IF pulseflag = 2 THEN
    WriteString('>>>> NO RF-PULSE DEFINED ERROR <<<<');WriteLn;
  ELSE
    CopyCMatrix(n,densmat,store);
    CMatAdd(n,Izee,Icoup,hc);    
    FOR k:=1 TO ndw DO
      CopyCMatrix(n,store,densmat);
      dw:=(x0+FLOAT(k-1)*dx)*gs;
      RMultCMat(n,dw,Izgrad,hs);
      CMatAdd(n,hc,hs,hs);
      FOR l:=0 TO ap-1 DO
        RMultCMat(n,xb[l],Ixtot,ht);
        CMatAdd(n,hs,ht,hb);
        RMultCMat(n,yb[l],Iytot,ht);
        CMatAdd(n,ht,hb,hb);
        RMultCMat(n,-1.0,hb,hb);
        ExpMatrix(n,hb,h,hb);
        DoRotation(n,hb,densmat);
      END;
    END;
  END;
END ExpectationRotation;        


PROCEDURE MakeExpectationValueOperator(n:INTEGER;VAR oper1:CMatrix);
VAR oper,store1,store2,oper3:CMatrix;
    i,type,choice,choice1:INTEGER;
    lr:COMPLEXLONGREAL;
    mult:REAL;
BEGIN
  CMatZero(n,store1,store1);CMatZero(n,store2,store2);
  LOOP
    ScreenMode(3);  
    WriteString('EXPECTATION VALUES OF AN ARBITRARY OPERATOR');WriteLn;    
    WriteString('         OR PRODUCT OPERATOR              ');WriteLn;
    WriteString('Two operatators {OPER1} and {OPER2} can be defined');WriteLn;
    WriteString('These operators can be 1. added or 2. multiplied :');WriteLn;WriteLn;
    WriteString('Define {OPER1} = 1');WriteLn;
    WriteString('Define {OPER2} = 2');WriteLn;
    WriteString('{OPER1} = {OPER1} + {OPER2} = 3');WriteLn;
    WriteString('{OPER1} = {OPER1} * {OPER2} = 4');WriteLn;WriteLn;
    WriteString('Defined {OPER1} O.K. = 5');WriteLn;
    WriteString('Your choice [1,2,3,4,5]  : ');ReadInt(choice);WriteLn;
    IF choice = 3 THEN
      CMatAdd(n,store1,store2,store1);
    END;
    IF choice= 4 THEN 
      CMatMult(n,store1,store2,store1);
    END;
    IF choice = 5 THEN EXIT END;
    IF ((choice =1) OR (choice = 2)) THEN        
      CMatZero(n,oper,oper);CMatZero(n,oper1,oper1);
      LOOP
        WriteString('Spin number = ');ReadInt(i);WriteLn;
        WriteString('Give type of the operator I(type) : type = [1,2,3,4,5] : ');WriteLn;
        WriteString(' type 1 = x        type 4 = + ');WriteLn;
        WriteString(' type 2 = y        type 5 = - ');WriteLn;
        WriteString(' type 3 = z ');WriteLn;
        WriteString('Give type number : ');ReadInt(type);WriteLn;
        CASE type OF
                    1:MakeMatrixIx(Ilist,ket,n,i,oper);|
                    2:MakeMatrixIy(Ilist,ket,n,i,oper);|
                    3:MakeMatrixIplus(Ilist,ket,n,i,oper);
                      MakeMatrixImin(Ilist,ket,n,i,oper3);
                      CMatCommutator(n,oper,oper3,oper);
                      RMultCMat(n,0.5,oper,oper);|                    
                    4:MakeMatrixIplus(Ilist,ket,n,i,oper);|
                    5:MakeMatrixImin(Ilist,ket,n,i,oper);
        END;            
        NormCMatrix(n,oper);
        WriteString('1 = ADD');WriteLn;  
        WriteString('2 = MULTIPLY TO THE ALREADY EXISTANT');WriteLn;
        WriteString('OPERATOR.   YOUR CHOICE [1,2] : ');ReadInt(type);WriteLn;
        CASE type OF
                    1:CMatAdd(n,oper,oper1,oper1);|
                    2:CMatMult(n,oper1,oper,oper1);
        END; 
        WriteLn;WriteLn;
        WriteString('OPERATOR OK [1 = yes]  ');ReadInt(type);WriteLn;
        IF type = 1 THEN 
          WriteString('Multiply Operator with a scalar [1=yes] ');
          ReadInt(choice1);WriteLn;
          IF choice1=1 THEN
            WriteString('Multiplication factor : ');ReadReal(mult);WriteLn;
            RMultCMat(n,mult,oper1,oper1);
          END;              
          EXIT;
        END;  
      END;
      IF choice = 1 THEN
        CopyCMatrix(n,oper1,store1);
      END;
      IF choice = 2 THEN 
        CopyCMatrix(n,oper1,store2);
      END;    
    END;
  END;    
  CopyCMatrix(n,store1,oper1);
END MakeExpectationValueOperator;  


PROCEDURE OperatorExpectationValues;
VAR oper:CMatrix;
    i,type,k,dummy,ftype,fn,fndw:INTEGER;
    lr:COMPLEXLONGREAL;
    in:File;
    address:ADDRESS;
    read,nbytes:CARDINAL;
    done:BOOLEAN;
    x,y:ARRAY[0..512] OF REAL;
BEGIN
  ScreenMode(3);CMatZero(n,oper,oper);
  MakeExpectationValueOperator(n,oper);
  nbytes:=n*8*2;
  Open(in,'dm.bin',ReadOnly,done);
  ReadBinFileHeader(in,ftype,fn,fndw);
  FOR k:= 1 TO fndw DO
    x[k]:=FLOAT(k);
    FOR i:=1 TO fn DO
      address:=ADR(densmat[i,1,1]);
      ReadNBytes(in,address,nbytes,read);
      IF read # nbytes THEN
        WriteString('>>> READ BINARY FILE ERROR <<<');Delay(1000);WriteLn;
      END;
    END;    
    ExpectationValue(n,oper,densmat,lr);
    WriteString('REAL{<Oper>} = ');
    WriteReal(lr[1],10);WriteLn;
    y[k]:=lr[1];
    WriteString('IMAGINARY{<Oper>}');WriteReal(lr[2],10);WriteLn;   
    WriteLn;
  END;
  IF fndw > 1 THEN  
    tdplot1(x,y,ndw,1,2);
  ELSE
    WriteString(' no xyplot of one point ... ');ReadInt(dummy);
  END;    
  Close(in,done);
  WriteString('Expectation value of an other operator [1=yes] : ');
  ReadInt(type);WriteLn;  
  IF type = 1 THEN 
    OperatorExpectationValues;
  END;
END OperatorExpectationValues;     
    
PROCEDURE RedefinePulseHamiltonian;
VAR choice:INTEGER;
BEGIN
  DefinePulseHamiltonian;
  WriteString('More changes in Pulse Hamilton-operator [1=yes]');
  ReadInt(choice);WriteLn;
  IF choice=1 THEN 
    RedefinePulseHamiltonian;
  END;
END RedefinePulseHamiltonian;


PROCEDURE RedefineSpinSystem;
VAR choice:INTEGER;
BEGIN
  ScreenMode(3);WriteLn;WriteLn;
  WriteString('Redefinition of the spin system');WriteLn;
  WriteString('Possible actions:');WriteLn;
  WriteString('1. total (re)-definition');WriteLn;
  WriteString('2. change Zeeman terms');WriteLn;
  WriteString('3. change scalar coupling constants');WriteLn;
  WriteString('4. change Zeeman terms & scalar coupling constants');
  WriteLn;WriteLn;
  WriteString('Your choice is [1,2,3,4] : ');ReadInt(choice);WriteLn;
  CASE choice OF
                 1:TotalStartUp;|
                 2:MakeZeeman;|
                 3:SpinSpinCoupling;|
                 4:MakeZeeman;
                 SpinSpinCoupling;
  END;
  WriteString('More changes of the spin system?  [1=yes]');ReadInt(choice);
  IF choice = 1 THEN  
    RedefineSpinSystem
  END;
END RedefineSpinSystem;

PROCEDURE TotalStartUp;
BEGIN
  SetupSpins;
  MakeZeeman;
  SpinSpinCoupling;
  MakeTotalIx(FALSE);
  MakeTotalIy(FALSE);
  MakeTotalIz(FALSE);
END TotalStartUp;

(*******************************************************************)
(***      THE FOLLOWING PROCEDURES ARE TAKEN FROM THE BOOK       ***)
(***     NUMERICAL RECIPES - THE ART OF SCIENTIFIC COMPUTING     ***)
(*** W.H. PRESS   B.P. FLANNERY   S.A. TEUKOLSKI W.T. VETTERLING ***)
(***   ISBN 0 521 30811 9   CAMBRIDGE UNIVERITY PRESS  (1986)    ***)
(*******************************************************************)

(*       Programs using routine FRPRMN must supply                  *  
 *       FUNCTION fnc(par: glnarray):real; and a                    *
 *       PROCEDURE dfnc(par: glnarray; VAR g: glnarray);            *
 *       which evaluate a function and its gradient. They must      *
 *       also define the type                                       *
 *       TYPE  glnarray = ARRAY [0..nn-1] OF REAL;                  * 
 *       in the main routine.                                       *) 

PROCEDURE frprmn(VAR par: glnarray; nn: INTEGER; ftol: REAL; 
                 VAR iter: INTEGER; VAR fret: REAL); 

CONST 
      itmax = 100; 
      eps = 1.0E-10; 
VAR 
    a,j,its,dummy,continue: INTEGER; 
    gg,gam,fp,dgg:REAL; 
    g,h,xi:glnarray;
    stop:BOOLEAN; 
BEGIN
  fp := fnc(par); 
  dfnc(fp,par,xi); 
  FOR j := 0 TO (nn-1) DO 
    g[j] := -xi[j]; 
    h[j] := g[j];       (* h=s(k), xi wordt g(k+1), waarna xi=h=s(k) *)
    xi[j] := h[j]       (* deze 1e stap is 'steepest descent'-stap   *)
  END; 
  LOOP
    FOR its := 1 TO itmax DO 
      WriteInt(its,5);WriteLn;
      iter := its;
      linmin(par,xi,nn,fret);     (* returns p(k+1), g(k+1), J(p(k+1)) *)
      IF ((2.0*ABS(fret-fp))<=(ftol*(ABS(fret)+ABS(fp)+eps))) THEN 
         EXIT;
      END;      
      fp := fnc(par);
      IF fp < -1.999 THEN
        WriteString('AAN CRITERIUM VOLDAAN');ReadInt(dummy);
        EXIT;
      END;
      stop:=KeyPressed();
      IF stop = TRUE THEN
        ScreenMode(3);
        WriteString('Function value = ');
        WriteReal(fp,18);WriteLn;WriteLn;
        WriteString('1. STOP CALCULATING');WriteLn;
        WriteString('2. CONTINUE CALCULATION');WriteLn;
        WriteString('YOUR CHOICE : ');ReadInt(continue);
        IF continue =1 THEN
          EXIT;
        END;
      END;       
      dfnc(fp,par,xi); (* DIFFERENT FROM THE PROCEDURE OF THE BOOK *)
      gg := 0.0; 
      dgg := 0.0; 
      FOR j := 0 TO (nn-1) DO 
       	gg := gg+(g[j]*g[j]); 
       	dgg := dgg+(xi[j]+g[j])*xi[j]
      END; 
      IF (gg = 0.0) THEN 
      	EXIT; 
      END; 
      gam := dgg/gg;            (* Polak-Ribiere [3.3.13], beta(k) *)
      FOR j := 0 TO (nn-1) DO 
        g[j] := (-xi[j]); 
       	h[j] := g[j]+gam*h[j];  (* s(k+1)=-g(k+1) + beta(k)s(k) *)
       	xi[j] := h[j]
      END;
    END; 
    WriteString("pause in routine FRPRMN"); 
    WriteLn; 
    WriteString("too many iterations"); 
    WriteLn; 
  END;
END frprmn; 


(*   Programs using routine LINMIN must define the type                     *
 *    TYPE  glnarray = ARRAY [0..nn-1] OF real;                             *
 *    They must also declare the variables                                  *
 *    VAR                                                                   *
 *        ncom: integer;                                                    *
 *        pcom,xicom: glnarray;                                             *
 *        in the main routine. Also the function FUNC referenced by BRENT   *
 *        and MNBRAK must be set to return the function F1DIM.              *) 



PROCEDURE linmin(VAR par, xi: glnarray; 
                   nn: INTEGER; 
                   VAR fret: REAL); 
    CONST 
      tol = 1.0E-4; 
    VAR 
      j: INTEGER; 
      xx, xmin, fx, fb, fa, bx, ax: REAL; 
BEGIN 
  ncom := nn; 
  FOR j := 0 TO (nn-1) DO 
    pcom[j] := par[j]; 
    xicom[j] := xi[j];
  END; 
  ax := 0.0; 
  xx := 1.0; 
  bx := 2.0; 
  mnbrak(ax, xx, bx, fa, fx, fb); 
  fret := brent(ax, xx, bx, tol, xmin); 
  FOR j := 0 TO (nn-1) DO 
    xi[j] := xmin*xi[j]; 
    par[j] := par[j]+xi[j];
  END;
END linmin; 


PROCEDURE sign(a, b: REAL): REAL; 
VAR signResult: REAL; 
BEGIN 
  IF (b > 0.0) THEN 
    signResult := ABS(a)
  ELSE 
    signResult := (-ABS(a))
  END; 
  RETURN signResult
END sign; 



  (* Programs using routine BRENT must supply an external function
  func(x:real):real whose minimum is to be found. *) 

PROCEDURE brent(ax, bx, cx, tol: REAL; 
                VAR xmin: REAL): REAL; 
CONST itmax = 100; 
      cgold = 0.3819660; 
      zeps = 1.0E-10; 

VAR a, b, d, e, etemp: REAL; 
    fu, fv, fw, fx: REAL; 
      iter: INTEGER; 
      p, q, r, tol1, tol2: REAL; 
      u, v, w, x, xm: REAL; 
      bool1,bool2,bool3:BOOLEAN;
      B: BOOLEAN;
      brentResult:REAL; 

BEGIN 
  IF ax < cx THEN a := ax ELSE a := cx END; 
  IF ax > cx THEN b := ax ELSE b := cx END; 
  v := bx; w := v; x := v; e := 0.0; 
  fx := func(x); 
  fv := fx; fw := fx;
  LOOP
    FOR iter := 1 TO itmax DO 
      xm := 0.5*(a+b); 
      tol1 := tol*ABS(x)+zeps; 
      tol2 := 2.0*tol1; 
      IF (ABS(x-xm) <= (tol2-0.5*(b-a))) THEN 
        EXIT; 
      END; 
      B:=FALSE;
      IF (ABS(e) > tol1) THEN 
        r := (x-w)*(fx-fv); 
        q := (x-v)*(fx-fw); 
        p := (x-v)*q-(x-w)*r; 
        q := 2.0*(q-r); 
        IF (q > 0.0) THEN 
          p := (-p)
        END; 
        q := ABS(q); 
        etemp := e; 
        e := d;
        IF (ABS(p)>=ABS(0.5*q*etemp)) THEN
          bool1:=TRUE;
        END;   
        IF (p<=q*(a-x)) THEN
          bool2:=TRUE;
        END;   
        IF bool1 OR bool2 OR (p>=q*(b-x)) THEN 
          B:=TRUE; 
        ELSE 
          d := p/q; 
          u := x+d; 
          IF (((u-a) < tol2) OR ((b-u) < tol2)) THEN 
            d := sign(tol1, xm-x)
          END; 
        END;
      ELSE
        B:=TRUE;
      END; 
      IF B THEN 
        IF (x >= xm) THEN 
          e := a-x
        ELSE 
          e := b-x
        END; 
        d := cgold*e; 
      END;
      IF (ABS(d) >= tol1) THEN 
        u := x+d
      ELSE 
        u := x+sign(tol1, d)
      END; 
      fu := func(u); 
      IF (fu <= fx) THEN 
        IF (u >= x) THEN 
          a := x
        ELSE 
          b := x
        END; 
        v := w; fv := fw; w := x; fw := fx; x := u; fx := fu
      ELSE 
        IF (u < x) THEN 
          a := u
        ELSE 
          b := u
        END; 
        IF ((fu <= fw) OR (w = x)) THEN 
          v := w; fv := fw; w := u; fw := fu
        ELSIF ((fu <= fv) OR (v = x) OR (v = FLOAT(2))) THEN 
          v := u; fv := fu
        END;
      END;
    END; 
    WriteString("pause in routine BRENT-too many iterations"); 
    WriteLn; 
  END;
  xmin := x; 
  brentResult := fx; 
  RETURN brentResult
END brent; 

PROCEDURE dfunc(fx,x:REAL):REAL;
VAR dfnc:REAL;
BEGIN
  dfnc:=df1dim(fx,x);
  RETURN dfnc
END dfunc;  

PROCEDURE df1dim(fx,x: REAL): REAL; 

  (* Programs using routine DF1DIM must define the type
  TYPE
     glnarray = ARRAY [0..n-1] OF real;
  where n is the dimension of vector x. They must also
  define the variables
  VAR
     ncom: integer;
     pcom,xicom: glnarray
  in the main routine, and externally assign them values. *) 

VAR   df1dimResult,df1: REAL; 
      j: INTEGER; 
      xt, df: glnarray; 
BEGIN 
  FOR j := 0 TO ncom-1 DO 
    xt[j] := pcom[j]+x*xicom[j]
  END; 
  dfnc(fx,xt,df); 
  df1 := 0.0; 
  FOR j := 0 TO ncom-1 DO 
    df1 := df1+df[j]*xicom[j]
  END; 
  df1dimResult := df1; 
  RETURN df1dimResult
END df1dim; 

  (* Programs using routine MNBRAK must supply an external
  function func(x:real):real for which a minimum is to be found *) 

PROCEDURE mnbrak(VAR ax,bx,cx,fa,fb,fc:REAL); 
CONST gold = 1.618034; 
      glimit = 100.0; 
      tiny = 1.0E-20; 
VAR ulim,u,r,q,fu,dum:REAL; 
 
 PROCEDURE max(a,b:REAL): REAL; 
    VAR maxResult:REAL; 
  BEGIN 
    IF (a > b) THEN 
      maxResult:=a
    ELSE 
      maxResult:=b
     END; 
    RETURN maxResult
   END max; 

BEGIN 
  fa := func(ax); 
  fb := func(bx); 
  IF (fb > fa) THEN 
    dum := ax; ax := bx; bx := dum; dum := fb; fb := fa; fa := dum
  END; 
  cx := bx+gold*(bx-ax); 
  fc := func(cx); 
  IF (fb >= fc) THEN 
    REPEAT
      LOOP
        r := (bx-ax)*(fb-fc); 
        q := (bx-cx)*(fb-fa); 
        u:=bx-((bx-cx)*q-(bx-ax)*r)/(2.0*sign(max(ABS(q-r),tiny),q-r)); 
        ulim := bx+glimit*(cx-bx); 
        IF ((bx-u)*(u-cx) > 0.0) THEN 
          fu := func(u); 
          IF (fu < fc) THEN 
            ax := bx; fa := fb; bx := u; fb := fu; 
            EXIT; 
          ELSIF (fu > fb) THEN 
            cx := u; fc := fu; 
            EXIT; 
          END; 
          u := cx+gold*(cx-bx); 
          fu := func(u)
        ELSIF ((cx-u)*(u-ulim) > 0.0) THEN 
          fu := func(u); 
          IF (fu < fc) THEN 
            bx := cx; cx := u; 
            u := cx+gold*(cx-bx); 
            fb := fc; fc := fu; 
            fu := func(u)
          END;
        ELSIF ((u-ulim)*(ulim-cx) >= 0.0) THEN 
          u := ulim; 
          fu := func(u)
        ELSE 
          u := cx+gold*(cx-bx); 
          fu := func(u)
        END; 
        ax := bx; bx := cx; cx := u; fa := fb; fb := fc; fc := fu; 
        EXIT; 
      END;
    UNTIL (fc >= fb);
  END;
END mnbrak; 

PROCEDURE func(x: REAL): REAL;
VAR a: INTEGER;
    xt: glnarray;
    f1dim: REAL;
BEGIN
  FOR a:=0 TO (ncom-1) DO
    xt[a]:=pcom[a]+x*xicom[a];
  END;
  f1dim:=fnc(xt);
  RETURN f1dim;
END func;


(*************************************************************************)
(**************      END OF PROCEDURES FROM NUMERICAL RECIPES    *********)
(*************************************************************************)

PROCEDURE fnc(VAR par:glnarray):REAL;
VAR i,j,l:INTEGER;
    hc,hs,ht,r,store:CMatrix;
    lr:COMPLEXLONGREAL;
    dw:REAL;
    x:ARRAY[0..64] OF REAL;
BEGIN
  FOR i:=0 TO 63 DO
    x[i]:=FLOAT(i);
  END;  
  CopyCMatrix(n,densmat,store);
  CMatAdd(n,Izee,Icoup,hc);
  ParArrayToSignal(npar,ap,par,xb,offset);
  CMatZero(n,r,r);
  FOR i:=1 TO n DO
    r[i,i,1]:=1.0;
  END;  
  FOR i:= 0 TO ap-1 DO
    dw:=x0*gs+offset[i];
    RMultCMat(n,dw,Izgrad,hs); 
    CMatAdd(n,hc,hs,hs);
    RMultCMat(n,xb[i],Ixtot,ht);
    CMatAdd(n,hs,ht,hs);
    RMultCMat(n,-1.0,hs,hs);
    ExpMatrix(n,hs,h,hs);
    CMatMult(n,r,hs,r);
  END;
  DoRotation(n,r,densmat);
  ExpectationValue(n,optr,densmat,lr);   
  WriteString('Expectation Value dw norm is : ');WriteReal(lr[1],18);WriteLn;  
  CopyCMatrix(n,store,densmat);
  RETURN lr[1];
END fnc;

PROCEDURE Deriv(ap:INTEGER;h:REAL;f:ARRAY OF REAL;
                VAR f1:ARRAY OF REAL);
VAR ww:INTEGER;
    a0,a1,a10,a2,a3,a4,a40,a5:REAL;
BEGIN

  a0:=2.0/3.0;a1:=-1.0/12.0;a10:=-25.0/12.0;
  a2:=0.5;a3:=-1.5;a4:=5.0/6.0;a40:=4.0/3.0;
  a5:=0.25;

  f1[0]:=(a10*f[0]+4.0*f[1]-3.0*f[2]+a40*f[3]-a5*f[4])/h;
  f1[1]:=-(a5*f[0]+a4*f[1]+a3*f[2]+a2*f[3]+a1*f[4])/h;
  f1[2]:=(-a1*f[0]-a0*f[1]+a0*f[3]+a1*f[4])/h;

  FOR ww:=2 TO ap-3 DO
    f1[ww+1]:=(a1*f[ww-2]+a2*f[ww-1]+a3*f[ww]+a4*f[ww+1]+a5*f[ww+2])/h;
  END;
  
  f1[ap-2]:=(a5*f[ap-1]+a4*f[ap-2]+a3*f[ap-3]+a2*f[ap-4]+a1*f[ap-5])/h;
  f1[ap-1]:=-(a10*f[ap-1]+4.0*f[ap-2]-3.0*f[ap-3]+a40*f[ap-4]-a5*f[ap-5])/h;
  
END Deriv;

PROCEDURE CalcFreqSweep(ap:INTEGER;h:REAL;xb,yb:ARRAY OF REAL;
                        VAR sweep:ARRAY OF REAL);
VAR squarenorm:REAL;
    dbx,dby:ARRAY[0..256] OF REAL;
    i:INTEGER;  
BEGIN
  Deriv(ap,h,xb,dbx);
  Deriv(ap,h,yb,dby);
  FOR i:=0 TO ap-1 DO
    squarenorm:=xb[i]*xb[i]+yb[i]*yb[i];
    sweep[i]:=-(dby[i]*xb[i]-yb[i]*dbx[i])/squarenorm;
  END;  
END CalcFreqSweep;


PROCEDURE dfnc(f1:REAL;VAR par,xi:glnarray);
VAR f2,store:REAL;
    i,ii:INTEGER;
BEGIN
  WriteString('*');WriteLn;
  FOR i:=0 TO npar-1 DO
    IF par[i]#0.0 THEN
      store:=par[i];
      par[i]:=par[i]+(par[i]/1.0e7);
      f2:=fnc(par);
      par[i]:=par[i]-(store/1.0e7);
      xi[i]:=(f2-f1)/(store/1.0e7);
    ELSE
      par[i]:=1.0e-7;
      f2:=fnc(par);
      par[i]:=0.0;
      xi[i]:=(f2-f1)/(1.0e-7);
    END; 
  END;     
END dfnc;

PROCEDURE Optimize;
VAR dummy:CHAR;
    x:ARRAY[0..128] OF REAL;
    i:INTEGER;
BEGIN
  ScreenMode(3);
  WriteString('   ********************************************************');WriteLn;
  WriteString('   ***        OPTIMIZATION OF A PULSE RESPONCE          ***');WriteLn;
  WriteString('   ***        FOR A N-SPIN SYSTEM AND A GIVEN           ***');WriteLn;
  WriteString('   ***            INITIAL PULSE SEQUENCE                ***');WriteLn;
  WriteString('   ********************************************************');WriteLn;
  WriteString('   ***      version 2.0 / TUD SST-SI april 1992         ***');WriteLn;
  WriteString('   ********************************************************');WriteLn;
  WriteLn;WriteLn;
  WriteString('INFORMATION :');WriteLn;
  WriteString('Given a: 1. spin system with coupling constants');WriteLn;
  WriteString('         2. RF - pulse Hamiltionian');WriteLn;
  WriteString('         3. a Zeeman Hamiltionian');WriteLn;
  WriteString('         4. an operator O whose expectation value <O>');WriteLn;
  WriteString('            has to be optimized');
  WriteLn;WriteLn;
  WriteString('this part of the program calculates a RF-pulse Hamiltionian');
  WriteLn;
  WriteString('which optimizes the expectation value <O>');WriteLn;           
  WriteString('Now you will be prompted to define [1,2,3,4]');WriteLn;
  WriteString('Press any key to continue');WriteLn;Read(dummy);
  RedefineSpinSystem;
  RedefinePulseHamiltonian;
  ScreenMode(3);WriteLn;
  WriteString('Define operator whose expectation value is to be optimized : ');
  WriteLn;Delay(3000);
  MakeExpectationValueOperator(n,optr);
  WriteString('The number of parameters is : ');
  ReadInt(npar);WriteLn;
  npar:=4*npar;
  FOR i:=0 TO ap-1 DO
    x[i]:=FLOAT(i);
  END;
  IF rotframe=0 THEN 
    CalcFreqSweep(ap,h,xb,yb,offset);
    tdplot1(x,offset,ap-1,1,2);
    FOR i:=0 TO ap-1 DO
      xb[i]:=sqrt(xb[i]*xb[i]+yb[i]*yb[i]);
    END;  
    tdplot1(x,xb,ap,1,2);
  END;  
  SignalToParArray(npar,ap,xb,offset,par);
  MakeGradientOperator;(* define Izgrad *)
  StartOptimization;
END Optimize;



PROCEDURE StartOptimization;
VAR i,coef,niter,dummy:INTEGER;
    minimum:REAL;
BEGIN
  frprmn(par,npar,3.0e-5,niter,minimum);
  WriteString('|Btr(t)| out        >> ');WriteLn;WriteLn;
  RealArrayOut(ap,xb);
  WriteString('Offset dw(t) out    >> ');WriteLn;WriteLn;
  RealArrayOut(ap,offset);
END StartOptimization;

  
PROCEDURE ParArrayToSignal(npar,ap:INTEGER;par:glnarray;
                           VAR rb1,rbz:RArray);
(*************************************************************************)
(***  THIS PROCEDURE TRANSFORMS THE RELEVANT npar COMPLEX FOURIER      ***)
(***  COEFFICIENTS TO AN ap POINT PULSE ARRAY IN THE TIME DOMAIN       ***)
(***  THE ARRAY real AND imaginary ARE EXPECTED TO CONTAIN ALSO        ***)
(***  FOURIER COEFFICIENTS. THE glnarr par IS CONSTRUCTED AS FOLLOWS   ***)
(***  {RFreal[0],RFimaginary[0], ... ,RFreal[npar],RFimaginary[npar]}  ***)
(***  CONTAINING THE FOURIER COEFFICIENTS OF THE RF PULSE              ***)
(***  DATE 14-09-1991 BY H.S.                                          ***)
(*************************************************************************)
VAR np,j,hap,i,ii:INTEGER;
    imag:ARRAY [0..256] OF REAL;
BEGIN
  np:=(npar DIV 4);
  hap:=ap DIV 2; 
  FOR i:=0 TO ap-1 DO
    imag[i]:=0.0;
  END;   
  fft(ap,rb1,imag,FALSE); (* CALCULATES FFT OF B1 OF RF PULSE TIME SIGNAL *)
  FOR j:=0 TO np-1 DO
    ii:=2*j;
    rb1[hap+j]:=par[ii];
    rb1[hap-j]:=rb1[hap+j];
    imag[hap+(j+1)]:=par[ii+1];
    imag[hap-(j+1)]:=-imag[hap+(j+1)];
  END;
  fft(ap,rb1,imag,TRUE); (* CALCULATES IFFT OF B1 OF RF PULSE SPECTRUM *)
  
  FOR i:=0 TO ap-1 DO
    imag[i]:=0.0;
  END;   
  
  fft(ap,rbz,imag,FALSE); (* CALCULATES FFT OF FM OF RF PULSE TIME SIGNAL *)
  FOR j:=0 TO np-1 DO
    ii:=2*j;
    rbz[hap+j]:=par[ii+2*np];
    rbz[hap-j]:=rbz[hap+j];
    imag[hap+(j+1)]:=par[ii+1+2*np];
    imag[hap-(j+1)]:=-imag[hap+(j+1)];
  END;
  fft(ap,rbz,imag,TRUE); (* CALCULATES IFFT OF FM OF RF PULSE SPECTRUM *)
END ParArrayToSignal;


PROCEDURE SignalToParArray(npar,ap:INTEGER;
                           rb1,rbz:RArray;VAR par:glnarray);
VAR np,j,hap,i,ii:INTEGER;
    imag:ARRAY [0..256] OF REAL;
    
BEGIN
  np:=(npar DIV 4);
  FOR i:= 0 TO ap-1 DO
    imag[i]:=0.0;
  END;  
  hap:=ap DIV 2;
  fft(ap,rb1,imag,FALSE); (* CALCULATES THE FFT OF B1 TIME SIGNAL *)
  FOR j:=0 TO np-1 DO
    ii:=2*j;
    par[ii]:=rb1[hap+j];
    par[ii+1]:=imag[hap+(j+1)];
  END;  
  FOR i:= 0 TO ap-1 DO
    imag[i]:=0.0;
  END;  
  fft(ap,rbz,imag,FALSE); (* CALCULATES THE FFT OF FM TIME SIGNAL *)
  FOR j:=0 TO np-1 DO
    ii:=2*j+2*np;
    par[ii]:=rbz[hap+j];
    par[ii+1]:=imag[hap+(j+1)];
  END;
END SignalToParArray;

BEGIN
  n:=Nstmax;
  init;
  SolveLiouville; 
END ilv.
 