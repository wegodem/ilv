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
FROM GradLib IMPORT CalcGrad;
FROM MatrixMathLib IMPORT CMatCommutator,CMatAdd,CMatMultAdd,CMatRMultAdd,
                          CMatMult,CMatZero,ShowCMatElements,CopyCMatrix,
                          RMultCMat,CMultCMat,ExpectationValue,CalcTrace,
                          ExpMatrix,DoRotation,NormCMatrix,CMatrix,
                          COMPLEXLONGREAL,DoSpectrum,TwoDiagonal;
FROM SpinOperatorLib IMPORT MakeKets,MakeMatrixIplus,MakeMatrixImin,
                            MakeMatrixIx,MakeMatrixIy,MakeMatrixIz,
                            Iarray,produktket,MakeTotalIx,MakeTotalIy,
                            MakeTotalIz;                          
FROM BinFiles IMPORT BinFileActions,ReadBinFileHeader,WriteBinFileHeader;
FROM FOUR3 IMPORT four2,fft;
FROM PulseSequence IMPORT ComposePulseSequence;
FROM DataFilesInOut IMPORT RealArrayOut,RealArrayIn;
FROM CalcSpec IMPORT SeparateSpectra,AverageSpectrum,Determine2DSpectrum;

CONST
     Nstmax=16;
     Nspmax=4;
     pi = 3.1415926535897932; 
     version = "            DOS-TEST VERSION 15 : february  15 , 1993        ";
TYPE 
     RArray = ARRAY[0..512] OF REAL;
     RMat = ARRAY[1..Nspmax],[1..Nspmax] OF REAL;
     String= ARRAY[0..80] OF CHAR;
     glnarray = ARRAY[0..160] OF REAL;  

     
VAR  
     ja:BOOLEAN; 
     spinsystemdefined,rfpulsedefined,meanchoice,
     offsetsdefined,rotframe,batch,pulseflag,
     ap,ndw,ndx,ndy,ndz,i,Nspins,n,calc,npar,ncom:INTEGER;
     h,ti,pt,x0,y0,z0,dx,dy,dz,gs,statoff:REAL;
     cn,cn1,cn2,cn3,cn4,cn5,Eval,x:COMPLEXLONGREAL;
     densmat,Izee,Izgrad,Ixtot,Iytot,
     Iztot,Icoup,Istat:CMatrix;
     Ilist: Iarray;
     ket: produktket;
     gx,gy,gz,xb,yb,offset:RArray;
     jc:RMat;
     par,pcom,xicom:glnarray;

PROCEDURE Ispins; 
VAR k, Inumber: INTEGER; 
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
      WriteInt( k,1); 
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

PROCEDURE NonIntIspins; 
VAR k, Inumber: INTEGER; 
    klaar: BOOLEAN; 
BEGIN 
  ScreenMode(3);
  klaar := FALSE; 
  WHILE ( NOT klaar) DO 
    klaar := TRUE; 
    FOR k := 1 TO Nspmax DO 
      Ilist[k] := 0
    END; 
    REPEAT 
      ReadInt(Nspins);
      WriteString('Number of spins                         : ');WriteInt(Nspins,3);WriteLn; 
    UNTIL (Nspins <= Nspmax); 
    FOR k := 1 TO Nspins DO 
      ReadInt(Inumber); 
      Ilist[k] := Inumber; 
      WriteString( "I quantum number of spin("); WriteInt(k,1); 
      WriteString( ") = x/2 |   x : "); WriteInt(Ilist[k],3);WriteLn;
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
  WriteString("Total number of states                  : ");WriteInt(n,2);WriteLn;
END NonIntIspins; 

PROCEDURE MakeBoltzmann; 
VAR i: INTEGER; 
    gamma: COMPLEXLONGREAL; 
    store: CMatrix; 
    okay:BOOLEAN;
BEGIN 
  CMatZero(n,densmat,densmat); 
  FOR i := 1 TO Nspins DO 
    CMatZero(n,store,store); 
    WriteLn;
    WriteString( "Relative Intensity of Spin ("); 
    WriteInt(i,1); 
    WriteString(")          : "); 
    ReadReal(gamma[1]);gamma[2]:=0.0;
    MakeMatrixIz(Ilist,ket,n,i,store); 
    NormCMatrix(n,store); 
    CMultCMat(n,gamma,store,store); 
    CMatMultAdd(n,store,cn1,densmat,cn1,densmat); 
  END;
END MakeBoltzmann; 

PROCEDURE NonIntMakeBoltzmann; 
VAR i: INTEGER; 
    gamma: COMPLEXLONGREAL; 
    store: CMatrix; 
    okay:BOOLEAN;
BEGIN 
  CMatZero(n,densmat,densmat); 
  FOR i := 1 TO Nspins DO 
    CMatZero(n,store,store); 
    ReadReal(gamma[1]);gamma[2]:=0.0;
    WriteString( "Relative Intensity of Spin ("); 
    WriteInt(i,1); 
    WriteString(")          : ");WriteReal(gamma[1],8);WriteLn;
    MakeMatrixIz(Ilist,ket,n,i,store); 
    NormCMatrix(n,store); 
    CMultCMat(n,gamma,store,store); 
    CMatMultAdd(n,store,cn1,densmat,cn1,densmat); 
  END;
END NonIntMakeBoltzmann; 

PROCEDURE MakeZeeman; 
VAR i: INTEGER; 
    gamma: COMPLEXLONGREAL; 
    store: CMatrix; 
    okay:BOOLEAN;
BEGIN 
  CMatZero(n,Izee,Izee); 
  FOR i := 1 TO Nspins DO 
    CMatZero(n,store,store); 
    WriteLn;
    WriteString( "Offset-frequency [Hz] of Spin("); 
    WriteInt(i,1); 
    WriteString(")        : "); 
    ReadReal(gamma[1]);gamma[2]:=0.0;
    gamma[1]:=gamma[1]*2.0*pi;
    MakeMatrixIz(Ilist,ket,n,i,store); 
    CMultCMat(n,gamma,store,store); 
    CMatMultAdd(n,store,cn1,Izee,cn1,Izee); 
  END;
END MakeZeeman; 

PROCEDURE NonIntMakeZeeman; 
VAR i: INTEGER; 
    gamma: COMPLEXLONGREAL; 
    store: CMatrix; 
    okay:BOOLEAN;
BEGIN 
  CMatZero(n,Izee,Izee); 
  FOR i := 1 TO Nspins DO 
    CMatZero(n,store,store); 
    ReadReal(gamma[1]);gamma[2]:=0.0;
    WriteString( "Offset-frequency [Hz] of Spin ("); 
    WriteInt(i,1); 
    WriteString(")       : "); 
    WriteReal(gamma[1],8);WriteLn;
    gamma[1]:=gamma[1]*2.0*pi;
    MakeMatrixIz(Ilist,ket,n,i,store); 
    CMultCMat(n,gamma,store,store); 
    CMatMultAdd(n,store,cn1,Izee,cn1,Izee); 
  END;
END NonIntMakeZeeman; 

PROCEDURE SpinSpinCoupling;
VAR mat,mat1,mat2:CMatrix;
    k,l,exact:INTEGER;
    j:REAL;
BEGIN
  CMatZero(n,mat,mat);ScreenMode(3);
  IF Nspins > 1 THEN
    WriteString('*************************************');WriteLn;
    WriteString('***  Define J coupling constants  ***');WriteLn;
    WriteString('*************************************');
    WriteLn;WriteLn;
    WriteString('1. Weak-Coupling approximation. ');WriteLn;
    WriteString('2. Exact calculation. ');WriteLn;
    WriteString(' Your choice [1,2] : ');ReadInt(exact);WriteLn;
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

PROCEDURE NonIntSpinSpinCoupling;
VAR mat,mat1,mat2:CMatrix;
    k,l,exact,dummy:INTEGER;
    j:REAL;
BEGIN
  CMatZero(n,mat,mat);
  IF Nspins > 1 THEN
    ReadInt(exact);
    ReadInt(dummy);
    FOR k:=1 TO Nspins DO
      FOR l:= 1 TO Nspins DO
        IF l>k THEN
          ReadReal(jc[k,l]);
          WriteString('Scalar Coupling Constant J(');WriteInt(k,1);
          WriteString(',');WriteInt(l,1);WriteString(')         : ');
          WriteReal(jc[k,l],10);WriteLn;
          j:=2.0*jc[k,l]*pi; (* ! *)
          IF j # 0.0 THEN
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
END NonIntSpinSpinCoupling;

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
    MakeMatrixIz(Ilist,ket,n,i,store); 
    CMatMultAdd(n,store,cn1,Izgrad,cn1,Izgrad); 
  END;
END MakeGradientOperator; 

PROCEDURE SetupSpins; 
BEGIN 
  ScreenMode(3);WriteLn;WriteLn;WriteLn; 
  WriteString('Definition of the spinsystem: ');
  WriteLn;
  Ispins; 
  MakeKets(ket,Ilist); 
  MakeBoltzmann; 
END SetupSpins; 

PROCEDURE init ;
BEGIN
  spinsystemdefined:=0;rfpulsedefined:=0;offsetsdefined:=0;
  rotframe:=0;
  batch:=0;
  x0:=0.0;dx:=0.0;
  y0:=0.0;dy:=0.0;
  z0:=0.0;dz:=0.0;
  ndx:=1;ndy:=1;ndz:=1;
  gs:=0.0;ndw:=1;(* default gradient setting *)
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
  WriteString('                *******************************');WriteLn;
  WriteString('                *** GRADIENT INITIALISATION ***');WriteLn;
  WriteString('                *******************************');WriteLn;WriteLn;
  MakeGradientOperator;
  WriteString('Define x-gradient : ');WriteLn;
  WriteString('# offset-frequency points in x-direction : ');
  ReadInt(ndx);WriteLn;
  IF ndx=0 THEN ndx:=1 END;
  WriteString('The stepsize dx [cm]                     : ');
  ReadReal(dx);WriteLn;
  WriteString('Minimal x-coordinate x0 [cm]             : ');ReadReal(x0);
  WriteLn;WriteLn;
  CalcGrad(ap,h,gx);
  WriteLn;WriteLn;
  WriteString('Define y-gradient : ');WriteLn;
  WriteString('# offset-frequency points in y-direction : ');
  ReadInt(ndy);WriteLn;
  IF ndy=0 THEN ndy:=1 END;
  WriteString('The stepsize dy [cm]                     : ');
  ReadReal(dy);WriteLn;
  WriteString('Minimal y-coordinate y0 [cm]             : ');ReadReal(y0);
  WriteLn;WriteLn;
  CalcGrad(ap,h,gy);
  WriteLn;WriteLn;
  WriteString('Define z-gradient : ');WriteLn;
  WriteString('# offset-frequency points in z-direction : ');
  ReadInt(ndz);WriteLn;
  IF ndz=0 THEN ndz:=1 END;
  WriteString('The stepsize dz [cm]                     : ');
  ReadReal(dz);WriteLn;
  WriteString('Minimal z-coordinate z0 [cm]             : ');ReadReal(z0);
  WriteLn;WriteLn;
  CalcGrad(ap,h,gz);
  WriteLn;WriteLn;
END DefinegradientOffset;

PROCEDURE DefinePulseHamiltonian;
VAR answer,hard:INTEGER;
BEGIN 
  ScreenMode(3);WriteLn;WriteLn;WriteLn; 
  WriteString('            *****************************************');WriteLn;
  WriteString('            ***  DEFINITION OF PULSE HAMILTONIAN  ***');WriteLn;
  WriteString('            *****************************************');WriteLn;
  WriteLn;WriteLn;
  WriteString('Apply :');WriteLn;WriteLn;
  WriteString('        1. a standard hardpulse 90 or 180');WriteLn;
  WriteString('        2. another pulse');WriteLn;WriteLn;
  WriteString('Your choice [1,2] : ');ReadInt(hard);
  WriteLn;
  IF hard=1 THEN
    pulseflag:=0;
    ap:=1;ti:=1.0e-5;h:=1.0e-5;(*both h and ti must have the same value!!*)
    AMHardPulseDefinition(ap,ti,xb,yb);
  ELSE  
    WriteString('What is the pulse time [ms]  : ');ReadReal(pt);
    pt:=pt/1000.0;
    WriteLn;WriteLn;
    WriteString(' How many pulse-time intervals [-] : '); 
    ReadInt(ap);WriteLn;WriteLn;
    h:=pt/FLOAT(ap);
    ti:=pt/FLOAT(ap);
    WriteString('Pulse Hamiltonian type : ');WriteLn;
    WriteLn;
    WriteString('     1. Hp(t) = f(t).Ix   (AM modulated) ');WriteLn;
    WriteString('     2. Hp(t) = f(t).Ix + g(t).Iy (FM or adiabatic)');WriteLn;
    WriteString('     3. Pulse file (Bx,By) from disk');WriteLn;
    WriteString('     4. Pulse file (Bx,Bz) from disk');WriteLn;
    WriteLn;
    WriteString(' Your choice is [1..4] : ');
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
  WriteString('Static offset frequency of pulse : ');ReadReal(statoff);
  WriteLn;  
END DefinePulseHamiltonian;

PROCEDURE ReadBxByPulse(ap:INTEGER;VAR xb,yb,offset:RArray);
VAR i:INTEGER;
    mult:REAL;
BEGIN
  WriteLn;WriteLn;
  WriteString('File name Bx(t)');
  RealArrayIn(ap,xb);
  WriteString('File name By(t)');
  RealArrayIn(ap,yb);
  WriteString('Multiplication constant : ');ReadReal(mult);WriteLn;
  FOR i:=0 TO ap-1 DO
    xb[i]:=xb[i]*mult;
    yb[i]:=yb[i]*mult;
    offset[i]:=0.0;
  END;  
END ReadBxByPulse;

PROCEDURE ReadBxBzPulse(ap:INTEGER;VAR xb,yb,offset:RArray);
VAR i:INTEGER;
    mult:REAL;
BEGIN
  WriteLn;WriteLn;
  WriteString('Name RF amplitude file Bx(t)');WriteLn;
  RealArrayIn(ap,xb);
  WriteString('Multiplication constant Bx(t) : ');ReadReal(mult);WriteLn;
  FOR i:=0 TO ap-1 DO
    xb[i]:=xb[i]*mult;
  END; 
  WriteString('Name of RF offset frequency file dW(t)');WriteLn;
  RealArrayIn(ap,offset);
  WriteString('Multiplication constant dw(t) : ');ReadReal(mult);WriteLn;
  FOR i:=0 TO ap-1 DO
    offset[i]:=offset[i]*mult;
  END; 
  FOR i:=0 TO ap-1 DO
    yb[i]:=0.0;
  END;  
END ReadBxBzPulse;

PROCEDURE LiouvilleInteractive;       
VAR choice,choice1:INTEGER;
     chr:CHAR;
BEGIN
  Sound(900);Delay(200);NoSound;
  ScreenMode(3);WriteString(version);WriteLn;
  WriteString('This program integrates the Liouville von Neumann equations');
  WriteLn;
  WriteString('          d sigma / dt   = -i [ H , sigma ]');WriteLn;WriteLn;
  WriteString('Possible actions:');WriteLn;WriteLn;
  WriteString('     1. (Re)-Define a spin system');WriteLn;
  WriteString('     2. (Re)-Define a pulse Hamilton-operator');WriteLn;
  WriteString('     3. (Re)-Define a gradient-field etc. ');WriteLn;
  WriteString('     4. Integrate using eigenvalue determination');WriteLn;
  WriteString('     5. spectral analyses');WriteLn;
  WriteString('     6. Determine expectation values of operator(-s) (-arrays). ');WriteLn;
  WriteString('     7. Integrate using eigenvalue determination');WriteLn;
  WriteString('        and determining a time dependent expectation-');WriteLn;
  WriteString('        value of an arbitrary operator');WriteLn;
  WriteString('     8. Read / write binary files');WriteLn;
  WriteString('     9. Optimize RF Pulse ');WriteLn;
  WriteString('    10. Effects B1-inhomogeneity');WriteLn;
  WriteString('    11. Store RF-pulse');WriteLn;
  WriteString('    12. Exit ilv');WriteLn;WriteLn;
  WriteString('Your choice is [1..12] : ');
  ReadInt(choice);WriteLn;
  IF choice#12 THEN
    CASE choice OF
                   1:RedefineSpinSystem;|
                   2:RedefinePulseHamiltonian;|
                   3:DefinegradientOffset;|
                   4:Rotate3D;
                     CalcExpVal;|
                   5:WriteString('Possibilities :');WriteLn;
                     WriteString('          1. calculate average spectrum ');WriteLn;
                     WriteString('          2. calculate spectra seperately ');WriteLn;
                     WriteString('Your choice [1,2] : ');ReadInt(choice1);WriteLn;
                     CMatAdd(n,Icoup,Izee,Istat);
                     MakeTotalIx(n,Nspins,ket,Ilist,TRUE,Ixtot);
                     MakeTotalIy(n,Nspins,ket,Ilist,TRUE,Iytot);
                     MakeTotalIz(n,Nspins,ket,Ilist,TRUE,Iztot);
                     ShowCMatElements(n,Iztot);
                     Read(chr);
                     IF choice1 = 1 THEN 
                       AverageSpectrum(n,ap,densmat,Istat,Ixtot,Iytot,Iztot,ket,Ilist); 
                     ELSE
                       SeparateSpectra(n,ap,densmat,Istat,Ixtot,Iytot,Iztot,ket,Ilist);
                     END;
                     MakeTotalIx(n,Nspins,ket,Ilist,FALSE,Ixtot);
                     MakeTotalIy(n,Nspins,ket,Ilist,FALSE,Iytot);
                     MakeTotalIz(n,Nspins,ket,Ilist,FALSE,Iztot);|           
                   6:OperatorExpectationValues;|
                   7:ExpectationValueRotation;
                     CalcExpVal;|
                   8:BinFileActions(ndw,n);|
                   9:Optimize;|
                  10:B1Inhomogeneity;
                     CalcExpVal;|               
                  11:OutputPulse;|
    END;
    LiouvilleInteractive;
  END;
END LiouvilleInteractive;

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

PROCEDURE Rotate3D;
VAR hc,hb,hs,ht,r:CMatrix;
    dw:REAL;
    k,l,m,nz,dummy,j,ftype,fn,fndw:INTEGER;
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
      ndw:=ndx*ndy*ndz;
      WriteBinFileHeader(inout,0,n,ndw); 
      nbytes:=n*8*2;
      FOR k:=1 TO (ndx*ndy*ndz) DO
        FOR j:= 1 TO n DO        
          address1:=ADR(densmat[j,1,1]);
          WriteNBytes(inout,address1,nbytes,written);          
          IF written # nbytes THEN
            WriteString('>>> WRITE BINARY-FILE ERROR <<<');
            Delay(1000);WriteLn;  
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
      FOR k:=1 TO ndx DO
        FOR m:=1 TO ndy DO
          FOR nz:=1 TO ndz DO
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
              dw:=(x0+FLOAT(k-1)*dx)*gx[l];
              dw:=dw+(y0+FLOAT(m-1)*dy)*gy[l];
              dw:=dw+(z0+FLOAT(nz-1)*dz)*gz[l]+offset[l]+statoff;
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
            WriteString('*');          
          END;
        END;
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
END Rotate3D;

PROCEDURE Rotate2D(n,ap,ndw,sectortype:INTEGER;h,statoff:REAL);
VAR hc,hb,hs,ht,r:CMatrix;
    dw,dt:REAL;
    k,l,m,nz,dummy,j,ftype,fn,fndw:INTEGER;
    written,read,nbytes,highpos,lowpos:CARDINAL;
    inout,out:File;
    result:DirResult;
    address1,address2:ADDRESS;
    done:BOOLEAN;
    chr:CHAR;
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
            WriteString('>>> WRITE BINARY-FILE ERROR <<<');
            Delay(1000);WriteLn;  
          END;
        END;     
      END;
      Close(inout,done);
      CMatZero(n,densmat,densmat);    
    ELSE 
      Open(inout,'dm.bin',ReadOnly,done); 
      ReadBinFileHeader(inout,ftype,fn,fndw);
      IF ndw#fndw THEN
        Create(out,'store',done);
        WriteBinFileHeader(out,ftype,fn,ndw);
        nbytes:=n*8*2;
        FOR j:= 1 TO n DO
          address1:=ADR(densmat[j,1,1]);
          ReadNBytes(inout,address1,nbytes,read);        
          IF read # nbytes THEN
            WriteString('>>> READ BINARY FILE ERROR <<<');Delay(1000);WriteLn;
          END;
        END;
        FOR k:=1 TO ndw DO
          FOR j:= 1 TO n DO        
            address1:=ADR(densmat[j,1,1]);
            WriteNBytes(out,address1,nbytes,written);          
            IF written # nbytes THEN
              WriteString('>>> WRITE BINARY-FILE ERROR <<<');
              Delay(1000);WriteLn;  
            END;
          END;     
        END;
        Close(inout,done);Close(out,done);
        Delete('dm.bin',result);
        Rename('store','dm.bin',result);  
      ELSE
        Close(inout,done);
      END;       
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
          dw:=statoff+offset[l];
          IF sectortype=2 THEN
            dt:=FLOAT(k)*h;
          ELSE
            dt:=h;
          END;    
          RMultCMat(n,dw,Izgrad,hs);
          CMatAdd(n,hc,hs,hs);
          RMultCMat(n,xb[l],Ixtot,ht);
          CMatAdd(n,hs,ht,hb);
          RMultCMat(n,yb[l],Iytot,ht);
          CMatAdd(n,ht,hb,hb);
          RMultCMat(n,-1.0,hb,hb);
          ExpMatrix(n,hb,dt,hb);
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
  Delay(10000);    
  ScreenMode(3);    
END Rotate2D;

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
    stx,sty:ARRAY[0..256] OF REAL;
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
          dw:=x0*gs+offset[l]+statoff;
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

PROCEDURE CalcExpVal;
VAR dummy:INTEGER;
BEGIN
  ScreenMode(3);
  WriteString('****************************************************');WriteLn;
  WriteString('***       CALCULATION OF EXPECTATION VALUES      ***');WriteLn;
  WriteString('****************************************************');
  WriteLn;WriteLn;
  WriteString('Possible actions :');WriteLn;WriteLn;
  WriteString('          1. Calculate expectation values of operators');WriteLn;
  WriteString('          2. Continue to main menu');WriteLn;
  WriteLn;WriteString('Your choice [1,2] : ');ReadInt(dummy);WriteLn;
  IF dummy#2 THEN
    ExpValNDensMat;
    CalcExpVal;
  END;
END CalcExpVal;

PROCEDURE WriteIDL_BW_Header(out:File;nrij,nkol,nkan:INTEGER);
VAR lbarr: ARRAY[1..256] OF CARDINAL;
    labelsize,bpint:INTEGER;
    address:ADDRESS;
    written:CARDINAL;
BEGIN
  labelsize:=256;
  bpint:=4;
  lbarr[1]:=nrij;lbarr[2]:=nkol;lbarr[3]:=nkan;
  address:=ADR(lbarr[1]);
  WriteNBytes(out,address,labelsize*bpint,written);
END WriteIDL_BW_Header;

PROCEDURE ExpValNDensMat;
VAR store1,store2:CMatrix;
    address,address1:ADDRESS;
    in,out:File;
    read,nbytes,written:CARDINAL;
    i,j,dummy,ftype,fn,fndw,choice:INTEGER;
    done:BOOLEAN;
    lr:COMPLEXLONGREAL;
    chr:CHAR;
    fnm:ARRAY[0..12] OF CHAR;
BEGIN
  Open(in,'dm.bin',ReadOnly,done);
  ReadBinFileHeader(in,ftype,fn,fndw);
  nbytes:=fn*8*2;
  ExpectationOperator(fn,Nspins,store1);
  WriteString('Create:  ');WriteLn;WriteLn;
  WriteString('       1. SG-plot file ');WriteLn;
  WriteString('       2. IDL file     ');WriteLn;WriteLn;
  WriteString('Your choice [1,2] : ');ReadInt(choice);WriteLn;
  IF choice=1 THEN
  WriteString('SG-plot file naam :');WriteLn;
    OpenOutput('DAT');
  ELSE
    WriteString('IDL-file name : ');ReadString(fnm);  
    Create(out,fnm,done);
    WriteIDL_BW_Header(out,ndx,ndy,ndz);
  END;  
  address1:=ADR(lr[1]);
  FOR i:=1 TO fndw DO
    FOR j:=1 TO fn DO
      address:=ADR(store2[j,1,1]);
      ReadNBytes(in,address,nbytes,read);
      IF read # nbytes THEN
        WriteString('>>> READ BINARY FILE ERROR <<<');Delay(1000);WriteLn;            
      END;
    END;  
    ExpectationValue(fn,store1,store2,lr); 
    IF choice=1 THEN
      WriteCard(1,5);WriteString("   ");WriteReal(FLOAT(i),10);
      WriteString("   ");WriteReal(lr[1],18);WriteLn; 
    ELSE
      WriteNBytes(out,address1,8,written);
    END;  
  END;
  Close(in,done);
  IF choice=1 THEN
    CloseOutput;  
  ELSE
    Close(out,done);
  END;    
  WriteLn;
  WriteString('Last Expectation value : ');
  WriteReal(lr[1],18);WriteLn;
  WriteString('Press <Enter> to continue');
  Read(chr);
  WriteLn;
END ExpValNDensMat;
    
PROCEDURE ExpectationValueRotation;
VAR hc,hb,hs,ht,oper:CMatrix;
    dw:REAL;
    inout,out:File;
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
        FOR l:=0 TO ap-1 DO
          dw:=(x0+FLOAT(k-1)*dx)*gs+offset[l]+statoff;
          RMultCMat(n,dw,Izgrad,hs);
          CMatAdd(n,hc,hs,hs);
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
      Close(inout,done);
      Close(out,done);
      calc:=calc+1;
      Delete('dm.bin',result);
      Rename('store','dm.bin',result);       
    ELSE
      CloseOutput;
      Close(inout,done);
      WriteString('Dimensions of dm.bin-file are : ');WriteLn;
      WriteInt(ftype,5);WriteInt(fn,5);WriteInt(fndw,5);WriteLn;
      WriteString('Expected dimensions are : ');WriteLn;
      WriteInt(0,5);WriteInt(n,5);WriteInt(ndw,5);WriteLn;
      WriteString('>>> NO EXPECTATION VALUES  <<<');ReadInt(dummy);      
    END;  
  END; 
END ExpectationValueRotation;


PROCEDURE ExpectationOperator(n,Nspins:INTEGER;VAR oper1:CMatrix);
VAR choice:INTEGER;
BEGIN
  ScreenMode(3);
  WriteString('*******************************************************');WriteLn;  
  WriteString('***   EXPECTATION VALUES OF AN ARBITRARY OPERATOR   ***');WriteLn;    
  WriteString('***                  OR PRODUCT OPERATOR            ***');WriteLn;
  WriteString('*******************************************************');WriteLn;
  WriteLn;
  WriteString('Define Operator : ');WriteLn;WriteLn;
  WriteString('    1. Choose from a list (only valid for (multiple) spin 1/2 system');
  WriteLn;
  WriteString('    2. Create manually');WriteLn;
  WriteLn;
  WriteString('Your choice [1,2] : ');ReadInt(choice);
  IF choice=2 THEN
    MakeExpectationValueOperator(n,oper1);
  ELSE
    CreateOperators(n,Nspins,oper1);
  END;    
END ExpectationOperator;

PROCEDURE MakeExpectationValueOperator(n:INTEGER;VAR oper1:CMatrix);
VAR oper,store1,store2,oper3:CMatrix;
    i,type,choice,choice1:INTEGER;
    lr:COMPLEXLONGREAL;
    mult:REAL;
BEGIN
  CMatZero(n,store1,store1);CMatZero(n,store2,store2);
  LOOP
    ScreenMode(3);
    WriteString('*******************************************************');WriteLn;  
    WriteString('***   EXPECTATION VALUES OF AN ARBITRARY OPERATOR   ***');WriteLn;    
    WriteString('***                  OR PRODUCT OPERATOR            ***');WriteLn;
    WriteString('*******************************************************');WriteLn;
    WriteLn;
    WriteString('Two operatators OPERATOR-1 and OPERATOR-2 can be defined');WriteLn;
    WriteString('These operators can be added or multiplied');WriteLn;WriteLn;
    WriteString('Possible actions : ');WriteLn;
    WriteString('1 : Define OPERATOR-1');WriteLn;
    WriteString('2 : Define OPERATOR_2');WriteLn;
    WriteString('3 : OPERATOR-1 := OPERATOR-1 + OPERATOR-2');WriteLn;
    WriteString('4 : OPERATOR-1 := OPERATOR-1 * OPERATOR-2');WriteLn;WriteLn;
    WriteString('5 : Calculate expectation values of  OPERATOR-1');WriteLn;
    WriteString('Your choice [1..5]  : ');ReadInt(choice);WriteLn;
    IF choice = 3 THEN
      CMatAdd(n,store1,store2,store1);
    END;
    IF choice= 4 THEN 
      CMatMult(n,store1,store2,store1);
      NormCMatrix(n,oper1);
    END;
    IF choice = 5 THEN EXIT END;
    IF ((choice =1) OR (choice = 2)) THEN        
      CMatZero(n,oper,oper);CMatZero(n,oper1,oper1);
      LOOP
        WriteString('Spin number = ');ReadInt(i);WriteLn;
        WriteString('Give type of the operator I(type) : type = [1,2,3,4,5] : ');WriteLn;
        WriteString(' TYPE 1 = x        TYPE 4 = + ');WriteLn;
        WriteString(' TYPE 2 = y        TYPE 5 = - ');WriteLn;
        WriteString(' TYPE 3 = z');WriteLn;        
        WriteString('(Be careful with non-hermitian operator types 4 and 5 !!');WriteLn;
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
        WriteString('1 = ADD to already existant operator');WriteLn;  
        WriteString('2 = MULTIPLY to the already existant operator');WriteLn;
        WriteLn;
        WriteString('Your choice [1,2] : ');ReadInt(type);WriteLn;
        CASE type OF
                    1:CMatAdd(n,oper,oper1,oper1);|
                    2:CMatMult(n,oper1,oper,oper1);
                      NormCMatrix(n,oper1);
        END; 
        WriteLn;WriteLn;
        WriteString('Previous defined operator O.K. [1=yes,2=no] :  ');ReadInt(type);WriteLn;
        IF type = 1 THEN 
          WriteString('Multiply Operator with a scalar [1=yes,2=no] :  ');
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


PROCEDURE CreateOperators(n,Nspins:INTEGER;VAR oper:CMatrix);
VAR count,i,j,sp,ntest,dummy,nlines,opno:INTEGER;
    symbolic,spinsymbol:ARRAY[0..4] OF CHAR;
    chr:CHAR;
BEGIN
  ScreenMode(3);
  CASE Nspins OF 
    1:WriteString('Spin(1) = I');
      spinsymbol[0]:='I';|
    2:spinsymbol[1]:='S';spinsymbol[0]:='I';
      WriteString('Spin(1) = I      Spin(2) = S');
      WriteLn;WriteLn;|
    3:spinsymbol[2]:='T';spinsymbol[1]:='S';spinsymbol[0]:='I';
      WriteString('Spin(1) = I      Spin(2) = S       Spin(3) = T');
      WriteLn;WriteLn;|
    4:spinsymbol[3]:='U';spinsymbol[2]:='T';spinsymbol[1]:='S';spinsymbol[0]:='I';
      WriteString('Spin(1) = I      Spin(2) = S       Spin(3) = T      Spin(4) = U');
      WriteLn;WriteLn;
  END;      
  ntest:=1;nlines:=1;
  FOR j:=1 TO 2*Nspins DO
    ntest:=ntest*2;
  END;
  count:=0;
  FOR i:=0 TO ntest-1 DO
    INC(count);
    DecimalTo4Mal(i,symbolic);
    WriteInt(i,1); 
    IF i<10 THEN WriteString(' ') END;
    WriteString('. ');
    FOR j:=0 TO Nspins-1 DO
      CASE j OF 
        0:IF symbolic[j]#'e' THEN 
            WriteString(spinsymbol[0]) 
          ELSE 
            WriteString(' ');
          END;|
        1:IF symbolic[j]#'e' THEN 
            WriteString(spinsymbol[1]) 
          ELSE 
            WriteString(' ');
          END;|
        2:IF symbolic[j]#'e' THEN 
            WriteString(spinsymbol[2]);
          ELSE 
            WriteString(' ');
          END;|
        3:IF symbolic[j]#'e' THEN 
            WriteString(spinsymbol[3]) 
          ELSE 
            WriteString(' ');
          END;
      END;   
      IF symbolic[j]#'e' THEN Write(symbolic[j]) ELSE WriteString(' ');END;    
    END;
    WriteString('     ');
    IF count=4 THEN
      nlines:=nlines+1;
      IF nlines=20 THEN 
        WriteLn;WriteLn;
        WriteString('Press any key to continue : ');
        Read(chr);
        ScreenMode(3);nlines:=1;
      END;  
      WriteLn;
      count:=0;
    END;  
  END;
  WriteString('Enter operator number : ');ReadInt(opno);WriteLn;
  DecimalTo4Mal(opno,symbolic);
  CreateNormOperator(n,Nspins,symbolic,oper);
END CreateOperators;

PROCEDURE CreateNormOperator(n,Nspins:INTEGER;symbolic:ARRAY OF CHAR;
                             VAR oper:CMatrix);
VAR store,store2:CMatrix;
    i,j:INTEGER;
BEGIN
  CMatZero(n,store,store);
  FOR i:=1 TO Nspins DO
    CASE symbolic[i-1] OF
        'e':FOR j:=1 TO n DO store[j,j,1]:=1.0 END;|
        'x':MakeMatrixIx(Ilist,ket,n,i,store);|
        'y':MakeMatrixIy(Ilist,ket,n,i,store);|
        'z':MakeMatrixIplus(Ilist,ket,n,i,store);
            MakeMatrixImin(Ilist,ket,n,i,store2);
            CMatCommutator(n,store,store2,store);
            RMultCMat(n,0.5,store,store);|     
     END;
     NormCMatrix(n,store);                           
     IF (i=1) THEN
       CopyCMatrix(n,store,oper);
     ELSE 
       IF symbolic[i-1]#'e' THEN
         CMatMult(n,oper,store,oper);
       END;  
     END;  
  END;
  NormCMatrix(n,oper);
END CreateNormOperator;  

PROCEDURE DecimalTo4Mal(number:INTEGER;VAR vtal:ARRAY OF CHAR);
VAR i,rest,noper:INTEGER;
BEGIN
  FOR i:=0 TO 4 DO vtal[i]:='e' END;
  noper:=1;
  LOOP
    IF (number=0) THEN EXIT END;
    rest:=(number MOD 4);
    number:=(number DIV 4);
    CASE rest OF
      0: vtal[noper-1]:='e';|
      1: vtal[noper-1]:='x';|
      2: vtal[noper-1]:='y';|
      3: vtal[noper-1]:='z';
    END;
    noper:=noper+1;  
  END;
END DecimalTo4Mal;    

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
  ExpectationOperator(n,Nspins,oper);
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
    WriteString('Real part of <Operator> ');
    WriteReal(lr[1],10);WriteLn;
    y[k]:=lr[1];
    WriteString('Imaginary part of <Operator> ');WriteReal(lr[2],10);WriteLn;   
    WriteLn;
  END;
  IF fndw > 1 THEN  
    tdplot1(x,y,ndw,1,2);
  ELSE
    WriteString(' no xyplot of one point ... ');ReadInt(dummy);
  END;    
  Close(in,done);
  WriteString('Expectation value of an other operator [1=yes, 2=no] : ');
  ReadInt(type);WriteLn;  
  IF type = 1 THEN 
    OperatorExpectationValues;
  END;
END OperatorExpectationValues;     
    
PROCEDURE RedefinePulseHamiltonian;
VAR choice:INTEGER;
BEGIN
  DefinePulseHamiltonian;
  WriteString('More changes in Pulse Hamilton-operator [1=yes, 2=no] : ');
  ReadInt(choice);WriteLn;
  IF choice=1 THEN 
    RedefinePulseHamiltonian;
  END;
END RedefinePulseHamiltonian;

PROCEDURE RedefineSpinSystem;
VAR choice:INTEGER;
BEGIN
  ScreenMode(3);WriteLn;WriteLn;
  WriteString('*****************************************');WriteLn;
  WriteString('***    Definition of the spin system  ***');WriteLn;
  WriteString('*****************************************');WriteLn;
  WriteString('Possible actions:');WriteLn;WriteLn;
  WriteString('    1. total (re)-definition');WriteLn;
  WriteString('    2. change only Zeeman terms');WriteLn;
  WriteString('    3. change only scalar coupling constants');WriteLn;
  WriteString('    4. change only Zeeman terms & scalar coupling constants');
  WriteLn;WriteLn;
  WriteString('Your choice is [1..4] : ');ReadInt(choice);WriteLn;
  CASE choice OF
                 1:TotalStartUp;|
                 2:MakeZeeman;|
                 3:SpinSpinCoupling;|
                 4:MakeZeeman;
                 SpinSpinCoupling;
  END;
  WriteString('More changes of the spin system?  [1=yes, 2=no] : ');
  ReadInt(choice);
  IF choice = 1 THEN  
    RedefineSpinSystem
  END;
END RedefineSpinSystem;

PROCEDURE TotalStartUp;
BEGIN
  SetupSpins;
  MakeZeeman;
  SpinSpinCoupling;
  MakeTotalIx(n,Nspins,ket,Ilist,FALSE,Ixtot);
  MakeTotalIy(n,Nspins,ket,Ilist,FALSE,Iytot);
  MakeTotalIz(n,Nspins,ket,Ilist,FALSE,Iztot);
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
      stop:=KeyPressed();
      IF stop = TRUE THEN
        ScreenMode(3);
        WriteString('Function value = ');
        WriteReal(fp,18);WriteLn;WriteLn;
        WriteString('1. Stop calculation.');WriteLn;
        WriteString('2. Continue calculation.');WriteLn;
        WriteString('Your choice [1,2] : ');ReadInt(continue);
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
  ExpectationValue(n,Istat,densmat,lr);   
  WriteString('Expectation Value dw norm is : ');WriteReal(lr[1],18);WriteLn;  
  CopyCMatrix(n,store,densmat);
  RETURN lr[1];
END fnc;

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
  ExpectationOperator(n,Nspins,Istat);
  WriteString('The number of parameters is : ');
  ReadInt(npar);WriteLn;
  npar:=4*npar;
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

(******************************************************************)
(******************************************************************)

PROCEDURE LiouvilleNonInteractive;
VAR choice,choice1:INTEGER;
    done:BOOLEAN;
    chr:CHAR;
BEGIN
  ScreenMode(3);
  WriteString('    ****************************************************');WriteLn;
  WriteString('    ***  Solve Liouville von Neuman Non-Interactive  ***');WriteLn; 
  WriteString('    ****************************************************');WriteLn;
  WriteString('Possibilities :');WriteLn;WriteLn;
  WriteString('               1. compose or change a pulse sequence');WriteLn;
  WriteString('               2. load and calculate a pulse sequence');WriteLn;
  WriteString('               3. load and calculate 1D/2D NMR pulse sequence');WriteLn;
  WriteString('               4. calculate expectation values');WriteLn;
  WriteString('               5. calculate spectra.');WriteLn;
  WriteString('               6. show pulse sequence file names in this directory');WriteLn;
  WriteString('               7. quit ilv');WriteLn;WriteLn;
  WriteString('Your choice [1..7] : ');ReadInt(choice);WriteLn;
  IF choice<7 THEN
    CASE choice OF
                1:ComposePulseSequence;|
                2:CalculatePulseSequence;|
                3:Calculate2DPulseSequence;|
                4:CalcExpVal;|
                5:WriteString('Possibilities :');WriteLn;
                  WriteString('          1. calculate average spectrum ');WriteLn;
                  WriteString('          2. calculate spectra seperately ');WriteLn;
                  WriteString('Your choice [1,2] : ');ReadInt(choice1);WriteLn;
                  CMatAdd(n,Icoup,Izee,Istat);
                  MakeTotalIx(n,Nspins,ket,Ilist,TRUE,Ixtot);
                  MakeTotalIy(n,Nspins,ket,Ilist,TRUE,Iytot);
                  IF choice1 = 1 THEN 
                    AverageSpectrum(n,ap,densmat,Istat,Ixtot,Iytot,Iztot,ket,Ilist); 
                  ELSE
                    SeparateSpectra(n,ap,densmat,Istat,Ixtot,Iytot,Iztot,ket,Ilist);
                  END;
                  MakeTotalIx(n,Nspins,ket,Ilist,FALSE,Ixtot);
                  MakeTotalIy(n,Nspins,ket,Ilist,FALSE,Iytot);|               
                6:ScreenMode(3);
                  Run('C:\DOS\D.COM','*.SEQ',done);
                  WriteString('Press any key to continue');WriteLn;
                  Read(chr);
    END;
    LiouvilleNonInteractive;
  END;   
END LiouvilleNonInteractive;

PROCEDURE Calculate2DPulseSequence;
VAR dummy:ARRAY[0..20] OF CHAR;
    remark:ARRAY[0..31] OF CHAR;
    nbytes,nsect,i,sequencetype,sectortype,sector:INTEGER;
    read:CARDINAL;
    pfile:ARRAY[0..7] OF CHAR;
    flnm,flnm1:String;
    copy:ARRAY[0..15] OF CHAR;
    in:File;
    address:ADDRESS;
    done,simulation2D:BOOLEAN;
    char,chr:CHAR;
    result:DirResult;
BEGIN
  ScreenMode(3);
  simulation2D:=FALSE;
  pulseflag:=1;
  OpenInput('SEQ');
  ReadString(dummy);ReadInt(sequencetype);
  NonIntIspins;
  MakeKets(ket,Ilist);
  NonIntMakeBoltzmann;
  NonIntMakeZeeman;
  NonIntSpinSpinCoupling;
  MakeTotalIx(n,Nspins,ket,Ilist,FALSE,Ixtot);
  MakeTotalIy(n,Nspins,ket,Ilist,FALSE,Iytot);
  MakeTotalIz(n,Nspins,ket,Ilist,FALSE,Iztot);
  MakeGradientOperator;
  ReadInt(nsect);
  WriteString('Number of pulse sectors to calculate    : ');
  WriteInt(nsect,5);WriteLn;
  FOR i:=1 TO nsect DO
    ReadString(dummy);ReadInt(sector);
    WriteString('Calculating sector                      : ');WriteInt(sector,5);WriteLn;
    ReadString(dummy);ReadInt(sectortype);
    WriteString('Sector type                             : ');WriteInt(sectortype,5);WriteLn;
    ReadString(dummy);ReadInt(ap);
    WriteString('Number of time intervals in this sector : ');WriteInt(ap,5);WriteLn;
    ReadString(dummy);ReadInt(ndw);
    WriteString('Number of (evolution) intervals         : ');WriteInt(ndw,5);WriteLn;
    ReadString(dummy);ReadReal(pt);
    WriteString('Sector or pulsetime                [ms] : ');WriteReal((pt*1000.0),18);WriteLn;
    h:=pt/FLOAT(ap);ti:=h;
    ReadString(dummy);ReadString(pfile);
    WriteString('Reading pulse file                      :  ');WriteString(pfile);WriteLn;
    Open(in,pfile,ReadOnly,done);
    nbytes:=ap*8;
    address:=ADR(xb[0]);
    ReadNBytes(in,address,nbytes,read);
    address:=ADR(yb[0]);
    ReadNBytes(in,address,nbytes,read);
    address:=ADR(offset[0]);
    ReadNBytes(in,address,nbytes,read);
    Close(in,done);
    ReadString(dummy);ReadReal(statoff);
    WriteString('Reading pulse file                      :  ');WriteReal(statoff,18);WriteLn;
    statoff:=statoff*2.0*pi;
    ReadString(dummy);ReadString(remark);
    WriteString('Remark on sector : ');
    WriteString(remark);WriteLn;
    Rotate2D(n,ap,ndw,sectortype,h,statoff);
    IF sectortype = 2 THEN
      simulation2D:=TRUE;
    END;  
  END;
  ReadString(flnm);
  Concat('copy ','dm.bin',copy);
  DosCommand(copy,flnm,done);
  CloseInput;
  CMatAdd(n,Izee,Icoup,Istat);
  IF simulation2D THEN
    MakeTotalIx(n,Nspins,ket,Ilist,TRUE,Ixtot);
    MakeTotalIy(n,Nspins,ket,Ilist,TRUE,Iytot);
    Determine2DSpectrum(n,ap,densmat,Istat,Ixtot,Iytot,Izgrad,ket,Ilist);
    MakeTotalIx(n,Nspins,ket,Ilist,FALSE,Ixtot);
    MakeTotalIy(n,Nspins,ket,Ilist,FALSE,Iytot);
  END;  
END Calculate2DPulseSequence;

PROCEDURE CalculatePulseSequence;
VAR dummy:ARRAY[0..20] OF CHAR;
    sequencetype,nbytes,nsect,i:INTEGER;
    read:CARDINAL;
    pfile:ARRAY[0..7] OF CHAR;
    flnm:ARRAY[0..11] OF CHAR;
    copy:ARRAY[0..15] OF CHAR;
    in:File;
    address:ADDRESS;
    done:BOOLEAN;
    char:CHAR;
    xas:ARRAY[0..129] OF REAL;
BEGIN
  ScreenMode(3);
  pulseflag:=1;
  OpenInput('SEQ');
  ReadString(dummy);ReadInt(sequencetype);
  NonIntIspins;
  MakeKets(ket,Ilist);
  NonIntMakeBoltzmann;
  NonIntMakeZeeman;
  NonIntSpinSpinCoupling;
  MakeTotalIx(n,Nspins,ket,Ilist,FALSE,Ixtot);
  MakeTotalIy(n,Nspins,ket,Ilist,FALSE,Iytot);
  MakeTotalIz(n,Nspins,ket,Ilist,FALSE,Iztot);
  MakeGradientOperator; 
  ReadInt(ndx);ReadInt(ndy);ReadInt(ndz);
  WriteString('# x-direction : ');WriteInt(ndx,5);
  WriteString(' # y-direction : ');WriteInt(ndy,5);
  WriteString(' # z-direction : ');WriteInt(ndz,5);WriteLn;
  ReadInt(nsect);
  WriteString('Number of pulse sectors to calculate      : ');
  WriteInt(nsect,5);WriteLn;
  FOR i:=1 TO nsect DO
    WriteString('Calculating sector                      : ');WriteInt(i,5);WriteLn;
    ReadString(dummy);ReadString(dummy);
    ReadString(dummy);ReadInt(ap);
    WriteString('Number of time intervals in this sector : ');WriteInt(ap,5);WriteLn;
    ReadString(dummy);ReadReal(pt);
    WriteString('Sector or pulsetime                [ms] : ');WriteReal(pt*1000.0,12);WriteLn;
    h:=pt/FLOAT(ap);ti:=h;
    ReadString(dummy);ReadString(pfile);
    WriteString('Reading pulse file                      : ');WriteString(pfile);WriteLn;
    Open(in,pfile,ReadOnly,done);
    nbytes:=ap*8;
    address:=ADR(xb[0]);
    ReadNBytes(in,address,nbytes,read);
    address:=ADR(yb[0]);
    ReadNBytes(in,address,nbytes,read);
    address:=ADR(offset[0]);
    ReadNBytes(in,address,nbytes,read);
    address:=ADR(gx[0]);
    ReadNBytes(in,address,nbytes,read);
    address:=ADR(gy[0]);
    ReadNBytes(in,address,nbytes,read);
    address:=ADR(gz[0]);
    ReadNBytes(in,address,nbytes,read);
    Close(in,done);
    ReadString(dummy);ReadReal(statoff);
    statoff:=statoff*2.0*pi;    
    ReadString(dummy);ReadReal(x0);     
    WriteString('Spatial starting point x-direction [cm] : ');
    WriteReal(x0,15);WriteLn;
    ReadString(dummy);ReadReal(dx);     
    WriteString('Spatial x-stepsize                 [cm] : ');
    WriteReal(dx,15);WriteLn;
    ReadString(dummy);ReadReal(y0);     
    WriteString('Spatial starting point y-direction [cm] : ');
    WriteReal(y0,15);WriteLn;
    ReadString(dummy);ReadReal(dy);     
    WriteString('Spatial y-stepsize                 [cm] : ');
    WriteReal(dy,15);WriteLn;
    ReadString(dummy);ReadReal(z0);     
    WriteString('Spatial starting point z-direction [cm] : ');
    WriteReal(z0,15);WriteLn;
    ReadString(dummy);ReadReal(dz);     
    WriteString('Spatial z-stepsize                 [cm] : ');
    WriteReal(dz,15);WriteLn;
    ReadString(dummy);ReadString(dummy);
    WriteString('Remark : ');WriteString(dummy);WriteLn;WriteLn;
    Rotate3D;
  END;       
  ReadString(flnm);
  Concat('copy ','dm.bin',copy);
  DosCommand(copy,flnm,done);
  CloseInput;
END CalculatePulseSequence;       

BEGIN
  n:=Nstmax;
  init;
  ScreenMode(3);
  WriteLn;WriteLn;
  WriteLn;WriteLn;WriteLn;
  WriteString('********************************************************************');WriteLn;
  WriteString('*           Integrate the Liouville Van Neumann Equations          *');WriteLn;
  WriteString('*                           version 1.0                            *');WriteLn;
  WriteString('*       The solution to nearly all your spin dynamical problems    *');WriteLn;
  WriteString('********************************************************************');WriteLn;
  WriteLn;WriteLn;
  Delay(2000);
  ScreenMode(3);
  WriteString('      *********************************************************');WriteLn;
  WriteString('      ***    Integrate the Liouville Van Neumann Equations  ***');WriteLn;
  WriteString('      *********************************************************');
  WriteLn;WriteLn;
  WriteString('Possibilities : ');WriteLn;WriteLn;
  WriteString('          1. Solve Liouville Von Neuman Equations interactive');
  WriteLn;
  WriteString('          2. Solve Liouville Von Neuman Equations non interactive');
  WriteLn;WriteLn;
  WriteString('Your choice [1,2] : ');ReadInt(meanchoice);
  WriteLn;WriteLn;
  IF meanchoice=1 THEN     
    LiouvilleInteractive; 
  ELSE
    LiouvilleNonInteractive;
  END;    
END ilv.
