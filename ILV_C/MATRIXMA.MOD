IMPLEMENTATION MODULE MatrixMathLib;
FROM MathLib0 IMPORT sqrt,sin,cos;
FROM InOut IMPORT Read,WriteLn,WriteInt,WriteString;
FROM RealInOut IMPORT WriteReal;
CONST pi = 3.1415926535897932; 
     
(* **********************************************************************
   Title:    Mathematical library containing procedures dealing
             with complex Nstmax x Nstmax (SQUARE) matrices
   LastEdit: 14-06-1991
   Author:   H. S. 
   System:   LOGITECH MODULA-2 
   ********************************************************************** *)
   

PROCEDURE CMatCommutator(n:INTEGER;a:CMatrix;b:CMatrix;
                         VAR outmat:CMatrix);
VAR matprod:CMatrix;
    m1,m2:COMPLEXLONGREAL;
BEGIN
  CMatMult(n,a,b,matprod);
  CMatMult(n,b,a,b);
  CMatRMultAdd(n,matprod,1.0,b,-1.0,outmat);
END CMatCommutator;

PROCEDURE CMatAdd(n:INTEGER;inmat1:CMatrix;
                     inmat2: CMatrix;
                      VAR outmat: CMatrix);
VAR j,k: INTEGER;
BEGIN
  FOR j:=1 TO n DO
    FOR k:= 1 TO n DO
      outmat[j,k,1]:=inmat1[j,k,1]+inmat2[j,k,1];
      outmat[j,k,2]:=inmat1[j,k,2]+inmat2[j,k,2];
    END;
  END;
END CMatAdd;

PROCEDURE CMatMultAdd(n:INTEGER;inmat1:CMatrix;mult1:COMPLEXLONGREAL;
                     inmat2: CMatrix;mult2:COMPLEXLONGREAL;
                      VAR outmat: CMatrix);
VAR j,k: INTEGER;
BEGIN
  CMultCMat(n,mult1,inmat1,inmat1);
  CMultCMat(n,mult2,inmat2,inmat2);
  FOR j:=1 TO n DO
    FOR k:= 1 TO n DO
      outmat[j,k,1]:=inmat1[j,k,1]+inmat2[j,k,1];
      outmat[j,k,2]:=inmat1[j,k,2]+inmat2[j,k,2];
    END;
  END;
END CMatMultAdd;

PROCEDURE CMatRMultAdd(n:INTEGER;inmat1:CMatrix;mult1:REAL;
                       inmat2: CMatrix;mult2:REAL;
                       VAR outmat: CMatrix);
VAR j,k: INTEGER;
BEGIN
  RMultCMat(n,mult1,inmat1,inmat1);
  RMultCMat(n,mult2,inmat2,inmat2);
  FOR j:=1 TO n DO
    FOR k:= 1 TO n DO
      outmat[j,k,1]:=inmat1[j,k,1]+inmat2[j,k,1];
      outmat[j,k,2]:=inmat1[j,k,2]+inmat2[j,k,2];
    END;
  END;
END CMatRMultAdd;

PROCEDURE CMatMult(n: INTEGER;mat1: CMatrix;mat2: CMatrix;
                   VAR outmat: CMatrix);
                                   
VAR j,k,l:INTEGER;
BEGIN
  FOR j := 1 TO n DO
    FOR k:= 1 TO n DO
      outmat[j,k,1]:=0.0;outmat[j,k,2]:=0.0;
      FOR l:=1 TO n DO
        outmat[j,k,1]:=outmat[j,k,1]+mat1[j,l,1]*mat2[l,k,1];
        outmat[j,k,1]:=outmat[j,k,1]-mat1[j,l,2]*mat2[l,k,2];
        outmat[j,k,2]:=outmat[j,k,2]+mat1[j,l,1]*mat2[l,k,2];
    	outmat[j,k,2]:=outmat[j,k,2]+mat1[j,l,2]*mat2[l,k,1];
      END;
    END;
  END;
END CMatMult;

PROCEDURE CMatZero(n:INTEGER;inmat:CMatrix;VAR outmat:CMatrix);
VAR j,k:INTEGER;
BEGIN
  FOR j:=1 TO n DO
    FOR k:=1 TO n DO
      outmat[j,k,1]:=0.0;outmat[j,k,2]:=0.0;
    END;
  END;
END CMatZero;


PROCEDURE ShowCMatElements(n:INTEGER;smat:CMatrix);
VAR j,k:INTEGER;
BEGIN
  FOR j:=1 TO n DO
    FOR k:=1 TO n DO
      WriteString("Sigma(");WriteInt(j,2);WriteString(",");
      WriteInt(k,2);WriteString(")  =  ");
      WriteReal(smat[j,k,1],10);WriteString("  + i*");
      WriteReal(smat[j,k,2],10);WriteLn;
    END;
  END;
END ShowCMatElements;



PROCEDURE CopyCMatrix(n:INTEGER;inmat:CMatrix;VAR copy:CMatrix);
VAR j,k:INTEGER;
BEGIN
  FOR j:=1 TO n DO
    FOR k:=1 TO n DO
      copy[j,k,1]:=inmat[j,k,1];
      copy[j,k,2]:=inmat[j,k,2];
    END;
  END;
END CopyCMatrix;

PROCEDURE RMultCMat(n:INTEGER;xx:REAL;inmat:CMatrix;
                   VAR outmat:CMatrix);

VAR j,k: INTEGER;
BEGIN
  FOR k:=1 TO n DO
    FOR j:=1 TO n DO
      outmat[j,k,1]:=xx*inmat[j,k,1];
      outmat[j,k,2]:=xx*inmat[j,k,2];
    END;
  END;
END RMultCMat;

PROCEDURE CMultCMat(n:INTEGER;xx:COMPLEXLONGREAL;inmat:CMatrix;
                   VAR outmat:CMatrix);

VAR j,k: INTEGER;
BEGIN
  FOR k:=1 TO n DO
    FOR j:=1 TO n DO
      outmat[j,k,1]:=xx[1]*inmat[j,k,1]-xx[2]*inmat[j,k,2];
      outmat[j,k,2]:=xx[1]*inmat[j,k,2]+xx[2]*inmat[j,k,1];
    END;
  END;
END CMultCMat;

PROCEDURE ExpectationValue(n:INTEGER;inmat,dmat:CMatrix;
                           VAR Eval: COMPLEXLONGREAL);
VAR k,l: INTEGER;
    mat:CMatrix;
BEGIN 
  Eval[1]:=0.0;Eval[2]:=0.0;
  FOR k:=1 TO n DO
    mat[k,k,1]:=0.0;mat[k,k,2]:=0.0;
    FOR l:=1 TO n DO
      mat[k,k,1]:=mat[k,k,1]+inmat[k,l,1]*dmat[l,k,1]-inmat[k,l,2]*dmat[l,k,2]; 
      mat[k,k,2]:=mat[k,k,2]+inmat[k,l,1]*dmat[l,k,2]+inmat[k,l,2]*dmat[l,k,1];
    END;
    Eval[1]:=Eval[1]+mat[k,k,1];
    Eval[2]:=Eval[2]+mat[k,k,2];
  END;
END ExpectationValue;

PROCEDURE CalcTrace(n : INTEGER;
                    mat: CMatrix; 
                    VAR trace:REAL); 
VAR 
   i: INTEGER; 
BEGIN 
  trace := 0.0; 
  FOR i := 1 TO n DO 
    trace := trace+mat[i,i,1]
  END; 
END CalcTrace; 

PROCEDURE NormCMatrix(n: INTEGER; VAR mat: CMatrix); 
VAR 
    store: CMatrix; 
    tr: REAL; 
    x:COMPLEXLONGREAL;
BEGIN 
  CMatMult(n, mat, mat, store); 
  CalcTrace(n, store, tr); 
  IF tr <> FLOAT(0) THEN 
    tr := 1.0/sqrt(tr); 
    x[1]:=tr;x[2]:=0.0;
    CMultCMat(n,x,mat,mat); 
  END; 
END NormCMatrix;
 
(****************************************************)
(***        VOLGT HIER EEN STUK UIT SPINNER       ***)
(****************************************************)

(*
PROCEDURE MakeRotation; 

VAR 
  i:INTEGER;
  coh: CmatRec; 
  name: naam; 
BEGIN 
  MakeEmpty(Nstates,coh.mat);
  FOR i:=1 TO Nspmax DO
    coh.isp[i]:=0;
  END;
  EraseScreen; 
  WriteString( "Construction of rotation-file"); 
  WriteLn; 
  WriteLn; 
  ReadCoherenceFile(coh, name); 
  WriteString( "Duration (in SECONDS) of the rotation = "); 
  READLNREAL(coh.dur);
  WriteLn;
  ExpMatrix(Nstates,coh.mat, coh.dur, coh.mat); 
  WriteRotFile(coh); 
END MakeRotation; 

PROCEDURE MultiplyRotations; 

VAR 
    answer: CHAR; 
    rot1, rot2: CmatRec; 
    save1, save2: naam; 
    save: naam; 
    time: REAL; 
BEGIN 
  EraseScreen; 
  WriteString( "MULTIPLICATION of ROTATIONS"); 
  WriteLn; 
  WriteString( "FIRST rotation"); 
  WriteLn; 
  ReadRotFile(rot1, save1); 
  time := rot1.dur; 
  save := save1; 
  answer := "z"; 
  REPEAT 
    WriteString( "NEXT rotation "); 
    WriteLn; 
    ReadRotFile(rot2, save2);
    dum1:=rot2.mat;
    dum2:=rot1.mat;
    dum3:=rot1.mat;
    Cmatmult(Nstates);
    rot1.mat:=dum3;
    Concat(save2, "*", save, ConcatSucc); 
    Concat(save, save, save, ConcatSucc); 
    time := time+rot2.dur; 
    WriteString( save); 
    WriteLn; 
    WriteString( "'e' = exit, any other key continues multiplication..."); 
    Read(answer);
    WriteLn;
  UNTIL answer = "e"; 
  rot1.inf := save; 
  rot1.dur := time; 
  WriteRotFile(rot1); 
END MultiplyRotations; 
*)

PROCEDURE DoRotation(n:INTEGER;romat:CMatrix;VAR densmat:CMatrix); 
VAR Aromat: CMatrix;   
BEGIN 
  Adjungate(n,romat,Aromat); 
  Transform(n,Aromat,densmat,romat,densmat); 
END DoRotation; 

(*
PROCEDURE EigenValVec; 

VAR 
    answ: CHAR; 
    op: CmatRec; 
    eigvec: Cmatrix; 
    eigval: Diagonal; 
    i: INTEGER; 
    comment: naam; 
    name: naam; 
BEGIN 
  EraseScreen; 
  WriteString( "EigenValues & EigenVectors of an operator-matrix:  "); 
  WriteLn; 
  WriteString( "Read operator from file ? (y/n) "); 
  Read( answ);
  WriteLn;
  IF answ = "y" THEN 
    ReadCoherenceFile(op, name)
  ELSE 
    DefineCoherence(op.mat, op.inf)
  END; 
  hermit(Nstates, op.mat, eigvec, eigval); 
  WriteString( "Eigen-Values:  "); 
  WriteLn; 
  WriteLn; 
  FOR i := 1 TO Nstates DO 
    WriteReal( eigval[i],tp,dp); 
    WriteLn
  END; 
  pause; 
  Concat("EigVctrs ", op.inf, comment, ConcatSucc); 
  showmatrix(eigvec, comment); 
END EigenValVec; 
*)

PROCEDURE DoSpectrum(n:INTEGER;densmat,Hmat,cohmat:CMatrix; 
                     VAR csp,ssp:TwoDiagonal; VAR Nc,Ns:INTEGER);
VAR 
    ii, jj, k, is, ic, r, c: INTEGER; 
    Eigval: Diagonal; 
    sto, Rot, RotA, sig: CMatrix; 
    graphchoice: CHAR; 
BEGIN 
  FOR ii:=1 TO Nstmax DO
    FOR jj:=1 TO Nstmax DO
      sto[ii,jj,1]:=0.0;
      sto[ii,jj,2]:=0.0;
      Rot[ii,jj,1]:=0.0;
      Rot[ii,jj,2]:=0.0;
      RotA[ii,jj,1]:=0.0;
      RotA[ii,jj,2]:=0.0;
      sig[ii,jj,1]:=0.0;
      sig[ii,jj,2]:=0.0;
    END;
    Eigval[ii]:=0.0;
  END;
  FOR ii:=1 TO 50 DO;
    csp[1,ii]:=0.0;
    csp[2,ii]:=0.0;
    ssp[1,ii]:=0.0;
    ssp[2,ii]:=0.0;
  END;
  hermit(n,Hmat,Rot,Eigval); 
  Adjungate(n,Rot,RotA); 
  Transform(n,RotA,densmat,Rot,sig); 
  Transform(n,RotA,cohmat,Rot,sto); 
  ic := 1; 
  is := 1; 
  FOR r := 1 TO n DO 
    FOR c := 1 TO n DO 
      csp[2,ic] := ABS(Eigval[c]-Eigval[r]); 
      csp[1,ic] := sto[r,c,1]*sig[c,r,1]-sto[r,c,2]*sig[c,r,2]; 
      IF ABS(csp[1,ic]) > 0.0001 THEN 
        INC(ic,1)
      END; 
      ssp[2,is] := Eigval[c]-Eigval[r]; 
      ssp[1,is] := sto[r,c,2]*sig[c,r,1]+sto[r,c,1]*sig[c,r,2]; 
      IF ssp[2, is] < 0.0 THEN 
        ssp[2,is] := (-ssp[2,is]); 
        ssp[1,is] := (-ssp[1,is]); 
      END; 
      IF ABS(ssp[1,is]) > 0.0001 THEN 
        INC(is, 1)
      END; 
    END; 
  END; 
  Nc := ic-1; 
  Ns := is-1; 
  SortSpectrum(Nc,csp); 
  SortSpectrum(Ns,ssp); 
  FOR k := 1 TO Nc DO 
    WriteReal(csp[1,k],18); 
    WriteString(" x COS (2 pi "); 
    csp[2,k]:=csp[2,k]/(2.0*pi);
    WriteReal(csp[2, k],18); 
    WriteString( " t )"); 
    WriteLn
  END; 
  FOR k := 1 TO Ns DO 
    WriteReal(ssp[1,k],18); 
    WriteString(" x SIN (2 pi ");
    ssp[2,k]:=ssp[2,k]/(2.0*pi); 
    WriteReal(ssp[2, k],18); 
    WriteString(" t )"); 
    WriteLn
  END; 
  WriteLn; 
END DoSpectrum; 



PROCEDURE HTRIBK(n: INTEGER; 
                 VAR A, Z: CMatrix; 
                 VAR tau: TwoDiagonal); 
VAR 
   i, j, k, L: INTEGER; 
   S, SI, H: REAL; 
BEGIN 
  FOR k := 1 TO n DO 
    FOR j := 1 TO n DO 
      Z[k,j,2] := -Z[k,j,1]*tau[2,k]; 
      Z[k,j,1] := Z[k,j,1]*tau[1,k]; 
    END; (*j*) 
  END; (*k*) 
  IF (n <> 1) THEN 
    FOR i := 2 TO n DO 
      L := i-1; 
      H := A[i,i,2]; 
      IF (H <> 0.0) THEN 
        FOR j := 1 TO n DO 
          S := 0.0; 
          SI := 0.0; 
          FOR k := 1 TO L DO 
            S := S+A[i,k,1]*Z[k,j,1]-A[i,k,2]*Z[k,j,2]; 
            SI := SI+A[i,k,1]*Z[k,j,2]+A[i,k,2]*Z[k,j,1]; 
          END; 
          S := S/H; 
          SI := SI/H; 
          FOR k := 1 TO L DO 
            Z[k,j,1] := Z[k,j,1]-S*A[i,k,1]-SI*A[i,k,2]; 
            Z[k,j,2] := Z[k,j,2]-SI*A[i,k,1]+S*A[i,k,2]; 
          END; (*k*) 
        END; (*j*) 
      END; (*if H*) 
    END; (*i*) 
  END; (*if n*) 
END HTRIBK; 


PROCEDURE TQL2(n: INTEGER; 
               VAR D, E: Diagonal; 
               VAR Z: CMatrix; 
               ierr: INTEGER); 
VAR 
   MML, M : INTEGER; 
   C, S, G, H, P, R, machep, F, B: REAL; 
   help, test: BOOLEAN; 
   k, i, j, L :INTEGER;

PROCEDURE pr1(VAR j:INTEGER;
              VAR P,R,H,F,C,S,G:REAL;
              VAR D,E:Diagonal;
              L,n,M:INTEGER;
              VAR Z:CMatrix);
VAR i,k:INTEGER;

BEGIN 
      j:=j+1;
      P := (D[L+1]-D[L])/(2.0*E[L]); 
      R := sqrt(P*P+1.0); 
      H := D[L]-E[L]/(P+sign(R, P)); 
      FOR i := L TO n DO 
        D[i] := D[i]-H
      END; 
      F := F+H; 
      P := D[M]; 
      C := 1.0; 
      S := 0.0; 
      FOR i := (M-1) TO L BY -1 DO 
        G := C*E[i]; 
        H := C*P; 
        IF (ABS(P) < ABS(E[i])) THEN 
          C := P/E[i]; 
          R := sqrt(C*C+1.0); 
          E[i+1] := S*E[i]*R; 
          S := 1.0/R; 
          C := C*S; 
        ELSE 
          C := E[i]/P; 
          R := sqrt(C*C+1.0); 
          E[i+1] := S*P*R; 
          S := C/R; 
          C := 1.0/R; 
        END; 
        P := C*D[i]-S*G; 
        D[i+1] := H+S*(C*G+S*D[i]); 
        FOR k := 1 TO n DO 
          H := Z[k,i+1,1]; 
          Z[k,i+1,1] := S*Z[k,i,1]+C*H; 
          Z[k,i,1] := C*Z[k,i,1]-S*H; 
        END; (*k*) 
      END; (*i*)
  E[L] := S*P;
  D[L] := C*P;
END pr1;  

PROCEDURE pr2(n:INTEGER;
              VAR P:REAL;
              VAR Z:CMatrix;
              VAR D:Diagonal);
VAR i,j,k:INTEGER;

BEGIN
  FOR i := 1 TO (n-1) DO 
    k := i; 
    P := D[i]; 
    FOR j := (i+1) TO n DO 
      IF (D[j] < P) THEN 
        k := j; 
        P := D[j]; 
      END; (*if*) 
    END; (*j*) 
    IF (k <> i) THEN 
      D[k] := D[i]; 
      D[i] := P; 
      FOR j := 1 TO n DO 
        P := Z[j,i,1]; 
        Z[j,i,1] := Z[j,k,1]; 
        Z[j,k,1] := P; 
      END; (*j*) 
    END; (*if*) 
  END; (*i*) 
END pr2;


BEGIN 
  machep := 1.0E-18; 
  ierr := 0; 
  IF (n <> 1) THEN
    FOR i := 2 TO n DO 
      E[i-1] := E[i]
    END; 
    F := 0.0; 
    B := 0.0; 
    E[n] := 0.0; 
    FOR L := 1 TO n DO 
      j := 0;
      H := machep*(ABS(D[L])+ABS(E[L])); 
      IF (B < H) THEN 
        B := H
      END; 
      M := L-1; 
      REPEAT 
        INC(M, 1); 
      UNTIL (ABS(E[M]) <= B); 
      IF (M = L) THEN
        D[L] := D[L]+F;
      ELSE pr1(j,P,R,H,F,C,S,G,D,E,L,n,M,Z);
        WHILE ((j<30) AND (ABS(E[L])>B)) DO
          pr1(j,P,R,H,F,C,S,G,D,E,L,n,M,Z);
        END;
        D[L] := D[L]+F;
        IF j=30  THEN
          WriteString('convergence failure TQL2...');
          WriteLn;
          ierr:=L;
          L:=n;
        END;
      END;
    END;
    pr2(n,P,Z,D);
  END;
END TQL2;


PROCEDURE HTRIDI(n: INTEGER; 
                 VAR A: CMatrix; 
                 VAR D, E: Diagonal; 
                 VAR tau: TwoDiagonal); 
VAR 
   tel,L, k, j, i: INTEGER; 
   help1, help2, H, G, F, SI, GI, FI, HH, scale: REAL; 
   E2: Diagonal; 


  PROCEDURE pr1(i:INTEGER;
                VAR E,E2,D:Diagonal;
                VAR HH,scale:REAL;
                VAR A:CMatrix);
  BEGIN
    E[i] := 0.0; 
    E2[i] := 0.0; 
    HH := D[i]; 
    D[i] := A[i,i,1]; 
    A[i,i,1] := HH; 
    A[i,i,2] := scale*scale*H; 
  END pr1;

  PROCEDURE pr3(VAR F,G,GI,H:REAL;
                i,L:INTEGER;
                A:CMatrix;
                VAR E:Diagonal;
                VAR tau:TwoDiagonal);

  VAR j,k:INTEGER;

  BEGIN
    F := 0.0; 
    FOR j := 1 TO L DO 
      G := 0.0; 
      GI := 0.0; 
      FOR k := 1 TO j DO 
        G := G+A[j,k,1]*A[i,k,1]+A[j,k,2]*A[i,k,2]; 
        GI := GI-A[j,k,1]*A[i,k,2]+A[j,k,2]*A[i,k,1]; 
      END; (*k*) 
      IF (L >= (j+1)) THEN 
        FOR k := (j+1) TO L DO 
          G := G+A[k,j,1]*A[i,k,1]-A[k,j,2]*A[i,k,2]; 
          GI := GI-A[k,j,1]*A[i,k,2]-A[k,j,2]*A[i,k,1]; 
        END; (*k*) 
      END; (*if L*) 
      E[j] := G/H; 
      tau[2, j] := GI/H; 
      F := F+E[j]*A[i,j,1]-tau[2,j]*A[i,j,2]; 
    END; (*j*) 
  END pr3;


  PROCEDURE pr4(VAR HH,F,H,G,FI,GI:REAL;
                i,L:INTEGER;
                VAR A:CMatrix;
                VAR E:Diagonal;
                VAR tau:TwoDiagonal);

  VAR k,j:INTEGER;
      help1,help2:REAL;

  BEGIN 
    HH:=F/(H+H);
    FOR j := 1 TO L DO 
      F := A[i,j,1]; 
      G := E[j]-HH*F; 
      E[j] := G; 
      FI := -A[i,j,2]; 
      GI := tau[2,j]-HH*FI; 
      tau[2,j] := -GI; 
      FOR k := 1 TO j DO 
        help1 := A[j,k,1]-F*E[k]-G*A[i,k,1]; 
        help2 := FI*tau[2,k]+GI*A[i,k,2]; 
        A[j,k,1] := help1+help2; 
        IF ABS(A[j,k,1]) < 1.0e-4 THEN
          A[j,k,1]:=0.0;
        END;
        help1 := A[j,k,2]-F*tau[2,k]-G*A[i,k,2]; 
        help2 := FI*E[k]+GI*A[i,k,1]; 
        A[j,k,2] := help1-help2; 
        IF ABS(A[j,k,2]) < 1.0e-4 THEN
          A[j,k,2]:=0.0;
        END;
      END; (*k*) 
    END; (*j*) 
  END pr4;

  PROCEDURE pr5(i,L:INTEGER;
                scale,SI:REAL;
                VAR HH:REAL;
                VAR A:CMatrix;
                VAR tau:TwoDiagonal;
                VAR D:Diagonal);

  VAR k:INTEGER;

  BEGIN
    FOR k := 1 TO L DO 
      A[i,k,1] := scale*A[i,k,1]; 
      A[i,k,2] := scale*A[i,k,2]; 
    END; (*k*) 
    tau[2, L] := -SI; 
    HH := D[i]; 
    D[i] := A[i,i,1]; 
    A[i,i,1] := HH; 
    A[i,i,2] := scale*scale*H; 
  END pr5;

BEGIN 
  FOR i:=1 TO n DO
    tau[1,i]:=0.0;
    tau[2,i]:=0.0;
    D[i]:=0.0;
    E[i]:=0.0;
    E2[i]:=0.0;
  END;  
  tau[1, n] := 1.0; 
  tau[2, n] := 0.0; 
  FOR i := 1 TO n DO 
    D[i] := A[i,i,1];
  END; 
  FOR i := n TO 1 BY -1 DO 
    L := i-1; 
    H := 0.0; 
    scale := 0.0; 
    IF (L < 1) THEN
      pr1(i,E,E2,D,HH,scale,A); 
    ELSE 
      FOR k := 1 TO L DO 
        scale := scale+ABS(A[i,k,1])+ABS(A[i,k,2])
      END; 
      IF (scale = 0.0) THEN
        tau[1,L] := 1.0; 
        tau[2,L] := 0.0;
        pr1(i,E,E2,D,HH,scale,A);
      ELSE
        FOR k := 1 TO L DO 
          A[i,k,1] := A[i,k,1]/scale; 
          A[i,k,2] := A[i,k,2]/scale; 
          H := H+A[i,k,1]*A[i,k,1]+A[i,k,2]*A[i,k,2]; 
        END; (*k*) 
        E2[i] := scale*scale*H; 
        G := sqrt(H); 
        E[i] := scale*G; 
        F := sqrt(A[i,L,1]*A[i,L,1]+A[i,L,2]*A[i,L,2]);
        IF (F = 0.0) THEN
          tau[1,L] := -tau[1,i]; 
          SI := tau[2,i]; 
          A[i,L,1] := G; 
          pr3(F,G,GI,H,i,L,A,E,tau);
          pr4(HH,F,H,G,FI,GI,i,L,A,E,tau);
          pr5(i,L,scale,SI,HH,A,tau,D);
        ELSE 
          tau[1,L]:=(A[i,L,2]*tau[2,i]-A[i,L,1]*tau[1,i])/F; 
          SI:=(A[i,L,1]*tau[2,i]+A[i,L,2]*tau[1,i])/F; 
          H := H+F*G; 
          G := 1.0+G/F; 
          A[i,L,1] := G*A[i,L,1]; 
          A[i,L,2] := G*A[i,L,2]; 
          IF L = 1  THEN
            pr5(i,L,scale,SI,HH,A,tau,D);
          ELSE
            pr3(F,G,GI,H,i,L,A,E,tau);
            pr4(HH,F,H,G,FI,GI,i,L,A,E,tau);
            pr5(i,L,scale,SI,HH,A,tau,D);
          END; (* if L=1 *)
        END; (* if F = 0.0 *)
      END; (* if scale=0 *)
    END; (* if L<1 *);
  END; (* for i *);
END HTRIDI;

PROCEDURE hermit(n : INTEGER;
                 A: CMatrix; 
                 VAR Z: CMatrix; 
                 VAR D: Diagonal); 
VAR 
   tau: TwoDiagonal; 
   i, j, ierr: INTEGER; 
   E: Diagonal; 
BEGIN 
  HTRIDI(n,A,D,E,tau); 
  CMatZero(n,Z,Z); 
  FOR i := 1 TO n DO 
    Z[i,i,1] := 1.0
  END; 
  TQL2(n,D,E,Z,ierr); 
  HTRIBK(n,A,Z,tau); 
END hermit; 

(*
PROCEDURE comparecolumns(Nstates : INTEGER;
                         col1, col2: INTEGER; 
                         VAR mat1, mat2: Cmatrix): BOOLEAN; 
VAR 
    rat: ARRAY [1..128] OF REAL; 
    comp,help1,help2: BOOLEAN; 
    tel,i, j, nrat: INTEGER; 
    VAR comparecolumnsResult: BOOLEAN; 
BEGIN 
  nrat := 1; 
  FOR i := 1 TO Nstates DO 
    IF ABS(mat2[i, col2].re) > 1.E-10 THEN 
      rat[nrat] := (mat1[i, col1].re/mat2[i, col2].re); 
      INC(nrat, 1); 
    END; 
    IF ABS(mat2[i, col2].im) > 1.E-10 THEN 
      rat[nrat] := (mat1[i, col1].im/mat2[i, col2].im); 
      INC(nrat, 1); 
    END; 
  END;         (*i*) 
  DEC(nrat, 1); 
  comp := TRUE; 
  FOR i := 1 TO nrat DO 
    IF ((ABS(rat[i]) > 1.01) OR (ABS(rat[i]) < 0.99)) THEN 
      comp := FALSE;
    END; 
    FOR j := i TO nrat DO
      help1:=ABS(rat[j]) > 1.E-10;
      help2:= rat[i]/rat[j] < FLOAT(0);
      IF ((help1) AND (help2)) THEN
        comp := FALSE;
      END;
    END; 
  END; 
  comparecolumnsResult := comp; 
  RETURN comparecolumnsResult
END comparecolumns; 

*)
    
PROCEDURE Transform(n:INTEGER;
                    Ut,H,U:CMatrix;
                    VAR prod:CMatrix);
VAR store: CMatrix;
BEGIN
  CMatMult(n,Ut,H,store);
  CMatMult(n,store,U,prod);
END Transform; 

PROCEDURE Adjungate(n : INTEGER;
                    P: CMatrix; 
                    VAR Q: CMatrix); 
VAR 
    i, j: INTEGER; 
BEGIN 
  FOR i := 1 TO n DO 
    FOR j := 1 TO n DO 
      Q[i,j,1] := P[j,i,1]; 
      Q[i,j,2] := -P[j,i,2]; 
    END; 
  END; 
END Adjungate; 

PROCEDURE sign(p,q:REAL):REAL;
VAR signResult:REAL;
BEGIN 
  IF (q >= 0.0) THEN
    signResult:=ABS(p);
  ELSE
    signResult:=-(ABS(p));
  END;
  RETURN signResult;
END sign;    
    

PROCEDURE Transpose(n : INTEGER;
                    p: CMatrix; 
                    VAR q: CMatrix); 
VAR 
    i, j: INTEGER; 
BEGIN 
  FOR i := 1 TO n DO 
    FOR j := 1 TO n DO 
      q[i,j,1] := p[j,i,1]; 
      q[i,j,2] := p[j,i,2]; 
    END; 
  END; 
END Transpose; 

PROCEDURE Epower(n : INTEGER;
                 EV: Diagonal; 
                 VAR lamb: CDiagonal; 
                 tau: REAL); 
VAR 
   i: INTEGER; 
BEGIN 
  FOR i := 1 TO n DO 
    lamb[i,1] := cos(EV[i]*tau); 
    lamb[i,2] := sin(EV[i]*tau); 
  END; 
END Epower; 


PROCEDURE ExpMatrix(n: INTEGER;
                    inm: CMatrix; 
                    tau: REAL; 
                    VAR outm: CMatrix); 
VAR 
    Tmat, TmatA: CMatrix; 
    Eigval: Diagonal; 
    lamb: CDiagonal; 
    r: INTEGER; 
BEGIN 
  hermit(n, inm, Tmat, Eigval); 
  Epower(n, Eigval, lamb, tau); 
  Adjungate(n, Tmat, TmatA); 
  CMatZero(n,outm,outm); 
  FOR r := 1 TO n DO 
    outm[r,r,1] := lamb[r,1];
    outm[r,r,2] := lamb[r,2];
  END; 
  Transform(n,Tmat,outm,TmatA,outm); 
END ExpMatrix; 


PROCEDURE SortSpectrum(VAR Npeaks: INTEGER; 
                       VAR spc: TwoDiagonal); 
VAR 
    i, j, k, NewNpeaks: INTEGER; 
    dum: TwoDiagonal; 
    bool1,bool2,present: BOOLEAN; 
    store1, store2: REAL; 
BEGIN 
  IF Npeaks <> 0 THEN 
    dum[1, 1] := spc[1, 1]; 
    dum[2, 1] := spc[2, 1]; 
    NewNpeaks := 1; 
    FOR i := 2 TO Npeaks DO 
      present := FALSE; 
      FOR j := 1 TO NewNpeaks DO 
        bool1:=spc[2, i] <= dum[2, j]+0.05;
        bool2:=spc[2, i] >= dum[2, j]-0.05;
        IF ((bool1) AND (bool2)) THEN 
          dum[1, j] := dum[1, j]+spc[1, i]; 
          present := TRUE; 
        END; (*if*) 
      END; (*j*) 
      IF  NOT present THEN 
        INC(NewNpeaks, 1); 
        dum[1, NewNpeaks] := spc[1, i]; 
        dum[2, NewNpeaks] := spc[2, i]; 
      END; (*if*) 
    END; (*i*) 
    Npeaks := NewNpeaks; 
    spc := dum; 
    FOR k := 1 TO Npeaks DO 
      FOR i := 2 TO (Npeaks-k+1) DO 
        IF (spc[2, i] <= spc[2, i-1]) THEN 
          store1 := spc[1, i]; 
          store2 := spc[2, i]; 
          spc[1, i] := spc[1, i-1]; 
          spc[2, i] := spc[2, i-1]; 
          spc[1, i-1] := store1; 
          spc[2, i-1] := store2; 
        END; (*if*) 
       END; (*i*) 
      END; (*k*) (*sorted for frequency*) 
      (*if npeaks*) 
    ELSE 
  END; 
END SortSpectrum; 



END MatrixMathLib.
