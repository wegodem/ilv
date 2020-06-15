IMPLEMENTATION MODULE SpinOperatorLib;

FROM MathLib0 IMPORT sqrt;

FROM MatrixMathLib IMPORT CMatCommutator,CMatAdd,CMatMultAdd,CMatRMultAdd,
                          CMatMult,CMatZero,ShowCMatElements,CopyCMatrix,
                          RMultCMat,CMultCMat,ExpectationValue,CalcTrace,
                          ExpMatrix,DoRotation,NormCMatrix,CMatrix,
                          COMPLEXLONGREAL,DoSpectrum,TwoDiagonal;
                
PROCEDURE MakeKets(VAR ket:produktket; Ilist:Iarray);
VAR a,m:Iarray;
    i,j:INTEGER;

  PROCEDURE ketproc(VAR ket:produktket;VAR j:INTEGER;
                    VAR m:Iarray);
  VAR k:INTEGER;
  BEGIN
    k:=1;
    WHILE k<=Nspmax DO
      ket[j,k]:=m[k];
      k:=k+1;
    END;
    j:=j+1;
  END ketproc;

  PROCEDURE forproc(VAR ket:produktket; Ilist:Iarray;
                    VAR a,m:Iarray; VAR i,j:INTEGER);
  VAR ii,iplus:INTEGER;
  BEGIN
    m[i]:=Ilist[i];
    ii:=a[i];
    FOR ii:=Ilist[i] TO 0 BY -1 DO
      a[i]:=ii;
      IF i=Nspmax THEN 
        ketproc(ket,j,m);
      ELSE 
        iplus:=i+1;
        forproc(ket,Ilist,a,m,iplus,j);
      END;
      m[i]:=m[i]-2;
    END;
  END forproc;
BEGIN
  i:=1;
  j:=1;
  forproc(ket,Ilist,a,m,i,j);
END MakeKets;

PROCEDURE operateIplus(Ilist:Iarray;
                       Xstate, Xspin: INTEGER; 
                       inket: produktket; 
                       VAR outket: produktket; 
                       VAR coefficient: REAL); 
VAR 
   i: INTEGER; 
BEGIN 
  FOR i := 1 TO Nspmax DO 
    outket[Xstate,i] := inket[Xstate,i]
  END; 
  outket[Xstate, Xspin] := inket[Xstate, Xspin]+2; 
  coefficient := FLOAT(Ilist[Xspin]-inket[Xstate,Xspin])/FLOAT(2); 
  coefficient := coefficient*FLOAT((Ilist[Xspin]+inket[Xstate,Xspin]+2))/FLOAT(2); 
  coefficient := sqrt(coefficient); 
END operateIplus; 


PROCEDURE InproductIplus(Ilist : Iarray;
                         VAR ket: produktket;
                         Brastate, Ketstate, Xspin: INTEGER; 
                         VAR element1,element2:REAL); 
VAR 
   coeff: REAL; 
   i: INTEGER; 
   newket: produktket; 
   orthogonal: BOOLEAN; 
BEGIN 
  operateIplus(Ilist,Ketstate, Xspin, ket, newket, coeff); 
  orthogonal := FALSE; 
  FOR i := 1 TO Nspmax DO 
    IF (ket[Brastate, i] <> newket[Ketstate, i]) THEN 
      orthogonal := TRUE
    END
  END; 
  IF orthogonal = FALSE THEN 
    element1 := coeff; 
    element2 := 0.0; 
  ELSE 
    element1 := 0.0; 
    element2 := 0.0; 
  END; 
END InproductIplus; 

PROCEDURE MakeMatrixIplus(Ilist : Iarray;
                          VAR ket: produktket;
                          n : INTEGER;
                          Xspin: INTEGER; 
                          VAR matrixIp: CMatrix); 
VAR 
   k,l: INTEGER; 
BEGIN 
  FOR k := 1 TO n DO 
    FOR l := 1 TO n DO 
      InproductIplus(Ilist,ket,k, l, Xspin, matrixIp[k,l,1],matrixIp[k,l,2]); 
    END; 
  END; 
END MakeMatrixIplus; 

PROCEDURE operateImin(Ilist: Iarray;
                      Xstate, Xspin: INTEGER; 
                      inket: produktket; 
                      VAR outket: produktket; 
                      VAR coefficient: REAL); 
VAR 
    i: INTEGER; 
BEGIN 
  FOR i := 1 TO Nspmax DO 
    outket[Xstate, i] := inket[Xstate, i]
   END; 
   outket[Xstate, Xspin] := inket[Xstate, Xspin]-2; 
   coefficient := FLOAT(Ilist[Xspin]+inket[Xstate, Xspin])/FLOAT(2); 
   coefficient := coefficient*FLOAT((Ilist[Xspin]-inket[Xstate, Xspin]+2))/FLOAT(2); 
   coefficient := sqrt(coefficient); 
END operateImin; 


PROCEDURE inproductImin(Ilist: Iarray;
                        VAR ket: produktket;
                        Brastate, Ketstate, Xspin: INTEGER; 
                        VAR element1,element2:REAL); 
VAR 
    coeff: REAL; 
    i: INTEGER; 
    newket: produktket; 
    orthogonal: BOOLEAN; 
BEGIN 
  operateImin(Ilist,Ketstate, Xspin, ket, newket, coeff); 
  orthogonal := FALSE; 
  FOR i := 1 TO Nspmax DO 
    IF (ket[Brastate, i] <> newket[Ketstate, i]) THEN 
      orthogonal := TRUE
    END
  END; 
  IF orthogonal = FALSE THEN 
    element1 := coeff; 
    element2 := 0.0; 
  ELSE 
    element1 := 0.0; 
    element2 := 0.0; 
  END; 
END inproductImin; 


PROCEDURE MakeMatrixImin(Ilist: Iarray;
                         VAR ket: produktket;
                         n : INTEGER;
                         Xspin: INTEGER; 
                         VAR matrixIm: CMatrix); 
VAR 
    k,l: INTEGER; 
BEGIN 
  FOR k := 1 TO n DO 
    FOR l := 1 TO n DO 
      inproductImin(Ilist,ket,k,l,Xspin,matrixIm[k,l,1],matrixIm[k,l,2]); 
    END; 
  END; 
END MakeMatrixImin; 


PROCEDURE MakeMatrixIx(Ilist: Iarray;
                       VAR ket: produktket;
                       n : INTEGER;
                       Xspin: INTEGER; 
                       VAR matIx: CMatrix); 
VAR 
    row, col: INTEGER; 
    plus, min: CMatrix; 
BEGIN 
  MakeMatrixIplus(Ilist,ket,n, Xspin, plus); 
  MakeMatrixImin(Ilist,ket,n, Xspin, min); 
  FOR row := 1 TO n DO 
    FOR col := 1 TO n DO 
      matIx[row, col,1] := (plus[row, col,1]+min[row, col,1])/FLOAT(2); 
      matIx[row, col,2] := 0.0; 
    END; 
  END; 
END MakeMatrixIx; 


PROCEDURE MakeMatrixIy(Ilist : Iarray;
                       VAR ket: produktket;
                       n : INTEGER;
                       Xspin: INTEGER; 
                       VAR matIy: CMatrix); 
VAR 
    row, col: INTEGER; 
    plus, min: CMatrix; 
BEGIN 
  MakeMatrixIplus(Ilist,ket,n, Xspin, plus); 
  MakeMatrixImin(Ilist,ket,n, Xspin, min); 
  FOR row := 1 TO n DO 
    FOR col := 1 TO n DO 
      matIy[row, col,2] := (min[row, col,1]-plus[row, col,1])/FLOAT(2); 
      matIy[row, col,1] := 0.0; 
    END; 
  END; 
END MakeMatrixIy; 

 
PROCEDURE MakeMatrixIz(Ilist:Iarray;
                       VAR ket:produktket;n:INTEGER;
                       Xspin:INTEGER;
                       VAR matIz:CMatrix);
VAR plus:CMatrix;
BEGIN
  MakeMatrixIplus(Ilist,ket,n,Xspin,plus);
  MakeMatrixImin(Ilist,ket,n, Xspin, matIz);
  CMatCommutator(n,plus,matIz,matIz);
  RMultCMat(n,0.5,matIz,matIz);
END MakeMatrixIz;

PROCEDURE MakeTotalIx(n,Nspins:INTEGER;ket:produktket;
                      Ilist:Iarray; Norm:BOOLEAN;
                      VAR Ixtot:CMatrix); 
VAR i: INTEGER; 
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
    CMatAdd(n,store,Ixtot,Ixtot); 
  END;
END MakeTotalIx; 

PROCEDURE MakeTotalIy(n,Nspins:INTEGER;ket:produktket;
                      Ilist:Iarray; Norm:BOOLEAN;
                      VAR Iytot:CMatrix); 
VAR i: INTEGER; 
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
    CMatAdd(n,store,Iytot,Iytot); 
  END;
END MakeTotalIy; 

PROCEDURE MakeTotalIz(n,Nspins:INTEGER;ket:produktket;
                      Ilist:Iarray; Norm:BOOLEAN;
                      VAR Iztot:CMatrix); 
VAR i: INTEGER; 
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
    CMatAdd(n,store,Iztot,Iztot); 
  END;
END MakeTotalIz;

END SpinOperatorLib.
