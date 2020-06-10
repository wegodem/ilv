IMPLEMENTATION MODULE GradLib;
FROM InOut IMPORT WriteString,WriteLn,ReadInt;
FROM RealInOut IMPORT ReadReal;
FROM MathLib0 IMPORT cos,sin,exp;

CONST pi=3.1415926535897932;



PROCEDURE CalcGrad(ap:INTEGER;h:REAL; VAR gs:ARRAY OF REAL);
VAR choice:INTEGER;
BEGIN  
  WriteString("Possible gradient modulation types:");WriteLn;
  WriteString("   1.  Constant gradient ");WriteLn;
  WriteString("   2.  cosine spiral (2D excitation)");WriteLn; 
  WriteString("   3.  sine spiral   (2D excitation)");WriteLn;
  WriteString("U kiest [1..3] : ");ReadInt(choice);WriteLn;
  CASE choice OF
            1: gradproc1(ap,h,gs);|
            2: gradproc2(ap,h,gs);|
            3: gradproc3(ap,h,gs);
  END;
END CalcGrad;

PROCEDURE gradproc2(ap:INTEGER; h:REAL; VAR gs :ARRAY OF REAL);
VAR a,n,tu,j,td,k:REAL;
    i:INTEGER;
BEGIN
  WriteString("g(t) = -(A/T)*(2*pi*n*(1-(t/T))*((sin(2*pi*n*t/T)+f(t))");
  WriteLn;
  WriteString("f(t) = cos(2*pi*n*t/T)");WriteLn;
  WriteString("A    : ");ReadReal(a);WriteLn;
  WriteString("n    : ");ReadReal(n);WriteLn;
  td:=FLOAT(ap)*h;
  FOR i:= 0 TO ap-1 DO
    j:=FLOAT(i);
    k:=j*h;
    tu:=2.0*pi*n*(1.0-(k/td))*sin(2.0*pi*n*k/td)+cos(2.0*pi*n*k/td);
    gs[i]:=-a*tu/td;
  END;
END gradproc2;

PROCEDURE gradproc3(ap:INTEGER; h:REAL; VAR gs: ARRAY OF REAL);
VAR a,n,tu,td,j,k:REAL;
    i:INTEGER;
BEGIN
  WriteString("g(t) = (A/T)*(2*pi*n*(1-(t/T))*((cos(2*pi*n*t/T)+f(t))");
  WriteLn;
  WriteString("f(t) = -sin(2*pi*n*t/T)");WriteLn;
  WriteString("A : ");ReadReal(a);WriteLn;
  WriteString("n : ");ReadReal(n);WriteLn;
  td:=FLOAT(ap)*h; 
  FOR i:= 0 TO ap-1 DO
    j:=FLOAT(i);
    k:=j*h;
    tu:=2.0*pi*n*(1.0-(k/td))*cos(2.0*pi*n*k/td)-sin(2.0*pi*n*k/td);
    gs[i]:=a*tu/td;
  END;
END gradproc3;

PROCEDURE gradproc1(ap:INTEGER; h:REAL; VAR gs:ARRAY OF REAL);
VAR g1:REAL;
     i:INTEGER;
BEGIN
  WriteString(" Constant gradient strength [Hz/cm] : ");
  ReadReal(g1);WriteLn;
  FOR i:=0 TO ap-1 DO
    gs[i]:=2.0*pi*g1;
  END;
END gradproc1; 

END GradLib.
