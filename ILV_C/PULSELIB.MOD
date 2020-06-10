IMPLEMENTATION MODULE PulseLib;

FROM MathLib0 IMPORT sqrt,cos,sin,exp,ln;
FROM InOut IMPORT WriteString,WriteLn,ReadInt,OpenInput,CloseInput; 
FROM RealInOut IMPORT ReadReal;
FROM Graphics IMPORT ScreenMode;

CONST pi= 3.1415926535897832;
VAR choice:INTEGER;

(*************************************************************************)
(****      THIS LIBRARY MODULE CALCULATES A FEW COMMON RF-PULSES      ****)
(****      Author        : H.S.                                       ****)
(****      Date          : october 1990                               ****)
(****      System        : SUN MODULA2                                ****)
(****      Last change   : 01-01-1993 BY HS /2D sinc/Gauss off centre ****)
(*************************************************************************)


PROCEDURE AMPulseDefinition(ap:INTEGER;ti:REAL;
                            VAR xb,yb:ARRAY OF REAL);
VAR choice:INTEGER;
BEGIN
  WriteString('What Amplitude Modulated Pulse-shape is to be used');
  WriteLn;WriteLn;
  WriteString('   1. rectangular or hard-pulse');WriteLn;
  WriteString('   2. sinc-Gauss pulse');WriteLn;
  WriteString('   3. 2D - GauB pulse');WriteLn;
  WriteString('   4. 2D - sinc-Pulse (centre (x,y) = (0,0)');WriteLn;WriteLn;
  WriteString('Your Choice is [1..4] : ');ReadInt(choice);WriteLn;
  CASE choice OF 1: ampulseproc1(ap,ti,xb,yb);|
                 2: ampulseproc2(ap,ti,xb,yb);|
                 3: ampulseproc3(ap,ti,xb,yb);|
                 4: ampulseproc4(ap,ti,xb,yb);|
  END;
END AMPulseDefinition;

PROCEDURE AMHardPulseDefinition(ap:INTEGER;ti:REAL;
                                VAR xb,yb:ARRAY OF REAL);
VAR choice:INTEGER;
    fi,amp:REAL;
BEGIN
  WriteLn;
  amp:=1.0;
  WriteString('Standard Hard Pulses:');
  WriteLn;WriteLn;
  WriteString("   1. (pi/2) (x')            5. (pi) (x')");WriteLn;
  WriteString("   2. (pi/2) (y')            6. (pi) (y')");WriteLn;
  WriteString("   3. (pi/2) (-x')           7. (pi) (-x')");WriteLn;
  WriteString("   4. (pi/2) (-y')           8. (pi) (-y')");WriteLn;WriteLn;
  WriteString('Your choice is [1..8] : ');ReadInt(choice);WriteLn;
  IF choice > 4 THEN 
    amp:=2.0;
  END;  	
  CASE choice OF 
          1: fi:=0.0;
             hardpulseproc(ap,fi,amp,xb,yb);|
          2: fi:=pi/2.0;
             hardpulseproc(ap,fi,amp,xb,yb);|
          3: fi:=pi;
             hardpulseproc(ap,fi,amp,xb,yb);|
          4: fi:=-pi/2.0; 
             hardpulseproc(ap,fi,amp,xb,yb);|
          5: fi:=0.0;
             hardpulseproc(ap,fi,amp,xb,yb);|
          6: fi:=pi/2.0;
             hardpulseproc(ap,fi,amp,xb,yb);|
          7: fi:=pi;
             hardpulseproc(ap,fi,amp,xb,yb);|
          8: fi:=-pi/2.0;
             hardpulseproc(ap,fi,amp,xb,yb);
  END;
END AMHardPulseDefinition;

PROCEDURE FMPulseDefinition(ap:INTEGER;ti:REAL;
                            VAR xb,yb:ARRAY OF REAL);
VAR choice:INTEGER;
BEGIN
  WriteString('What Frequency Modulated Pulse-shape is to be used');
  WriteLn;WriteLn;
  WriteString('      1. 180 sech-tanh (-T,T)');WriteLn;
  WriteString('      2. 90 sech-tanh (-T,0)');WriteLn;
  WriteString('      3. 360 sech-tanh 2*(-T,T)');WriteLn;
  WriteString('      4. 2D - sinc-Pulses off centre ');WriteLn;
  WriteString('     5. Sum of off centre 2D-sinc-Pulses ');WriteLn;
  WriteString('     6. Sum of off centre 2D-sinc-Pulses using file parameters');
  WriteLn;WriteLn;
  WriteString('Your choice is [1..6] : ');ReadInt(choice);WriteLn;
  CASE choice OF 
                 1: fmpulseproc1(ap,ti,xb,yb);|
                 2: fmpulseproc2(ap,ti,xb,yb);|
                 3: fmpulseproc3(ap,ti,xb,yb);|
                 4: fmpulseproc4(ap,ti,xb,yb);|
                 5: fmpulseproc5(ap,ti,xb,yb);|
                 6: fmpulseproc6(ap,ti,xb,yb);
                 
  END;
END FMPulseDefinition;

PROCEDURE hardpulseproc(ap:INTEGER;fi,amp:REAL;
                        VAR xb,yb:ARRAY OF REAL);
VAR b1:REAL;
BEGIN
  b1:=25000.0*2.0*pi;(*B1-field strength is 25kHz*)
  xb[0]:=b1*cos(fi)*amp;
  yb[0]:=b1*sin(fi)*amp;
END hardpulseproc;

PROCEDURE ampulseproc1(ap:INTEGER;ti:REAL;
                       VAR xb,yb: ARRAY OF REAL);
VAR r,fi:REAL;
   ww:INTEGER;
BEGIN
  WriteString(' Rectangular pulse or hard pulse');WriteLn;
  WriteString(' pulse-phase (in degrees) : ');ReadReal(fi);
  WriteLn;
  WriteString(' pulse amplitude [Hz]     : ');ReadReal(r);
  WriteLn;
  r:=r*2.0*pi;
  FOR ww:= 0 TO ap-1 DO
    xb[ww]:=r*cos(pi*fi/180.0);
    yb[ww]:=r*sin(pi*fi/180.0);
  END;
END ampulseproc1;


PROCEDURE ampulseproc2(ap:INTEGER;ti:REAL;  
                       VAR xb,yb: ARRAY OF REAL);
VAR ww:INTEGER;
    fi,b,f,q,j,z,k,l:REAL;
BEGIN
  WriteString(' sinc Gauss - AM-pulse shape :');WriteLn;
  WriteLn;
  WriteString(' q.sin(2.pi.f.t)*exp(-b.b.t.t)/pi.t ');WriteLn;
  WriteString(' pulse-phase (in degrees) : ');ReadReal(fi);WriteLn;
  WriteString(' q  (teta = q*180/pi)     : ');ReadReal(q);WriteLn;
  WriteString(' f (pulse bandwidth)      : ');ReadReal(f);WriteLn;
  WriteString(' b (damping constant)     : ');ReadReal(b);WriteLn;
  f:=f*2.0*pi;b:=b*b;
  FOR ww:=0 TO ap-1 DO
    j:= FLOAT(ww)-(FLOAT( ap)/2.0);
    z:=j+0.5;
    IF j=0.0 THEN j:=0.000000001;END;
    IF z=0.0 THEN z:=0.000000001;END;
    k:=j*ti;
    xb[ww]:=q*sin(f*k)*exp(-b*k*k)*cos(pi*fi/180.0)/(pi*k);
    yb[ww]:=q*sin(f*k)*exp(-b*k*k)*sin(pi*fi/180.0)/(pi*k);
  END;
END ampulseproc2;


PROCEDURE fmpulseproc1(ap:INTEGER;ti:REAL;
                       VAR xb,yb :ARRAY OF REAL);
VAR ww:INTEGER;
    fi,b,w,u,z,j,k,l,ex:REAL;
BEGIN
  WriteString(' 180 degree FM sech-tanh pulse (-T,T) :');WriteLn;
  WriteLn;
  WriteString(' x  : w*sech(b*t)*cos(2*pi*u*ln(sech(bt))) ');WriteLn;
  WriteString(' y  : w*sech(b*t)*sin(2*pi*u*ln(sech(bt))) ');WriteLn;
  WriteString(' pulse-phase (in degrees) : ');ReadReal(fi);WriteLn;
  WriteString(' w  [Hz]                  : ');ReadReal(w);WriteLn;w:=w*2.0*pi;
  WriteString(' b (damping constant)     : ');ReadReal(b);WriteLn;
  WriteString(' u (selectivity constant) : ');ReadReal(u);WriteLn;
  fi:=pi*fi/180.0;
  FOR ww:=0 TO ap-1 DO
    j:= FLOAT(ww)-(FLOAT( ap)/2.0); 
    z:=j+0.5;
    k:=j*ti;
    ex:=2.0/(exp(b*k)+exp(-b*k));
    xb[ww]:=w*ex*cos(2.0*pi*u*ln(ex)+fi);
    yb[ww]:=w*ex*sin(2.0*pi*u*ln(ex)+fi);
  END;
END fmpulseproc1;

PROCEDURE fmpulseproc2(ap:INTEGER; ti: REAL;
                       VAR xb,yb: ARRAY OF REAL);
VAR ww:INTEGER;
    fi,b,w,u,ex,j,l,z,k:REAL;
BEGIN
  WriteString(' 90 degree FM sech-tanh pulse (-T,0) :');WriteLn;
  WriteLn;
  WriteString(' x : w*sech(b*t)*cos(2*pi*u*ln(sech(bt))) ');WriteLn;
  WriteString(' y : w*sech(b*t)*sin(2*pi*u*ln(sech(bt))) ');WriteLn;
  WriteString(' pulse-phase (in degrees) : ');ReadReal(fi);WriteLn;
  WriteString(' w  [Hz]                  : ');ReadReal(w);WriteLn;w:=w*2.0*pi;
  WriteString(' b (damping constant)     : ');ReadReal(b);WriteLn;
  WriteString(' u (selectivity constant) : ');ReadReal(u);WriteLn;
  fi:=pi*fi/180.0;
  FOR ww:=0 TO ap-1 DO
    j:= FLOAT(ww)-FLOAT(ap); 
    z:=j+0.5;
    k:=j*ti;
    ex:=2.0/(exp(b*k)+exp(-b*k)); 
    xb[ww]:=w*ex*cos(2.0*pi*u*ln(ex)+fi);
    yb[ww]:=w*ex*sin(2.0*pi*u*ln(ex)+fi);
  END;
END fmpulseproc2;

PROCEDURE fmpulseproc3(ap:INTEGER;ti:REAL;
                       VAR xb,yb :ARRAY OF REAL);
VAR p,ww:INTEGER;
    fi,b,w,u,z,j,k,l,ex:REAL;
BEGIN
  WriteString(' 360 degree FM sech-tanh pulse 2*(-T,T) :');WriteLn;
  WriteLn;
  WriteString(' x  : w*sech(b*t)*cos(2*pi*u*ln(sech(bt))) ');WriteLn;
  WriteString(' y  : w*sech(b*t)*sin(2*pi*u*ln(sech(bt))) ');WriteLn;WriteLn;
  WriteString(' pulse-phase (in degrees)  : ');ReadReal(fi);WriteLn;
  WriteString(' w                    [Hz] : ');ReadReal(w);WriteLn;w:=w*2.0*pi;
  WriteString(' b (damping constant)      : ');ReadReal(b);WriteLn;
  WriteString(' u (selectivity constant)  : ');ReadReal(u);WriteLn;
  fi:=pi*fi/180.0;
  p:=(ap DIV 2);
  FOR ww:=0 TO p-1 DO
    j:= FLOAT(ww)-(FLOAT(p)/2.0); 
    z:=j+0.5;
    k:=j*ti/2.0;
    ex:=2.0/(exp(b*k)+exp(-b*k));
    xb[ww]:=w*ex*cos(2.0*pi*u*ln(ex)+fi);
    yb[ww]:=w*ex*sin(2.0*pi*u*ln(ex)+fi);
  END;
  FOR ww:=0 TO p-1 DO
    xb[ww+p]:=xb[ww];
    yb[ww+p]:=yb[ww];
  END;      
END fmpulseproc3;

PROCEDURE ampulseproc3(ap:INTEGER;h:REAL;VAR xb,yb:ARRAY OF REAL);
VAR alfa,a,b,n,td,k,l,j,tu:REAL;
    i:INTEGER;
BEGIN
  WriteString("bx(t) = (a.A/T)*exp(-(B*(1-t/T))^2)*sqrt(f(t))  -");
  WriteLn;
  WriteString("With : f(t) = ((2*pi*n(1-(t/T))^2)+1");
  WriteLn;WriteLn;
  WriteString("a   : ");ReadReal(alfa);WriteLn;
  WriteString("A   : ");ReadReal(a);WriteLn;
  WriteString("B   : ");ReadReal(b);WriteLn;
  WriteString("n   : ");ReadReal(n);WriteLn;
  td:=FLOAT(ap)*h;
  FOR i:= 0 TO ap-1 DO
    j:=FLOAT(i);
    k:=j*h;
    tu:=(1.0-(k/td))*(1.0-(k/td));
    xb[i]:=alfa*a*exp(-b*b*tu)*sqrt(4.0*pi*pi*n*n*tu+1.0)/td;
    yb[i]:=0.0;
  END;
END ampulseproc3;

PROCEDURE ampulseproc4(ap:INTEGER;h:REAL;VAR xb,yb:ARRAY OF REAL);
VAR alfa,a,b,n,td,k,l,j,tu,tu1,tu2,dx,dy,kx,ky,beta:REAL;
    i:INTEGER;
BEGIN
  WriteString("bx(t) = (a.A/T)*sinc(kx(t).dx/2)*sinc(ky(t).dy/2).f(t)");
  WriteLn;
  WriteString("f(t) = sqrt((2.pi.n.(1-i(t/T)))^2 + 1)");
  WriteLn;
  WriteString("kx(t) = B.(1-(t/T)).cos(2.pi.n.t/T)");
  WriteLn;
  WriteString("ky(t) = B.(1-(t/T)).sin(2.pi.n.t/T)");
  WriteLn;
  WriteString("a     : ");ReadReal(alfa);WriteLn;
  WriteString("A     : ");ReadReal(a);WriteLn;
  WriteString("B     : ");ReadReal(beta);WriteLn;
  WriteString("n     : ");ReadReal(n);WriteLn;
  WriteString("dx    : ");ReadReal(dx);WriteLn;
  WriteString("dy    : ");ReadReal(dy);WriteLn;
  td:=FLOAT(ap)*h;
  FOR i:= 0 TO ap-1 DO
    j:=FLOAT(i);
    k:=j*h;
    kx:=beta*(1.0-(k/td))*cos(2.0*pi*n*k/td);
    ky:=beta*(1.0-(k/td))*sin(2.0*pi*n*k/td);
    IF kx=0.0 THEN
      kx:=1.0e-5;
    END;
    IF ky=0.0 THEN
      ky:=1.0e-5
    END;
    tu:=(1.0-(k/td))*(1.0-(k/td));
    tu1:=sin(kx*dx/2.0)/(kx*dx/2.0);
    tu2:=sin(ky*dy/2.0)/(ky*dy/2.0);
    xb[i]:=alfa*a*tu1*tu2*sqrt(4.0*pi*pi*n*n*tu+1.0)/td;
    yb[i]:=0.0;
  END;
END ampulseproc4;


PROCEDURE fmpulseproc4(ap:INTEGER;h:REAL;VAR xb,yb:ARRAY OF REAL);
VAR alfa,a,b,n,td,k,l,j,tu,tu1,tu2,tu3,tu4,tu5,tu6,
    dx,dy,x0,y0,kx,ky,beta:REAL;
    i:INTEGER;
BEGIN
  ScreenMode(3);
  WriteString(" A 2D sinc pulse which excites a rectangular area");WriteLn;
  WriteString(" with a shifted centre from (0,0) to (x0,y0)");
  WriteLn;WriteLn;
  WriteString(" Is given by the complex pulse : ");WriteLn;
  WriteString("b1(t) = (a.A/T)*sinc(kx(t).dx/2)*sinc(ky(t).dy/2)");WriteLn;
  WriteString("                      * exp(i kx(t).x0).exp(i ky(t).y0).f(t)");
  WriteLn;
  WriteString("f(t) = sqrt((2.pi.n.(1-i(t/T)))^2 + 1)");
  WriteLn;
  WriteString("kx(t) = B.(1-(t/T)).cos(2.pi.n.t/T)");
  WriteLn;
  WriteString("ky(t) = B.(1-(t/T)).sin(2.pi.n.t/T)");
  WriteLn;
  WriteString("a     : ");ReadReal(alfa);WriteLn;
  WriteString("A     : ");ReadReal(a);WriteLn;
  WriteString("B     : ");ReadReal(beta);WriteLn;
  WriteString("n     : ");ReadReal(n);WriteLn;
  WriteString("dx    : ");ReadReal(dx);WriteLn;
  WriteString("dy    : ");ReadReal(dy);WriteLn;
  WriteString("x0    : ");ReadReal(x0);WriteLn;
  WriteString("y0    : ");ReadReal(y0);WriteLn;
  td:=FLOAT(ap)*h;
  FOR i:= 0 TO ap-1 DO
    j:=FLOAT(i);
    k:=j*h;
    kx:=beta*(1.0-(k/td))*cos(2.0*pi*n*k/td);
    ky:=beta*(1.0-(k/td))*sin(2.0*pi*n*k/td);
    IF kx=0.0 THEN
      kx:=1.0e-5;
    END;
    IF ky=0.0 THEN
      ky:=1.0e-5;
    END;
    tu:=(1.0-(k/td))*(1.0-(k/td));
    tu1:=sin(kx*dx/2.0)/(kx*dx/2.0);
    tu2:=sin(ky*dy/2.0)/(ky*dy/2.0);
    tu3:=cos(kx*x0);    tu4:=sin(kx*x0);
    tu5:=cos(ky*y0);    tu6:=sin(ky*y0);
    xb[i]:=alfa*a*tu1*tu2*(tu3*tu5-tu4*tu6)*sqrt(4.0*pi*pi*n*n*tu+1.0)/td;
    yb[i]:=alfa*a*tu1*tu2*(tu3*tu6+tu4*tu5)*sqrt(4.0*pi*pi*n*n*tu+1.0)/td;
  END;
END fmpulseproc4;

PROCEDURE fmpulseproc5(ap:INTEGER;h:REAL;VAR xb,yb:ARRAY OF REAL);
VAR i:INTEGER;
    dumx,dumy:ARRAY[0..395] OF REAL;
BEGIN
  ScreenMode(3);
  WriteString("*************************************************************");WriteLn;
  WriteString("** A sum of 2D sinc pulse which excite a rectangular areas **");WriteLn;
  WriteString("**      with a shifted centres : (0,0) to (x0,y0)          **");WriteLn;
  WriteString("*************************************************************");WriteLn;
  WriteLn;WriteLn;
  FOR i:=0 TO ap-1 DO 
    xb[i]:=0.0;yb[i]:=0.0;
  END;
  LOOP
    WriteString('    1. Add new 2D pulse  ');WriteLn;
    WriteString('    2. Exit              ');WriteLn;
    WriteString('Your choice [1,2] : ');ReadInt(choice);
    IF choice#1 THEN EXIT END;
    fmpulseproc4(ap,h,dumx,dumy);
    FOR i:=0 TO ap-1 DO
      xb[i]:=xb[i]+dumx[i];
      yb[i]:=yb[i]+dumy[i];
    END;  
  END;
END fmpulseproc5;



PROCEDURE fmpulseproc6(ap:INTEGER;h:REAL;VAR xb,yb:ARRAY OF REAL);
VAR i,dummy:INTEGER;
    dumx,dumy:ARRAY[0..395] OF REAL;
BEGIN
  ScreenMode(3);
  WriteString("*************************************************************");WriteLn;
  WriteString("** A sum of 2D sinc pulse which excite a rectangular areas **");WriteLn;
  WriteString("**      with a shifted centres : (0,0) to (x0,y0)          **");WriteLn;
  WriteString("**           with pulse parameters from disk               **");WriteLn;
  WriteString("*************************************************************");WriteLn;
  WriteLn;WriteLn;
  FOR i:=0 TO ap-1 DO 
    xb[i]:=0.0;yb[i]:=0.0;
  END;
  WriteString('Pulse parameter file name  ');
  OpenInput('PLS');
  LOOP
    ReadInt(dummy);
    IF dummy=0 THEN EXIT END;
    fmpulseproc4(ap,h,dumx,dumy);
    FOR i:=0 TO ap-1 DO
      xb[i]:=xb[i]+dumx[i];
      yb[i]:=yb[i]+dumy[i];
    END;  
  END;
  CloseInput;
END fmpulseproc6;


END PulseLib.
