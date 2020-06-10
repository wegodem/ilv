IMPLEMENTATION MODULE FOUR3;(*1 november 1989 / LAST CHANGE 12-06-1991 BY HS*)
FROM MathLib0 IMPORT  sin;
CONST pi = 3.1415926535897932; 

PROCEDURE four2(VAR data:ARRAY OF REAL; nn:CARDINAL;terug:BOOLEAN); 
  (*5 april 1989 WB *)
  (* IF terug = TRUE, then inverse FFT *)
VAR ii, jj, n, mmax, m, j, istep, i, mmaxdiv2, nmdivistep: CARDINAL; 
    wtemp, wr, wpr, wwpr, wpi, wi, theta, scale:REAL; 
    tempr, tempi, pi2: REAL; 
BEGIN 
  pi2:=2.0*pi;
  n := 2*nn; 
  j := 1;i:=1; 
  FOR ii := 1 TO nn DO 
    i := 2*ii-1; 
    IF (j > i) THEN 
      tempr := data[j-1]; 
      tempi := data[j]; 
      data[j-1] := data[i-1]; 
      data[j] := data[i]; 
      data[i-1] := tempr; 
      data[i] := tempi
    END; 
    INC(i,2);
    m := n DIV 2; 
    WHILE ((m >= 2) AND (j > m)) DO 
      DEC(j, m); 
      m := m DIV 2
    END; 
    INC(j, m)
  END; 
  mmax := 2; 
  WHILE (n > mmax) DO 
    istep := 2*mmax; 
    theta := pi2/FLOAT(mmax); 
    IF terug THEN theta:=-theta END;
      wwpr:=sin(0.5*theta);
      wpr := -2.0*wwpr*wwpr; 
      wpi := sin(theta); 
      wr := 1.0; 
      wi := 0.0; 
      mmaxdiv2:=mmax DIV 2;
      nmdivistep:=(n-m) DIV istep;
      m := 1; 
      FOR ii:=1 TO mmaxdiv2 DO
        i:=m;
        FOR jj := 0 TO nmdivistep DO 
          j := i+mmax; 
          tempr := wr*data[j-1]-wi*data[j]; 
          tempi := wr*data[j]+wi*data[j-1]; 
          data[j-1] := data[i-1]-tempr; 
          data[j] := data[i]-tempi; 
          data[i-1] := data[i-1]+tempr; 
          data[i] := data[i]+tempi;
          INC(i,istep)
        END; 
        INC(m,2);
        wtemp := wr; 
        wr := wr*wpr-wi*wpi+wr; 
        wi := wi*wpr+wtemp*wpi+wi
      END; 
      mmax := istep;
    END;
    IF terug THEN scale:=1.0/FLOAT(nn); 
      FOR i:=0 TO n-1 DO
        data[i]:=scale*data[i]
      END;
    END;
  END four2;
  
PROCEDURE fft(ap:INTEGER;VAR real,imaginary:ARRAY OF REAL;
                        boole1:BOOLEAN);
VAR ww,i,hap:INTEGER;
    fourier,x:ARRAY[0..512] OF REAL;
BEGIN
(************************************************************************)
(***  THIS PROCEDURE CALCULATES THE (I)FFT OF THE ap POINTS COMPLEX   ***)
(***  SIGNAL (real,imaginary). ap SHOULD BE A POWER OF 2. IF boole1   ***)
(***  = TRUE THE IFFT IS CALCULATED. IF boole1 = FALSE A FFT IS CAL-  ***)
(***  CULATED. THE ZERO OF THE EXPECTED AND RETURNED TIME SIGNAL      ***)
(***  IS LOCATED AT THE ap/2 th POINT.                                ***)
(***  THE ZERO OF THE EXPECTED AND RETURNED SPECTRUM IS ALSO          ***)
(***  LOCATED AT THE ap/2 th POINT  DATE:12-06-1991 /H.S.             ***)
(************************************************************************)                  
  FOR i:=0 TO ap DO 
    x[i]:=FLOAT(i);
  END;   
  hap:=ap DIV 2;
  FOR i:= 0 TO hap-1 DO                 (* ROTATE TIME SIGNAL *)
    ww:=2*i;
    fourier[ww]:=real[i+hap]; 
    fourier[ww+1]:=imaginary[i+hap];
    fourier[ww+ap]:=real[i];
    fourier[ww+ap+1]:=imaginary[i];  
  END;
  four2(fourier,ap,boole1);
  FOR i:= 0 TO hap-1 DO                (* ROTATE SPECTRUM *)
    ww:=2*i;
    real[i]:=fourier[ww+ap];
    imaginary[i]:=fourier[ww+ap+1];
    real[i+hap]:=fourier[ww];
    imaginary[i+hap]:=fourier[ww+1];  
  END;  
END fft;   

  
END FOUR3.
