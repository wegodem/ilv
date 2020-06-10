
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
