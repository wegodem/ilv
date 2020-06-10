PROCEDURE Rotate2D(n,ap,ndw:INTEGER;h:REAL);
VAR hc,hb,hs,ht,r:CMatrix;
    dw,dt:REAL;
    k,l,m,nz,dummy,j,ftype,fn,fndw,evolute:INTEGER;
    written,read,nbytes,highpos,lowpos:CARDINAL;
    inout,out:File;
    result:DirResult;
    address1,address2:ADDRESS;
    done:BOOLEAN;
    chr:CHAR;
BEGIN
  evolute:=0;
  IF pulseflag = 2 THEN
    WriteString('>>>> NO RF-PULSE DEFINED ERROR <<<<');WriteLn;
  ELSE
    IF calc = 0 THEN
      IF ndw#1 THEN
        evolute:=1;
      END;  
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
        evolute:=1;
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
          dw:=statoff;
          IF evolute=1 THEN
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
        WriteString('*');WriteInt(evolute,2);          
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
END Rotate2D;

