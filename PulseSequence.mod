IMPLEMENTATION MODULE PulseSequence;

FROM InOut IMPORT Read, Write, ReadInt, ReadCard, ReadString, WriteInt,
                  WriteCard,WriteString,WriteLn,EOL,OpenInput,
                  CloseInput,OpenOutput,CloseOutput,Done;
FROM PulseLib IMPORT AMPulseDefinition,FMPulseDefinition,
                     AMHardPulseDefinition;
FROM DataFilesInout IMPORT RealArrayOut,RealArrayIn;

IMPORT FIO;
FROM FpuIO IMPORT RealToStr, StrToReal, WriteReal, ReadReal;
FROM WholeStr IMPORT IntToStr,ConvResults, StrToInt;
FROM Strings IMPORT Assign, Insert, Delete, Pos, Copy, ConCat, Length,
                    CompareStr;
                    
FROM SYSTEM IMPORT ADDRESS,ADR;
(* FROM RealInOut IMPORT WriteReal,ReadReal; *)
(* FROM Graphics IMPORT ScreenMode;          *)
FROM GradientLib IMPORT CalcGradientFields;


TYPE smallstring=ARRAY[0..7] OF CHAR;
     bigstring=ARRAY[0..20] OF CHAR;                        
CONST pi = 3.1415926535897932; 

VAR quantnum:ARRAY[1..6] OF INTEGER;
    relspinamp:ARRAY[1..6] OF REAL;
    resofreqs:ARRAY[1..6] OF REAL;
    Nspins,Jexact,Jcount,sequencetype:INTEGER;
    resultflnm:ARRAY[0..12] OF CHAR;
    fnames:ARRAY[0..20] OF smallstring;
    couplings,sectdur,x0,y0,z0,dx,dy,dz,statoff:ARRAY[0..20] OF REAL;
    sectype,noti,nevo:ARRAY[0..20] OF INTEGER;
    sector,ndx,ndy,ndz,nos,systemset,offsetset,sectorset:INTEGER;
    xb,yb,offset,gx,gy,gz:ARRAY[0..8192] OF REAL;
    remarks:ARRAY[0..20] OF bigstring;

(***************************************************************************)
(***    About      : Definition of pulse sequences to be simulated       ***)
(***    Author     : H.S.                                                ***)
(***    Date       : 20 - 03 -1992                                       ***)
(***    System     : LOGITECH MODULA2 -> gcc gm2 compiler 2019/2020      ***)
(***    Change     : 22 - 03 - 1992  BY H.S                              ***)
(***    Last change: 14 - 01 - 2020  BY H.S     gm2-version working      ***)
(***************************************************************************)    

PROCEDURE ComposePulseSequence;
VAR choice:INTEGER;
BEGIN
  Initialize;
  (* ScreenMode(3); *)
  WriteString('*****************************************************');WriteLn;
  WriteString('*** Compose an ILV executable pulse sequence file ***');WriteLn;
  WriteString('***                version 1                      ***');WriteLn;
  WriteString('*****************************************************');WriteLn;
  WriteLn;
  WriteString('    1. Change an existing spatial resolved pulse sequence');WriteLn;
  WriteString('    2. Create a new spatial resolved pulse sequence');WriteLn;
  WriteString('    3. Change an existing 1D or 2D NMR pulse sequence');WriteLn;
  WriteString('    4. Create a new 1D or 2D NMR pulse sequence');WriteLn;
  WriteString('    5. Quit composing pulsesequences');WriteLn;WriteLn; 
  WriteString('Your choice [1..5] : ');ReadInt(choice);WriteLn;
  IF choice=1 THEN
    sequencetype:=1;
    systemset:=1;sectorset:=1;offsetset:=1;    
    ReadPulseSeqFile;
    EditPulseSequence;
  END;
  IF choice=2 THEN
    sequencetype:=1;
    EditPulseSequence;
  END;
  IF choice=3 THEN
    sequencetype:=2;
    systemset:=1;sectorset:=1;offsetset:=1;    
    Read2DPulseSeqFile;
    Edit2DPulseSequence;
  END;
  IF choice=4 THEN
    sequencetype:=2;
    Edit2DPulseSequence;
  END;
END ComposePulseSequence;

PROCEDURE EditPulseSequence;
VAR  i,choice:INTEGER;
BEGIN
  (* ScreenMode(3); *)
  WriteString('             ******************************************************');WriteLn;
  WriteString('             ***     Edit a spatial resolved pulse sequence     ***');WriteLn;
  WriteString('             ******************************************************');
  WriteLn;WriteLn;
  WriteString('Current pulse sequence status :');WriteLn;
  WriteString(' # spins   =');WriteInt(Nspins,2);WriteString('|');
  WriteString(' # x-steps =');WriteInt(ndx,2);WriteString('|');
  WriteString(' # y-steps =');WriteInt(ndy,2);WriteString('|');
  WriteString(' # z-steps =');WriteInt(ndz,2);WriteString('|');
  WriteString(' # sectors =');WriteInt(nos,2);WriteLn;
  FOR i:=1 TO nos DO
    WriteString('Sector ');WriteInt(i,2);WriteLn;
    WriteString('Time ints ');WriteInt(noti[i],2);
    WriteString('| Sector duration=');
    WriteReal(sectdur[i]*1000.0,12, 11);
    WriteString(' ms ');
    WriteString('| Filename of pulse=');WriteString(fnames[i]);WriteLn;
    WriteString( 'Offset frequency=' );WriteReal( statoff[i], 12, 11 );WriteLn;
    WriteString(' x0 = ');WriteReal(x0[i],12, 11);WriteString('| dx=');WriteReal(dx[i],12, 11);WriteLn;
    WriteString(' y0 = ');WriteReal(y0[i],12, 11);WriteString('| dy=');WriteReal(dy[i],12, 11);WriteLn;
    WriteString(' z0 = ');WriteReal(z0[i],12, 11);WriteString('| dz=');WriteReal(dz[i],12, 11);WriteLn;
  END;    
  WriteString('Possibilities :');WriteLn;WriteLn;
  WriteString('    1. Define spin system');WriteLn;
  WriteString('    2. Define number of offset frequency points');WriteLn;
  WriteString('    3. Define a sequence sector');WriteLn;
  WriteString('    4. Define name of binary file to store results in');WriteLn;
  WriteString('    5. Create an executable ASCII pulse sequence file');WriteLn;
  WriteString('    6. Quit');WriteLn;WriteLn;
  WriteString('Your choice is [1..6] : ');ReadInt(choice);WriteLn;
  IF choice<6 THEN
    CASE choice OF
               1:Ispins;
                 MakeBoltzmann;
                 MakeZeeman;
                 SpinSpinCoupling;
                 systemset:=1;|
               2:WriteString('Number of x-coordinates    : ');ReadInt(ndx);
                 IF ndx=0 THEN ndx:=1 END;
                 WriteLn;  
                 WriteString('Number of y-coordinates    : ');ReadInt(ndy);
                 IF ndy=0 THEN ndy:=1 END;
                 WriteLn; 
                 WriteString('Number of z-coordinates    : ');ReadInt(ndz);
                 IF ndz=0 THEN ndz:=1 END;
                 WriteLn;  
                 offsetset:=1;|                 
               3:WriteString('Sector number to edit = ');ReadInt(sector);WriteLn;
                 DefineSeqSector(sector);
                 sectorset:=1;
                 IF sector>nos THEN 
                   nos:=sector; 
                 END;|
               4:WriteString('File name to store results in : ');
                 ReadString(resultflnm);|
               5:MakePulseSeqFile;

    END;
    EditPulseSequence;
  END;                   
END EditPulseSequence;

PROCEDURE Edit2DPulseSequence;
VAR  i,choice:INTEGER;
BEGIN
  (* ScreenMode(3); *)
  WriteString('             ***************************************************');WriteLn;
  WriteString('             ***     Edit an 1D or 2D NMR pulse sequence     ***');WriteLn;
  WriteString('             ***************************************************');
  WriteLn;WriteLn;
  WriteString('Current pulse sequence status :');WriteLn;
  WriteString(' # spins : ');WriteInt(Nspins,2);WriteString('|');
  WriteString(' # evolution steps : ');WriteInt(nevo[0],2);WriteString('|');
  WriteString(' # sectors : ');WriteInt(nos,2);WriteLn;
  FOR i:=1 TO nos DO
    WriteString('Sector    : ');WriteInt(i,2);WriteString('| Sector type : ');
    WriteInt(sectype[i],2);WriteLn;
    WriteString('Time ints : ');WriteInt(noti[i],2);
    WriteString('| Sector duration [ms] :');WriteReal(sectdur[i]*1000.0, 12, 11);
    WriteLn;
    WriteString(' # evolution steps : ');WriteInt(nevo[i],2);WriteLn;
    WriteString('Filename of pulse : ');WriteString(fnames[i]);WriteLn;
    WriteString('Offset frequency : ');WriteReal(statoff[i], 12, 11);WriteLn;WriteLn;
  END;    
  WriteString('Possibilities :');WriteLn;WriteLn;
  WriteString('    1. Define spin system');WriteLn;
  WriteString('    2. Define a sequence sector');WriteLn;
  WriteString('    3. Define name of binary file to store results in');WriteLn;
  WriteString('    4. Create an executable ASCII pulse sequence file');
  WriteLn;
  WriteString('    5. Quit');WriteLn;WriteLn;
  WriteString('Your choice is [1..5] : ');ReadInt(choice);WriteLn;
  IF choice<5 THEN
    CASE choice OF
               1:Ispins;
                 MakeBoltzmann;
                 MakeZeeman;
                 SpinSpinCoupling;
                 systemset:=1;|
               2:WriteString('Sector number to edit = ');ReadInt(sector);WriteLn;
                 Define2DSeqSector(sector);
                 sectorset:=1;
                 IF sector>nos THEN 
                   nos:=sector; 
                 END;|
               3:WriteString('File name to store results in : ');
                 ReadString(resultflnm);|
               4:Make2DPulseSeqFile;

    END;
    Edit2DPulseSequence;
  END;                   
END Edit2DPulseSequence;

PROCEDURE Define2DSeqSector(sector:INTEGER);
VAR ap,answer,i:INTEGER;
    ti:REAL;
    chr:CHAR;
BEGIN
  (* ScreenMode(3); *)
  WriteLn;WriteLn;
  WriteString('You are editing pulse sequence sector : ');
  WriteInt(sector,5);
  WriteLn;WriteLn;
  WriteString('Sector type : ');WriteLn;WriteLn;
  WriteString('             1. normal sector');WriteLn;
  WriteString('             2. evolution sector');WriteLn;WriteLn;
  WriteString('Your choice [1,2] : ');ReadInt(sectype[sector]);WriteLn;
  IF sectype[sector]=1 THEN
    nevo[sector]:=nevo[sector-1];
    WriteString('Which form has the Pulse Hamiltonian ?');
    WriteLn;WriteLn;
    WriteString('    1. Hardpulse (90 x,y,-x-y and 180 x,y,-x,-y are available');WriteLn; 
    WriteString('    2. Hp(t) = f(t).Ix   (AM modulated) ');WriteLn;
    WriteString('    3. Hp(t) = f(t).Ix + g(t).Iy (FM or adiabatic)');WriteLn;
    WriteString('    4. Pulse file (Bx,By) from disk');WriteLn;
    WriteString('    5. Pulse file (Bx,Bz) from disk');WriteLn;
    WriteString('    6. Waiting time (free precession)');WriteLn;
    WriteLn;WriteLn;
    WriteString(' Your choice is [1..6] : ');
    ReadInt(answer);WriteLn;
    IF (answer#1) AND (answer#6) THEN  
      WriteString('Number of integration time intervals : ');
      ReadInt(noti[sector]);
      ap:=noti[sector];
      WriteLn;
      WriteString('Total sector duration [ms ] : ');
      ReadReal(sectdur[sector]);
      sectdur[sector]:=sectdur[sector]/1000.0;
      WriteLn;
      ti:=sectdur[sector]/FLOAT(noti[sector]);
    ELSE 
      ti:=1.0E-5;ap:=1;  
    END;
    CASE answer OF 
      1:AMHardPulseDefinition(ap,ti,xb,yb);
        noti[sector]:=1;sectdur[sector]:=1.0E-5;offset[0]:=0.0;
        WriteString('File name of this sectors RF-pulse : ');
        ReadString(fnames[sector]);
        BinArrayOut2D(ap,xb,yb,offset,fnames[sector]);|
      2:AMPulseDefinition(ap,ti,xb,yb);
        FOR i:=0 TO ap-1 DO
          offset[i]:=0.0;
        END;
        WriteString('File name of this sectors RF-pulse : ');
        ReadString(fnames[sector]);
        BinArrayOut2D(ap,xb,yb,offset,fnames[sector]);|
      3:FMPulseDefinition(ap,ti,xb,yb);            
        FOR i:=0 TO ap-1 DO
          offset[i]:=0.0;
        END;
        WriteString('File name of this sectors RF-pulse : ');
        ReadString(fnames[sector]);
        BinArrayOut2D(ap,xb,yb,offset,fnames[sector]);|
      4:ReadBxByPulse(ap,xb,yb,offset);
        WriteString('File name of this sectors RF-pulse : ');
        ReadString(fnames[sector]);
        BinArrayOut2D(ap,xb,yb,offset,fnames[sector]);|
      5:ReadBxBzPulse(ap,xb,yb,offset);
        WriteString('File name of this sectors RF-pulse : ');
        ReadString(fnames[sector]);
        BinArrayOut2D(ap,xb,yb,offset,fnames[sector]);|
      6:WriteString('Waiting-time (free precession time) [ms] : ');
        ReadReal(sectdur[sector]);noti[sector]:=1;
        sectdur[sector]:=sectdur[sector]/1000.0;
        xb[0]:=0.0;yb[0]:=0.0;offset[0]:=0.0;
        WriteLn;
        WriteString('File name of this sectors RF-pulse : ');
        ReadString(fnames[sector]);
        BinArrayOut2D(ap,xb,yb,offset,fnames[sector]);|
    END;
    WriteLn;
  ELSE
    IF nevo[sector-1]#1 THEN 
      WriteString('A 3D experiment is not allowed !!!');WriteLn;
      WriteString('Press <enter> to continue');Read(chr);WriteLn;
    ELSE  
      WriteString('Basic evolution-sector-time [ms] : '); 
      ReadReal(sectdur[sector]);WriteLn;
      sectdur[sector]:=sectdur[sector]/1000.0;
      noti[sector]:=1;
      WriteString('# of evolutions                  : ');
      ReadInt(nevo[sector]);WriteLn;
      ap:=1;xb[0]:=0.0;yb[0]:=0.0;offset[0]:=0.0;
      WriteString('File name zero RF field : ');
      ReadString(fnames[sector]);WriteLn;
      BinArrayOut2D(ap,xb,yb,offset,fnames[sector]);
    END;
  END;
  WriteString('Static offset frequency is      [Hz] : ');
  ReadReal(statoff[sector]);WriteLn;  
  WriteString('Remark (max. 20 characters)');
  ReadString(remarks[sector]);WriteLn;
END Define2DSeqSector;


(* This routine has to be made in a different way, *) 
(* since FIO does not support ReadInt etc.         *)
PROCEDURE Make2DPulseSeqFile;
VAR i:INTEGER;
    char:CHAR;
    sequenceTypeStr: ARRAY[0..5] OF CHAR;
    outRealConvStr: ARRAY[0..15] OF CHAR;
    outIntConvStr: ARRAY[0..15] OF CHAR;
    fnm:ARRAY[0..128] OF CHAR;
    out:FIO.File;
BEGIN
  IF ( sectorset#0 ) AND ( systemset#0 ) THEN
    (* OpenOutput('SEQ'); *)
    
    WriteString( 'Enter pulse sequence output file name >> ' );
    ReadString( fnm );
    WriteLn( );
    
    out := FIO.OpenToWrite( fnm );    

    FIO.WriteString( out, 'sequence_type: ' );
    IntToStr( sequencetype, outIntConvStr );
    FIO.WriteString( out, outIntConvStr );
    FIO.WriteLine( out );
    
    IntToStr( Nspins, outIntConvStr );
    FIO.WriteString( out, outIntConvStr );
    FIO.WriteLine( out );
    
    FIO.WriteString( out, 'Quantum numbers of spins * 1/2' );
    FIO.WriteLine( out );
    FOR i:=1 TO Nspins DO
      IntToStr( quantnum[i], outIntConvStr );
      FIO.WriteString( out, outIntConvStr );
      FIO.WriteString( out, '  ' );
    END;
    
    FIO.WriteLine( out );
    
    FIO.WriteString( out, 'Relative spin amplitudes ' );
    FIO.WriteLine( out );
    FOR i:=1 TO Nspins DO
      RealToStr( relspinamp[i], 15, 14, outRealConvStr );
      FIO.WriteString( out, outRealConvStr );
      FIO.WriteString( out, '  ' );
    END;
    
    FIO.WriteLine( out );
    
    FIO.WriteString( out, 'Resonance frequencies of the spins ' );
    FIO.WriteLine( out );
    FOR i:=1 TO Nspins DO
      RealToStr( resofreqs[i], 15, 14, outRealConvStr );
      FIO.WriteString( out, outRealConvStr );
      FIO.WriteString( out, '  ' );
    END;
    
    FIO.WriteLine( out );
    
    IF Nspins>1 THEN
      
      IntToStr( Jexact, outIntConvStr );
      FIO.WriteString( out, outIntConvStr );
      FIO.WriteLine( out );
      IntToStr( Jcount, outIntConvStr );
      FIO.WriteString( out, outIntConvStr );
      FIO.WriteLine( out );  
      
      FOR i:=1 TO Jcount DO
        RealToStr( couplings[i], 15, 14, outRealConvStr );
        FIO.WriteString( out, outRealConvStr );
        FIO.WriteLine( out );
      END;
      
      FIO.WriteLine( out );
      
    END;
    IntToStr( nos, outIntConvStr );
    FIO.WriteString( out, outIntConvStr );
    FIO.WriteLine( out );
    FIO.WriteLine( out );(* nos = number of sectors *)
    FOR i := 1 TO nos DO
    
      FIO.WriteString( out, '*******sector_number: ' );
      WriteString( '*******sector_number: ' );
      WriteInt( i, 6 );
      WriteLn;
      IntToStr( i, outIntConvStr );
      FIO.WriteString( out, outIntConvStr );
      FIO.WriteLine( out );
      
      
      FIO.WriteString( out, '*********sector_type: ' );
      WriteString( '*********sector_type: ' );
      WriteInt( sectype[i], 6 );
      WriteLn;
      IntToStr( sectype[i], outIntConvStr );
      FIO.WriteString( out, outIntConvStr );
      FIO.WriteLine( out );
      
      FIO.WriteString( out, 'number_of_timepoints: ' );
      WriteString( 'number_of_timepoints: ' );
      WriteInt( noti[i], 6 );
      WriteLn;
      IntToStr( noti[i], outIntConvStr );
      FIO.WriteString( out, outIntConvStr );
      FIO.WriteLine( out );
      
      FIO.WriteString( out, 'number_of_evolpoints: ' );
      WriteString( 'number_of_evolpoints: ' );
      WriteInt( nevo[i], 6 );
      WriteLn;
      IntToStr( nevo[i], outIntConvStr );
      FIO.WriteString( out, outIntConvStr );
      FIO.WriteLine( out );
      
      FIO.WriteString( out, '******total_duration: ' );
      WriteString( '******total_duration: ' );
      WriteReal( sectdur[i], 15, 14 );
      WriteLn;
      RealToStr( sectdur[i], 15, 14, outRealConvStr );
      FIO.WriteString( out, outRealConvStr );
      FIO.WriteLine( out );
      
      
      FIO.WriteString( out, '******pulse_filename: ' );
      WriteString( '******pulse_filename: ' );
      WriteString( fnames[i] );
      FIO.WriteString( out, fnames[i] );
      FIO.WriteLine( out );
      
      FIO.WriteString( out, '****offset_frequency: ' );
      WriteString( '****offset_frequency: ' );
      WriteReal( statoff[i], 15, 14 );
      WriteLn;
      RealToStr( statoff[i], 15, 14, outRealConvStr );
      FIO.WriteString( out, outRealConvStr );
      FIO.WriteLine( out );
      
      FIO.WriteString( out, '**************remark: ' );
      WriteString( '**************remark: ' );
      WriteString( remarks[i] );
      WriteLn;
      FIO.WriteString( out, remarks[i] );
      FIO.WriteLine( out );
      FIO.WriteLine( out );
    END;
    FIO.WriteString( out, resultflnm );
    WriteString(' Result filename: '); 
    WriteString( resultflnm );
    FIO.WriteLine( out );
    FIO.Close( out );
 ELSE
   WriteString('INCOMPLETE 2D PULSE EXPERIMENT : NO PARAMETER FILE GENERATION');
   WriteLn;
   WriteString('Press <enter>to continue ');
   Read(char);
 END;    
END Make2DPulseSeqFile;    

PROCEDURE Read2DPulseSeqFile;
VAR i,intdummy:INTEGER;
    dummy:ARRAY[0..20] OF CHAR;
    fnm: ARRAY[0..128] OF CHAR;
    stringInt: ARRAY[0..18] OF CHAR;
    stringReal: ARRAY[0..18] OF CHAR;
    convRes:ConvResults;
    in:FIO.File;
BEGIN
  WriteString('File name of pulse sequence to read : ');
  (* OpenInput('SEQ'); *)
  ReadString( fnm );WriteLn;
  
  in := FIO.OpenToRead( fnm );
  
 
  FIO.ReadString( in, dummy );
  WriteString( dummy );WriteLn;
  
  (* FIO.ReadInt( sequencetype ); *)
  FIO.ReadString( in, stringInt );
  StrToInt( stringInt, sequencetype, convRes );
  WriteInt( sequencetype, 6 );WriteLn;  
  
  (* FIO.ReadInt( in, Nspins ); *)
  FIO.ReadString( in, stringInt );
  StrToInt( stringInt, Nspins, convRes );
  WriteString( 'Number of spins : ' );
  WriteInt( Nspins, 6 );
  WriteLn;
  
  FIO.ReadString( in, dummy );
  WriteString( dummy );WriteLn;
  
  FOR i:=1 TO Nspins DO
    (* FIO.ReadInt( in, quantnum[i] );*) 
    FIO.ReadString( in, stringInt );
    StrToInt( stringInt, quantnum[i], convRes );
    WriteInt( quantnum[i], 6 );WriteString( '  ' );
    FIO.ReadString( in, dummy );
  END;
  WriteLn;
  
  FIO.ReadString( in, dummy );
  WriteString( dummy );WriteLn;
  
  FOR i:=1 TO Nspins DO
    (* FIO.ReadReal( in, relspinamp[i]); *)
    FIO.ReadString( in, stringReal );
    StrToReal( stringReal, relspinamp[i] );
    WriteReal( relspinamp[i], 15, 14 );
    WriteString( '  ' );
  END;
  WriteLn;
  
  FIO.ReadString( in, dummy );
  WriteString(dummy);WriteLn;
  
  FOR i:=1 TO Nspins DO
    (* FIO.ReadReal( in, resofreqs[i]); *)
    FIO.ReadString( in, stringReal );
    StrToReal( stringReal, resofreqs[i] );
    WriteReal( resofreqs[i], 15, 14 );WriteString( '  ' );
  END;
  WriteLn; 
  
  IF Nspins>1 THEN
    
    (* FIO.ReadInt( in, Jexact ); *)
    FIO.ReadString( in, stringInt );
    StrToInt( stringInt, Jexact, convRes );
    WriteString( 'Jexact : ' );WriteInt( Jexact, 6 );WriteLn;
    (* FIO.ReadInt( Jcount ); *)
    FIO.ReadString( in, stringInt );
    StrToInt( stringInt, Jcount, convRes );
    WriteString( 'Jcount : ' );WriteInt( Jcount, 6 );WriteLn;
    
    FOR i:=1 TO Jcount DO
      (* FIO.ReadReal( in, couplings[i] );*)
      FIO.ReadString( in, stringReal );
      StrToReal( stringReal, couplings[i] );
      WriteReal( couplings[i], 15, 14 ); WriteString( '  ' );
    END;
    WriteLn;
  END;
  
  (* FIO.ReadInt( in, nos ); *)  (* nos = number of sectors *)
  FIO.ReadString( in, stringInt );
  StrToInt( stringInt, nos, convRes );
  WriteString( "nos : ");
  WriteInt( nos, 6 );
  WriteLn;
  
  FOR i:=1 TO nos DO
    FIO.ReadString( in, dummy );
    (* FIO.ReadInt( in, intdummy ); *)
    FIO.ReadString( in, stringInt );
    StrToInt( stringInt, intdummy, convRes );
    WriteString( "intdummy : ");
    WriteInt( intdummy, 6 );
    WriteLn;
    
    FIO.ReadString( in, dummy );
    (* FIO.ReadInt( in, sectype[i] ); *)
    FIO.ReadString( in, stringInt );
    StrToInt( stringInt, sectype[i], convRes );
    WriteString( "sectype[i] : ");
    WriteInt( sectype[i], 6 );
    WriteLn;
    
    FIO.ReadString( in, dummy );
    (* FIO.ReadInt( in, nevo[i] ); *)
    FIO.ReadString( in, stringInt );
    StrToInt( stringInt, nevo[i], convRes );
    WriteString( "nevo[i] : ");
    WriteInt( nevo[i], 6 );
    WriteLn;
    
    FIO.ReadString( in, dummy );
    (* FIO.ReadInt( in, noti[i] ); *)
    FIO.ReadString( in, stringInt );
    StrToInt( stringInt, noti[i], convRes );
    WriteString( "noti[i] : ");
    WriteInt( noti[i], 6 );
    WriteLn;
    
    FIO.ReadString( in, dummy );
    (* FIO.ReadReal( in, sectdur[i] ); *)
    FIO.ReadString( in, stringReal );
    StrToReal( stringReal, sectdur[i] );
    WriteString( "sectdur[i] : " );
    WriteReal( sectdur[i], 15, 14 );
    WriteLn;
    
    FIO.ReadString( in, dummy );
    FIO.ReadString( in, fnames[i] );
    WriteString( "fnames[i] : ");
    WriteString( fnames[i] );
    WriteLn;
    
    FIO.ReadString( in, dummy );
    (* FIO.ReadReal( in, statoff[i] ); *)
    FIO.ReadString( in, stringReal );
    StrToReal( stringReal, statoff[i] );
    WriteString( "statoff[i] : " );
    WriteReal( statoff[i], 15, 14 );
    WriteLn;
    
    FIO.ReadString( in, dummy );
    FIO.ReadString( in, remarks[i] );
    WriteString( "remark : ");
    WriteString( remarks[i] );
    WriteLn;
    
  END;
  FIO.ReadString( in, resultflnm );
  WriteString( "resultflnm : ");
  WriteString( resultflnm );
  WriteLn;
  
  FIO.Close( in );
END Read2DPulseSeqFile;    

PROCEDURE MakePulseSeqFile;
VAR i:INTEGER;
    char:CHAR;
    fnm: ARRAY[0..128] OF CHAR;
    outRealConvStr: ARRAY[0..15] OF CHAR;
    outIntConvStr: ARRAY[0..15] OF CHAR;
    out:FIO.File;
BEGIN
  IF (sectorset#0) AND (systemset#0) AND (offsetset#0) THEN
    (* OpenOutput('SEQ'); *)
    WriteString( 'Pulse sequence output file name >> ' );
    ReadString( fnm );
    WriteLn( );
    
    out := FIO.OpenToWrite( fnm );    
    
    FIO.WriteString( out, 'sequence_type: ');
    FIO.WriteLine(out);
    WriteString( 'sequence_type: ');
    IntToStr( sequencetype, outIntConvStr );
    FIO.WriteString( out, outIntConvStr );
    WriteInt( sequencetype, 6 );
    FIO.WriteLine( out );
    WriteLn;
    
    FIO.WriteString( out, 'Number of spins: ' );
    FIO.WriteLine(out);
    IntToStr( Nspins, outIntConvStr );
    FIO.WriteString( out, outIntConvStr );
    WriteString( 'Number of spins: ' );
    WriteInt( Nspins, 6 );
    WriteLn;
    
    FIO.WriteString( out, 'Spin Quantum I (number x 2) : ' );
    WriteString( 'Spin Quantum I (number x 2) : ' );
    FIO.WriteLine(out);
    
    FOR i:=1 TO Nspins DO
      IntToStr( quantnum[i], outIntConvStr );
      FIO.WriteString( out, outIntConvStr );
      WriteInt( quantnum[i], 6 );
      (* FIO.WriteString( out, '  ' );*)
      FIO.WriteLine( out );
      WriteString( '  ' );
    END;
    WriteLn;
    
    WriteString( 'Relative amplitudes: ' );
    FIO.WriteString( out, 'Relative amplitudes: ' );
    FIO.WriteLine( out );
    FOR i:=1 TO Nspins DO
      RealToStr( relspinamp[i], 15, 14, outRealConvStr );
      FIO.WriteString( out, outRealConvStr );
      WriteReal( relspinamp[i], 15, 14 );
      FIO.WriteLine( out );
      WriteString( '  ' );
    END;
    FIO.WriteLine( out );
    WriteLn;
    
    WriteString( 'Resonance frequencies: ' );
    FIO.WriteString( out, 'Resonance frequencies: ' );
    FIO.WriteLine( out );
    
    FOR i:=1 TO Nspins DO
      RealToStr( resofreqs[i], 15, 14, outRealConvStr );
      FIO.WriteString( out, outRealConvStr );
      FIO.WriteLine( out );
      WriteReal( resofreqs[i], 15, 14 );
      (* FIO.WriteString( '  ' ); *)
      WriteLn;
    END;
    
    IF Nspins>1 THEN
      
      FIO.WriteString( out, 'Exact simulation of J-coupling : ' );
      FIO.WriteLine( out );
      IntToStr( Jexact, outIntConvStr );
      FIO.WriteString( out, outIntConvStr );
      FIO.WriteLine( out );
      WriteString( 'Exact simulation of J-coupling : ' );
      WriteInt( Jexact, 6 );
      WriteLn;
      
      FIO.WriteString( out, 'Jcount : ' );
      FIO.WriteLine( out );
      IntToStr( Jcount, outIntConvStr );
      FIO.WriteString( out, outIntConvStr );
      FIO.WriteLine( out );
      WriteString( 'Jcount : ' );
      WriteInt( Jcount, 6 );
      WriteLn;
      
      WriteString( 'J-coupling constants: ' );
      FIO.WriteString( out, 'J-coupling constants: ' );
      FIO.WriteLine( out );
      
      FOR i:=1 TO Jcount DO
        RealToStr( couplings[i], 15, 14, outRealConvStr );
        FIO.WriteString( out, outRealConvStr );
        FIO.WriteLine( out );
        WriteReal( couplings[i], 15, 14 );
        WriteString( '  ' );
      END;
      FIO.WriteLine( out );
      WriteLn;
    END;

    FIO.WriteString( out, 'ndx : ' );
    FIO.WriteLine( out );
    WriteString( 'ndx : ' ); 
    IntToStr( ndx, outIntConvStr );
    FIO.WriteString( out, outIntConvStr ); 
    FIO.WriteLine( out );
    WriteInt( ndx, 6 );WriteLn;
    
    FIO.WriteString( out, 'ndy : ' );
    FIO.WriteLine( out );
    WriteString( 'ndy : ' );
    IntToStr( ndy, outIntConvStr );
    FIO.WriteString( out, outIntConvStr );
    FIO.WriteLine( out );
    WriteInt( ndy, 6 );WriteLn;
    
    FIO.WriteString( out, 'ndz : ' );
    FIO.WriteLine( out );
    WriteString( 'ndz : ' );
    IntToStr( ndz, outIntConvStr );
    FIO.WriteString( out, outIntConvStr ); 
    FIO.WriteLine( out );
    WriteInt( ndz, 6 );WriteLn;
    
    FIO.WriteString( out, 'Number of sectors: ');
    FIO.WriteLine( out );
    WriteString( 'Number of sectors: ' );
    IntToStr( nos, outIntConvStr );
    FIO.WriteString( out, outIntConvStr );
    FIO.WriteLine( out ); (* nos = number of sectors *)
    WriteInt( nos, 6 ); 
    WriteLn;
    
    FOR i:=1 TO nos DO
      
      FIO.WriteString( out, '*******sector_number: ' );
      FIO.WriteLine( out );
      WriteString( '*******sector_number: ' );
      IntToStr( i, outIntConvStr );
      FIO.WriteString( out, outIntConvStr );
      FIO.WriteLine( out );
      WriteInt( i, 6 );
      WriteLn;
      
      FIO.WriteString( out, 'number_of_timepoints: ' );
      FIO.WriteLine( out );
      
      WriteString( 'number_of_timepoints: ' );
      IntToStr( noti[i], outIntConvStr );
      FIO.WriteString( out, outIntConvStr );
      FIO.WriteLine( out );
      
      WriteInt( noti[i], 6 );
      WriteLn;
      
      FIO.WriteString( out, '******total_duration: ' );
      WriteString( '******total_duration: ' );
      RealToStr( sectdur[i], 15, 14, outRealConvStr );
      FIO.WriteString( out, outRealConvStr );
      FIO.WriteLine( out );
      
      WriteReal( sectdur[i], 15, 14 );
      WriteLn;
      
      FIO.WriteString( out, '******pulse_filename: ' );
      FIO.WriteLine( out );
      
      WriteString( '******pulse_filename: ' );
      WriteLn;
      
      FIO.WriteString( out, fnames[i] );
      FIO.WriteLine( out );
      WriteString( fnames[i] );
      WriteLn;
      
      
      FIO.WriteString( out, '****offset_frequency: ' );
      FIO.WriteLine( out );
      
      WriteString( '****offset_frequency: ' );
      RealToStr( statoff[i], 15, 14, outRealConvStr );
      FIO.WriteString( out, outRealConvStr );
      FIO.WriteLine( out );
      
      WriteReal( statoff[i], 15, 14 );
      WriteLn;
      
      FIO.WriteString( out, '****x-spatial_offset: ' );
      FIO.WriteLine( out );
      
      WriteString( '****x-spatial_offset: ' );
      RealToStr( x0[i], 15, 14, outRealConvStr );
      FIO.WriteString( out, outRealConvStr );
      FIO.WriteLine( out );
      
      WriteReal( x0[i], 15, 14 );
      WriteLn;
      
      FIO.WriteString( out, '*x-spatial_step_size: ' );
      FIO.WriteLine( out );
      
      WriteString( '*x-spatial_step_size: ' );
      RealToStr( dx[i], 15, 14, outRealConvStr );
      FIO.WriteString( out, outRealConvStr );
      FIO.WriteLine( out );
      
      WriteReal( dx[i], 15, 14 );
      WriteLn;
      
      FIO.WriteString( out, '****y-spatial_offset: ' );
      FIO.WriteLine( out );
      
      WriteString( '****y-spatial_offset: ' );
      RealToStr( y0[i], 15, 14, outRealConvStr );
      FIO.WriteString( out, outRealConvStr);
      FIO.WriteLine( out );
      
      WriteReal( y0[i], 15, 14 );
      WriteLn;
      
      FIO.WriteString( out, '*y-spatial_step_size: ' );
      FIO.WriteLine( out );
      
      WriteString( '*y-spatial_step_size: ' );
      RealToStr( dy[i], 15, 14, outRealConvStr );
      FIO.WriteString( out, outRealConvStr );
      FIO.WriteLine( out );
      WriteReal( dy[i] , 15, 14);
      WriteLn;
      
      FIO.WriteString( out, '****z-spatial_offset: ' );
      FIO.WriteLine( out );
      
      WriteString( '****z-spatial_offset: ' );
      RealToStr( z0[i], 15, 14, outRealConvStr );
      FIO.WriteString( out, outRealConvStr );
      FIO.WriteLine( out );
      WriteReal( z0[i], 15, 14 );
      WriteLn;
      
      FIO.WriteString( out, '*z-spatial_step_size: ' );
      FIO.WriteLine( out );
      
      WriteString( '*z-spatial_step_size: ' );
      RealToStr( dz[i], 15, 14, outRealConvStr );
      FIO.WriteString( out, outRealConvStr );
      FIO.WriteLine( out );
      
      WriteReal( dz[i], 15, 14 );
      WriteLn;
      
      FIO.WriteString( out, '**************remark: ' );
      FIO.WriteLine( out );
      
      WriteString( '**************remark: ' );
      FIO.WriteString( out, remarks[i] );
      WriteString( remarks[i] );
      
      FIO.WriteLine( out );
      WriteLn;
      FIO.WriteLine( out );
      WriteLn;
    END;
    FIO.WriteString( out, resultflnm );
    WriteString( 'Results filename:' );
    WriteString( resultflnm );
    FIO.WriteLine( out );
    WriteLn;
    FIO.Close( out );
 ELSE
   WriteString( 'INCOMPLETE PULSE EXPERIMENT : NO PARAMETER FILE GENERATION' );
   WriteLn;
   WriteString( 'Press any key to continue ' );
   Read( char );
 END;    
END MakePulseSeqFile; 

PROCEDURE ReadPulseSeqFile;
VAR i:INTEGER;
    dummy:ARRAY[0..15] OF CHAR;
    fnm: ARRAY[0..128] OF CHAR;
    lineAsString: ARRAY[0..33] OF CHAR;
    stringInt: ARRAY[0..18] OF CHAR;
    stringReal: ARRAY[0..18] OF CHAR;
    convRes:ConvResults;
    in: FIO.File;
BEGIN

  WriteString( 'File name of pulse sequence to read : ' );WriteLn;
  (* OpenInput('SEQ'); *)
  ReadString( fnm );
  
  in := FIO.OpenToRead( fnm );
  (* FIO.ReadInt( sequencetype ); *)
  FIO.ReadString( in, stringInt );
  StrToInt( stringInt, sequencetype, convRes );
  
  WriteString( 'Sequence type: ' );
  WriteInt( sequencetype, 5 );WriteLn;
  
  (* FIO.ReadInt( in, Nspins );*)
  FIO.ReadString( in, stringInt );
  StrToInt( stringInt, Nspins, convRes );
  
  WriteString( 'Quantum numbers : ' );WriteLn;
  FOR i:=1 TO Nspins DO
    (* FIO.ReadInt( in, quantnum[i] ); *)
    FIO.ReadString( in, stringInt );
    StrToInt( stringInt, quantnum[i], convRes );
    WriteInt( quantnum[i], 6 ); WriteString( '  ' );
  END;
  WriteLn;
  
  WriteString( 'Relative intensities : ' );WriteLn;
  FOR i:=1 TO Nspins DO
    (* FIO.ReadReal( in, relspinamp[i] ); *)
    FIO.ReadString( in, stringReal );
    StrToReal( stringReal, relspinamp[i] );
    WriteReal( relspinamp[i], 15, 14 ); WriteString( '  ' );
  END;
  
  WriteString( 'Resonance frequencies : ' );WriteLn;
  FOR i:=1 TO Nspins DO
    (* FIO.ReadReal( in, resofreqs[i] ); *)
    FIO.ReadString( in, stringReal );
    StrToReal( stringReal, resofreqs[i] );
    WriteReal( resofreqs[i], 15, 14 ); WriteString( '  ' );
  END;
  WriteLn; 
  
  
  IF Nspins>1 THEN
    
    WriteString( 'Jexact : ');
    (* FIO.ReadInt( in, Jexact ); *)
    FIO.ReadString( in, stringInt );
    StrToInt( stringInt, Jexact, convRes );
    WriteInt( Jexact, 6 ); WriteLn;
    
    WriteString( 'Jcount : ');
    (* FIO.ReadInt( in, Jcount ); *)
    FIO.ReadString( in, stringInt );
    StrToInt( stringInt, Jcount, convRes );
    WriteInt( Jcount, 6 ); WriteLn;
    
    WriteString( 'Coupling constants :' );
    FOR i:=1 TO Jcount DO
      (* FIO.ReadReal( in, couplings[i] ); *)
      FIO.ReadString( in, stringReal );
      StrToReal( stringReal, couplings[i] );
    END;
  END;
  
  (* FIO.ReadInt( in, ndx ); *)
  FIO.ReadString( in, stringInt );
  StrToInt( stringInt, ndx, convRes );
  WriteString( 'Number of samples in x-direction :' ); WriteInt( ndx, 6 );
  
  (* FIO.ReadInt( in, ndy ); *)
  FIO.ReadString( in, stringInt );
  StrToInt( stringInt, ndy, convRes );
  WriteString( 'Number of samples in y-direction :' ); WriteInt( ndy, 6 );
  
  (* FIO.ReadInt( in, ndz ); *)
  FIO.ReadString( in, stringInt );
  StrToInt( stringInt, ndz, convRes );
  WriteString( 'Number of samples in z-direction :' ); WriteInt( ndz, 6 );
  
  (* FIO.ReadInt( in, nos ); *) 
  (* nos = number of sectors *)
  FIO.ReadString( in, stringInt );
  StrToInt( stringInt, nos, convRes );
  WriteString( 'Number of samples sectors        :' ); WriteInt( nos, 6 );
  
  FOR i:=1 TO nos DO
    FIO.ReadString( in, dummy );
    (* FIO.ReadInt(i); *)
    FIO.ReadString( in, stringInt );
    StrToInt( stringInt, i, convRes );
    WriteString( 'Read i : ' );
    WriteInt( i, 6 );
    WriteLn;
    
    FIO.ReadString( in, dummy );
    (* FIO.ReadInt( in, noti[i] ); *)
    FIO.ReadString( in, stringInt );
    StrToInt( stringInt, noti[i], convRes );
    WriteString( 'Read noti[i] : ' );
    WriteInt( noti[i], 6 );
    WriteLn;
    FIO.ReadString( in, dummy );
    
    (* FIO.ReadReal( in, sectdur[i] );*)
    FIO.ReadString( in, stringReal );
    StrToReal( stringReal, sectdur[i] );
    WriteString( 'Read sectdur[i] : ' );
    WriteReal( sectdur[i], 15, 14 );WriteLn;
    
    FIO.ReadString( in, dummy );
    FIO.ReadString( in, fnames[i] );
    WriteString( 'Read fnames[i] : ' );WriteString( fnames[i] );WriteLn;
    
    FIO.ReadString( in, dummy );
    (* FIO.ReadReal( in, statoff[i] ); *)
    FIO.ReadString( in, stringReal );
    StrToReal( stringReal, statoff[i] );
    WriteString( 'Read statoff[i] : ' );
    WriteReal( statoff[i], 15, 14 );
    WriteLn;
    
    
    FIO.ReadString( in, dummy );
    (* FIO.ReadReal( in, x0[i] );*)
    FIO.ReadString( in, stringReal );
    StrToReal( stringReal, x0[i] );
    WriteString( 'Read x0[i] : ' );
    WriteReal( x0[i], 15, 14 );
    WriteLn;
    
    
    FIO.ReadString( in, dummy );
    (* FIO.ReadReal( in, dx[i] ); *)
    FIO.ReadString( in, stringReal );
    StrToReal( stringReal, dx[i] );
    WriteString( 'Read dx[i] : ' );
    WriteReal( dx[i], 15, 14 );
    WriteLn;
    
    FIO.ReadString( in, dummy );
    (* FIO.ReadReal( in, gx[i] ); *)
    FIO.ReadString( in, stringReal );
    StrToReal( stringReal, gx[i] );
    WriteString( 'Read gx[i] : ' );
    WriteReal( gx[i], 15, 14 );
    WriteLn;
    
    FIO.ReadString( in, dummy );
    (* FIO.ReadReal( in, y0[i] ); *)
    FIO.ReadString( in, stringReal );
    StrToReal( stringReal, y0[i] );
    WriteString( 'Read y0[i] : ' );
    WriteReal( y0[i], 15, 14 );
    WriteLn;
    
    
    
    FIO.ReadString( in, dummy );
    (* FIO.ReadReal( in, dy[i] );*)
    FIO.ReadString( in, stringReal );
    StrToReal( stringReal, dy[i] );
    WriteString( 'Read dy[i] : ' );
    WriteReal( dy[i], 15, 14 );
    WriteLn;
    
    FIO.ReadString( in, dummy );
    (* FIO.ReadReal( in, gy[i] ); *)
    FIO.ReadString( in, stringReal );
    StrToReal( stringReal, gy[i] );
    WriteString( 'Read gy[i] : ' );
    WriteReal( gy[i], 15, 14 );
    WriteLn;
    
    FIO.ReadString( in, dummy );
    (* FIO.ReadReal( in, z0[i] ); *)
    FIO.ReadString( in, stringReal );
    StrToReal( stringReal, z0[i] );
    WriteString( 'Read z0[i] : ' );
    WriteReal( z0[i], 15, 14 );
    WriteLn;
    
    FIO.ReadString( in, dummy );
    (* FIO.ReadReal( in, dz[i] ); *)
    FIO.ReadString( in, stringReal );
    StrToReal( stringReal, dz[i] );
    WriteString( 'Read dz[i] : ' );
    WriteReal( dz[i], 15, 14 );
    WriteLn;
    
    FIO.ReadString( in, dummy );
    (* FIO.ReadReal( in, gz[i] ); *)
    FIO.ReadString( in, stringReal );
    StrToReal( stringReal, gz[i] );
    WriteString( 'Read gz[i] : ' );
    WriteReal( gz[i], 15, 14 );
    WriteLn;
    
    FIO.ReadString( in, dummy );
    FIO.ReadString( in, remarks[i] );
    WriteString( 'Read remark : ' );
    WriteString( remarks[i] );
    WriteLn;
    
  END;
  FIO.ReadString( in, resultflnm );
  WriteString( 'Read resultflnm : ' );WriteString( resultflnm );WriteLn;
  FIO.Close( in );
  
END ReadPulseSeqFile;    
   

PROCEDURE SpinSpinCoupling;
VAR k,l,count:INTEGER;
BEGIN
  (* ScreenMode(3); *)
  IF Nspins > 1 THEN
    WriteString('1. Weak-Coupling approximation. ');WriteLn;
    WriteString('2. Exact calculation. ');WriteLn;
    WriteString(' Your choice [1,2] : ');ReadInt(Jexact);WriteLn;
    WriteString('Give all Scalar coupling constants J(i,j)');WriteLn;
    WriteString('from the term J(i,j).I(i).S(j)');WriteLn;WriteLn;
    Jcount:=0;
    FOR k:=1 TO Nspins DO
      FOR l:= 1 TO Nspins DO
        IF l>k THEN
          Jcount:=Jcount+1;
          WriteString('Scalar Coupling Constant J(');WriteInt(k,1);
          WriteString(',');WriteInt(l,1);WriteString(')  = ');  
          ReadReal(couplings[Jcount]);WriteLn;WriteLn;
          couplings[Jcount]:=couplings[Jcount]; (* ! *)
        END;
      END;
    END;
  END;
END SpinSpinCoupling;

PROCEDURE MakeZeeman; 
  VAR 
    i: INTEGER; 
BEGIN 
  FOR i := 1 TO Nspins DO 
    WriteLn;
    WriteString( "Offset-frequency [Hz] of Spin ("); 
    WriteInt( i,3); 
    WriteString( ") = "); 
    ReadReal(resofreqs[i]);
    resofreqs[i]:=resofreqs[i];
  END;
END MakeZeeman; 

PROCEDURE Ispins; 
VAR 
    n,k,Inumber,Nstmax,Nspmax:INTEGER; 
    klaar: BOOLEAN;     
BEGIN
  (* ScreenMode(3); *)
  WriteLn;WriteLn;WriteLn; 
  WriteString('Definition of the spinsystem: '); 
  Nstmax:=64;Nspmax:=6;
  klaar:=FALSE; 
  WHILE ( NOT klaar) DO 
    klaar:=TRUE; 
    FOR k:=1 TO Nspmax DO 
      quantnum[k]:=0;
    END;
    REPEAT
      WriteLn;
      WriteString( "Number of Spins = "); 
      ReadInt(Nspins); 
    UNTIL (Nspins <= Nspmax); 
    FOR k:=1 TO Nspins DO 
      WriteLn;
      WriteString( "I quantum number of spin("); 
      WriteInt( k,3); 
      WriteString( ") = x/2  with   x = "); 
      ReadInt(Inumber); 
      quantnum[k] := Inumber; 
    END; 
    n:=1; 
    FOR k:=1 TO Nspins DO 
      n:=(quantnum[k]+1)*n
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
VAR i: INTEGER; 
BEGIN 
  FOR i := 1 TO Nspins DO 
    WriteLn;
    WriteString( "Relative Intensity of Spin ("); 
    WriteInt( i,3); 
    WriteString( ") = "); 
    ReadReal(relspinamp[i]);WriteLn;
  END;
END MakeBoltzmann; 


PROCEDURE DefineSeqSector(sector:INTEGER);
VAR ap,answer,i:INTEGER;
    ti:REAL;
BEGIN
  (* ScreenMode(3); *)
  WriteLn;WriteLn;
  WriteString('You are editing pulse sequence sector : ');
  WriteInt(sector,5);
  WriteLn;WriteLn;
  WriteString('Which form has the Pulse Hamiltonian ?');
  WriteLn;WriteLn;
    WriteString('    1. Hardpulse (90 x,y,-x-y and 180 x,y,-x,-y are available');WriteLn; 
    WriteString('    2. Hp(t) = f(t).Ix   (AM modulated) ');WriteLn;
    WriteString('    3. Hp(t) = f(t).Ix + g(t).Iy (FM or adiabatic)');WriteLn;
    WriteString('    4. Pulse file (Bx,By) from disk');WriteLn;
    WriteString('    5. Pulse file (Bx,Bz) from disk');WriteLn;
    WriteString('    6. Waiting time (free precession)');WriteLn;
    WriteLn;
    WriteString(' Your choice is [1..6] : ');
    ReadInt(answer);WriteLn;
    IF (answer#1) AND (answer#6) THEN  
      WriteString('Number of integration time intervals : ');
      ReadInt(noti[sector]);
      ap:=noti[sector];
      WriteLn;
      WriteString('Total sector duration [ms] : ');
      ReadReal(sectdur[sector]);
      sectdur[sector]:=sectdur[sector]/1000.0;
      WriteLn;
      ti:=sectdur[sector]/FLOAT(noti[sector]);
    ELSE 
      ti:=1.0E-5;ap:=1;  
    END;
    CASE answer OF 
      1:AMHardPulseDefinition(ap,ti,xb,yb);
        noti[sector]:=1;sectdur[sector]:=1.0E-5;offset[0]:=0.0;|
      2:AMPulseDefinition(ap,ti,xb,yb);
        FOR i:=0 TO ap-1 DO
          offset[i]:=0.0;
        END;|
      3:FMPulseDefinition(ap,ti,xb,yb);            
        FOR i:=0 TO ap-1 DO
          offset[i]:=0.0;
        END;|
      4:ReadBxByPulse(ap,xb,yb,offset);|
      5:ReadBxBzPulse(ap,xb,yb,offset);|
      6:WriteString('Waiting-time (free precession time) [ms] : ');
        ReadReal(sectdur[sector]);noti[sector]:=1;
        sectdur[sector]:=sectdur[sector]/1000.0;
        xb[0]:=0.0;yb[0]:=0.0;offset[0]:=0.0;
    END;
  WriteLn;
  WriteString('Static offset frequency is [Hz] : ');
  ReadReal(statoff[sector]);WriteLn;

  WriteString('Offset x-coordinate  : ');
  ReadReal(x0[sector]);WriteLn;
  WriteString('Spatial stepsize dx  : ');
  ReadReal(dx[sector]);WriteLn;
  WriteString('Define X-gradient strength  : ');
  CalcGradientFields(ap,ti,gx);
  
  WriteString('Offset y-coordinate  : ');
  ReadReal(y0[sector]);WriteLn;
  WriteString('Spatial stepsize dy  : ');
  ReadReal(dy[sector]);WriteLn;
  WriteString('Define Y-gradient    : ');
  CalcGradientFields(ap,ti,gy);
  
  WriteString('Offset z-coordinate  : ');
  ReadReal(z0[sector]);WriteLn;
  WriteString('Spatial stepsize dz  : ');
  ReadReal(dz[sector]);WriteLn;
  WriteString('Define Z-gradient    : ');
  CalcGradientFields(ap,ti,gz);

  WriteString('File name of this sectors RF and gradient modulation : ');
  ReadString(fnames[sector]);
  BinArrayOut(ap,xb,yb,offset,gx,gy,gz,fnames[sector]);WriteLn;
  WriteString('Remark (max. 20 characters)');
  ReadString(remarks[sector]);WriteLn;

END DefineSeqSector;

PROCEDURE ReadBxByPulse(ap:INTEGER;VAR xb,yb,offset:ARRAY OF REAL);
VAR i:INTEGER;
    mult:REAL;
    xmin_arg:REAL;
    xinc_arg:REAL;
BEGIN
  WriteLn;WriteLn;
  WriteString('File name Bx(t)');
  RealArrayIn(ap,xb,xmin_arg,xinc_arg);
  WriteString('File name By(t)');
  RealArrayIn(ap,yb,xmin_arg,xinc_arg);
  WriteLn;WriteLn;
  FOR i:=0 TO ap-1 DO
    offset[i]:=0.0;
  END;  
  WriteString('Multiplication factor : ');ReadReal(mult);
  FOR i:=0 TO ap-1 DO 
    xb[i]:=xb[i]*mult;
    yb[i]:=yb[i]*mult;
  END;  
END ReadBxByPulse;

PROCEDURE ReadBxBzPulse(ap:INTEGER;VAR xb,yb,offset:ARRAY OF REAL);
VAR i:INTEGER;
    mult:REAL;
    xmin_arg:REAL; 
    xinc_arg:REAL;
BEGIN
  WriteLn;WriteLn;
  WriteString('Name RF amplitude file Bx(t)');WriteLn;
  RealArrayIn(ap,xb,xmin_arg,xinc_arg);
  WriteString('Multiplication factor RF amplitude : ');ReadReal(mult);
  FOR i:=0 TO ap-1 DO 
    xb[i]:=xb[i]*mult;
  END;  
  WriteString('Name of RF offset frequency file dw(t)');WriteLn;
  RealArrayIn(ap,offset,xmin_arg,xinc_arg);
  WriteString('Multiplication factor dw(t) : ');ReadReal(mult);
  FOR i:=0 TO ap-1 DO 
    offset[i]:=offset[i]*mult;
  END;  
  FOR i:=0 TO ap-1 DO
    yb[i]:=0.0;
  END;  
END ReadBxBzPulse;

PROCEDURE BinArrayOut(ap:INTEGER;xb,yb,offset,gx,gy,gz:ARRAY OF REAL;name:smallstring);
VAR address1:ADDRESS;
    out:FIO.File;
    done:BOOLEAN;
    nbytes:CARDINAL;
    written:CARDINAL;
BEGIN
  nbytes:=8*ap;

  (* Create(out,name,done); *)
  out := FIO.OpenToWrite( name );
  address1:=ADR(xb[0]);
  written := FIO.WriteNBytes(out,nbytes,address1);
  address1:=ADR(yb[0]);
  written := FIO.WriteNBytes(out,nbytes,address1);
  address1:=ADR(offset[0]);
  written := FIO.WriteNBytes(out,nbytes,address1);
  address1:=ADR(gx[0]);
  written := FIO.WriteNBytes(out,nbytes,address1);
  address1:=ADR(gy[0]);
  written := FIO.WriteNBytes(out,nbytes,address1);
  address1:=ADR(gz[0]);
  written := FIO.WriteNBytes(out,nbytes,address1);
  FIO.Close(out);

END BinArrayOut;  

PROCEDURE BinArrayOut2D(ap:INTEGER;xb,yb,offset:ARRAY OF REAL;name:smallstring);
VAR address1:ADDRESS;
    out:FIO.File;
    done:BOOLEAN;
    nbytes:CARDINAL;
    written:CARDINAL;
BEGIN
  nbytes:=8*ap;
  (* Create(out,name,done); *)
  out := FIO.OpenToWrite( name );
  address1:=ADR(xb[0]);
  written := FIO.WriteNBytes(out,nbytes,address1);
  address1:=ADR(yb[0]);
  written := FIO.WriteNBytes(out,nbytes,address1);
  address1:=ADR(offset[0]);
  written := FIO.WriteNBytes(out,nbytes,address1);
  FIO.Close(out);
END BinArrayOut2D;  

PROCEDURE Initialize;
VAR
BEGIN
  nos:=0;nevo[0]:=1;
  systemset:=0;sectorset:=0;offsetset:=0;    
  Nspins:=0;ndx:=0;ndy:=0;ndz:=0;
  resultflnm:='result.bin'
END Initialize;  
  
END PulseSequence.    
