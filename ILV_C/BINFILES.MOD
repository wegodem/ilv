IMPLEMENTATION MODULE BinFiles;
FROM Exec IMPORT Run, DosCommand;
FROM InOut IMPORT WriteLn,WriteString,ReadCard,ReadString,ReadInt,WriteInt;
FROM Graphics IMPORT ScreenMode;
FROM Delay IMPORT Delay;
FROM Directories IMPORT Delete,DirResult;
FROM Strings IMPORT Concat;
FROM SYSTEM IMPORT ADDRESS,ADR;
FROM LogiFile IMPORT WriteNBytes,Create,File,Close,Open,ReadNBytes,
                     OpenMode,EndFile,Reset,GetPos,SetPos;

  PROCEDURE BinFileActions(VAR ndw,n:INTEGER);
    TYPE  String = ARRAY[0..15] OF CHAR;
    VAR choice,nbytes,read,written:CARDINAL;
      i,j,dummy,dummy1:INTEGER;
      result:DirResult;
      binflnm,copy:String;
      done:BOOLEAN;
  BEGIN
    ScreenMode(3);WriteString('Binary File Manipulation');WriteLn;WriteLn;
    WriteString('All density-matrices and rotation matrices are stored in binary files');
    WriteLn;
    WriteString('The density matrices of your last calculations are stored in "dm.bin"');
    WriteLn;WriteString('A next calculation will overwrite this file');WriteLn; 
    WriteLn;WriteLn;
    WriteString('Possible actions: ');WriteLn;WriteLn;
    WriteString('1. Copy dm.bin ');WriteLn;
    WriteString('2. Copy an existant binary file on hard-disk to dm.bin');WriteLn;
    WriteString('3. Give a list of all excistant binary files on hard disk');WriteLn;
    WriteString('4. Changes number of densmatrices and DIM|densmat|');WriteLn;
    WriteString('5. Quit Binary File Manipulation Menu');WriteLn;
    WriteString('Your choice [1..5] is : ');WriteLn;ReadCard(choice);
    IF choice # 5 THEN
      CASE choice OF
        1:ScreenMode(3);
          WriteString('Copy dm.bin to {Filename}');WriteLn;WriteLn;
          WriteString('{Filename} = ');ReadString(binflnm);
          DosCommand('copy dm.bin',binflnm,done);
          Delay(2000);|
        2:WriteString('Copy {Filename} to dm.bin');WriteLn;WriteLn;
          WriteString('{Filename} = ');ReadString(binflnm);
          Delete('dm.bin',result);
          Concat('copy ',binflnm,copy);
          DosCommand(copy,'dm.bin',done);|              
        3:ScreenMode(3);
          Run('C:\DOS\D.COM','*.bin',done);
          ReadInt(dummy1);|
        4:ScreenMode(3);
          WriteString('Actual number density matrices = ');WriteInt(ndw,5);WriteLn;
          WriteString('Actual dimension of density matrix = ');WriteInt(n,5);WriteLn;
          WriteString('Do you want to change this [1=yes, 2=no] : ');
          ReadCard(choice);
          WriteLn;
          IF choice = 1 THEN
            WriteString('Number of density matrices is = ');ReadInt(ndw);WriteLn;
            WriteString('Dimension of the matrices is = ');ReadInt(n);WriteLn;
          END;
      END;   
    END;
  END BinFileActions; 
  
  PROCEDURE ReadBinFileHeader(fn:File;VAR type,num1,num2: INTEGER);
  VAR address1:ADDRESS;
      nbytes,read:CARDINAL;
  BEGIN
    nbytes:=2;
    address1:=ADR(type);
    ReadNBytes(fn,address1,nbytes,read);
    IF read#nbytes THEN
      WriteString('>>> READ BINARY FILE ERROR <<<');Delay(1000);WriteLn;
    END;
    address1:=ADR(num1);
    ReadNBytes(fn,address1,nbytes,read);
    IF read#nbytes THEN
      WriteString('>>> READ BINARY FILE ERROR <<<');Delay(1000);WriteLn;
    END;
    address1:=ADR(num2);
    ReadNBytes(fn,address1,nbytes,read);
    IF read#nbytes THEN
      WriteString('>>> READ BINARY FILE ERROR <<<');Delay(1000);WriteLn;
    END;
  END ReadBinFileHeader;

  PROCEDURE WriteBinFileHeader(fn:File;type,num1,num2: INTEGER);
  VAR address1:ADDRESS;
      nbytes,written:CARDINAL;
  BEGIN
    nbytes:=2;
    address1:=ADR(type);
    WriteNBytes(fn,address1,nbytes,written);
    IF written#nbytes THEN
      WriteString('>>> WRITE BINARY FILE ERROR <<<');Delay(1000);WriteLn;
    END;
    address1:=ADR(num1);
    WriteNBytes(fn,address1,nbytes,written);
    IF written#nbytes THEN
      WriteString('>>> WRITE BINARY FILE ERROR <<<');Delay(1000);WriteLn;
    END;
    address1:=ADR(num2);
    WriteNBytes(fn,address1,nbytes,written);
    IF written#nbytes THEN
      WriteString('>>> WRITE BINARY FILE ERROR <<<');Delay(1000);WriteLn;
    END;
  END WriteBinFileHeader;
 
END BinFiles. 
