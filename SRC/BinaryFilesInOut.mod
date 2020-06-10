IMPLEMENTATION MODULE BinaryFilesInOut;

FROM InOut IMPORT WriteLn,WriteString,ReadCard,ReadString,ReadInt,WriteInt;
(* FROM Graphics IMPORT ScreenMode; *)
FROM Delay IMPORT Delay;

FROM Strings IMPORT ConCat;
FROM SYSTEM IMPORT ADDRESS,ADR;

FROM FileSystem IMPORT File, Response, Lookup, Rename, Close, 
                       SetWrite, Delete, WriteNBytes, ReadNBytes;
IMPORT FIO;

(* 21-12-2019 - Conversion to gm2 not complete, but file compiles! *)

  PROCEDURE BinFileActions(VAR ndw,n:INTEGER);
    TYPE  String = ARRAY[0..15] OF CHAR;
    VAR choice,nbytes,read,written:CARDINAL;
      i,j,dummy,dummy1:INTEGER;
      (* result:DirResult; *)
      binflnm,copy:String;
      done:BOOLEAN;
  BEGIN
    (* ScreenMode(3); *)
    WriteString('Binary File Manipulation');WriteLn;WriteLn;
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
        1: (* ScreenMode(3); *)
          WriteString('Copy dm.bin to {Filename}');WriteLn;WriteLn;
          WriteString('{Filename} = ');ReadString(binflnm);
          (* DosCommand('copy dm.bin',binflnm,done); *)
          Delay(2000);|
        2:WriteString('Copy {Filename} to dm.bin');WriteLn;WriteLn;
          WriteString('{Filename} = ');ReadString(binflnm);|
          (* Delete('dm.bin',result); *)
          (* Concat('copy ',binflnm,copy);*)
          (* DosCommand(copy,'dm.bin',done); *)              
        3:(* ScreenMode(3); *)
          (* Run('C:\DOS\D.COM','*.bin',done); *)
          ReadInt(dummy1);|
        4:(* ScreenMode(3); *)
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
  
  PROCEDURE ReadBinFileHeader(fn:FIO.File;VAR type,num1,num2: INTEGER);
  VAR address1:ADDRESS;
      nbytes: CARDINAL;
      read: CARDINAL;
  BEGIN
    WriteString("Reading binary file header ..");WriteLn;
    nbytes:=4;
    address1:=ADR(type);
    
    (* read := ReadNBytes(fn,nbytes,address1); *)
    read := FIO.ReadNBytes( fn, nbytes, address1 );

    IF read#nbytes THEN
      WriteString('>>> READ BINARY FILE HEADER ERROR <<<');Delay(1000);WriteLn;
    END;
    IF read = nbytes THEN
      WriteString('file type id = ');WriteInt(type,4);WriteLn;
      (* WriteString('>>> READ BINARY FILE HEADER OK <<<');Delay(1000);WriteLn; *)
    END;

    address1:=ADR(num1);
    (* read := ReadNBytes(fn,nbytes,address1); *)
    read := FIO.ReadNBytes( fn, nbytes, address1 );

    IF read # nbytes THEN
      WriteString('>>> READ BINARY FILE HEADER ERROR <<<');Delay(1000);WriteLn;
    END;
    IF read = nbytes THEN
      WriteString('header value 1 = ');WriteInt(num1,4);WriteLn;
      (* WriteString('>>> READ BINARY FILE HEADER OK <<<');Delay(1000);WriteLn; *)
    END;

    address1:=ADR(num2);
    (* read := ReadNBytes(fn,nbytes,address1); *)
    read := FIO.ReadNBytes( fn, nbytes, address1 );
   
    IF read # nbytes THEN
      WriteString('>>> READ BINARY FILE HEADER ERROR <<<');Delay(1000);WriteLn;
    END;
    IF read = nbytes THEN
      WriteString('header value 2 = ');WriteInt(num2,4);WriteLn;
      (* WriteString('>>> READ BINARY FILE HEADER OK <<<');Delay(1000);WriteLn; *)
    END;
  END ReadBinFileHeader;


  PROCEDURE WriteBinFileHeader(fn:FIO.File;type,num1,num2: INTEGER);
  VAR address1: ADDRESS;
      nbytes:   CARDINAL;
      written:  CARDINAL;
  BEGIN
    WriteString("Writing binary file header ..");WriteLn;
    nbytes:=4;
    address1:=ADR(type);
    (* written := WriteNBytes(fn,nbytes,address1); *)
    written := FIO.WriteNBytes(fn,nbytes,address1);
    IF written#nbytes THEN
      WriteString('>>> WRITE BINARY FILE HEADER ERROR <<<');Delay(1000);WriteLn;
    END;
    IF written = nbytes THEN
      WriteString('Written type = ');WriteInt(type,4);WriteLn;
      (* WriteString('>>> WRITE BINARY FILE HEADER OK <<<');Delay(1000);WriteLn; *)
    END;

    address1:=ADR(num1);
    (* written := WriteNBytes(fn,nbytes,address1); *)
    written := FIO.WriteNBytes(fn,nbytes,address1);
    IF written#nbytes THEN
      WriteString('>>> WRITE BINARY FILE HEADER ERROR <<<');Delay(1000);WriteLn;
    END;
    IF written = nbytes THEN
      WriteString('Written num1 = ');WriteInt(num1,4);WriteLn;
      (* WriteString('>>> WRITE BINARY FILE HEADER OK <<<');Delay(1000);WriteLn; *)
    END;

    address1:=ADR(num2);
    (* written := WriteNBytes(fn,nbytes,address1); *)
    written := FIO.WriteNBytes(fn,nbytes,address1);
    IF written#nbytes THEN
      WriteString('>>> WRITE BINARY FILE HEADER ERROR <<<');Delay(1000);WriteLn;
    END;
    IF written = nbytes THEN
      WriteString('Written num2 = ');WriteInt(num2,4);WriteLn;
      (* WriteString('>>> WRITE BINARY FILE HEADER OK <<<');Delay(1000);WriteLn; *)
    END;

  END WriteBinFileHeader;
 
END BinaryFilesInOut.
