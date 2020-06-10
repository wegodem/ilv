MODULE rename;

FROM StrIO IMPORT WriteString, WriteLn ;
FROM FileSystem IMPORT File, Response, Lookup, Rename, Close, SetWrite, Delete;

IMPORT FIO;

VAR
   f, s: File ;
BEGIN
  WriteString("Hello World"); WriteLn; 
  Lookup (f, "first.txt", TRUE) ;
  
  WriteString("Lookup ... passed ..."); WriteLn; 
   IF f.res = done
   THEN

      WriteString("Entered IF ... passed ..."); WriteLn; 
      SetWrite (f) ;
      WriteString("SetWrite ... passed ..."); WriteLn;
      FIO.WriteString (f.fio, "hello world") ;
      WriteString("FIO.WriteString ... passed ..."); WriteLn; 
      FIO.WriteLine (f.fio) ;
      WriteString("FIO.WriteLn ... passed ..."); WriteLn; 
      Close (f) ;
      WriteString("Close ... passed ..."); WriteLn; 
      Lookup (f, "first.txt", FALSE) ;
      WriteString("Lookup ... passed ..."); WriteLn; 
      Rename (f, 'second.txt') ;
      WriteString("Rename ... passed ..."); WriteLn; 
      Lookup (s, "second.txt", FALSE) ;
      WriteString("Lookup (2) ... passed ..."); WriteLn;
      IF s.res = done
      THEN
         WriteString("s.res = done ... passed ..."); WriteLn;
	 Delete( "second.txt", s );
         WriteString("second.txt is deleted again ... passed ..."); WriteLn;
         HALT ( 0 );
      END
   ELSE
      WriteString("At rename end of program");WriteLn;
      HALT
   END
END rename.

