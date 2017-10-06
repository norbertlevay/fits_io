 -- writes Header to position given by HDU
 -- this func always creates new temporary FitsFile assuming
 -- that size of Headers do not match and so Blocks
 -- need to be shifted. After succesful copy, temp file is
 -- renamed back to name of the original fits file
 procedure Write_Header_To_New_FitsFile ( InFitsFile : in File_Type;
                                          HDU        : in HDU_Position_Type;
                                          Header     : in String )
 is
  OutFitsName : String := Name(InFitsFile) & ".part";
  OutFitsFile : File_Type;
  OutFitsStreamAccess  : Stream_Access;
  InFitsStreamAccess   : Stream_Access := Stream(InFitsFile);

  type Buffer_Type is new String(1 .. BlockSize);
  Buffer : Buffer_Type;

  Succeeded : Boolean := False;
 begin

  Create (File => OutFitsFile,
          Mode => Out_File,
          Name => OutFitsName);
  OutFitsStreamAccess := Stream(OutFitsFile);

  -- copy from begining until Header starts
  Set_Index(InFitsFile,1);
  loop
   exit when HDU.Header_Index = Index(OutFitsFile);
   Buffer_Type'Read(InFitsStreamAccess,Buffer);
   Buffer_Type'Write(OutFitsStreamAccess,Buffer);
  end loop;

  -- write new Header
  String'Write(OutFitsStreamAccess,Header);
  -- skip old Header
  Set_Index(InFitsFile,HDU.Data_Index);

  -- copy the rest after Header
  while not End_OF_File(InFitsFile)
  loop
   Buffer_Type'Read( InFitsStreamAccess, Buffer);
   Buffer_Type'Write(OutFitsStreamAccess,Buffer);
  end loop;

  Close(OutFitsFile);

  -- rename <filename>.fits.part -> <filename>.fits
  GNAT.OS_Lib.Rename_File (OutFitsName, Name(InFitsFile), Succeeded);
  if not Succeeded then
    null;
    -- raise exception
  end if;
 end Write_Header_To_New_FitsFile;

