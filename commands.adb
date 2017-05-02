
with Ada.Text_IO, Ada.Integer_Text_IO,
     Ada.Streams.Stream_IO,
     GNAT.OS_Lib;

use  Ada.Streams.Stream_IO;

package body Commands is

 --
 -- read header from file and print to stdout
 --
 procedure Print_Header( FileName : in String;
                         HDU_Num  : Positive := 1 )
 is
   FileHandle : File_Type;
   HDU        : HDU_Type;
 begin

   Open(HDU, In_File, FileName, HDU_Num);

   -- print the Header
   declare
     HeaderBlocks : HeaderBlocks_Type := Get(HDU);
   begin

     for I in HeaderBlocks'Range
     loop

      for J in HeaderBlocks(I)'Range
      loop
       Ada.Integer_Text_IO.Put( I, 5 );
       Ada.Integer_Text_IO.Put( J, 5 );
       Ada.Text_IO.Put_Line(  " >"  & HeaderBlocks(I)(J)  & "<");
      end loop;

     end loop;

   end;

   Close(HDU);

 end Print_Header;

 --
 -- convert HeaderText -> HeaderBlocks
 --
 subtype CardBuffer is String(1 .. CardSize);

 function To_CardBuffer ( Item : in String )
 return CardBuffer is
  mycard : CardBuffer := (others => ' ');
 begin

   if Item'Length <= CardSize then
      for i in Item'Range 
      loop
       mycard(i) := Item(i);
      end loop;
   end if;

   return mycard;
 end To_CardBuffer;

 function NoOfLines( FileName : in String )
  return Positive
 is
  FileHandle     : Ada.Text_IO.File_Type;
  NoLines : Natural := 0;
  ENDFound : Boolean := False;
 begin
    Ada.Text_IO.Open( FileHandle, Ada.Text_IO.In_File, FileName );
    loop
      exit when ENDFound;
        declare
         Dummy : String := Ada.Text_IO.Get_Line( FileHandle );
         -- raises exception if no END-Line found but EOF reached
        begin
         ENDFound := (Dummy(1..3) = "END");
        end;
        NoLines := NoLines + 1;
    end loop;
    Ada.Text_IO.Close(FileHandle);
  return NoLines;
 end NoOfLines;

 function Read_HeaderFromTextFile( FileName : in String )
  return String
 is
   FileHandle     : Ada.Text_IO.File_Type;
   FitsCardBuffer : CardBuffer := (others => ' ');

   NoLines : Positive := NoOfLines(FileName); -- constraint exception if NoLines = 0
   HeaderSizeInBlocks : Positive := (NoLines - 1)/ CardsCntInBlock + 1;
   subtype Header_Type is String( 1 .. HeaderSizeInBlocks*BlockSize );
   Header : Header_Type := (others => ' ');

   from,to : Positive;
   Count : Positive := 1;
 begin
   Ada.Text_IO.Open( FileHandle, Ada.Text_IO.In_File, FileName );
   loop
      exit when (FitsCardBuffer(1..3) = "END");

      from := (Count - 1) * CardSize + 1;
      to   := from + CardSize - 1;
      FitsCardBuffer := To_CardBuffer(Ada.Text_IO.Get_Line( FileHandle ));

--      Ada.Text_IO.Put_Line("Debug Count    " & Integer'Image(Count));
--      Ada.Text_IO.Put_Line("Debug from     " & Integer'Image(from));
--      Ada.Text_IO.Put_Line("Debug to       " & Integer'Image(to));
--      Ada.Text_IO.Put_Line("Debug to - from " & Integer'Image(to-from));
--      Ada.Text_IO.Put_Line("Debug FitsCardBuffer " & Integer'Image(FitsCardBuffer'Size/8));

      Header( from .. to ) := FitsCardBuffer;
      Count := Count + 1;
   end loop;
   Ada.Text_IO.Close(FileHandle);
   return Header;
 end Read_HeaderFromTextFile;

 --
 -- write header to file
 --
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


 procedure Write_Header( FitsFileName   : in String;
 			 HeaderFileName : in String;
                         HDU_Num        : Positive := 1 )
 is
   BitsInByte : Positive := 8;--FIXME get this from System.xxxx
   InFileHandle  : File_Type;
   OutFileHandle : File_Type;
   HDU    : HDU_Position_Type;
   HeaderBlocks : String := Read_HeaderFromTextFile( HeaderFileName );
 begin
   Ada.text_IO.Put_Line("DBG: " & FitsFileName );

   -- first read where are HDUs
   Open(InFileHandle, In_File, FitsFileName );
   HDU := Parse_HDU_Positions ( InFileHandle , HDU_Num );

   -- now write
   if Positive(HDU.Header_Size) = (HeaderBlocks'Size / BitsInByte) then
--    Write_Header ( InFileHandle, HDU, HeaderBlocks );
    null;-- FIXME with Put()
   else
    Write_Header_To_New_FitsFile ( InFileHandle, HDU, HeaderBlocks );
   end if;

   Close(InFileHandle);

 end Write_Header;

end Commands;

