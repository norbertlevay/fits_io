
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

   Open(HDU, In_HDU, FileName, HDU_Num);

   -- print the Header
   declare
     HeaderBlocks : HeaderBlocks_Type := Read(HDU);
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

 --
 -- Convert Header from text file into HeaderBlocks_Type
 --
 function Read_HeaderFromTextFile( FileName : in String )
  return HeaderBlocks_Type
 is
   FileHandle     : Ada.Text_IO.File_Type;
   FitsCardBuffer : CardBuffer := EmptyCard;

   NoLines : Positive := NoOfLines(FileName); -- constraint exception if NoLines = 0
   HeaderSizeInBlocks : Positive := (NoLines - 1)/ CardsCntInBlock + 1;
   HeaderBlocks : HeaderBlocks_Type(1..HeaderSizeInBlocks) := (others => EmptyBlock);
   CardsCount : Positive := 1;
   IxBlock : Positive;
   IxCard : Positive;
 begin
   Ada.Text_IO.Open( FileHandle, Ada.Text_IO.In_File, FileName );
   loop
      exit when (FitsCardBuffer(1..3) = "END");
      FitsCardBuffer := To_CardBuffer(Ada.Text_IO.Get_Line( FileHandle ));
      IxBlock := (CardsCount - 1) / CardsCntInBlock   + 1;
      IxCard  := (CardsCount - 1) mod CardsCntInBlock + 1;
      HeaderBlocks(IxBlock)(IxCard) := FitsCardBuffer;
      CardsCount := CardsCount + 1;
   end loop;
   Ada.Text_IO.Close(FileHandle);
   return HeaderBlocks;
 end Read_HeaderFromTextFile;

 --
 -- write header to file
 --
 procedure Write_Header( FitsFileName   : in String;
 			 HeaderFileName : in String;
                         HDU_Num        : Positive := 1 )
 is
   BitsInByte : Positive := 8;--FIXME get this from System.xxxx
   InHDUHandle  : HDU_Type;
   HeaderBlocks : HeaderBlocks_Type := Read_HeaderFromTextFile( HeaderFileName );
   InFileHeaderSizeInBlocks : Positive;
 begin
--   Ada.text_IO.Put_Line("DBG: " & FitsFileName );

   Open(InHDUHandle, In_HDU, FitsFileName );
   InFileHeaderSizeInBlocks := Header_Size(InHDUHandle);--Positive(Header_Size(InHDUHandle)) / BlockSize;
   Close(InHDUHandle);

   Ada.Text_IO.Put_Line("Debug FileHSize va NewHSzie >" & Integer'Image(InFileHeaderSizeInBlocks) &" - " & Integer'Image(HeaderBlocks'Length));

   if InFileHeaderSizeInBlocks = HeaderBlocks'Length
   then
     Ada.text_IO.Put_Line("DBG: SHORT Version " & FitsFileName );
     -- new Header fits into empty space in file, simply write it there

     Open( InHDUHandle, Inout_HDU, FitsFileName );
     Write ( InHDUHandle, HeaderBlocks );-- FIXME switches mode internally for write-without-truncate
     Close(InHDUHandle);

   else
     Ada.text_IO.Put_Line("DBG: LOONG Version " & FitsFileName );

     -- new HeaderSize and empty-space differ -> shrink or extend the file:

       -- create new file <filename>.fits.part
       -- copy from start until the Header
       -- write the new Header
       -- copy rest until end of file
       -- rename <filename>.fits.part -> <filename>.fits

     declare
       OutFitsName  : String := FitsFileName & ".part";
       OutHDUHandle : HDU_Type;
       Succeeded    : Boolean := False;
       BeforeHeaderStart : Natural;
       FirstDataBlock   : Positive;
       FileEnd : Positive := Positive'Last / BlockSize;
       -- Copy_Blocks exits with end-of-file
     begin

       Open  ( InHDUHandle,  In_HDU,  FitsFileName );
       Create( OutHDUHandle, Out_HDU, OutFitsName );

       BeforeHeaderStart := Header_Index(InHDUHandle) - 1;
       Copy_Blocks(InHDUHandle,1,BeforeHeaderStart, OutHDUHandle);

       -- insert new header
       Write(OutHDUHandle, HeaderBlocks);
       -- skip old header
       FirstDataBlock := Data_Index(InHDUHandle);

       Copy_Blocks(InHDUHandle,FirstDataBlock,FileEnd, OutHDUHandle);

--       Ada.Text_IO.Put_Line("Debug FromBlock ToBlock >" & Integer'Image(FromBlock) &" - " & Integer'Image(ToBlock));

       Close(OutHDUHandle);
       Close(InHDUHandle);

       -- rename <filename>.fits.part -> <filename>.fits
       GNAT.OS_Lib.Rename_File (OutFitsName, FitsFileName, Succeeded);
       if not Succeeded then
         null;
         -- FIXME raise exception
       end if;
     end;

   end if;

 end Write_Header;

end Commands;

