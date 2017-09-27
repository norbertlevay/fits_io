
with Ada.Text_IO,-- Ada.Integer_Text_IO,
     Ada.Strings.Fixed,
     Ada.Streams.Stream_IO,
     GNAT.OS_Lib,
     FITS_IO;

use  Ada.Streams.Stream_IO;

package body Commands_IO is

 --
 -- read header from file and print to stdout
 --
 procedure FITSIO_Print_Header( FileName : in String;
                                HDU_Num  : Positive := 1 )
 is
   File : FITS_IO.FITS_File_Type;
 begin
   FITS_IO.Open(File, FITS_IO.In_File, FileName);
   declare
     Header : FITS_IO.Header_Type := FITS_IO.Read(File,HDU_Num);
   begin
     for I in Header'Range
     loop
         Ada.Text_IO.Put_Line( FITS_IO.Card.To_String(Header(I)) );
     end loop;
   end;
   FITS_IO.Close(File);
 end FITSIO_Print_Header;

 --
 -- count lines in Header file
 --
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
         if Dummy'Length >= 3 then
           ENDFound := (Dummy(1..3) = "END");
         end if;
        end;
        NoLines := NoLines + 1;
    end loop;
    Ada.Text_IO.Close(FileHandle);
  return NoLines;
 end NoOfLines;

 --
 -- Convert Header from text file into Header_Type
 --
 function Read_HeaderFromTextFile( FileName : in String )
  return FITS_IO.Header_Type
 is
   FileHandle : Ada.Text_IO.File_Type;
   NoLines : Positive := NoOfLines(FileName); -- constraint exception if NoLines = 0
   Header  : FITS_IO.Header_Type(1 .. NoLines);
 begin
   Ada.Text_IO.Open( FileHandle, Ada.Text_IO.In_File, FileName );
   for I in Header'Range loop
      Header(I) := FITS_IO.Card.To_Bounded_String(Ada.Text_IO.Get_Line( FileHandle ));
   end loop;
   Ada.Text_IO.Close(FileHandle);
   return Header;
 end Read_HeaderFromTextFile;

 function Size( Header : FITS_IO.Header_Type ) return Natural
 -- returns Header size in Blocks
 is
 begin
  if 0 = Header'Length then
   return 0;
  else
   return 1 + (Header'Length - 1) / 36; -- FIXME CardsCntInBlock;
  end if;
 end Size;


 --
 -- write header to file
 --
 procedure Write_Header( FitsFileName   : in String;
 			 HeaderFileName : in String;
                         HDU_Num        : Positive := 1 )
 is
   InHDUHandle : FITS_IO.FITS_File_Type;
   Header : FITS_IO.Header_Type := Read_HeaderFromTextFile( HeaderFileName );
   HeaderSizeInBlocks       : Positive := Size(Header);
--   InFileHeaderSizeInBlocks : Positive;
 begin
   FITS_IO.Open ( InHDUHandle, FITS_IO.Inout_File, FitsFileName );
   FITS_IO.Write( InHDUHandle, Header, HDU_Num  );-- FIXME switches mode internally for write-without-truncate
   FITS_IO.Close( InHDUHandle );
 end Write_Header;

-- procedure Write_Header( FitsFileName   : in String;
-- 			 HeaderFileName : in String;
--                         HDU_Num        : Positive := 1 )
-- is
--   InHDUHandle : HDU_Type;
--   Header : Header_Type := Read_HeaderFromTextFile( HeaderFileName );
--   HeaderSizeInBlocks       : Positive := Size(Header);
--   InFileHeaderSizeInBlocks : Positive;
-- begin
--
--   Open(InHDUHandle, In_HDU, FitsFileName, HDU_Num );
--  InFileHeaderSizeInBlocks := Header_Size(InHDUHandle);
--   Close(InHDUHandle);
--
--   Ada.Text_IO.Put_Line("Debug FileHSize va NewHSzie >" & Integer'Image(InFileHeaderSizeInBlocks) & " - " & Integer'Image(HeaderSizeInBlocks));
--
--   if InFileHeaderSizeInBlocks = HeaderSizeInBlocks
--   then
--     Ada.text_IO.Put_Line("DBG: SHORT Version " & FitsFileName );
--     -- new Header fits into empty space in file, simply write it there
--
--     Open ( InHDUHandle, Inout_HDU, FitsFileName, HDU_Num );
--    Write( InHDUHandle, Header );-- FIXME switches mode internally for write-without-truncate
--     Close( InHDUHandle );
--
--   else
--     Ada.text_IO.Put_Line("DBG: LOONG Version " & FitsFileName );
--
--     -- new HeaderSize and empty-space differ -> shrink or extend the file:
--
       -- create new file <filename>.fits.part
       -- copy from start until the Header
       -- write the new Header
       -- copy rest until end of file
       -- rename <filename>.fits.part -> <filename>.fits

--     declare
--       OutFitsName  : String := FitsFileName & ".part";
--       OutHDUHandle : HDU_Type;
--       Succeeded    : Boolean := False;
--       BeforeHeaderStart : Natural;
--       FirstDataBlock    : Positive;
--       FileEnd           : Positive;
--     begin
--
--       Open  ( InHDUHandle,  In_HDU,  FitsFileName, HDU_Num );
--       Create( OutHDUHandle, Out_HDU, OutFitsName , HDU_Num );
--
--       BeforeHeaderStart := Header_Index(InHDUHandle) - 1;
--       Ada.Text_IO.Put_Line("Debug From-To Block >" & Integer'Image(1) &" - " & Integer'Image(BeforeHeaderStart));
--       Copy_Blocks(InHDUHandle,1,BeforeHeaderStart, OutHDUHandle);
--
--       -- insert new header
--       Write(OutHDUHandle, Header);
--       -- skip old header
--       FirstDataBlock := Data_Index(InHDUHandle);
--
--       FileEnd := Size(InHDUHandle);
--       Ada.Text_IO.Put_Line("Debug From-To Block >" & Integer'Image(FirstDataBlock) &" - " & Integer'Image(FileEnd));
--       Copy_Blocks(InHDUHandle,FirstDataBlock,FileEnd, OutHDUHandle);
--
--
--       Close(OutHDUHandle);
--       Close(InHDUHandle);
--
--       -- rename <filename>.fits.part -> <filename>.fits
--       GNAT.OS_Lib.Rename_File (OutFitsName, FitsFileName, Succeeded);
--       if not Succeeded then
--         null;
--         -- FIXME raise exception
--       end if;
--     end;

--   end if;

-- end Write_Header;

 procedure Print_Struct (FitsFileName : in String)
 is
  FitsFile : FITS_IO.FITS_File_Type;
  procedure print_HDUInfo (HDUInfo : FITS_IO.HDU_Info_Type; Index : Positive)
  is
      FreeSlotCnt : Natural;
  begin
       -- calc free slots
       FreeSlotCnt := 36 - (HDUInfo.CardsCnt mod 36);
       -- mod is 0 when Block has 36 cards e.g. is full
       if FreeSlotCnt = 36 then
        FreeSlotCnt := 0;
       end if;

       Ada.Text_IO.Put("HDUVect HDU#" & Integer'Image(Index) );
       Ada.Text_IO.Put("   Cards: " &
                       Ada.Strings.Fixed.Tail(Integer'Image(HDUInfo.CardsCnt),5,' ') &
                       " EmptyCardSlots: " &
                       Ada.Strings.Fixed.Tail(Integer'Image( FreeSlotCnt ),2,' ') );

       if HDUInfo.Naxes > 0 then
        Ada.Text_IO.Put("   Data: "  & Ada.Strings.Fixed.Head( FITS_IO.BITPIX_Type'Image(HDUInfo.Data),8,' ') );
        Ada.Text_IO.Put(" ( ");
        for J in 1 .. (HDUInfo.Naxes - 1)
         loop
          Ada.Text_IO.Put(Fits_IO.Count'Image(HDUInfo.Naxis(J)) & " x " );
        end loop;
        Ada.Text_IO.Put(Fits_IO.Count'Image(HDUInfo.Naxis(HDUInfo.Naxes)));
        Ada.Text_IO.Put_Line(" ) ");
       end if;
  end print_HDUInfo;
 begin
   FITS_IO.Open(FitsFile,FITS_IO.In_File,FitsFileName);
   FITS_IO.List_HDUInfo (FitsFile, print_HDUInfo'Access);
   FITS_IO.Close(FitsFile);
 end Print_Struct;

end Commands_IO;

