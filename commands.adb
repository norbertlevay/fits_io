
with Ada.Text_IO,-- Ada.Integer_Text_IO,
     Ada.Strings.Fixed,
     Ada.Streams.Stream_IO,
     GNAT.OS_Lib,
     FITS_IO;

use  Ada.Streams.Stream_IO;

package body Commands is

 --
 -- read header from file and print to stdout
 --
 procedure Print_Header( FileName : in String;
                         HDU_Num  : Positive := 1 )
 is
   FileHandle : Ada.Streams.Stream_IO.File_Type;
   HDU        : HDU_Type;
 begin
   Open(HDU, In_HDU, FileName, HDU_Num);
   declare
     Header : Header_Type := Read(HDU);
   begin
     for I in Header'Range
     loop
         Ada.Text_IO.Put_Line( SB.To_String(Header(I)) );
     end loop;
   end;
   Close(HDU);
 end Print_Header;

 procedure FITSIO_Print_Header( FileName : in String;
                                HDU_Num  : Positive := 1 )
 is
   File : FITS_IO.File_Type;
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
  return Header_Type
 is
   FileHandle : Ada.Text_IO.File_Type;
   NoLines : Positive := NoOfLines(FileName); -- constraint exception if NoLines = 0
   Header  : Header_Type(1 .. NoLines);
 begin
   Ada.Text_IO.Open( FileHandle, Ada.Text_IO.In_File, FileName );
   for I in Header'Range loop
      Header(I) := SB.To_Bounded_String(Ada.Text_IO.Get_Line( FileHandle ));
   end loop;
   Ada.Text_IO.Close(FileHandle);
   return Header;
 end Read_HeaderFromTextFile;

 --
 -- write header to file
 --
 procedure Write_Header( FitsFileName   : in String;
 			 HeaderFileName : in String;
                         HDU_Num        : Positive := 1 )
 is
   InHDUHandle : HDU_Type;
   Header : Header_Type := Read_HeaderFromTextFile( HeaderFileName );
   HeaderSizeInBlocks       : Positive := Size(Header);
   InFileHeaderSizeInBlocks : Positive;
 begin

   Open(InHDUHandle, In_HDU, FitsFileName, HDU_Num );
   InFileHeaderSizeInBlocks := Header_Size(InHDUHandle);
   Close(InHDUHandle);

   Ada.Text_IO.Put_Line("Debug FileHSize va NewHSzie >" & Integer'Image(InFileHeaderSizeInBlocks) & " - " & Integer'Image(HeaderSizeInBlocks));

   if InFileHeaderSizeInBlocks = HeaderSizeInBlocks
   then
     Ada.text_IO.Put_Line("DBG: SHORT Version " & FitsFileName );
     -- new Header fits into empty space in file, simply write it there

     Open ( InHDUHandle, Inout_HDU, FitsFileName, HDU_Num );
     Write( InHDUHandle, Header );-- FIXME switches mode internally for write-without-truncate
     Close( InHDUHandle );

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
       FirstDataBlock    : Positive;
       FileEnd           : Positive;
     begin

       Open  ( InHDUHandle,  In_HDU,  FitsFileName, HDU_Num );
       Create( OutHDUHandle, Out_HDU, OutFitsName , HDU_Num );

       BeforeHeaderStart := Header_Index(InHDUHandle) - 1;
       Ada.Text_IO.Put_Line("Debug From-To Block >" & Integer'Image(1) &" - " & Integer'Image(BeforeHeaderStart));
       Copy_Blocks(InHDUHandle,1,BeforeHeaderStart, OutHDUHandle);

       -- insert new header
       Write(OutHDUHandle, Header);
       -- skip old header
       FirstDataBlock := Data_Index(InHDUHandle);

       FileEnd := Size(InHDUHandle);
       Ada.Text_IO.Put_Line("Debug From-To Block >" & Integer'Image(FirstDataBlock) &" - " & Integer'Image(FileEnd));
       Copy_Blocks(InHDUHandle,FirstDataBlock,FileEnd, OutHDUHandle);


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

 procedure Print_Struct (FitsFileName : in String)
 is
  FitsFile : FITS_IO.File_Type;
 begin
   FITS_IO.Open(FitsFile,FITS_IO.In_File,FitsFileName);

   declare
    HDUInfoArr : FITS_IO.HDU_Info_Arr := FITS_IO.List_HDUInfo (FitsFile);
    FreeSlotCnt : Natural;
   begin
    for I in HDUInfoArr'Range
     loop
       -- calc free slots
       FreeSlotCnt := 36 - (HDUInfoArr(I).CardsCnt mod 36);
       -- mod is 0 when Block has 36 cards e.g. is full
       if FreeSlotCnt = 36 then
        FreeSlotCnt := 0;
       end if;

       Ada.Text_IO.Put("HDU#" & Integer'Image(I) );
       Ada.Text_IO.Put("   Cards: " &
                       Ada.Strings.Fixed.Tail(Integer'Image(HDUInfoArr(I).CardsCnt),5,' ') &
                       " EmptyCardSlots: " &
                       Ada.Strings.Fixed.Tail(Integer'Image( FreeSlotCnt ),2,' ') );

       if HDUInfoArr(I).Naxes > 0 then
        Ada.Text_IO.Put("   Data: "  & Ada.Strings.Fixed.Head( FITS_IO.BITPIX_Type'Image(HDUInfoArr(I).Data),8,' ') );
        Ada.Text_IO.Put(" ( ");
        for J in 1 .. (HDUInfoArr(I).Naxes - 1)
         loop
          Ada.Text_IO.Put(Fits_IO.Count'Image(HDUInfoArr(I).Naxis(J)) & " x " );
        end loop;
        Ada.Text_IO.Put(Fits_IO.Count'Image(HDUInfoArr(I).Naxis(HDUInfoArr(I).Naxes)));
        Ada.Text_IO.Put_Line(" ) ");
       end if;

    end loop;
   end;

   FITS_IO.Close(FitsFile);
 end Print_Struct;

end Commands;

