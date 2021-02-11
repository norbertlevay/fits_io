
with Ada.Text_IO;

with HDU;
with FITS_IO; use FITS_IO;
with Ada.Tags;
with V3_Types; use V3_Types;
--with Pool_For_Numeric_Type; use Pool_For_Numeric_Type;

package body FITS_IO.Serialize is

   package TIO renames Ada.Text_IO;


   -- check stream type, and call stream-specific implementation of 'Write 'Read attribs
   -- currently only File-stream supported

   procedure HDU_SRead
      (Stream : access  Ada.Streams.Root_Stream_Type'Class;
      Item : out T_Arr)
   is
      use type Ada.Tags.Tag;
   begin

      TIO.Put(" HDU_SRead::" );
      if(Stream.all'Tag = FITS_Stream_Type'Tag)
      then
         declare
            Last : Count;
            File : File_Type := File_Type(Stream);
            procedure iRead is new HDU.My_Read( T, T_Arr, "+", "+", Is_Undef,To_BITPIX);
            use type FITS.Count;
         begin
            TIO.Put("HDU_Stream" );
            iRead(File.SIO_File, File.PHDU, Item, Last);
            if(Last < Item'length) then TIO.Put("HDU_SRead: ERR End-Of-DataUnit"); end if;
         end;
      else
         TIO.Put("Stream" );
         T_Arr'Read(Stream, Item);
      end if;

   end HDU_SRead;


   procedure HDU_SWrite
      (Stream : access  Ada.Streams.Root_Stream_Type'Class;
      Item : T_Arr)
   is
      use type Ada.Tags.Tag;
   begin
       TIO.Put(" HDU_SWrite::" );

      if(Stream.all'Tag = FITS_Stream_Type'Tag)
      then
         declare
            File : File_Type := FITS_IO.File_Type(Stream);
            procedure iWrite is new HDU.My_Write( T, T_Arr, "+", "+", Is_Undef,To_BITPIX);
         begin
            TIO.Put("HDU_Stream" );
            iWrite(File.SIO_File, File.PHDU, Item);
         end;
      else
         TIO.Put("Stream" );
         T_Arr'Write(Stream, Item);
      end if;

   end HDU_SWrite;


end FITS_IO.Serialize;

