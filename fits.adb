--
-- Implementation notes:
--
-- FIXME shouldn't FitsFile : File_Type be 'in out' ? we update the Index...
--
-- Re-consider API:
--   Offset in Set_Index() should be in 'Read() 'Write().
--   Motivation:
--   concept of Block is part of the FITS standard.
--   This API would support read/write in blocks -
--   e.g. always guarantee naturally correct size and fill-in areas.
--   If Offset is in Set_Index only Stream_IO is supported.
--   Interfaces like Direct_IO(Block) are not.
--   E.g. such new API:
--   procedure Stream(File_Type; HDUNum) <- position to begining of HDU
--   procedure used for 'Read (FITSStream, FITS_Data_Type, Offset )
--   procedure used for 'Write(FITSStream, FITS_Data_Type, Offset )
--   Drawback:
--   Read/Write of DataUnit would need to calculate Header size
--   at first call of multiple writes. Successive writes can be
--   done directly by Stream: 'Write/'Read(FITSStream, Data).
--   After first read/write we are correctly positioned, and read/write
--   move the file pointer.
--   For multiple writes (reads) to (from) continuous area,
--   a client would do:
--   FIST_Data_Type'Write(FITS, Data(1), Offset); <-- func from FITS package with initial positioning
--   FIST_Data_Type'Write(FITS, Data(2) );        <-- func from Stream_IO, only write, no positioning
--   FIST_Data_Type'Write(FITS, Data(3) );
--   ... n-times
--

--with Ada.Streams.Stream_IO;
--use  Ada.Streams.Stream_IO;
-- 'use' needed for + operator on Count type and all its subtypes(?)

--with Ada.Text_IO; -- debug only

package body FITS is


   --
   -- convert BITPIX keyword from Header to internal FitsData_Type
   --
   function  To_FitsDataType (BITPIX : in Integer ) return FitsData_Type
   is
    bp : FitsData_Type;
   begin
    case BITPIX is
    when   8 => bp := Int8;
    when  16 => bp := Int16;
    when  32 => bp := Int32;
    when  64 => bp := Int64;
    when -32 => bp := Float32;
    when -64 => bp := Float64;
    when others =>
     null;
     -- FIXME ? raise exception "out of range"
    end case;
    return bp;
   end To_FITSDataType;

end FITS;

