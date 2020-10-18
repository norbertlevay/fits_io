--
-- FITS-lib packages in lib/src
--
-- Media management:
--
-- File package: does file management for FITS-files:
-- positioning in FITS-file, list content etc...
-- It buids on Ada.Streams.Stream_IO and so many
-- procedures accept Ada.Stream_IO.File_Type and File_Mode
-- as parameter.
-- Network: ...?
--
-- Data Access:
--
-- parser/Keqword_Record Image packages:
-- Once Ada-file Stream is successfully acquired,
-- serialization/deserialization into/from stream
-- happens simply by using Read Write attributes of
-- the predefined types, specifically <FitsData_Type>'Read and 'Write,
-- which is array of base Ada-types and so supported by
-- Ada GNAT implementation [FITS ?][GNAT ?].
--

with Ada.IO_Exceptions;
with Ada.Streams.Stream_IO;

with Ada.Strings.Bounded;

package FITS_IO is

   package SIO renames Ada.Streams.Stream_IO;

   type     Count          is new Ada.Streams.Stream_IO.Count;
   subtype  Positive_Count is Count range 1 .. Count'Last;

   subtype  NAXIS_Index is Integer range 1 .. 999;
   type     NAXIS_Array is array (NAXIS_Index range <>) of Positive_Count;

   -- card related
   package BS  renames Ada.Strings.Bounded;

   package BS_8 is new BS.Generic_Bounded_Length( 8);
   package BS70 is new BS.Generic_Bounded_Length(70);

   Null_Undefined_Value : constant BS70.Bounded_String
                                 := BS70.To_Bounded_String("");


   -- NOTE All Header section:  Scaling_Rec and Read_/Write_Header
   -- should by T-type independent:
   -- In Scaling_Rec use Floats or Strings for UndefVal
   -- String better: define Null_Undef := "" -> means no-Undef (no need for Boolean Valid flag)
   -- FIXME should the Scaling_Rec declaration be on FITS_IO then ?
                                 -- see it as result of Header read/write
                                 -- os see it as start of data unit access ??


   type Image_Data_Model(NAXIS_Last : Natural) is
      record
         BITPIX   : Integer;
         NAXISn   : NAXIS_Array(1..NAXIS_Last);
         Undef    : BS70.Bounded_String;
         Unit     : BS70.Bounded_String;
         A,B      : Float;
      end record;

   -- FIXME still missing Min,Max -> DATAMIN DATAMAX
   -- NOTE Min Max Undef -> must have format corresponding to type BITPIX=T

   -- FIXME Undef value when stored in Header as BLANK must have string-format of type BITPIX
   -- FIXME Undef value when read from BLANK must have string-format
   -- of type Image.BITPIX in Image_Data_Model - e.g. Undef must be in the same type as Data

   -- Data Unit length
   function Data_Element_Count(NAXISn : NAXIS_Array) return Count;


   -- "Low-level" API / Raw-valued Image

   -- for Write
   function To_Raw
      (Physical_Image : Image_Data_Model;
      Raw_BITPIX : Integer;
      Raw_Undef  : BS70.Bounded_String) -- caller's preference for Undef
      return Image_Data_Model;

   -- for Read
   function To_Physical
      (Raw_Image  : Image_Data_Model;
      Phys_BITPIX : Integer;
      Phys_Undef  : BS70.Bounded_String)-- caller's preference for Undef
      return Image_Data_Model;


   procedure Read_Raw_Header
     (File  : SIO.File_Type;
      Raw_Image : in out Image_Data_Model);

   procedure Write_Raw_Header
      (File  : SIO.File_Type;
       Raw_Image : Image_Data_Model);





   -- "High-level" API /Physical-valued Image


   -- Read_Header IS FULLY DEFINED
   -- Mem_BITPIX = func(T) (choice: use application dictates)
   -- -- Mem_Undef (optional override)
   procedure Read_Header
     (File  : SIO.File_Type;
      Image : in out Image_Data_Model);
   -- in out must supply Image.BITPIX

   -- Xfer params:
   -- Raw_BITPIX
   -- -- Raw_Undef (optional override)
   procedure Write_Header
      (File  : SIO.File_Type;
       Image : Image_Data_Model);


   -- State after Read/Write_Header:
   -- * both Raw & Physical Image_DataModel were established   <-- needed to Read/Write Data_Unit (Scaling_Rec)
   -- * Header is in FITS-file                                 <-- needed to know Data_Unit Start
   -- NOTE:
   -- * Tfile/Raw_BITPIX  NAXISn                               <-- needed to know Data_Unit Length

   -- Exceptions

   End_Error    : exception renames Ada.IO_Exceptions.End_Error;


end FITS_IO;

