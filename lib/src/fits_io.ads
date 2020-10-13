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


   procedure Read_Header
     (File  : SIO.File_Type;
      Image : out Image_Data_Model);

   procedure Write_Header
      (File  : SIO.File_Type;
       Image : Image_Data_Model);



   -- Exceptions

   End_Error    : exception renames Ada.IO_Exceptions.End_Error;


end FITS_IO;

