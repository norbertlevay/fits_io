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

   package Bounded_String_8 is new BS.Generic_Bounded_Length(8);
   package BS_8 renames Bounded_String_8;
   package BS70 is new BS.Generic_Bounded_Length(70);


   -- Header

   Null_Undefined_Value : constant BS70.Bounded_String
                                 := BS70.To_Bounded_String("");

   type Scaling_Rec is
      record
         Memory_Undefined_Value : BS70.Bounded_String := Null_Undefined_Value;
         File_Undefined_Value   : BS70.Bounded_String := Null_Undefined_Value;
         A : Float    := 0.0;
         B : Float    := 1.0;
         Memory_BITPIX   : Integer := 0; -- zero means the same as T, no scaling needed A,B=(0,1)
         File_BITPIX     : Integer := 0;
      end record;

   Null_Scaling : constant Scaling_Rec := (  Null_Undefined_Value, Null_Undefined_Value,
                                             0.0,1.0,
                                             0,0);

   -- NOTE All Header section:  Scaling_Rec and Read_/Write_Header
   -- should by T-type independent:
   -- In Scaling_Rec use Floats or Strings for UndefVal
   -- String better: define Null_Undef := "" -> means no-Undef (no need for Boolean Valid flag)


   procedure Read_Header
     (File    : SIO.File_Type;
      Scaling : out Scaling_Rec;
      NAXISn : out NAXIS_Array;
      Undef  : in out BS70.Bounded_String);

   procedure Write_Header
      (File    : SIO.File_Type;
       Scaling : Scaling_Rec;
       NAXISn : NAXIS_Array;
       Undef  : BS70.Bounded_String := Null_Undefined_Value);



   -- Exceptions

   End_Error    : exception renames Ada.IO_Exceptions.End_Error;


end FITS_IO;

