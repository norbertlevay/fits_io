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

with FITS; use FITS;
with Numeric_Type;

generic
type T is private;

with function "+"(V : in Float)   return T is <>; 
with function "+"(V : in T) return Float   is <>; 
with function Is_Undef  (V,U : in T) return Boolean is <>; 
with function To_BITPIX (V   : in T) return Integer is <>; 

package FITS_IO is


type T_Arr is array (Positive_Count range <>) of T;

   package SIO renames Ada.Streams.Stream_IO;

--   subtype Count          is Ada.Streams.Stream_IO.Count;
--   subtype Positive_Count is Ada.Streams.Stream_IO.Positive_Count;--Count range 1 .. Count'Last;
--   subtype FIndex is Integer range 1 .. 999;
--   type NAXIS_Arr is array (FIndex range <>) of Positive_Count;
-- FIXME all four types above moved to FITS.ads because are shared with Numeric_Type
   -- ans cause cyclic dependency


   -- Header

   type Scaling_Rec is
      record
         Memory_Undefined_Value  : T;
         Memory_Undefined_Valid  : Boolean := False; -- if valid
         File_Undefined_Value    : Float := 0.0;
         File_Undefined_Valid    : Boolean := False;
         A : Float    := 0.0;
         B : Float    := 1.0;
         Memory_BITPIX   : Integer := 0; -- zero means the same as T, no scaling needed A,B=(0,1)
         File_BITPIX     : Integer := 0;
      end record;


   procedure Read_Header
     (File    : SIO.File_Type;
      Scaling : out Scaling_Rec;
      NAXISn : out NAXIS_Arr;
      Undef  : in out T);

   procedure Write_Header
      (File    : SIO.File_Type;
       Scaling : Scaling_Rec;
       NAXISn : NAXIS_Arr;
       Undef  : T);-- := Physical.Null_Numeric) is null;


   -- Data Unit

   procedure Read
     (File    : SIO.File_Type;
      Scaling : Scaling_Rec;
      Item : out T_Arr;
      Last : out Count);

   procedure Write
     (File    : SIO.File_Type;
      Scaling : Scaling_Rec;
      Item : T_Arr);


   -- Exceptions

   End_Error    : exception renames Ada.IO_Exceptions.End_Error;


end FITS_IO;

