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

with Ada.Streams.Stream_IO;


package FITS is

   type Count          is new Ada.Streams.Stream_IO.Count;
   subtype Positive_Count is Count range 1 .. Count'Last;
   --subtype Positive_Count is Ada.Streams.Stream_IO.Positive_Count;--Count range 1 .. Count'Last;

   subtype FIndex is Integer range 1 .. 999;
   type NAXIS_Arr is array (FIndex range <>) of Positive_Count;

end FITS;

