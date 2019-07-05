--
-- FITS.* packages
--
-- Media management:
--
-- FITS.File child-package: helps to do file management
-- for FITS-files, positioning in FITS-file, list content etc...
-- It buids on Ada.Streams.Stream_IO and so many
-- procedures accept Ada.Stream_IO.File_Type and File_Mode
-- as parameter.
-- Network: ...?
--
-- Data Access:
--
-- FITS.Header FITS.Data packages:
-- Once Ada-file Stream is successfully acquired,
-- serialization/deserialization into/from stream
-- happens simply by using Read Write attributes of
-- the predefined types, specifically FitsData_Type'Read and 'Write,
-- which is array of base Ada-types and so supported by
-- Ada GNAT implementation [FITS ?][GNAT ?].
--
-- FIXME Note: currently only PrimaryHeader & Image extension supported.
-- Table Extensions to be added.
-- Parse_Card recognizes only mandatory keywords for primary header which
-- are essentially the same as for IMAGE extension [FITS Sect 7.1, Table 13].
-- Each extension-type has its own set of mandatory keywords.
-- [FITS Sect 4.4.1.2 Conforming Extensions]: Extensions have
-- PCOUNT & GCOUNT keys, but except of BINTABLE (where PCOUNT/=0)
-- their value is 0 & 1 respectively, and so yield (Eq2) the same
-- DU size as Primary haeder (Eq1).
-- PCOUNT & GCOUNT were meant for Random Groups [FITS Section 6]
--

--with System.Storage_Elements; use System.Storage_Elements;

with Interfaces;
with Ada.Streams.Stream_IO;

package FITSlib is

   type Byte is mod 256;
   for Byte'Size use 8;
   -- [FITS] defines Byte as 8-bit


   -- [FITS] defines BigEndian for all numeric types in file
   -- revert byte order when reading/writing from/to FITS file

-- FIXME only Float_32 Endian handling implemented

--   type Unsigned_8 is new Interfaces.Unsigned_8;
--   type Integer_16 is new Interfaces.Integer_16;
--   type Integer_32 is new Interfaces.Integer_32;
--   type Integer_64 is new Interfaces.Integer_64;
   type Float_32   is new Interfaces.IEEE_Float_32;
--   type Float_64   is new Interfaces.IEEE_Float_64;

  procedure Float32_Read_BigEndian
    		(S    : access Ada.Streams.Root_Stream_Type'Class;
             	 Data : out Float_32 );

   procedure Float32_Write_BigEndian
    		(S    : access Ada.Streams.Root_Stream_Type'Class;
             	 Data : in Float_32 );

   for Float_32'Read  use Float32_Read_BigEndian;
   for Float_32'Write use Float32_Write_BigEndian;
   -- FIXME Optimization: Endianess: do efficient IO reading/writing
   --       all array at once (as tech-note on adacore: ada-streams-write-all-array-at-one-call.pdf),
   --       and handle endianness on all array in memory, rather then each
   --       data element separately

end FITSlib;

