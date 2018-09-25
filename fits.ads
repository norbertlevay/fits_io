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


package FITS is

   type Byte is mod 256;
   for Byte'Size use 8;
   -- [FITS] defines Byte as 8-bit

   CardSize : constant Positive := 80;
   -- [FITS Sects. 3.3.1, 4.4.1]

   subtype Card_Type is String(1..CardSize);
   -- makes sure index start with 1

   ENDCard   : constant Card_Type := ( 1=>'E', 2=>'N', 3=>'D', others => ' ');
   EmptyCard : constant Card_Type := (others => ' ');

   type Card_Arr   is array (Positive range <>)    of Card_Type;
   type Card_Block is array (Positive range 1..32) of Card_Type;
--   for Card_Arr'Size use Card_Arr'Length*(CardSize);
-- how to guarantee these Arrs are packed OR do we need to guarantee ?

   -- 2. deriving from FITS-Standard:
   type    FInteger  is new Long_Long_Integer;
   subtype FNatural  is FInteger range 0 .. FInteger'Last;
   subtype FPositive is FNatural range 1 .. FNatural'Last;

   NAXIS_Max : constant Positive := 999;
   type NAXISn_Type is array (1 .. NAXIS_Max) of FPositive;
   type dNAXISn_Type is array (Positive range <>) of FPositive;
  -- FIXME would it be possible use array with length given at run-time (NAXIS read)?

private

   procedure dummy;

end FITS;

