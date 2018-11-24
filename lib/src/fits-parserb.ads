--
-- Notes:
--
-- FIXME make sure Ada Character type [Ada?][GNAT?]
-- is of same size as FITS Standard [FITS?] header-character

with Ada.Strings.Bounded;
with Ada.Streams.Stream_IO;




package FITS.ParserB is

   package SIO renames Ada.Streams.Stream_IO;

   -------------
   -- Parsers --
   -------------

   --
   -- create Header parsers
   --
   -- will read all Header up to END-card
   -- before calling, make sure File-pointer is at Header start
   -- e.g. call Set_Index(F,HDUNum)
   generic
     type Parsed_Type is private;
     with procedure Parse_Card
                    (CardIndex : in FPositive;
                     Card      : in  Card_Type;
                     Data      : in out Parsed_Type);
   function gen_Read_Header (FitsFile : in SIO.File_Type)
     return Parsed_Type;



   -- Parsers below are used by FITS.File.gen_Read_Header
   -- Enables to instatiate
   --   SomeType = Read_Header(FITSFile)
   -- functions


   -- Parsing HDU/XTENSION type

   type HDU_Type is record                          -- set by:
      SIMPLE   : String(1..10) := (others => ' ');  -- Primary HDU
      XTENSION : String(1..10) := (others => ' ');  -- Extension HDU
   end record;
   -- XTENSION type string or empty [empty: FITS 4.2.1 undefined keyword]

   procedure Parse_HDU_Type(Index: in  FPositive;
   			    Card : in  Card_Type;
                            Data : in out HDU_Type);


   -- Parsing HDU size

   type NAXIS999_Type is array (1 .. NAXIS_Type'Last) of FPositive;

   type HDU_Size_Type is record
      CardsCnt      : FPositive; -- number of cards in this Header (gives Header-size)
      -- Primary HDU:
      BITPIX : Integer;       -- BITPIX from header (data size in bits)
      NAXIS  : NAXIS_Type;    -- NAXIS  from header, 0 means no DataUnit
      NAXISn : NAXIS999_Type; -- NAXISn from header, 0 means dimension not in use
      -- Conforming extensions:
      PCOUNT : FNatural;    -- BINTABLE: size of heap OR Random Groups: param count preceding each group
      GCOUNT : FPositive;   -- Number of Random Groups present
      -- FIXME what type to use for P/GCOUNT ? -> implementation limited?
   end record;

   procedure Parse_HDU_Size_Type (Index   : in FPositive;
                                  Card    : in Card_Type;
                                  HDUSize : in out HDU_Size_Type);

   type HDU_Size_UserArea_Type is record
      CardsCnt      : FPositive; -- number of cards in this Header (gives Header-size)
   end record;

   procedure Parse_HDU_Size_Type22
                    (Card      : in  Card_Type;
                     Data      : in out HDU_Size_Type;
                     UData     : in out HDU_Size_UserArea_Type);

end FITS.ParserB;
