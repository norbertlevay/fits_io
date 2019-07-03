--
-- Notes:
--
-- FIXME make sure Ada Character type [Ada?][GNAT?]
-- is of same size as FITS Standard [FITS?] header-character

with Ada.Strings.Bounded;
use Ada.Strings.Bounded;


package FITS.Header is

   ------------------
   -- Create Cards --
   ------------------

   package Max_8 is
       new Ada.Strings.Bounded.Generic_Bounded_Length (Max =>  8);
   package Max20 is
       new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 20);
   package Max48 is
       new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 48);
   package Max70 is
       new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 70);


   -------------
   -- Parsers --
   -------------

   -- Parsers below are used by FITS.File.gen_Read_Header
   -- Enables to instatiate
   --   SomeType = Read_Header(FITSFile)
   -- functions


   -- Parsing HDU/XTENSION type

   type  XXXHDU_Type is record                          -- set by:
      SIMPLE   : String(1..10) := (others => ' ');  -- Primary HDU
      XTENSION : Max20.Bounded_String;-- := (others => ' ');  -- Extension HDU
   end record;
   -- XTENSION type string or empty [empty: FITS 4.2.1 undefined keyword]

   procedure XXXParse_HDU_Type(Index: in  FPositive;
   			    Card : in  Card_Type;
                            Data : in out XXXHDU_Type) is null;


   -- Parsing HDU size

   type NAXIS999_Type is array (1 .. NAXIS_Type'Last) of FPositive;

   type XXXHDU_Size_Type is record
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

   procedure XXXParse_HDU_Size_Type (Index   : in FPositive;
                                  Card    : in Card_Type;
                                  HDUSize : in out XXXHDU_Size_Type) is null;


   -----------
   -- Utils --
   -----------

   -- FIXME rename to Cards_For_Size & and make it a function
   function  Write_Cards_For_Size
              (BITPIX : Integer;
               Dim    : NAXIS_Arr ) return Card_Arr;

   --
   -- calc number of free cards to fill up HeaderBlock
   --
   function  Free_Card_Slots (CardsCnt : in FPositive ) return Natural;
   --  always 0..35 < 36(=Cards per Block)



end FITS.Header;
