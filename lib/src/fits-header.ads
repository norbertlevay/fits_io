--
-- Purpose:
-- convert between array of cards and data struct
-- Used when reading data from FITS.Header
--
--
-- Notes:
--
-- FIXME make sure Ada Character type [Ada?][GNAT?]
-- is of same size as FITS Standard [FITS?] header-character

with Ada.Strings.Bounded; use Ada.Strings.Bounded; -- Max_8 ...


package FITS.Header is
    
   ENDCard   : constant Card_Type := ( 1=>'E', 2=>'N', 3=>'D', others => ' ');

   procedure Parse
	      (Cards        : Card_Arr;
	       ENDCardFound : out Boolean);


   subtype Positive_Count is Natural range 1 .. Natural'Last;
   Limit999 : constant := 999;

   type PosCnt_Arr is array (Positive_Count range <>) of Positive_Count;

   type HDUPos_Type is (PRIMARY, RANDGROUP, CONF_EXT);
     -- HDUPos type absorps SIMPLE/XTENSION/GROUPS cards

   -- FIXME Use better name: DataSize --> DataSize_Keys : exact size in bits (without padding)
   -- NOTE DU_Size is then number of blocks (includes padding)
   -- NOTE DataSize - DataUnitSize = Padding

   type DataSize_Keys ( HDUType : HDUPos_Type;
                        Last    : Positive_Count ) is
           record
                   BITPIX : Positive_Count;
                   NAXIS  : Positive_Count;
                   NAXISn : PosCnt_Arr(1..Last);
                   case HDUType is
                           when PRIMARY => -- formula (1)
                                   null;
                           when RANDGROUP .. CONF_EXT => -- formula (4)+(6) and (2)
                                   PCOUNT   : Positive_Count;
                                   GCOUNT   : Positive_Count;
                    end case;
           end record;

     procedure Parse
	       (Cards : Card_Arr;
                Keys  : out DataSize_Keys);

     procedure Compose
                (Keys  : DataSize_Keys;
                 Cards : Card_Arr);
		 -- NOTE when composing and writing Mandatory keys to File many rules
		 -- must be kept: FITS-standard defines order of keys etc...





-- -------------------------------------------------------------------------
	------------------------
	-- OLD below
	-----------------------
	
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


end FITS.Header;
