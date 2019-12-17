
with Ada.Streams.Stream_IO;

with Ada.Strings.Bounded; use Ada.Strings.Bounded; -- Max20 only FIXME !!

with Keyword_Record; use Keyword_Record; -- FPositive needed
with Strict; -- Positive_Arr needed
with Optional; -- Bounded_String_8 Card_Arr needed 
use Optional; -- Card_Arr used elsewhere then Optional
with FITS; use FITS;

package File is

   package SIO renames Ada.Streams.Stream_IO;

   -- FIXME needed also by Misc
   BlockSize_bits : constant FPositive := 2880 * Byte'Size; -- 23040 bits
    -- [FITS 3.1 Overall file structure]

   CardsCntInBlock : constant Positive := 36;
   type Card_Block is array (Positive range 1..CardsCntInBlock) of Card_Type;
   pragma Pack (Card_Block); -- not guaranteed ??
	-- FIXME how to guarantee Arrs are packed OR do we need to guarantee ?

   -------------------------------
   -- Write Header/Data Padding --
   -------------------------------

   HeaderPadValue : constant Unsigned_8 := 32; -- Space ASCII value
   DataPadValue   : constant Unsigned_8 :=  0;

   procedure Write_Padding(FitsFile : in SIO.File_Type;
                           From     : in SIO.Positive_Count;
                           PadValue : in Unsigned_8);
   -- [FITS ??]: FITS file consists of 2880-bytes long blocks.
   -- If last Header- or Data-block is not filled up,
   -- Write_Padding puts PadValue from FileOffset until end of the block.
   -- If Block is filled up, Write_Padding does nothing.

   ---------------------
   -- Read/Write Data --
   ---------------------

   -- FIXME notes on Item (Item_Arr) below:
   --       e.g. no need to define arrays as in FITS.ads
   --       instantation can be done by types rather then array types
   --       Q: How to guarantee then that the Item_Arr array would be packed?
   --       A: At instation the supplied array must be packed.
   --          No represantation clauses allowed in generic
   --
   generic
    type Item is private;
--    type Item_Arr is array (FPositive range <>) of Item;
    with function Element (Coord : in Strict.Positive_Arr) return Item;
   procedure Write_DataUnit (FitsFile  : in  SIO.File_Type;
                             MaxCoords : in  Strict.Positive_Arr);
   -- Item is any of FITS.UInt8 ... FITS.Float64
   -- Write_Data adds DataUnit padding after last Item written


   -----------------------
   -- FITS file content --
   -----------------------
   package Max20 is
        new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 20);

   type HDU_Info_Type(NAXIS : Positive) is record
      XTENSION : Max20.Bounded_String;   -- XTENSION string or empty
      CardsCnt : FPositive;       -- number of cards in this Header
      BITPIX   : Integer;             -- data type
      NAXISn   : Strict.Positive_Arr(1..NAXIS); -- data dimensions
   end record;

   function  Get (FitsFile : in  SIO.File_Type)
      return HDU_Info_Type;


   -- read optional cards given by Keys
   -- FIXME consider renaming Get* to Read_* because they move File_Index

   function BS8(S : String; Drop :Ada.Strings.Truncation := Ada.Strings.Error) 
	return Bounded_String_8.Bounded_String
		renames Bounded_String_8.To_Bounded_String;

   Descriptive_Keys : constant Optional.Bounded_String_8_Arr :=
	(BS8("DATE"),BS8("REFERENC"),BS8("ORIGIN"),BS8("EXTEND"),BS8("BLOCKED"));

   Observation_Keys : constant Optional.Bounded_String_8_Arr :=
	(BS8("DATE-OBS"),BS8("DATExxxx"),
	 BS8("TELESCOP"),BS8("INSTRUME"),BS8("OBSERVER"),BS8("OBJECT"));
	-- FIXME DATExxxx needs special handling!?

   -- NOTE bounded string operator * repeats the string (and performs the conversions??)
   -- so it can be (mis)used to initialize bounded_string with string like this:
   use Bounded_String_8;
   Biblio_Keys : constant Optional.Bounded_String_8_Arr := 
		(1 * "AUTHOR", 1 * "REFERENC");

   Commentary_Keys : constant Optional.Bounded_String_8_Arr := 
		(1 * "COMMENT", 1 * "HISTORY", 8 * " ");

   Array_Keys : constant Optional.Bounded_String_8_Arr := 
		(1 * "BSCALE", 1 * "BZERO", 1 * "BUNIT", 1 * "BLANK",
		 1 * "DATAMAX", 1 * "DATAMIN");

-- WCS keys : ... see Section 8

   Extension_Keys : constant Optional.Bounded_String_8_Arr :=
	(1 * "EXTNAME", 1 * "EXTVER", 1 * "EXTLEVEL");

   Reserved_Keys : constant Optional.Bounded_String_8_Arr :=
	(Descriptive_Keys & Observation_Keys & Biblio_Keys & Array_Keys);

   function  Get_Cards (FitsFile : in  SIO.File_Type;
			Keys : in Optional.Bounded_String_8_Arr)
      return Card_Arr;
-- FIXME consider returning also position at which the card was in the Header
-- e.g. some array of records needed -> then consider returning also 
-- separately key and value/comment??

   -------------------------
   -- Positioning in file --
   -------------------------

   procedure Set_Index(File : in SIO.File_Type;
                       HDUNum   : in Positive);



   -- Misc FIXME

	function  DU_Size_blocks (FitsFile : in SIO.File_Type) return Positive;	
	-- used in commands
private
	-- for Misc subpackage
	
	function  HDU_Size_blocks (FitsFile : in SIO.File_Type) return Positive;	

end File;
