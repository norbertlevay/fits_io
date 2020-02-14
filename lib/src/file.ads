
with Ada.Streams.Stream_IO;

with Ada.Strings.Bounded; use Ada.Strings.Bounded; -- Max20 only FIXME !!

with Keyword_Record; use Keyword_Record; -- FPositive needed
with Mandatory; -- Positive_Arr needed
with Optional; -- Bounded_String_8 Card_Arr needed 
use Optional; -- Card_Arr used elsewhere then Optional
with FITS; use FITS;

package File is

   package SIO renames Ada.Streams.Stream_IO;

   CardsCntInBlock : constant Positive := 36;

   type Card_Block is array (Positive range 1..CardsCntInBlock) of Card_Type;
   pragma Pack (Card_Block);
   -- FIXME does Pack guarantee arr is packed? how to guarantee Arrs are packed 
   -- OR do we need to guarantee at all ?


   -----------------------
   -- FITS file content --
   -----------------------
   package Max20 is
        new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 20);

   type HDU_Info_Type(NAXIS : Positive) is record
      XTENSION : Max20.Bounded_String;   -- XTENSION string or empty
      CardsCnt : FPositive;       -- number of cards in this Header
      BITPIX   : Integer;             -- data type
      NAXISn   : Mandatory.Positive_Arr(1..NAXIS); -- data dimensions
   end record;

   function Read_Header (FitsFile : in  SIO.File_Type)
      return HDU_Info_Type;


   -- read optional cards given by Keys

   function  Read_Header (FitsFile : in  SIO.File_Type;
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



private

        function  Calc_HeaderUnit_Size_blocks
                (CardsCount : in Positive) 
                return Positive;
    
    function  Calc_DataUnit_Size_blocks  
                (Res : in Mandatory.Result_Rec) return Keyword_Record.FNatural;

    function  Read_Header (FitsFile : in SIO.File_Type) return Mandatory.Result_Rec;


end File;
