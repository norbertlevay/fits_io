
with Ada.Streams.Stream_IO;

with Ada.Strings.Bounded; use Ada.Strings.Bounded; -- Max20 only FIXME !!

with Keyword_Record; use Keyword_Record; -- Card_Type needed
with Mandatory; -- NAXIS_Arr needed
with Optional; -- Bounded_String_8 Card_Arr needed 
use Optional; -- Card_Arr used elsewhere then Optional
with FITS; use FITS;

package Header is

    package SIO renames Ada.Streams.Stream_IO;
   
    CardsCntInBlock : constant Positive := 36;

   type Card_Block is array (Positive range 1..CardsCntInBlock) of Card_Type;
   pragma Pack (Card_Block);
   -- FIXME does Pack guarantee arr is packed? how to guarantee Arrs are packed
   -- OR do we need to guarantee at all ?



    procedure Read_Card
       (File : in SIO.File_Type;
        HStart : in SIO.Positive_Count;
        CurBlkNum : in out Natural;
        Blk : in out Card_Block;
        CardNum : in Positive;
        Card    : out  String);

    -- read Mandatory cards

    function  Read_Header (FitsFile : in SIO.File_Type) return Mandatory.Result_Rec;

   -- read optional/reserved cards given by Keys

   function  Read_Header (FitsFile : in  SIO.File_Type;
            Keys : in Optional.Bounded_String_8_Arr)
      return Card_Arr;
-- FIXME consider returning also position at which the card was in the Header
-- e.g. some array of records needed -> then consider returning also 
-- separately key and value/comment??

end Header;

