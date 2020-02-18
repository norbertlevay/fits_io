
with Ada.Streams.Stream_IO;

with Ada.Strings.Bounded; use Ada.Strings.Bounded; -- Max20 only FIXME !!

with Mandatory; -- NAXIS_Arr needed
with Optional; -- Bounded_String_8 Card_Arr needed 
use Optional; -- Card_Arr used elsewhere then Optional

package File is

   package SIO renames Ada.Streams.Stream_IO;

   -----------------------
   -- FITS file content --
   -----------------------
   package Max20 is
        new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 20);

   type HDU_Info_Type(NAXIS : Positive) is record
      XTENSION : Max20.Bounded_String;   -- XTENSION string or empty
      CardsCnt : SIO.Positive_Count;       -- number of cards in this Header
      BITPIX   : Integer;             -- data type
      NAXISn   : Mandatory.NAXIS_Arr(1..NAXIS); -- data dimensions
   end record;

   function Read_Header (FitsFile : in  SIO.File_Type)
      return HDU_Info_Type;


   -------------------------
   -- Positioning in file --
   -------------------------

   procedure Set_Index(File : in SIO.File_Type;
                       HDUNum   : in Positive);


private

    function  Calc_HeaderUnit_Size_blocks
                (CardsCount : in SIO.Positive_Count) return SIO.Positive_Count;

    function  Calc_DataUnit_Size_blocks  
                (Res : in Mandatory.Result_Rec) return SIO.Count;

end File;
