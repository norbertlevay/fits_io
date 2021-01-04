
with FITS_IO; use FITS_IO;
with Ada.Streams.Stream_IO;

with Ada.Strings.Bounded; -- Max20 only FIXME !!

with Mandatory; -- NAXIS_Array needed
with Optional; -- Bounded_String_8 Card_Arr needed 
use Optional; -- Card_Arr used elsewhere then Optional

package File is

 package SIO renames Ada.Streams.Stream_IO;
--   package SIO renames FITS_IO;

   -----------------------
   -- FITS file content --
   -----------------------
   package Max20 is
        new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 20);

   type HDU_Info_Type(NAXIS : Positive) is record
      XTENSION : Max20.Bounded_String;   -- XTENSION string or empty
      CardsCnt : Positive_Count;       -- number of cards in this Header
      BITPIX   : Integer;             -- data type
      NAXISn   : NAXIS_Array(1..NAXIS); -- data dimensions
   end record;

   function Read_Header (FitsFile : in  SIO.File_Type)
      return HDU_Info_Type;


   -------------------------
   -- Positioning in file --
   -------------------------

   procedure Set_Index(File : in SIO.File_Type;
                       HDUNum   : in Positive);

   function File_Block_Index(File : SIO.File_Type) return Positive_Count;

   procedure Set_File_Block_Index
       (File        : SIO.File_Type;
       Block_Index : in Positive_Count);



private

    function  Calc_HeaderUnit_Size_blocks
                (CardsCount : in Positive_Count) return Positive_Count;

    function  Calc_DataUnit_Size_blocks
                (Res : in Mandatory.Result_Rec) return Count;

end File;
