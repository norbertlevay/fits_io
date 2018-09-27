
--------------------------------
-- Header and Data unit Sizes --
--------------------------------

package FITS.Size is

   BlockSize_bits : constant FPositive := 2880 * Byte'Size; -- 23040 bits
   -- [FITS 3.1 Overall file structure]

   -----------------------
   -- Size computations --
   -----------------------

   --
   -- calculate Header size in FITS Blocks
   --
   function  Size_blocks (CardsCnt      : in FPositive   ) return FPositive;

   --
   -- calc number of free data array slots to fill up DataBlock
   --
   procedure Free_Data_Slots (DataCnt :  in FPositive;
                              FreeDataCnt: out Natural) is null;
   -- FIXME add (as function) later when needed

   procedure Parse_Card (Card         : in Card_Type;
                         XtensionType : out String);

end FITS.Size;


