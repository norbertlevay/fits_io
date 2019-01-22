
with Ada.Streams.Stream_IO;
use Ada.Streams.Stream_IO;


package body FITS_IO.File is

   procedure Move_Index
             (FitsFile : in SIO.File_Type;
              ByCount  : in SIO.Positive_Count) is
   begin
     SIO.Set_Index(FitsFile, SIO.Index(FitsFile) + ByCount);
   end Move_Index;
   pragma Inline (Move_Index);
   -- util: consider this part of Stream_IO



   procedure Set_Index(FitsFile : in SIO.File_Type;
                       HDUNum   : in Positive)
   is
    --CurHDUSize_blocks : FITS_IO.Count;
    CurHDUSize_bytes  : FITS_IO.Count;
    CurHDUNum : Positive := 1;

   begin

    SIO.Set_Index(FitsFile, 1);
    -- move to begining of the Primary HDU

    while CurHDUNum < HDUNum
    loop

--     HDUSize := Read_Header_And_Parse_Size(FitsFile);
--     CurHDUSize_bytes  := DU_Size_bytes(FitsFile); í<-- should come from
--        FIT_IO.Header containing Parser as Read_Header_And_Parse_Size(HDU_Size_Type) + Size_bytes(HDU_Size_Type)
     -- move past current Header

     -- skip DataUnit if exists
--     if HDUSize.NAXIS /= 0
-- FIXME do we need this ?     if HDUSize.NAXIS /= 0
     if CurHDUSize_bytes /= 0
     then
       Move_Index(FitsFile, SIO.Positive_Count(CurHDUSize_bytes));
     end if;

     CurHDUNum := CurHDUNum + 1;
    end loop;

   end Set_Index;

end FITS_IO.File;
