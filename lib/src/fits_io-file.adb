
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

with FITS_IO.Header;

package body FITS_IO.File is

   function Next(Source : in File_Type)
     return Card_Block
   is
    CBlock : Card_Block;
   begin
    -- FIXME
    return CBlock;
   end Next;


   package FitsFile is
       new FITS_IO.Header (Source_Type => File_Type,
                           Next        => FITS_IO.File.Next);
   use FitsFile;


   procedure Move_Index
             (FitsFile : in SIO.File_Type;
              ByCount  : in SIO.Positive_Count) is
   begin
     SIO.Set_Index(FitsFile, SIO.Index(FitsFile) + ByCount);
   end Move_Index;
   pragma Inline (Move_Index);



   procedure Set_Index(FitsFile : in SIO.File_Type;
                       HDUNum   : in Positive)
   is
    --CurHDUSize_blocks : FITS_IO.Count;
    CurDUSize_bytes  : FITS_IO.Count;
    CurHDUNum : Positive := 1;

   begin

    SIO.Set_Index(FitsFile, 1);
    -- move to begining of the Primary HDU

    while CurHDUNum < HDUNum
    loop

     CurDUSize_bytes  := Read_DUSize_bytes(FitsFile);

     if CurDUSize_bytes /= 0
     then
       Move_Index(FitsFile, SIO.Positive_Count(CurDUSize_bytes));
     end if;

     CurHDUNum := CurHDUNum + 1;
    end loop;

   end Set_Index;

end FITS_IO.File;
