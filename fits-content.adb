
with Ada.Text_IO;


package body FITS.Content is

   --
   --
   --
   procedure List_Content(FitsFile   : in Ada.Streams.Stream_IO.File_Type;
                          HDUInfoArr : in out HDU_Info_Arr)
   is
    HDUCnt  : Positive := 1;
    HDUInfo : HDU_Info_Type;
--    CurDUSize : Positive;
    CurIndex  : Ada.Streams.Stream_IO.Count := 0;
   begin

    -- start from begining
    Ada.Streams.Stream_IO.Set_Index(FitsFile,1);

    while not Ada.Streams.Stream_IO.End_Of_File(FitsFile)
    loop

     -- read current DU-size
     Parse_Header(FitsFile, HDUInfo);
     HDUInfoArr(HDUCnt) := HDUInfo;

     Ada.Text_IO.Put_Line("CC: " & Integer'Image(HDUInfo.CardsCnt));

     -- read next HDU Header if any

     HDUCnt := HDUCnt + 1;
     Set_Index(FitsFile, HDUCnt, Card); -- FIXME Card illogical??

    end loop;

   end List_Content;

end FITS.Content;

