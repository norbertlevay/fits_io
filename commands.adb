
with Ada.Text_IO,-- Ada.Integer_Text_IO,
     Ada.Strings.Fixed,
     Ada.Streams.Stream_IO,
     Ada.Characters.Latin_1,
     GNAT.OS_Lib,
     FITS_SIO;

use  Ada.Streams.Stream_IO;

package body Commands is

 procedure List_HDUs_In_File (FitsFileName : in String)
 is
  FitsFile : FITS_SIO.SIO.File_Type;

  procedure Print_HDU_Sizes (Index : Positive; HDUInfo : FITS_SIO.HDU_Size_Type)
  is
      FreeSlotCnt : Natural;
      Tab : Character := Ada.Characters.Latin_1.HT;
  begin
       -- calc free slots
       FreeSlotCnt := 36 - (HDUInfo.CardsCnt mod 36);
       -- mod is 0 when Block has 36 cards e.g. is full
       if FreeSlotCnt = 36 then
        FreeSlotCnt := 0;
       end if;

       Ada.Text_IO.Put( Integer'Image(Index) &
                        Tab &
                        Ada.Strings.Fixed.Tail( Integer'Image(HDUInfo.CardsCnt),5,' ') &
                        " (" &
                        Ada.Strings.Fixed.Tail(Integer'Image( FreeSlotCnt ),2,' ') &
                        ")" );

       if HDUInfo.DUSizeParam.Naxes > 0 then
        Ada.Text_IO.Put( Tab & Ada.Strings.Fixed.Head( FITS_SIO.FITSData_Type'Image(HDUInfo.DUSizeParam.Data),8,' ') );
        Ada.Text_IO.Put(" ( ");
        for J in 1 .. (HDUInfo.DUSizeParam.Naxes - 1)
         loop
          Ada.Text_IO.Put(FITS_SIO.FPositive'Image(HDUInfo.DUSizeParam.Naxis(J)) & " x " );
        end loop;
        Ada.Text_IO.Put(Fits_SIO.FPositive'Image(HDUInfo.DUSizeParam.Naxis(HDUInfo.DUSizeParam.Naxes)));
        Ada.Text_IO.Put_Line(" ) ");
       end if;
  end Print_HDU_Sizes;

  procedure Print_Headline is
    Tab : Character := Ada.Characters.Latin_1.HT;
  begin
   Ada.Text_IO.Put_Line ("HDU#" & Tab & " Cards" & Tab & "Data");
  end Print_Headline;


 begin
   FITS_SIO.SIO.Open(FitsFile,FITS_SIO.SIO.In_File,FitsFileName);
   Print_Headline;
   FITS_SIO.List_Content (FitsFile, Print_HDU_Sizes'Access);
   FITS_SIO.SIO.Close(FitsFile);
 end List_HDUs_In_File;

end Commands;

