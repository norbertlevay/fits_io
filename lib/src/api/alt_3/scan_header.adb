
with Ada.Text_IO; --use Ada.Text_IO;

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;-- Positive_Count needed
with Mandatory; use Mandatory;-- NAXIS_Arr needed
with Optional;
with Optional.Reserved;
with Header; use Header;
with File;
--with V3_Types; use V3_Types;-- types needed






package body Scan_Header is

package TIO renames Ada.Text_IO;

procedure Put_Image_Data_Rec(Rec : Image_Data_Rec)
is
begin
--    TIO.Put_Line("DUStart  [blocks] : " & Positive_Count'Image(Rec.DUStart));
    TIO.Put_Line("BITPIX : " & Integer'Image(Rec.BITPIX));
    TIO.Put("NAXISn[" & Positive'Image(Rec.NAXISn'Length) & "] : ");
    for I in Rec.NAXISn'Range
    loop
        TIO.Put(Positive_Count'Image(Rec.NAXISn(I)));
    end loop;
    TIO.New_Line;
    TIO.Put_Line("BZERO  : " & Rec.BZERO);
    TIO.Put_Line("BSCALE : " & Rec.BSCALE);
    if(Rec.BLANK_Valid)
    then TIO.Put_Line("BLANK  : " & Rec.BLANK);
    else TIO.Put_Line("BLANK not found");
    end if;

end Put_Image_Data_Rec;



function Data_Unit_Info
    (F : in SIO.File_Type;
    HDUStart : in  Positive_Count) -- blocks
    return Image_Data_Rec
is
    HDUInfo : File.HDU_Info_Type := File.Read_Header(F);
    ImInfo  : Image_Data_Rec(HDUInfo.NAXISn'Length);
begin

    File.Set_File_Block_Index(F, HDUStart);

    ImInfo.BITPIX := HDUInfo.BITPIX;
    ImInfo.NAXISn := HDUInfo.NAXISn;
    ImInfo.BZERO  := "                 0.0";
    ImInfo.BSCALE := "                 1.0";
    ImInfo.BLANK_Valid := False;

  declare
    Cards : Optional.Card_Arr := Read_Optional(F, Optional.Reserved.Reserved_Keys);
    Key : String(1..8);
  begin
    for I in Cards'Range
    loop
      Key := Cards(I)(1..8);
      if   (Key = "BZERO   ") then ImInfo.BZERO  := Cards(I)(11..30);
      elsif(Key = "BSCALE  ") then ImInfo.BSCALE := Cards(I)(11..30);
      elsif(Key = "BLANK   ") then
          ImInfo.BLANK := Cards(I)(11..30);
          ImInfo.BLANK_Valid := True; 
      end if;
    end loop;
  end;

  File.Set_File_Block_Index(F, HDUStart);

   return ImInfo;

end Data_Unit_Info;


end Scan_Header;

