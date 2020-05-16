
with Ada.Text_IO;
with Ada.Streams.Stream_IO;
with Ada.Command_Line; use Ada.Command_Line;

with V3_Types; use V3_Types;
with File;
with Raw;
--with Raw_BLANK;

with Scan_Header;

procedure undefvals is

    package TIO renames Ada.Text_IO;
    package SIO renames Ada.Streams.Stream_IO;

    -- FIXME out these to separate file V3_Arrays or V3_Types 
    -- NOTE dependent on Positive-Count -> make generic by Index_Type ??
    type F64_Arr is array (SIO.Positive_Count range <>) of Float_64;
    type I16_Arr is array (SIO.Positive_Count range <>) of Integer_16;


    package F64_Raw is new Raw(Float_64, F64_Arr);
    package I16_Raw is new Raw(Integer_16, I16_Arr);

--    package F64_BLRaw is new Raw_BLANK(Float_64, F64_Arr);
--    package I16_BLRaw is new Raw_BLANK(Integer_16, I16_Arr);

    InFile   : SIO.File_Type;
    HDUStart : SIO.Positive_Count := 1; -- Primary HDU only$

begin

    if(Argument_Count /= 1 ) 
    then 
      TIO.Put_Line("Usage  " & Command_Name & " <file name>");
      return;
    else
      SIO.Open(InFile, SIO.In_File, (Argument(1)));
    end if;

    File.Set_File_Block_Index(InFile,HDUStart);


    declare
        ImData : Scan_Header.Image_Data_Rec := Scan_Header.Data_Unit_Info(InFile, HDUStart);
    begin
        Scan_Header.Put_Image_Data_Rec(ImData);
    end; -- Scan_Header


    SIO.Close(InFile);

end undefvals;
