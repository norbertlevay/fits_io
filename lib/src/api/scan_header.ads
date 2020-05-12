
--with Ada.Text_IO; --use Ada.Text_IO;

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;-- Positive_Count needed
with Mandatory; use Mandatory;-- NAXIS_Arr needed
--with Optional;
--with Optional.Reserved;
--with Header; use Header;
--with File;
--with V3_Types; use V3_Types;-- types needed

--with Linear_Private;





package Scan_Header is

--package TIO renames Ada.Text_IO;
package SIO renames Ada.Streams.Stream_IO;

subtype String_20 is String(1 .. 20);-- FIXME use 'First 'First + 20

type Image_Data_Rec(NAXIS : Natural) is record
--    DUStart : Positive_Count;-- in blocks
    BITPIX  : Integer;
    NAXISn  : NAXIS_Arr(1..NAXIS);
    BZERO, BSCALE : String_20;
    BLANK_Valid   : Boolean;
    BLANK         : String_20;
end record;
-- pulls together mandatory and optional keyrecords

procedure Put_Image_Data_Rec(Rec : Image_Data_Rec);


function Data_Unit_Info
    (F : in SIO.File_Type;
    HDUStart : in  Positive_Count) -- blocks
    return Image_Data_Rec;

end Scan_Header;

