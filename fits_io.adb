

package body FITS_IO is

 -- HDU positions within FITS-file

 type HDU_Pos is record
  HeaderStart   : Positive; -- BlockIndex where header starts with the FitsFile
  HeaderSize    : Positive; -- Header size in Blocks (0 not allowed, HDU starts always with Header)
  DataUnitStart : Positive; -- BlockIndex where header starts with the FitsFile
  DataSize      : Natural;  -- Data size in Blocks (0 allowed, Primary HDU may have no data)
 end record;

 -- File_Type will hold to array of those above for each HDU in FITS-file

 type File_Data is record
  HDUPos  : HDU_Pos;
  HDUInfo : HDU_Info;
 end record;
 type File_Data_Array is array (Positive range <>) of File_Data;

 File : File_Type;


 function NoOfHDUs(FitsFile : File_Type) return Positive
 is
 begin
  return File'Length;
 end NoOfHDUs;

 function FitsFile_Info (FitsFile : File_Type) return All_HDU_Info
 is
  All_HDU : All_HDU_Info(1..NoOfHDUs(FitsFile));
 begin
  return All_HDU;
 end FitsFile_Info;


 procedure Copy_HDU (FromFile : File_type; FirstHDU : Positive; LastHDU : Positive;
                     ToFile   : File_type)
 is
 begin
  null;
 end;

 procedure Copy_Blocks (FromFile : File_type; FirstBlock : Positive; LastBlock : Positive;
                        ToFile   : File_type)
 is
 begin
  null;
 end;



end FITS_IO;
