

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

 procedure Create ( Fits : in out File_Type;
                    Mode : in Mode_Type;
                    Name : in String;
                    Form : in String    := "") is
 begin
  null;
 end Create;
 -- if Mode Out_File   : creates first HDU of a new file
 -- if Mode Append_Mode: creates new HDU of an existing file
 -- FIXME check Create + Append behaviour: 2nd case should map to OpenFile in Append_Mode ?

 procedure Open ( Fits : in out File_Type;
                  Mode : in Mode_Type;
                  Name : in String;
                  Form : in String   := "") is
 begin
  null;
 end Open;

 procedure Close ( Fits : in out File_Type ) is
 begin
  null;
 end Close;



 function NoOfHDUs( Fits : in File_Type ) return Positive
 is
 begin
  return Fits'Length;
 end NoOfHDUs;

 function FitsFile_Info ( Fits : File_Type ) return All_HDU_Info
 is
  All_HDU : All_HDU_Info( 1 .. Fits'Length );
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
