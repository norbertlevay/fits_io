
with Ada.Streams.Stream_IO;


package body FITS_IO is

 -- HDU positions within FITS-file

 type HDU_Pos is record
  HeaderStart   : Positive; -- BlockIndex where header starts with the FitsFile
  HeaderSize    : Positive; -- Header size in Blocks (0 not allowed, HDU starts always with Header)
  DataUnitStart : Positive; -- BlockIndex where header starts with the FitsFile
  DataSize      : Natural;  -- Data size in Blocks (0 allowed, Primary HDU may have no data)
 end record;

 -- File_Type will hold to array of those above for each HDU in FITS-file

 MaxHDU : constant Positive := 10;
 type HDU_Data is record
  HDUPos  : HDU_Pos;
  HDUInfo : HDU_Info;
 end record;
 type HDU_Data_Array_Type is array (1 .. MaxHDU) of HDU_Data;

 type File_Data is record
  FitsFile : Ada.Streams.Stream_IO.File_Type;
  HDU_Arr  : HDU_Data_Array_Type;
 end record;


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
  return Fits.HDU_Arr'Length;
 end NoOfHDUs;

 function FitsFile_Info ( Fits : File_Type ) return All_HDU_Info
 is
  All_HDU : All_HDU_Info( 1 .. Fits.HDU_Arr'Length ) := (others=>Null_HDU_Info);
 begin
  return All_HDU;
 end FitsFile_Info;


 procedure Copy_HDU (FromFile : File_type; FirstHDU : Positive; LastHDU : Positive;
                     ToFile   : File_type)
 is
 begin
  null;
 end;

---------------------------
-- Low-level file access --
---------------------------

 -- Read/Write implemented with Stream_IO

 -- [GNAT] Element_Type is Byte FIXME check this
 -- current architecture Byte is Octet FIXME get this
 -- [GNAT] from System.Storage_Unit (=Byte)
 function To_BlockIndex( OctetIndex : in  Positive ) return Positive is
  begin
   return (OctetIndex - 1) / BlockSize + 1;
  end To_BlockIndex;

 function To_OctetIndex( BlockIndex : in  Positive ) return Positive is
  begin
   return (BlockIndex - 1) * BlockSize + 1;
  end To_OctetIndex;
 -- Use System.Storage_Unit to implement the above
 -- Handle Endianess: System.Bit_Order : High_Order_First(=BigEndian) Low_Order_First Default_Bit_Order

 function Index ( File  : in File_Type ) return Positive
 is
 begin
  return To_BlockIndex(Positive(Ada.Streams.Stream_IO.Index( File.FitsFile )));
  -- FIXME verify this direct conversion Count -> Positive
 end Index;

 procedure Set_Index ( File  : in File_Type;
                       Index : in Positive ) -- Block Index
 is
 begin
  Ada.Streams.Stream_IO.Set_Index( File.FitsFile,
      Ada.Streams.Stream_IO.Positive_Count(To_OctetIndex(Index)) );
  -- FIXME verify this direct conversion Positive -> Positive_Count
 end Set_Index;

 procedure Write(File    : in File_Type;
                 Blocks  : in BlockArray_Type;
                 NBlocks : in Positive := 1)
 is
   FileSA : Ada.Streams.Stream_IO.Stream_Access :=
            Ada.Streams.Stream_IO.Stream(File.FitsFile);
 begin
   BlockArray_Type'Write( FileSA, Blocks );
 end Write;

 function Read (File    : in  File_Type;
                NBlocks : in  Positive := 1) return BlockArray_Type
 is
   FileSA : Ada.Streams.Stream_IO.Stream_Access :=
            Ada.Streams.Stream_IO.Stream(File.FitsFile);
   Blocks : BlockArray_Type( 1 .. NBlocks );
 begin
   BlockArray_Type'Read( FileSA, Blocks );
   return Blocks;
 end Read;

end FITS_IO;
