
with Ada.Streams.Stream_IO;


package body FITS_IO is

 -- HDU positions within FITS-file

 type HDU_Pos is record
  HeaderStart : Positive; -- BlockIndex where header starts with the FitsFile
  HeaderSize  : Positive; -- Header size in Blocks (0 not allowed, HDU starts always with Header)
  DataStart   : Positive; -- BlockIndex where header starts with the FitsFile
  DataSize    : Natural;  -- Data size in Blocks (0 allowed, Primary HDU may have no data)
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
  HDU_Cnt  : Positive;
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

 function FitsFile_Info ( Fits : File_Type ) return All_HDU_Info
 is
  All_HDU : All_HDU_Info( 1 .. Fits.HDU_Cnt ) := (others=>Null_HDU_Info);
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

 MaxNAXIS : constant Positive := 999;-- [FITS standard Sect 4.4.1 ]
 type AxisArray_Type is array (1..MaxNAXIS) of Natural;
 type Axes_Type is record
  BitPix : Integer; -- FIXME actually fixed values only
  Naxes  : Natural;
  Naxis  : AxisArray_Type;
 end record;
 Null_AxisArray : AxisArray_Type := (others=> 1);
 Null_Axes : Axes_Type := (0,0, Null_AxisArray);
 -- Axis dimesions allow to calculate Data Unit size and position to next HDU in file


 --
 -- returns DU-size in Octets
 --
 function  Calc_DataUnit_Size( AxesDimensions : in Axes_Type )
  return Natural
 is
  DUSize : Natural := 0;
 begin

   if AxesDimensions.Naxes > 0 then
     DUSize := 1;
     for I in 1..AxesDimensions.Naxes loop
      DUSize := DUSize * AxesDimensions.Naxis(I);
     end loop;
     DUSize := DUSize * (abs AxesDimensions.BitPix/8);-- 8 bits in Octet
     DUSize := BlockSize*(1 + DUSize/BlockSize);
     -- must be multiple of BlockSize
   end if;

  return DUSize;
 end Calc_DataUnit_Size;

 --
 -- parse keywords needed to calculate DU size
 --
 procedure Parse_KeyRecord( Card : in Card_Type;
                            AxesDimensions  : in out Axes_Type )
 is
  dim : Positive;
 begin
   -- pos 9..10 is '= '
   -- pos 31 is comment ' /'
   -- then : pos 10..20 is value
   if    (Card(1..9) = "BITPIX  =") then
     AxesDimensions.BitPix := Integer'Value(Card(10..30));
   elsif (Card(1..5) = "NAXIS") then

     if (Card(1..9) = "NAXIS   =") then
         AxesDimensions.Naxes := Positive'Value(Card(10..30));
     else
         dim := Positive'Value(Card(6..8));
         AxesDimensions.Naxis(dim) := Positive'Value(Card(10..30));
     end if;
     -- FIXME what to do if NAXIS and NAXISnn do not match -> see standard

   end if;
 end Parse_KeyRecord;

 --
 -- walk through each HeaderBlock
 --
 function  Parse_HeaderBlock( Block : in Block_Type ;
                              AxesDimensions : in out Axes_Type )
  return Boolean
 is
  ENDFound : Boolean := False;
  Card     : Card_Type;
  CardCnt  : Positive := 1;
  from     : Positive := 1;
  next     : Positive := 1;
 begin
  loop
    next := from + CardSize;
    Card := Block( from .. (next - 1) );
    Parse_KeyRecord( Card, AxesDimensions );
    ENDFound := (Card = ENDCard);
    exit when ENDFound or CardCnt >= CardsCntInBlock;
    CardCnt := CardCnt + 1;
    from := next;
  end loop;
  return ENDFound;
 end Parse_HeaderBlock;

 -- [GNAT] Element_Type is Byte FIXME check this
 -- current architecture Byte is Octet FIXME get this
 -- [GNAT] from System.Storage_Unit (=Byte)
 function  To_BlockIndex( OctetIndex : in  Positive ) return Positive is
  begin
   return (OctetIndex - 1) / BlockSize + 1;
  end To_BlockIndex;

 function  To_OctetIndex( BlockIndex : in  Positive ) return Positive is
  begin
   return (BlockIndex - 1) * BlockSize + 1;
  end To_OctetIndex;
 -- Use System.Storage_Unit to implement the above
 -- Handle Endianess: System.Bit_Order : High_Order_First(=BigEndian) Low_Order_First Default_Bit_Order

 --
 -- for Open - using existing HDU
 --
 function  Parse_HDU_Positions ( File : in File_Type )
  return HDU_Data_Array_Type
 is
   FitsSA  : Ada.Streams.Stream_IO.Stream_Access :=
             Ada.Streams.Stream_IO.Stream( File.FitsFile );
   HDU_Arr : HDU_Data_Array_Type;

   -- controlling the loops
   HDU_Cnt : Positive := 1;
   EndCardFound : Boolean := false;

   -- positions & size in file
   HeadStart_Index : Positive;
   DataStart_Index : Positive;
   DataUnit_Size   : Natural := 0;

   AxesDimensions : Axes_Type;
   Block : BlockArray_Type(1..1);
 begin

   while not Ada.Streams.Stream_IO.End_OF_File(File.FitsFile)
   loop

      AxesDimensions := Null_Axes;

      -- Header

      HeadStart_Index := Index( File );

      loop
         Block := Read( File );
         EndCardFound := Parse_HeaderBlock( Block(1) , AxesDimensions );
         exit when EndCardFound ;
      end loop;

      -- DataUnit

      DataStart_Index := Index(File);
      DataUnit_Size   := To_BlockIndex(Calc_DataUnit_Size( AxesDimensions ));
      Set_Index( File, DataStart_Index + DataUnit_Size );
      -- skip data unit
                                                              
      HDU_Arr(HDU_Cnt).HDUPos.HeaderStart := HeadStart_Index;
      HDU_Arr(HDU_Cnt).HDUPos.HeaderSize  := DataStart_Index - HeadStart_Index;
      HDU_Arr(HDU_Cnt).HDUPos.DataStart   := DataStart_Index;
      HDU_Arr(HDU_Cnt).HDUPos.DataSize    := DataUnit_Size;
      -- store positions of this HDU

      HDU_Cnt := HDU_Cnt + 1;
      -- next HDU

   end loop;

  return HDU_Arr;
  end Parse_HDU_Positions;

 function  Index ( File  : in File_Type ) return Positive
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

 function  Read (File    : in  File_Type;
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
