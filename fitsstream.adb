
-- with Interfaces;
with Ada.Streams.Stream_IO;
use  Ada.Streams.Stream_IO; -- needed for + operator on Count type

package body FITSStream is

   type Unit_Type is (HeaderUnit, DataUnit);

   -------------------------------------
   -- calculate DataUnit size in Bits
   -- implements [FITS 4.4.1.1 Primary Header (1)]
   -- FIXME define record which has only the needed values and use that also in HDU_Info_Type
   function  Size_Bits(DUSizeParam : in out DUSizeParam_Type) return Natural
   is
    DUSize     : Natural := 1;
    BITPIXSize : Positive := abs (DUSizeParam.BITPIX);
    -- BITPIX carries size in bits [FITS ??]
   begin

     for I in 1..DUSizeParam.Naxes
     loop
      DUSize := DUSize * DUSizeParam.Naxis(I);
     end loop;
     -- note Naxis(I) cannot be 0, Parse_Card would throw exception

    return (DUSize*BITPIXSize);
   end Size_Bits;

   -------------------------------------
   -- convert BITPIX keyword from Header to internal FITSData_Type
   function  To_FITSDataType( BITPIX : in Integer )
    return FITSData_Type
   is
    bp : FITSData_Type;
   begin
    case BITPIX is
    when   8 => bp := Int8;
    when  16 => bp := Int16;
    when  32 => bp := Int32;
    when  64 => bp := Int64;
    when -32 => bp := Float32;
    when -64 => bp := Float64;
    when others =>
     null;
     -- FIXME ? raise exception "out of range"
    end case;
    return bp;
   end To_FITSDataType;

   -------------------------------------
   -- parse from Card value if it is one of HDU_Info_Type, do nothng otherwise
   -- and store parse value to HDUInfo
   -- TODO what to do if NAXIS and NAXISnn do not match in a broken FITS-file
   -- [FITS,Sect 4.4.1.1]: NAXISn keys _must_ match NAXIS keyword.
   procedure Parse_Card(Card        : in Card_Type;
                        DUSizeParam : in out DUSizeParam_Type)
   is
    dim : Positive;
   begin
     -- [FITS 4.1.2 Components]:
     -- pos 9..10 is '= '
     -- pos 31 is comment ' /'
     -- then : pos 10..20 is value
     if    (Card(1..9) = "BITPIX  =") then
       DUSizeParam.Data   := To_FITSDataType(Integer'Value(Card(10..30)));
       DUSizeParam.BITPIX := Integer'Value(Card(10..30));

     elsif (Card(1..5) = "NAXIS") then

       if (Card(1..9) = "NAXIS   =") then
           DUSizeParam.Naxes := Positive'Value(Card(10..30));
       else
           dim := Positive'Value(Card(6..8));
           DUSizeParam.Naxis(dim) := Positive'Value(Card(10..30));
       end if;

     end if;

   end Parse_Card;

   -------------------------------------
   -- parse one header for HDU-size information
   --  from current file-index,
   --  read cards until END-card found and try to fill in HDUInfo
   procedure Parse_Header(FitsFile : in Ada.Streams.Stream_IO.File_Type;
                          HDUInfo  : in out HDU_Info_Type)
   is
    Card : Card_Type;
   begin

    Card_Type'Read( Stream(FitsFile), Card );
    HDUInfo.CardsCnt := 1;

    while Card /= ENDCard
    loop
      Parse_Card (Card, HDUInfo.DUSizeParam);
      Card_Type'Read( Stream(FitsFile), Card );
      HDUInfo.CardsCnt := HDUInfo.CardsCnt + 1;
    end loop;

   end Parse_Header;

   -------------------------------------
   -- from Data-type decide whether it is for Header or Data Unit
   --  arrays derived from Character are for Header Read/Write
   --  other (arrays derived from BITPIX) are for Data Unit access
   function  To_UnitType(DataType : in FITSData_Type) return Unit_Type
   is
    UType : Unit_Type := HeaderUnit;
   begin

    case DataType is
    when Int8 | Int16 | Int32 | Int64 | Float32 | Float64 =>
     UType := DataUnit;
    when others =>
     UType := HeaderUnit;
    end case;

    return UType;
   end To_UnitType;

   -------------------------------------
   -- Set file index to position given by params
   procedure Set_Index(FitsFile : in Ada.Streams.Stream_IO.File_Type;
                       HDUNum   : in Positive;      -- which HDU
                       DataType : in FITSData_Type; -- decide to position to start of HeaderUnit or DataUnit
                       Offset   : in Natural := 0)  -- offset within the Unit (in units of FITSData_Type)
   is
    CurHDUNum : Positive := 1;
    CurDUSize : Positive;
    CurIndex  : Ada.Streams.Stream_IO.Count := 0;
    OffsetInRootElem : Ada.Streams.Stream_IO.Count;
    HDUInfo   : HDU_Info_Type;
    StreamRootElemSizeInBits : Positive := 8; -- FIXME [GNAT somwhere says it is 8bits]
   begin
    null;

    -- move to begining of HDU
    while CurHDUNum < HDUNum
    loop
     -- move past current Header
     Parse_Header(FitsFile, HDUInfo);
     CurDUSize := Size_Bits(HDUInfo.DUSizeParam) / StreamRootElemSizeInBits;
     CurIndex  := Ada.Streams.Stream_IO.Index(FitsFile);
     Ada.Streams.Stream_IO.Set_Index(FitsFile, CurIndex + Count(CurDUSize));
      -- FIXME check explicit conversaion Ada.Streams.Stream_IO.Count - Positive

     CurHDUNum := CurHDUNum + 1;
    end loop;

    -- if DataType indicates DataUnit move past current Header
    if DataUnit = To_UnitType(DataType)
    then
     -- move past current Header
     Parse_Header(FitsFile, HDUInfo);
     CurDUSize := Size_Bits(HDUInfo.DUSizeParam) / StreamRootElemSizeInBits;
    end if;

    -- add Offset
    if Offset /= 0
    then
      CurIndex := Ada.Streams.Stream_IO.Index(FitsFile);
      OffsetInRootElem := Count(Offset * DataType'Size / StreamRootElemSizeInBits );
      -- explicit conversion Natural -> Count ok: it is under if then... cannot be zero
      Ada.Streams.Stream_IO.Set_Index(FitsFile, CurIndex + OffsetInRootElem);
    end if;

   end Set_Index;

   -------------------------------------
   -------------------------------------
   -------------------------------------
   procedure Read (FitsStream : in Ada.Streams.Stream_IO.Stream_Access;
                   Data       : in out DataArray_Type)
   is
   begin

        DataArray_Type'Read( FitsStream, Data );

   end Read;

   procedure Write (FitsStream : in Ada.Streams.Stream_IO.Stream_Access;
                    Data       : in DataArray_Type)
   is
   begin

        DataArray_Type'Write( FitsStream, Data );

   end Write;

end FITSStream;

