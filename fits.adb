

-- FIXME shouldn't FitsFile : File_Type be 'in out' ? we update the Index...

-- for debug only
with Ada.Text_IO;



-- with Interfaces;
with Ada.Streams.Stream_IO;
use  Ada.Streams.Stream_IO; -- needed for + operator on Count type

package body FITS is

   type Unit_Type is (HeaderUnit, DataUnit);

   -------------------------------------
   -- calculate DataUnit size in Bits
   -- implements [FITS 4.4.1.1 Primary Header (1)]
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
   -- parse from Card value if it is one of DUSizeParam_Type, do nothng otherwise
   -- and store parse value to DUSizeParam
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
   --  read cards until END-card found and try to fill in HDUSize
   procedure Parse_Header(FitsFile : in Ada.Streams.Stream_IO.File_Type;
                          HDUSize  : in out HDU_Size_Type)
   is
    Card : Card_Type;
    FreeSlotCnt : Natural;
   begin
--    Ada.Text_IO.Put_Line("Parse_Header...");

    Card_Type'Read( Stream(FitsFile), Card );
    HDUSize.CardsCnt := 1;

--    Ada.Text_IO.Put_Line("Card#" &
--                          Integer'Image(HDUSize.CardsCnt) &
--                          " >" &
--                          Card &
--                          "< ");

    while Card /= ENDCard
    loop
      Parse_Card (Card, HDUSize.DUSizeParam);
      Card_Type'Read( Stream(FitsFile), Card );
      HDUSize.CardsCnt := HDUSize.CardsCnt + 1;

--      Ada.Text_IO.Put_Line("Card#" &
--                          Integer'Image(HDUSize.CardsCnt) &
--                          " >" &
--                          Card &
--                          "< ");

    end loop;

    -- read up to block-limit: calc free slots
    FreeSlotCnt := 36 - (HDUSize.CardsCnt mod 36);
    -- mod is 0 when Block has 36 cards e.g. is full
    if FreeSlotCnt = 36 then
     FreeSlotCnt := 0;
    end if;

    while FreeSlotCnt /= 0
    loop
      Card_Type'Read( Stream(FitsFile), Card );
      FreeSlotCnt := FreeSlotCnt - 1;

--      Ada.Text_IO.Put_Line("Card#" &
--                          Integer'Image(FreeSlotCnt) &
--                          " >" &
--                          Card &
--                          "< ");
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


   -- Skip DataUnit
   -- Bofore this call file's internal pointer must be pointing to
   -- the begining of the DataUnit
   procedure Move_Index_Behind_DataUnit
             (FitsFile : in Ada.Streams.Stream_IO.File_Type;
              DUSize   : in Positive)
   is
    CurIndex  : Ada.Streams.Stream_IO.Count := 0;
    DUSizeInBlocks : Natural;
   begin
     -- next assumes we have read past HeaderUnit (including freecard slots)
     CurIndex  := Ada.Streams.Stream_IO.Index(FitsFile);
--     Ada.Text_IO.Put_Line("CurIndex:  " & Count'Image(CurIndex));
--     Ada.Text_IO.Put_Line("CurDUSize: " & Count'Image(Count(CurDUSize)));

     -- FIXME DUSize must be also rounded in blocks
     DUSizeInBlocks := ((DUSize - 1) / 2880 + 1);

     Ada.Streams.Stream_IO.Set_Index(FitsFile,
                                     CurIndex + Count(DUSizeInBlocks * 2880));
      -- FIXME check explicit conversaion Ada.Streams.Stream_IO.Count - Positive
--     CurIndex  := Ada.Streams.Stream_IO.Index(FitsFile);
--     Ada.Text_IO.Put_Line("CurIndex:  " & Count'Image(CurIndex));
--     Ada.Text_IO.Put_Line("SetIndexCurHDUNum: " & Integer'Image(CurHDUNum));
   end Move_Index_Behind_DataUnit;

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
    HDUSize   : HDU_Size_Type;
   begin

    Ada.Streams.Stream_IO.Set_Index(FitsFile, 1);
    -- move to begining of the Primary HDU

    while CurHDUNum < HDUNum
    loop
     -- move past current Header
     Parse_Header(FitsFile, HDUSize);
     CurDUSize := Size_Bits(HDUSize.DUSizeParam) / StreamRootElemSizeInBits;
     -- move past DataUnit
     Move_Index_Behind_DataUnit(FitsFile,CurDUSize);
     -- next HDU
     CurHDUNum := CurHDUNum + 1;
    end loop;

    -- if DataType indicates DataUnit move past current Header
    if DataUnit = To_UnitType(DataType)
    then
     -- move past current Header
     Parse_Header(FitsFile, HDUSize);
     CurDUSize := Size_Bits(HDUSize.DUSizeParam) / StreamRootElemSizeInBits;
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

   --
   --
   --
   procedure List_Content (FitsFile : in Ada.Streams.Stream_IO.File_Type;
                           Print: not null access
                             procedure(HDUNum : Positive; HDUSize : HDU_Size_Type))
   is
    HDUCnt  : Positive := 1;
    HDUSize : HDU_Size_Type;
    CurDUSize : Positive;
    CurIndex  : Ada.Streams.Stream_IO.Count := 0;
   begin

    -- start from begining
    Ada.Streams.Stream_IO.Set_Index(FitsFile,1);

    while not Ada.Streams.Stream_IO.End_Of_File(FitsFile)
    loop
     -- read current DU-size
     Parse_Header(FitsFile, HDUSize);

     -- do the callback
     Print(HDUCnt,HDUSize);

     -- skip DataUnit
     CurDUSize := Size_Bits(HDUSize.DUSizeParam) / StreamRootElemSizeInBits;
     Move_Index_Behind_DataUnit(FitsFile,CurDUSize);

     -- read next HDU Header if any
     HDUCnt := HDUCnt + 1;
    end loop;

   end List_Content;

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

end FITS;

