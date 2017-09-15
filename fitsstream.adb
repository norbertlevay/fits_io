
-- with Interfaces;
with Ada.Streams.Stream_IO;
use  Ada.Streams.Stream_IO; -- needed for + operator on Count type

package body FITSStream is

   -------------------------------------
   -- calculate DataUnit size in Bits
   -- implements [FITS 4.4.1.1 Primary Header (1)]
   -- FIXME define record which has only the needed values and use that also in HDU_Info_Type
   function Size_Bits(HDUInfo  : in out HDU_Info_Type) return Natural
   is
    DUSize : Natural := 1;
    BITPIXSize : Positive := abs (HDUInfo.BitPix);
    -- BITPIX carries size in bits [FITS ??]
   begin

     for I in 1..HDUInfo.Naxes loop
      DUSize := DUSize * HDUInfo.Naxis(I);
     end loop;
     -- note Naxis(I) cannot be 0, Parse_Card would throw excpetion

    return (DUSize*BITPIXSize);
   end Size_Bits;

   -------------------------------------
   -- parse from Card value if it is one of HDU_Info_Type
   -- and store parse value to HDUInfo
   -- FIXME consider: Throw exception StandardViolation if Card value incorrect ?
   procedure Parse_Card(Card    : in Card_Type;
                        HDUInfo : in out HDU_Info_Type)
   is
   begin
    null;
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

    while Card /= ENDCard
    loop
      Parse_Card (Card, HDUInfo);
      Card_Type'Read( Stream(FitsFile), Card );
    end loop;

    -- FIXME how to check HDUInfo is complete? (broken headers) do we need to check?
   end Parse_Header;

   -------------------------------------
   -- from Data-type decide whether it is for Header or Data Unit
   --  arrays derived from Character are for Header Read/Write
   --  other (arrays derived from BITPIX) are for Data Unit access
   function To_UnitType(DataType : in FITSData_Type) return Unit_Type
   is
    UType : Unit_Type := HeaderUnit;
   begin
    -- FIXME implement
    return UType;
   end To_UnitType;

   -------------------------------------
   -- Set file index to position given by params
   procedure Set_Index(FitsFile : in Ada.Streams.Stream_IO.File_Type;
                       HDUNum   : in Positive;     -- which HDU
                       UnitType : in Unit_Type;    -- position to start of HeaderUnit or DataUnit
                       Offset   : in Natural := 0) -- offset within the Unit (in units of FITSData_Type)
   is
    CurHDUNum : Positive := 1;
    CurDUSize : Ada.Streams.Stream_IO.Count := 0;
    CurIndex  : Ada.Streams.Stream_IO.Count := 0;
   begin
    null;

    -- move to begining of HDU
    while CurHDUNum < HDUNum
    loop
     -- move past current Header
     CurDUSize := 0;-- parse Header
     CurIndex  := Ada.Streams.Stream_IO.Index(FitsFile);
     Ada.Streams.Stream_IO.Set_Index(FitsFile, CurIndex + CurDUSize);

     CurHDUNum := CurHDUNum + 1;
    end loop;

    -- if UnitType is DataUnit move past current Header
    if UnitType = DataUnit
    then
     -- move past current Header
     CurDUSize := 0;-- parse Header
     null;
    end if;

    -- add Offset
     CurIndex  := Ada.Streams.Stream_IO.Index(FitsFile);
     Ada.Streams.Stream_IO.Set_Index(FitsFile, CurIndex + Count(Offset));
     -- FIXME explicit conversion Natural -> Count

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

