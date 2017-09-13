
-- with Interfaces;
with Ada.Streams.Stream_IO;
use  Ada.Streams.Stream_IO; -- needed for + operator on Count type

package body FITSStream is

   function To_UnitType(DataType : in FITSData_Type) return Unit_Type
   is
    UType : Unit_Type := HeaderUnit;
   begin
    return UType;
   end To_UnitType;

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

   end Set_Index;

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

