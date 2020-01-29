--
-- Example convert FLoat32 -> Int16
--
-- FIXME: assume Primary HDU. What if other HDU is IMAGE type too?
--
-- demonstrate usage if data unit is "big":
-- all data will not fit into memory, needs to be processed
-- in chunks.


with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with Ada.Exceptions;   use Ada.Exceptions;
with GNAT.Traceback.Symbolic;

with Ada.Streams.Stream_IO;

with File;   use File;
with File.Misc;   use File.Misc;
with Keyword_Record; use Keyword_Record;
with Strict; use Strict; -- Positive_Arr needed

-- new Data interface
with Data_Types; use Data_Types;
with Data_Funcs; use Data_Funcs;

procedure minmax
is

 package SIO renames Ada.Streams.Stream_IO;
 use SIO;

 function DU_Count(NAXISn : Positive_Arr) return FNatural
 is
	Cnt : FNatural := 1;
 begin
	for I in NAXISn'Range
        loop
		Cnt := Cnt * NAXISn(I);
	end loop;
	return Cnt;
 end DU_Count;


 InFile      : SIO.File_Type;
 InFileName  : String := Argument(1);
 -- FIXME might raise excpetion before Usage written

 BITPIX : Integer;
 DUSize : FPositive;

begin

 Put_Line("Usage  " & Command_Name & " <file name>");

 SIO.Open   (InFile,  SIO.In_File,  InFileName);

 -- FIXME now only Primary HDU, later consider other HDUs

 SIO.Set_Index(InFile,1);
 -- interpret header: DataUnit length and type needed
 declare
  HDUInfo : HDU_Info_Type := Read_Header(InFile);
 begin
  BITPIX := HDUInfo.BITPIX;
  DUSize := DU_Count (HDUInfo.NAXISn);
 end;

 -- read write sequentially by Blocks

 declare
  F32Block  : Data_Types.F32.Data.Block;
  DUSize_blocks : constant Positive := DU_Block_Index(Positive(DUSize),4);-- 4 ->Float_32 InFile
  Last_Data_Element_In_Block : constant Positive := Offset_In_Block(Positive(DUSize), Data_Types.F32.Data.N);

  F32Value : Data_Types.Float_32;
  B_Min    : Data_Types.Float_32 := Data_Types.Float_32'Last;
  B_Max    : Data_Types.Float_32 := Data_Types.Float_32'First;
 begin

--        Put_Line("LastELEM: " & Positive'Image(Last_Data_Element_In_Block));

	for I in 1 .. (DUSize_Blocks - 1)
	loop
		F32.Data.Block'Read(SIO.Stream(InFile),F32Block);
		B_Min := F32.Min(F32Block, B_Min);
		B_Max := F32.Max(F32Block, B_Max);
	end loop;

	-- Last Block of InFIle
	
	F32.Data.Block'Read(SIO.Stream(InFile),F32Block);
	for K in 1 .. (Last_Data_Element_In_Block)
	loop
	--	Put(Positive'Image(K)&" ");
		F32Value := F32Block(K);
		if(F32Value < B_Min) then B_Min := F32Value; end if;
		if(F32Value > B_Max) then B_Max := F32Value; end if;
	end loop;

 	Put_Line("Min: " & Float_32'Image(B_Min));
 	Put_Line("Max: " & Float_32'Image(B_Max));
 end;

 SIO.Close(InFile);

 Put_Line("done.");


 exception

  when Except_ID : others =>
     declare
      Error : Ada.Text_IO.File_Type := Standard_Error;
     begin
      New_Line(Error);
      Put_Line(Error, "Exception_Information: ");
      Put_Line(Error, Exception_Information( Except_ID ) );
      New_Line(Error);
      Put_Line(Error, "Call stack traceback symbols: addr2line -e ./fits addr1 addr2 ...");
      Put_Line(" > Trace-back of call stack: " );
      Put_Line( GNAT.Traceback.Symbolic.Symbolic_Traceback(Except_ID) );
      -- See more at: http://compgroups.net/comp.lang.ada/gnat-symbolic-traceback-on-exceptions/1409155#sthash.lNdkTjq6.dpuf
      -- Do the same manually, use:
      -- addr2line -e ./fits addr1 addr2 ...
     end;
end minmax;

