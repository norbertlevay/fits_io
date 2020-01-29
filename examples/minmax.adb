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

with Generic_Data_Float;

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


 -- Find Min Max values in DataUnit
 -- implemented by generic for Float types
 -- FIXME do the same for Integers

 generic
--  type T is digits <>;
  with package Floats is new Generic_Data_Float(<>);
 package MinMax is
  procedure Find_MinMax(F : SIO.File_Type; Min : out Floats.T; Max : out Floats.T);
 end MinMax;

 package body MinMax is
  procedure Find_MinMax(F : SIO.File_Type; Min : out Floats.T; Max : out Floats.T)
  is
   FBlock  : Floats.Data.Block;
   DUSize_blocks : constant Positive := DU_Block_Index(Positive(DUSize),Floats.T'Size/8);
   Last_Data_Element_In_Block : constant Positive := 
					Offset_In_Block(Positive(DUSize), Floats.Data.N);

  FValue : Floats.T;
  B_Min    : Floats.T := Floats.T'Last;
  B_Max    : Floats.T := Floats.T'First;
  use Floats;
  begin
 	for I in 1 .. (DUSize_Blocks - 1)
	loop
		Floats.Data.Block'Read(SIO.Stream(F),FBlock);
		B_Min := Floats.Min(FBlock, B_Min);
		B_Max := Floats.Max(FBlock, B_Max);
	end loop;

	-- Last Block of InFIle
	
	Floats.Data.Block'Read(SIO.Stream(F),FBlock);
	for K in 1 .. (Last_Data_Element_In_Block)
	loop
		FValue := FBlock(K);
		if(FValue < B_Min) then B_Min := FValue; end if;
		if(FValue > B_Max) then B_Max := FValue; end if;
	end loop;

	Min := B_Min;
	Max := B_Max;

 	Put_Line("Min: " & Floats.T'Image(B_Min));
 	Put_Line("Max: " & Floats.T'Image(B_Max));
  end Find_MinMax;
 end MinMax;

 package FMinMax is new MinMax(Floats => Data_Types.F32);
 FMin, FMax : Float_32;

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


 -- read data sequntially by blocks
 FMinMax.Find_MinMax(InFile, FMin, Fmax);
 Put_Line("F Min: " & Float_32'Image(FMin));
 Put_Line("F Max: " & Float_32'Image(FMax));
 

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

