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
with Generic_Data_Types;

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

 generic
  type T is private;
  B_Min, B_Max : in T; -- init values
  with function "<" (L : in T; R : in T) return Boolean;
  with function ">" (L : in T; R : in T) return Boolean;
 procedure DU_MinMax(F : SIO.File_Type; Min : out T; Max : out T);

 procedure DU_MinMax(F : SIO.File_Type; Min : out T; Max : out T)
 is
   package gen is new Generic_Data_Types (T => T);
   function gen_Min is new gen.Min("<");
   function gen_Max is new gen.Max(">");
   gBlock  : gen.Block;
   DUSize_blocks : constant Positive := DU_Block_Index(Positive(DUSize),T'Size/8);
   Last_Data_Element_In_Block : constant Positive :=  
                                        Offset_In_Block(Positive(DUSize), gen.N);
  gValue : T;
  lMin : T := B_Min;
  lMax : T := B_Max;
 begin
  	for I in 1 .. (DUSize_Blocks - 1)
	loop
		gen.Block'Read(SIO.Stream(F),gBlock);
		lMin := gen_Min(gBlock, lMin);
		lMax := gen_Max(gBlock, lMax);
	end loop;

	-- Last Block of InFIle
	
	gen.Block'Read(SIO.Stream(F),gBlock);
	for K in 1 .. (Last_Data_Element_In_Block)
	loop
		gValue := gBlock(K);
		if(gValue < lMin) then lMin := gValue; end if;
		if(gValue > lMax) then lMax := gValue; end if;
	end loop;

	Min := lMin;
	Max := lMax;

 end DU_MinMax;

 procedure F32_MinMax is 
    new DU_MinMax(Float_32, Float_32'Last, Float_32'First, "<", ">");

 procedure I32_MinMax is 
   new DU_MinMax(Integer_32, Integer_32'Last, Integer_32'First, "<", ">");
 procedure I16_MinMax is 
   new DU_MinMax(Integer_16, Integer_16'Last, Integer_16'First, "<", ">");

 F32Min, F32Max : Float_32;
 I32Min, I32Max : Integer_32;
 I16Min, I16Max : Integer_16;
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
 
 if(BITPIX = -32)
 then
  F32_MinMax(InFile, F32Min, F32Max);
  Put_Line("F32 Min: " & Float_32'Image(F32Min));
  Put_Line("F32 Max: " & Float_32'Image(F32Max));
 elsif(BITPIX = 32)
 then
  I32_MinMax(InFile, I32Min, I32Max);
  Put_Line("I32 Min: " & Integer_32'Image(I32Min));
  Put_Line("I32 Max: " & Integer_32'Image(I32Max));
 elsif(BITPIX = 16)
 then
  I16_MinMax(InFile, I16Min, I16Max);
  Put_Line("I16 Min: " & Integer_16'Image(I16Min));
  Put_Line("I16 Max: " & Integer_16'Image(I16Max));
 end if;

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

