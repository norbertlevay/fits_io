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
with Ada.Unchecked_Conversion;
with Ada.Streams.Stream_IO;
with Interfaces;

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

 -- convert array values to physical values
 -- FIXME add BLANK or make another implementation with BLANK param-> BLANK is optional
 generic
  type Tp is private; -- physical-value type
  with function "*" (L, R : in Tp) return Tp;
  with function "+" (L, R : in Tp) return Tp;
 function Convert_Value(BZERO : in Tp; BSCALE : in Tp; Va : in Tp) return Tp;
 function Convert_Value(BZERO : in Tp; BSCALE : in Tp; Va : in Tp) return Tp
 is
 begin
  return BZERO + BSCALE * Va; 
 end Convert_Value;

 generic
  type Tin  is (<>); -- any discrete type
  type Tout is (<>); -- any discrete type
 function Conv_Signed_Unsigned(Vin : in Tin) return Tout;
 function Conv_Signed_Unsigned(Vin : in Tin) return Tout
 is
   type BArr is array (1 .. Tin'Size) of Boolean;
   pragma Pack (BArr);
   function Tin2BArr  is new Ada.Unchecked_Conversion(Tin,BArr);
   function BArr2Tout is new Ada.Unchecked_Conversion(BArr,Tout);
   Arr : BArr := Tin2BArr(Vin);
 begin
  -- convert sign-unsigned by flipping MSB bit
  Arr(Arr'Last) := not Arr(Arr'Last);
  return BArr2Tout(Arr);
 end Conv_Signed_Unsigned;

 function I16_To_U16 is new Conv_Signed_Unsigned(Integer_16, Unsigned_16);



 -- access array values 

 generic
  type T is private;
  type Tp is private;
  BZERO : Tp;
  BSCALE : Tp;
  with function Convert_Value(BZERO : in Tp; BSCALE : in Tp; Va : in T) return Tp;
  B_Min, B_Max : in Tp; -- init values
  with function "<" (L : in Tp; R : in Tp) return Boolean;
  with function ">" (L : in Tp; R : in Tp) return Boolean;
 procedure DU_MinMax(F : SIO.File_Type; Min : out Tp; Max : out Tp);

 procedure DU_MinMax(F : SIO.File_Type; Min : out Tp; Max : out Tp)
 is
   package gen is new Generic_Data_Types (T => T);
--   function gen_Min is new gen.Min("<");
--   function gen_Max is new gen.Max(">");
   gBlock  : gen.Block;
   DUSize_blocks : constant Positive := DU_Block_Index(Positive(DUSize),T'Size/8);
   Last_Data_Element_In_Block : constant Positive :=  
                                        Offset_In_Block(Positive(DUSize), gen.N);
  gValue : T;
  lMin : Tp := B_Min;
  lMax : Tp := B_Max;
  pVal : Tp;
 begin
  	for I in 1 .. (DUSize_Blocks - 1)
	loop
--		gen.Block'Read(SIO.Stream(F),gBlock);
--		lMin := gen_Min(gBlock, lMin);
--		lMax := gen_Max(gBlock, lMax);

	        gen.Block'Read(SIO.Stream(F),gBlock);
        	for K in 1 .. gen.N
        	loop
                	gValue := gBlock(K);
 			pVal := Convert_Value(BZERO,BSCALE,gValue);
                	if(pVal < lMin) then lMin := pVal; end if;
                	if(pVal > lMax) then lMax := pVal; end if;
        	end loop;
	end loop;

	-- Last Block of InFIle
	
	gen.Block'Read(SIO.Stream(F),gBlock);
	for K in 1 .. (Last_Data_Element_In_Block)
	loop
		gValue := gBlock(K);
 		pVal := Convert_Value(BZERO,BSCALE,gValue);
		if(pVal < lMin) then lMin := pVal; end if;
		if(pVal > lMax) then lMax := pVal; end if;
	end loop;

	Min := lMin;
	Max := lMax;

 end DU_MinMax;

 -- instantiations

 function Conv_Int16_To_F32(BZERO : Float_32; BSCALE : Float_32; Va : Integer_16 ) return Float_32
 is
   Vt : Float_32 := Float_32(Va);
 begin
   return BZERO + BSCALE * Vt;
 end Conv_Int16_To_F32;

 BSCALE : constant Data_Types.Float_32 :=   0.003891051;
 BZERO  : constant Data_Types.Float_32 := 127.501945525;

 --procedure F32_MinMax is 
   -- new DU_MinMax(Float_32, Float_32'Last, Float_32'First, "<", ">");

-- procedure I32_MinMax is 
  -- new DU_MinMax(Integer_32, Integer_32'Last, Integer_32'First, "<", ">");
 procedure I16_MinMax is 
   new DU_MinMax(Integer_16, Float_32, BZERO, BSCALE, Conv_Int16_To_F32, Float_32'Last, Float_32'First, "<", ">");

 F32Min, F32Max : Float_32;
 I32Min, I32Max : Integer_32;
 I16Min, I16Max : Integer_16;

begin

 Put_Line("Usage  " & Command_Name & " <file name>");

 Put_Line("CC -32768 : " & Unsigned_16'Image(I16_To_U16(-32768)) );
 Put_Line("CC     -1 : " & Unsigned_16'Image(I16_To_U16(-1)) );
 Put_Line("CC      0 : " & Unsigned_16'Image(I16_To_U16(0)) );
 Put_Line("CC      1 : " & Unsigned_16'Image(I16_To_U16(1)) );
 Put_Line("CC  32767 : " & Unsigned_16'Image(I16_To_U16(+32767)) );

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
 
-- if(BITPIX = -32)
-- then
--  F32_MinMax(InFile, F32Min, F32Max);
--  Put_Line("F32 Min: " & Float_32'Image(F32Min));
--  Put_Line("F32 Max: " & Float_32'Image(F32Max));
-- elsif(BITPIX = 32)
-- then
--  I32_MinMax(InFile, I32Min, I32Max);
--  Put_Line("I32 Min: " & Integer_32'Image(I32Min));
--  Put_Line("I32 Max: " & Integer_32'Image(I32Max));
 if(BITPIX = 16)
 then
  I16_MinMax(InFile, F32Min, F32Max);
  Put_Line("F32phys Min: " & Float_32'Image(F32Min));
  Put_Line("F32phys Max: " & Float_32'Image(F32Max));
--  Put_Line("I16 Min: " & Integer_16'Image(I16Min));
--  Put_Line("I16 Max: " & Integer_16'Image(I16Max));
--  Put_Line("F32 Min: " & Float_32'Image(BZERO + BSCALE * Float_32(I16Min)));
--  Put_Line("F32 Max: " & Float_32'Image(BZERO + BSCALE * Float_32(I16Max)));
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

