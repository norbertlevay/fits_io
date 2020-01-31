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
with Optional;
with Optional.Reserved; use Optional.Reserved;


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

 -- access array values 

 BZERO  : Float_32 := 0.0;
 BSCALE : Float_32 := 1.0;
 -- will be filled in by Analyze_Array_Keys()

 generic
  type T is private;
  type TF is digits <>;
  with function Conv_TF(P : in T) return TF;

  B_Min, B_Max : in TF; -- init values
  with function "<" (L : in TF; R : in TF) return Boolean;
  with function ">" (L : in TF; R : in TF) return Boolean;
 procedure DU_MinMax(F : SIO.File_Type; Min : out TF; Max : out TF);

 procedure DU_MinMax(F : SIO.File_Type; Min : out TF; Max : out TF)
 is
   package gen is new Generic_Data_Types (T => T);
   gBlock  : gen.Block;
   function T_Physical_Value is new gen.Physical_Value(TF => TF, To_TF => Conv_TF);
   DUSize_blocks : constant Positive := DU_Block_Index(Positive(DUSize),T'Size/8);
   Last_Data_Element_In_Block : constant Positive :=  
                                        Offset_In_Block(Positive(DUSize), gen.N);
  gValue : T;
  lMin : TF := B_Min;
  lMax : TF := B_Max;
  pVal : TF;
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
 			pVal := T_Physical_Value(TF(BZERO),TF(BSCALE),gValue);
                	if(pVal < lMin) then lMin := pVal; end if;
                	if(pVal > lMax) then lMax := pVal; end if;
        	end loop;
	end loop;

	-- Last Block of InFIle
	
	gen.Block'Read(SIO.Stream(F),gBlock);
	for K in 1 .. (Last_Data_Element_In_Block)
	loop
		gValue := gBlock(K);
 		pVal := T_Physical_Value(TF(BZERO),TF(BSCALE),gValue);
		if(pVal < lMin) then lMin := pVal; end if;
		if(pVal > lMax) then lMax := pVal; end if;
	end loop;

	Min := lMin;
	Max := lMax;

 end DU_MinMax;

 -- instantiations
 
 generic
  type T is private;
 function Null_Conv(Vin : in T) return T;
 function Null_Conv(Vin : in T) return T
 is
 begin
  return Vin;
 end Null_Conv;

 function F32_Null_Conv is new Null_Conv(Float_32);
 function F64_Null_Conv is new Null_Conv(Float_64);

 generic
  type TI is range <>;
  type TF is digits <>;
 function Int_To_Float(P:in TI) return TF;
 function Int_To_Float(P:in TI) return TF
 is
 begin
  return TF(P);
 end Int_To_Float;

 function I32_To_F32 is new Int_To_Float(Integer_32, Float_32);
 function I16_To_F32 is new Int_To_Float(Integer_16, Float_32);
 function U8_To_F32(P:Unsigned_8) return Float_32
  is begin return Float_32(P); end U8_to_F32;

 function I64_To_F64 is new Int_To_Float(Integer_64, Float_64);
 function I32_To_F64 is new Int_To_Float(Integer_32, Float_64);
 function I16_To_F64 is new Int_To_Float(Integer_16, Float_64);
 function U8_To_F64(P:Unsigned_8) return Float_64
  is begin return Float_64(P); end U8_to_F64;


procedure F64_MinMax is 
 new DU_MinMax(Float_64, Float_64, F64_Null_Conv, Float_64'Last, Float_64'First, "<", ">");

procedure I64_MinMax is 
 new DU_MinMax(Integer_64, Float_64, I64_To_F64, Float_64'Last, Float_64'First, "<", ">");


procedure F32_MinMax is 
 new DU_MinMax(Float_32, Float_32, F32_Null_Conv, Float_32'Last, Float_32'First, "<", ">");

procedure I32_MinMax is 
 new DU_MinMax(Integer_32, Float_32, I32_To_F32, Float_32'Last, Float_32'First, "<", ">");

procedure I16_MinMax is 
 new DU_MinMax(Integer_16, Float_32, I16_To_F32, Float_32'Last, Float_32'First, "<", ">");

procedure U8_MinMax is 
 new DU_MinMax(Unsigned_8, Float_32, U8_To_F32, Float_32'Last, Float_32'First, "<", ">");

 -- store results 

 F32Min, F32Max : Float_32;
 F64Min, F64Max : Float_64;

 -- analyze array keys

 ArrKeysGiven : Boolean;

 function Analyze_Array_Keys( Cards : Optional.Card_Arr; 
			      BZERO  : in out Float_32; 
			      BSCALE : in out Float_32 ) return Boolean
 is
  BZfound : Boolean := False;
  BSfound : Boolean := False;
 begin
   for I in Cards'Range
   loop
     Put_Line("RESKEYS: >" & Cards(I) & "<" );
     if(Cards(I)(1..5) = "BZERO")
     then
	BZfound := True;
	BZERO := Float_32'Value((Cards(I)(11..30)));
     elsif(Cards(I)(1..6) = "BSCALE")
     then
	BSfound := True;
	BSCALE := Float_32'Value((Cards(I)(11..30)));
     end if;
     -- FIXME parse BLANK
   end loop;

  return BZfound AND BSfound;
 end Analyze_Array_Keys;

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
 
 -- reset to File start
 SIO.Set_Index(InFile,1);

 declare
   Cards : Optional.Card_Arr := Read_Header(InFile, Optional.Reserved.Array_Keys);
 begin
   ArrKeysGiven := Analyze_Array_Keys(Cards, BZERO, BSCALE);
 end;

 Put_Line("BZERO  :" & Float_32'Image(BZERO)); 
 Put_Line("BSCALE :" & Float_32'Image(BSCALE)); 
 
 -- read data sequentially by blocks

 if ( abs BITPIX > 32 )
 then
 
 if(BITPIX = -64)
 then
   F64_MinMax(InFile, F64Min, F64Max);
 elsif(BITPIX = 64)
 then
   I64_MinMax(InFile, F64Min, F64Max);
 end if;

 Put_Line("F64 Min: " & Float_64'Image(F64Min));
 Put_Line("F64 Max: " & Float_64'Image(F64Max));

 else
 
 if(BITPIX = -32)
 then
   F32_MinMax(InFile, F32Min, F32Max);
 elsif(BITPIX = 32)
 then
   I32_MinMax(InFile, F32Min, F32Max);
 elsif(BITPIX = 16)
 then
   I16_MinMax(InFile, F32Min, F32Max);
 elsif(BITPIX = 8)
 then
   U8_MinMax(InFile, F32Min, F32Max);
 end if;

 Put_Line("F32 Min: " & Float_32'Image(F32Min));
 Put_Line("F32 Max: " & Float_32'Image(F32Max));

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

