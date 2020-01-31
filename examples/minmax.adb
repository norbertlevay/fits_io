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
with V3_Types; use V3_Types;
with Data_Funcs; use Data_Funcs;

with Generic_Data_Block;
with Generic_Data_Unit;

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
 DUSize : Positive;

 -- access array values 

 BZERO  : Float_32 := 0.0;
 BSCALE : Float_32 := 1.0;
 BZERO_F64  : Float_64 := 0.0;-- FIXME needed F64 because generic_data_unit has these as in out
 BSCALE_F64 : Float_64 := 1.0; -- so 64bit variants require memory too
 -- will be filled in by Analyze_Array_Keys()

 -- instantiations
 -- FIXME should these go to V3_Types ??
 
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


-- Data Unit

-- FIXME check these conversions like I64->F64 loss of precision??
package F64_DU is new Generic_Data_Unit(Float_64,  Float_64, BZERO_F64, BSCALE_F64, F64_Null_Conv);
package I64_DU is new Generic_Data_Unit(Integer_64,Float_64, BZERO_F64, BSCALE_F64, I64_To_F64);
package F32_DU is new Generic_Data_Unit(Float_32,  Float_32, BZERO, BSCALE, F32_Null_Conv);
package I32_DU is new Generic_Data_Unit(Integer_32,Float_32, BZERO, BSCALE, I32_To_F32);
package I16_DU is new Generic_Data_Unit(Integer_16,Float_32, BZERO, BSCALE, I16_To_F32);
package U8_DU  is new Generic_Data_Unit(Unsigned_8,Float_32, BZERO, BSCALE, U8_To_F32);

-- MinMax func

procedure F64_MinMax is new F64_DU.MinMax(Float_64'Last, Float_64'First, "<", ">");
procedure I64_MinMax is new I64_DU.MinMax(Float_64'Last, Float_64'First, "<", ">");
procedure F32_MinMax is new F32_DU.MinMax(Float_32'Last, Float_32'First, "<", ">");
procedure I32_MinMax is new I32_DU.MinMax(Float_32'Last, Float_32'First, "<", ">");
procedure I16_MinMax is new I16_DU.MinMax(Float_32'Last, Float_32'First, "<", ">");
procedure U8_MinMax  is new U8_DU.MinMax (Float_32'Last, Float_32'First, "<", ">");


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
	BZERO_F64 := Float_64(BZERO); -- FIXME find betted solution
     elsif(Cards(I)(1..6) = "BSCALE")
     then
	BSfound := True;
	BSCALE := Float_32'Value((Cards(I)(11..30)));
	BSCALE_F64 := Float_64(BSCALE); -- FIXME find betted solution
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
  DUSize := Positive(DU_Count (HDUInfo.NAXISn));
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
   F64_MinMax(InFile, DUSize, F64Min, F64Max);
 elsif(BITPIX = 64)
 then
   I64_MinMax(InFile, DUSize, F64Min, F64Max);
 end if;

 Put_Line("F64 Min: " & Float_64'Image(F64Min));
 Put_Line("F64 Max: " & Float_64'Image(F64Max));

 else
 
 if(BITPIX = -32)
 then
   F32_MinMax(InFile, DUSize, F32Min, F32Max);
 elsif(BITPIX = 32)
 then
   I32_MinMax(InFile, DUSize, F32Min, F32Max);
 elsif(BITPIX = 16)
 then
   I16_MinMax(InFile, DUSize, F32Min, F32Max);
 elsif(BITPIX = 8)
 then
   U8_MinMax(InFile, DUSize, F32Min, F32Max);
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

