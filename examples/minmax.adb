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
with Generic_Data_Value; use Generic_Data_Value;

procedure minmax
is

 package SIO renames Ada.Streams.Stream_IO;
 use SIO;

 -- experimental
 -- code blocks which access values (need only position and sizes) use: type T is private;
 -- code blocks which operate on Values and do computations use numeric types (digits, range, mod)

 generic
  type TF is digits <>;
  with procedure Element(V : in TF);
 procedure Read_Float_Values(F : SIO.File_Type; DUSize : in Positive);
 procedure Read_Float_Values(F : SIO.File_Type; DUSize : in Positive)
 is
   package FDU is new Generic_Data_Unit(T => TF);
   procedure RFV is new FDU.Read_Array_Values(Element);
 begin
   RFV(F, DUSize);  
 end Read_Float_Values;

 generic
  type TI is range <>;
  type TF is digits <>;-- obsolete if Physical_Value removed from genDataUnit
  with procedure Element(V : in TI);
 procedure Read_Integer_Values(F : SIO.File_Type; DUSize : in Positive);
 procedure Read_Integer_Values(F : SIO.File_Type; DUSize : in Positive)
 is
   package FDU is new Generic_Data_Unit(T => TI);
   procedure RFV is new FDU.Read_Array_Values(Element);
 begin
   RFV(F, DUSize);  
 end Read_Integer_Values;

  
 generic
  type TM is mod <>; -- any modular type a.k.a. unsigned
  with procedure Element(V : in TM);
 procedure Read_Unsigned_Values(F : SIO.File_Type; DUSize : in Positive);
 procedure Read_Unsigned_Values(F : SIO.File_Type; DUSize : in Positive)
 is
   package FDU is new Generic_Data_Unit(T => TM);
   procedure RFV is new FDU.Read_Array_Values(Element);
 begin
   RFV(F, DUSize);  
 end Read_Unsigned_Values;


-- BEG 1 try return Physical values

 generic
  type Tarr is mod <>; 
  type Tphs is digits <>; 
  BZERO, BSCALE : in out Tphs;
  with procedure Element(V : in Tphs);
  with function "+" (L,R : in Tphs) return Tphs;
  with function "*" (L,R : in Tphs) return Tphs;
 procedure Read_Physical_Values(F : SIO.File_Type; DUSize : in Positive);
 procedure Read_Physical_Values(F : SIO.File_Type; DUSize : in Positive)
 is
   procedure LocElement(V : in Tarr)
   is
    function PhysFromFloat is new Physical_Value(Tphs, BZERO, BSCALE,"*","+");
   begin
    Element(PhysFromFloat(Tphs(V)));-- <- This conversion
-- supplied as generic arg func then both Ta Tp can be private - only one code needed, 
-- If conversion left here in code -> needs this code repeated for combinationes of Taa Tphs
--
-- digits-digits -> Floats    scaling  always check NaN <- not recommended but used in practise
-- X range-range   -> Integers  scaling  <- No point in scaling
-- X mod-mod       -> Unsigneds scaling  <- No point in scaling
-- FOR ABOVE THREE USE ONE CODE WITH PRIVATE -> there is no conversion needed
-- range-mod mod-range -> integer sign converionsin
-- digits-range -> I16->F32                         
-- digits-mod   -> U? ->F32  makes sense ???
-- -- except floats all other twice: with/without BLANK
   end LocElement;
   package FDU is new Generic_Data_Unit(T => Tarr);
   procedure RFV is new FDU.Read_Array_Values(LocElement);
 begin
   RFV(F, DUSize);  
 end Read_Physical_Values;
 -- NOTE all Array    Val types: F64 F32 I64 I32 I16 U8
 -- NOTE all Physical Val types: F64 F32 I64 I32 I16 U8, U64 U32 U16 I8

 -- above in lib
 -- below user in application
 
 generic
  type T is private;
  Min, Max : in out T;
  with function "<" (L,R : in T) return Boolean;
  with function ">" (L,R : in T) return Boolean;
 procedure ElemMinMax(V : in T);
 procedure ElemMinMax(V : in T)
 is
 begin
   if(V < Min) then Min := V; end if;
   if(V > Max) then Max := V; end if;
 end ElemMinMax;

 BZEROF32  : Float_32;
 BSCALEF32 : Float_32;
 MinF32 : Float_32;
 MaxF32 : Float_32;
 procedure F32_ElemMinMax is new ElemMinMax(Float_32, MinF32, MaxF32, "<", ">");
 procedure I16F32_ReadPhysVal is
  new Read_Physical_Values(Integer_16,Float_32, BZEROF32, BSCALEF32, F32_ElemMinMax, "+","*");

 -- END 1 experiment

generic
 type T is private;
 Min, Max : in out T;
 with function "<" (L : T; R : T) return Boolean;
 with function ">" (L : T; R : T) return Boolean;
procedure cbMinMax(V : in T);
procedure cbMinMax(V : in T)
is
 Vph : T;-- := Phys_Val(V);
-- use DU;
begin
 if(Vph < Min) then Min := Vph; end if; 
 if(Vph > Max) then Max := Vph; end if; 
end cbMinMax;

 generic
  type TI is range <>;
  Min, Max : in out TI;
 procedure IntElement(V : in TI);
 procedure IntElement(V : in TI)
 is
  procedure IntMinMax is new cbMinMax(TI, Min, Max, "<",">");
 begin
  IntMinMax(V); 
 end IntElement;

 -- instantiate MinMax algorithm on I16

 I16_Min : Integer_16 := Integer_16'Last;
 I16_Max : Integer_16 := Integer_16'First;
 procedure I16_Elem        is new IntElement(Integer_16, I16_Min, I16_Max); 
 procedure I16_Read_Values is new Read_Integer_Values(Integer_16, Float_32, I16_Elem);

-- END experiment

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

 -- Phyiscal Value from Array Value

-- use Generic_Data_Value module:
function F64_Phys_Val is new Physical_Value_From_Float(Float_64, BZERO_F64, BSCALE_F64);
function I64_Phys_Val is new Physical_Value_From_Int(Integer_64, Float_64, BZERO_F64, BSCALE_F64);
function F32_Phys_Val is new Physical_Value_From_Float(Float_32, BZERO, BSCALE);
function I32_Phys_Val is new Physical_Value_From_Int(Integer_32, Float_32, BZERO, BSCALE);
function I16_Phys_Val is new Physical_Value_From_Int(Integer_16, Float_32, BZERO, BSCALE);
function U8_Phys_Val  is new Physical_Value_From_UInt(Unsigned_8, Float_32,BZERO, BSCALE);

-- variants with BLANK FIXME not in use !

BLANK_I64 : Integer_64;
BLANK_I32 : Integer_32;
BLANK_I16 : Integer_16;
BLANK_U8  : Unsigned_8;
function I64_Phys_Val_wBLANK is
  new Physical_Value_From_Int_With_BLANK(Integer_64, Float_64, BZERO_F64, BSCALE_F64, BLANK_I64);
function I32_Phys_Val_wBLANK is
  new Physical_Value_From_Int_With_BLANK(Integer_32, Float_32, BZERO, BSCALE, BLANK_I32);
function I16_Phys_Val_wBLANK is
  new Physical_Value_From_Int_With_BLANK(Integer_16, Float_32, BZERO, BSCALE, BLANK_I16);
function U8_Phys_Val_wBLANK  is
  new Physical_Value_From_UInt_With_BLANK(Unsigned_8, Float_32,BZERO, BSCALE, BLANK_U8);


-- FIXME should all above go to V3_Types ? nneds to be done once

 -- store results 

 F32Min, F32Max : Float_32;
 F64Min, F64Max : Float_64;

-- prepare Element() callback for Read_Array_Value()

-- FIXME this only no-BLANK variant

generic
 type TF is digits <>;
 with package DU is new Generic_Data_Unit(<>);
 with function Phys_Val(Va : DU.T) return TF;
 Min, Max : in out TF;
procedure MinMax(V : in DU.T);
procedure MinMax(V : in DU.T)
is
 Vph : TF := Phys_Val(V);
 use DU;
begin
 if(Vph < Min) then Min := Vph; end if; 
 if(Vph > Max) then Max := Vph; end if; 
end MinMax;

-- Data Unit

-- FIXME check these conversions like I64->F64 loss of precision??
package F64_DU is new Generic_Data_Unit(Float_64);
package I64_DU is new Generic_Data_Unit(Integer_64);
package F32_DU is new Generic_Data_Unit(Float_32);
package I32_DU is new Generic_Data_Unit(Integer_32);
package I16_DU is new Generic_Data_Unit(Integer_16);
package U8_DU  is new Generic_Data_Unit(Unsigned_8);

-- MinMax funcs

procedure F64_MinMax_Elem is new MinMax(Float_64, F64_DU, F64_Phys_Val, F64Min,F64Max);
procedure F64_MinMax is new F64_DU.Read_Array_Values(F64_MinMax_Elem);

procedure F32_MinMax_Elem is new MinMax(Float_32, F32_DU, F32_Phys_Val, F32Min,F32Max);
procedure F32_MinMax is new F32_DU.Read_Array_Values(F32_MinMax_Elem);

procedure I64_MinMax_Elem is new MinMax(Float_64, I64_DU, I64_Phys_Val, F64Min,F64Max);
procedure I64_MinMax is new I64_DU.Read_Array_Values(I64_MinMax_Elem);

procedure I32_MinMax_Elem is new MinMax(Float_32, I32_DU, I32_Phys_Val, F32Min,F32Max);
procedure I32_MinMax is new I32_DU.Read_Array_Values(I32_MinMax_Elem);

procedure I16_MinMax_Elem is new MinMax(Float_32, I16_DU, I16_Phys_Val, F32Min,F32Max);
procedure I16_MinMax is new I16_DU.Read_Array_Values(I16_MinMax_Elem);

procedure U8_MinMax_Elem is new MinMax(Float_32, U8_DU, U8_Phys_Val, F32Min,F32Max);
procedure U8_MinMax is new U8_DU.Read_Array_Values(U8_MinMax_Elem);


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
   F64_MinMax(InFile, DUSize);
 elsif(BITPIX = 64)
 then
   I64_MinMax(InFile, DUSize);
 end if;

 Put_Line("F64 Min: " & Float_64'Image(F64Min));
 Put_Line("F64 Max: " & Float_64'Image(F64Max));

 else
 
 if(BITPIX = -32)
 then
   F32_MinMax(InFile, DUSize);
 elsif(BITPIX = 32)
 then
   I32_MinMax(InFile, DUSize);
 elsif(BITPIX = 16)
 then
   I16_MinMax(InFile, DUSize);
 elsif(BITPIX = 8)
 then
   U8_MinMax(InFile, DUSize);
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

