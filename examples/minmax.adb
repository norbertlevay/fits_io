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

 -- Value conversions: Array -> Physical value

 -- A, BZERO BSCALE (BLANK) not given 	=> PhysVal = ArrVal
 -- B, BITPIX IntNN and Tab11         	=> PhysVal = Conv_Signed_Unsigned( ArrVal )
 -- C, BITPIX Int16 and not Tab11     	=> PhysVal = I16_To_F32( ArrVal )
 -- D, BITPIX Float		      	=> PhysVal = Convert_Float_Value( ArrVal )
 -- other combinations for case C not supported

 generic
  type T is digits <>; -- any floating point type
 function Convert_Float_Value(BZERO : in T; BSCALE : in T; Va : in T) return T;
 function Convert_Float_Value(BZERO : in T; BSCALE : in T; Va : in T) return T
 is
 begin
  return BZERO + BSCALE * Va; 
 end Convert_Float_Value;


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


 -- all types FITS v3

 function U8_To_I8   is new Conv_Signed_Unsigned(Unsigned_8, Integer_8);
 function I16_To_U16 is new Conv_Signed_Unsigned(Integer_16, Unsigned_16);
 function I32_To_U32 is new Conv_Signed_Unsigned(Integer_32, Unsigned_32);
 function I64_To_U64 is new Conv_Signed_Unsigned(Integer_32, Unsigned_32);

 function F32_PhysValue is new Convert_Float_Value(Float_32);
 function F64_PhysValue is new Convert_Float_Value(Float_64);

 function I16_To_F32(BZERO : in Float_32; BSCALE : in Float_32; 
                     BLANK : in Integer_16; Va : in Integer_16) return Float_32
 is
  function loc_I16_To_F32 is new Convert_Float_Value(Float_32);
 begin
  if(BLANK = Va)
  then
    null; -- Raise exception Undefined Value
  end if;
  return loc_I16_To_F32(BZERO, BSCALE, Float_32(Va)); 
 end I16_To_F32;


 -- access array values 

 generic
  type T is private;
  type Tp is private;
  with function Convert_Value(Va : in T) return Tp;

  B_Min, B_Max : in Tp; -- init values
  with function "<" (L : in Tp; R : in Tp) return Boolean;
  with function ">" (L : in Tp; R : in Tp) return Boolean;
 procedure DU_MinMax(F : SIO.File_Type; Min : out Tp; Max : out Tp);

 procedure DU_MinMax(F : SIO.File_Type; Min : out Tp; Max : out Tp)
 is
   package gen is new Generic_Data_Types (T => T);
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
 			pVal := Convert_Value(gValue);
                	if(pVal < lMin) then lMin := pVal; end if;
                	if(pVal > lMax) then lMax := pVal; end if;
        	end loop;
	end loop;

	-- Last Block of InFIle
	
	gen.Block'Read(SIO.Stream(F),gBlock);
	for K in 1 .. (Last_Data_Element_In_Block)
	loop
		gValue := gBlock(K);
 		pVal := Convert_Value(gValue);
		if(pVal < lMin) then lMin := pVal; end if;
		if(pVal > lMax) then lMax := pVal; end if;
	end loop;

	Min := lMin;
	Max := lMax;

 end DU_MinMax;

 -- instantiations

 BZERO, BSCALE : Float_32;
 -- will be filled in by Analyze_Array_Keys()

 function Conv_Int16_To_F32(Va : Integer_16 ) return Float_32
 is
  BLANK  : constant Integer_16 := 0; -- FIXME not in use
 begin
   return I16_To_F32(BZERO, BSCALE, BLANK, Va);
 end Conv_Int16_To_F32;


 function Conv_F32_PhysValue(Va : Float_32 ) return Float_32
 is
 begin
   return F32_PhysValue(BZERO,BSCALE, Va);
 end Conv_F32_PhysValue;

 function I32_Null_Conv(Va : Integer_32 ) return Integer_32
 is
 begin
   return Va;
 end I32_Null_Conv;

 function F32_Null_Conv(Va : Float_32 ) return Float_32
 is
 begin
   return Va;
 end F32_Null_Conv;



procedure F32_MinMax_NullConv is 
 new DU_MinMax(Float_32, Float_32, F32_Null_Conv, Float_32'Last, Float_32'First, "<", ">");

procedure F32_MinMax is 
 new DU_MinMax(Float_32, Float_32, Conv_F32_PhysValue, Float_32'Last, Float_32'First, "<", ">");

procedure I32_MinMax_NullConv is 
 new DU_MinMax(Integer_32, Integer_32, I32_Null_Conv, Integer_32'Last, Integer_32'First, "<", ">");

procedure I16_MinMax_I16F32 is 
 new DU_MinMax(Integer_16, Float_32, Conv_Int16_To_F32, Float_32'Last, Float_32'First, "<", ">");


 -- store results 

 F32Min, F32Max : Float_32;
 I32Min, I32Max : Integer_32;

 -- analyze array keys

 ArrKeysGiven : Boolean;

 -- FIXME this should operate on Value-strings rather then converted floats
 function Is_Tab11(BZERO : Float_32; BSCALE : Float_32) return Boolean
 is
 begin
  if((BSCALE = 1.0) AND 
     ((BZERO = -128.0) OR
      (BZERO = 32768.0) OR
      (BZERO = Float_32(2**31)) OR
      (BZERO = Float_32(2**63)))	)
  then
   return True;
  else
   return False;
  end if;
 end Is_Tab11;

 function Analyze_Array_Keys( Cards : Optional.Card_Arr; 
				BZERO : out Float_32; 
				BSCALE : out Float_32 ) return Boolean
 is
  BZfound : Boolean := False;
  BSfound : Boolean := False;
  Tab11found : Boolean := False;
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
   end loop;

   if(BZfound AND BSfound)
   then
    Tab11found := Is_Tab11(BZERO,BSCALE);
    Put_Line("BZERO  :" & Float_32'Image(BZERO)); 
    Put_Line("BSCALE :" & Float_32'Image(BSCALE)); 
   end if;
   return BZfound AND BSfound;
 end Analyze_Array_Keys;

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

 -- reset to File start
 SIO.Set_Index(InFile,1);

 declare
   Cards : Optional.Card_Arr := Read_Header(InFile, Optional.Reserved.Array_Keys);
 begin
   ArrKeysGiven := Analyze_Array_Keys(Cards, BZERO, BSCALE);
 end;


 -- read data sequentially by blocks
 
 if(BITPIX = -32)
 then

  if(ArrKeysGiven) 
  then
   F32_MinMax(InFile, F32Min, F32Max);-- PhysVal = BZERO + BSCALE*ArrVal
 else
   F32_MinMax_NullConv(InFile, F32Min, F32Max);-- PhysVal = ArrVal
  end if;
  Put_Line("F32 Min: " & Float_32'Image(F32Min));
  Put_Line("F32 Max: " & Float_32'Image(F32Max));
 
 elsif(BITPIX = 32)
 then

  if(ArrKeysGiven) 
  then
   null;-- FIXME instantiate, sign conversion
  else
   I32_MinMax_NullConv(InFile, I32Min, I32Max); -- PhysValue = ArrValue
   Put_Line("I32 Min: " & Integer_32'Image(I32Min));
   Put_Line("I32 Max: " & Integer_32'Image(I32Max));
 end if;

 elsif(BITPIX = 16)
 then

  if(ArrKeysGiven) 
  then
   if(Is_Tab11(BZERO,BSCALE))
   then
    null; -- FIXME sign conversion instantiate
   else
    I16_MinMax_I16F32(InFile, F32Min, F32Max);-- Float32 stored as Int16
    Put_Line("I16F32 Min: " & Float_32'Image(F32Min));
    Put_Line("I16F32 Max: " & Float_32'Image(F32Max));
   end if;
  else
   null; -- FIXME instantiate, PhysVal = ArrVal 
  end if;

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

