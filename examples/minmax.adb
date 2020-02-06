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
with V3_Data_Unit; use V3_Data_Unit;
with Data_Funcs; use Data_Funcs;

with Generic_Data_Block;
with Generic_Data_Unit;
with Generic_Data_Value; use Generic_Data_Value;

procedure minmax
is

 procedure print_ranges
 is
 begin
  New_Line;
  Put_Line("Float_64: " & Natural'Image(Float_64'Digits) & " -> " & Float_64'Image(Float_64'First)  & " - " & Float_64'Image(Float_64'Last));
  Put_Line("Float_32: " & Natural'Image(Float_32'Digits) & " -> " & Float_32'Image(Float_32'First)  & " - " & Float_32'Image(Float_32'Last));
  Put_Line("Int_64: " & Integer_64'Image(Integer_64'First)  & " - " & Integer_64'Image(Integer_64'Last));
  Put_Line("Int_32: " & Integer_32'Image(Integer_32'First)  & " - " & Integer_32'Image(Integer_32'Last));
  Put_Line("Int_16: " & Integer_16'Image(Integer_16'First)  & " - " & Integer_16'Image(Integer_16'Last));
  Put_Line("UInt_8: " & Unsigned_8'Image(Unsigned_8'First)  & " - " & Unsigned_8'Image(Unsigned_8'Last));
 end print_ranges;

 package SIO renames Ada.Streams.Stream_IO;
 use SIO;



-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------


 generic
    type Ti is range <>;  -- type in file
    type Tf is digits <>; -- physical value type
 package V3_Data is

  type Array_Keys_Rec is
    record
	DATAMIN_Avail : Boolean;
	DATAMAX_Avail : Boolean;
 	BLANK_Avail   : Boolean;
 	BZERO,BSCALE    : Tf;
	DATAMIN,DATAMAX : Tf;
	BLANK 		: Ti;-- FIXME how to handle UInt8
  end record;

 function Array_Value_Rec(Cards : Optional.Card_Arr; BITPIX : Integer) return Array_Keys_Rec;

 end V3_Data; 

 -- Body

 package body V3_Data is

 function Array_Value_Rec(Cards : Optional.Card_Arr; BITPIX : Integer) return Array_Keys_Rec
 is
  V : Array_Keys_Rec;
 begin

   -- Init FIXME how to do with NUllRec init ??
   V.DATAMIN_Avail := False;
   V.DATAMAX_Avail := False;
   V.BLANK_Avail   := False;
   V.BZERO  := 0.0;
   V.BSCALE := 1.0;

   for I in Cards'Range
   loop

     Put_Line("V3_Data::RESKEYS: >" & Cards(I) & "<" );

     if(Cards(I)(1..5) = "BZERO")
     then
	V.BZERO := Tf'Value(Cards(I)(11..30));

     elsif(Cards(I)(1..6) = "BSCALE")
     then
	V.BSCALE := Tf'Value(Cards(I)(11..30));

     elsif(Cards(I)(1..5) = "BLANK")
     then
	V.BLANK := Ti'Value(Cards(I)(11..30));
	V.BLANK_Avail := True;

     elsif(Cards(I)(1..7) = "DATAMIN")
     then
	V.DATAMIN := Tf'Value(Cards(I)(11..30));
	V.DATAMIN_Avail := True;

     elsif(Cards(I)(1..7) = "DATAMAX")
     then
	V.DATAMAX := Tf'Value(Cards(I)(11..30));
	V.DATAMAX_Avail := True;
   end if;

   end loop;

  return V;
 end Array_Value_Rec;

 end V3_Data; 

 package I64F64_V3_Data is new V3_Data(Integer_64, Float_64);
 package I32F64_V3_Data is new V3_Data(Integer_32, Float_64);
 package I16F32_V3_Data is new V3_Data(Integer_16, Float_32);
 --package UI8F32_V3_Data is new V3_Data(Float_32, Unsigned_8);
 --FIXME reconsider as not reasonable: F32 = BZERO + BSCALE * UI8




-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------

  -- MinMax implementation

 generic
  type T is private;
  Min, Max : in out T;
  with function "<" (L,R : in T) return Boolean is <>;
  with function ">" (L,R : in T) return Boolean is <>;
 procedure ElemMinMax(V : in T);
 procedure ElemMinMax(V : in T)
 is
 begin
   if(V < Min) then Min := V; end if;
   if(V > Max) then Max := V; end if;
 end ElemMinMax;


 MinF32 : Float_32 := Float_32'Last;
 MaxF32 : Float_32 := Float_32'First;
 procedure F32_ElemMinMax is new ElemMinMax(Float_32, MinF32, MaxF32);

 MinF64 : Float_64 := Float_64'Last;
 MaxF64 : Float_64 := Float_64'First;
 procedure F64_ElemMinMax is new ElemMinMax(Float_64, MinF64, MaxF64);

 -- example of signed-unsigned conversion

 MinU64 : Unsigned_64 := Unsigned_64'Last;
 MaxU64 : Unsigned_64 := Unsigned_64'First;
 procedure U64_ElemMinMax is new ElemMinMax(Unsigned_64, MinU64, MaxU64);

 MinU32 : Unsigned_32 := Unsigned_32'Last;
 MaxU32 : Unsigned_32 := Unsigned_32'First;
 procedure U32_ElemMinMax is new ElemMinMax(Unsigned_32, MinU32, MaxU32);

 MinU16 : Unsigned_16 := Unsigned_16'Last;
 MaxU16 : Unsigned_16 := Unsigned_16'First;
 procedure U16_ElemMinMax is new ElemMinMax(Unsigned_16, MinU16, MaxU16);

 MinI8 : Integer_8 := Integer_8'Last;
 MaxI8 : Integer_8 := Integer_8'First;
 procedure I8_ElemMinMax is new ElemMinMax(Integer_8, MinI8, MaxI8);

 -- MinMax for all array types

 -- integer data if BLANK available (undefined value check)
 
 UndefValCnt : Natural := 0; -- FIXME must be reset at each Header read start
 procedure UndefVal is begin UndefValCnt := UndefValCnt + 1; end UndefVal;

 -- integer data if BLANK is available

 procedure I64_Checked_MinMax is new I64F64.Read_Checked_Scaled_Integers(F64_ElemMinMax, UndefVal);
 procedure I32_Checked_MinMax is new I32F64.Read_Checked_Scaled_Integers(F64_ElemMinMax, UndefVal);
 procedure I16_Checked_MinMax is new I16F32.Read_Checked_Scaled_Integers(F32_ElemMinMax, UndefVal);
 procedure UI8_Checked_MinMax is new UI8F32.Read_Checked_Scaled_Integers(F32_ElemMinMax, UndefVal);

 -- integer data if BLANK not available

 procedure I64_MinMax is new I64F64.Read_Scaled_Values(F64_ElemMinMax);
 procedure I32_MinMax is new I32F64.Read_Scaled_Values(F64_ElemMinMax);
 procedure I16_MinMax is new I16F32.Read_Scaled_Values(F32_ElemMinMax);
 procedure UI8_MinMax is new UI8F32.Read_Scaled_Values(F32_ElemMinMax);

 -- integer sign conversion variants if BZERO BSCALE = Tab11 (BLANK converts also)

 procedure U64_MinMax is new I64U64.Read_Sign_Converted_Integers(U64_ElemMinMax);
 procedure U32_MinMax is new I32U32.Read_Sign_Converted_Integers(U32_ElemMinMax);
 procedure U16_MinMax is new I16U16.Read_Sign_Converted_Integers(U16_ElemMinMax);
 procedure I8_MinMax  is new UI8I8.Read_Sign_Converted_Integers( I8_ElemMinMax);

 -- floats (always with undefined value check)

 procedure F64_Checked_MinMax is new F64F64.Read_Checked_Scaled_Floats(F64_ElemMinMax, UndefVal);
 procedure F32_Checked_MinMax is new F32F32.Read_Checked_Scaled_Floats(F32_ElemMinMax, UndefVal);

 -- vars

 InFile   : SIO.File_Type;
 HDUStart : Positive := 1; -- Primary HDU only
 
 -- info from Header

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

 BITPIX  : Integer;
 DUSize  : Positive;
 DUStart : Positive;

-------------------------------------------------------------------------------
-- MAIN -------------------------------------------- MAIN ---------------------
begin 
 
 if(Argument_Count /= 1 )
 then 
  Put_Line("Usage  " & Command_Name & " <file name>");
  return;
 else
   SIO.Open(InFile, SIO.In_File, (Argument(1)));
 end if;

 Set_File_Block_Index(InFile,HDUStart);

 -- interpret header: DataUnit length and type needed, also store DUStart

 declare
  HDUInfo : HDU_Info_Type := Read_Header(InFile);
 begin
  DUStart := File_Block_Index(InFile);
  DUSize  := Positive(DU_Count (HDUInfo.NAXISn));
  BITPIX := HDUInfo.BITPIX;
 end;
 
 Put_Line("DU Start [blocks] :" & Positive'Image(DUStart)); 
 Put_Line("DU Size  [element count]:" & Positive'Image(DUSize)); 

 -- reset to Header start and read it again
 Set_File_Block_Index(InFile,HDUStart);

 declare
   Cards : Optional.Card_Arr := Read_Header(InFile, Optional.Reserved.Array_Keys);
   F64Keys : I32F64_V3_Data.Array_Keys_Rec;
   F32Keys : I16F32_V3_Data.Array_Keys_Rec;
   I64Keys : I64F64_V3_Data.Array_Keys_Rec;
   I32Keys : I32F64_V3_Data.Array_Keys_Rec;
   I16Keys : I16F32_V3_Data.Array_Keys_Rec;
 begin

 -- read data

 if ( abs BITPIX > 32 OR  BITPIX = 32 )
 then

 -- 64 bit Physical data
 
 if(BITPIX = -64)
 then
   F64Keys := I32F64_V3_Data.Array_Value_Rec(Cards,BITPIX);   
   F64_Checked_MinMax(InFile, DUSize, F64Keys.BZERO, F64Keys.BSCALE);
   Put_Line("UndefVal count: " & Natural'Image(UndefValCnt));

 elsif(BITPIX = 64)
 then
   I64Keys := I64F64_V3_Data.Array_Value_Rec(Cards,BITPIX);   
   if(I64Keys.BLANK_Avail)
   then
     Put_Line("BLANK : " & Integer_64'Image(I64Keys.BLANK));
     I64_Checked_MinMax(InFile, DUSize, I64Keys.BZERO, I64Keys.BSCALE,I64Keys.BLANK);
     Put_Line("UndefVal count: " & Natural'Image(UndefValCnt));
   else
     I64_MinMax(InFile, DUSize, I64Keys.BZERO, I64Keys.BSCALE);
   end if;

 elsif(BITPIX = 32)
 then
   I32Keys := I32F64_V3_Data.Array_Value_Rec(Cards,BITPIX);   
   if(I32Keys.BLANK_Avail)
   then
     Put_Line("BLANK : " & Integer_32'Image(I32Keys.BLANK));
     I32_Checked_MinMax(InFile, DUSize, I32Keys.BZERO, I32Keys.BSCALE,I32Keys.BLANK);
     Put_Line("UndefVal count: " & Natural'Image(UndefValCnt));
   else
     I32_MinMax(InFile, DUSize, I32Keys.BZERO, I32Keys.BSCALE);
   end if;
 end if;

 Put_Line("F64 Min: " & Float_64'Image(MinF64));
 Put_Line("F64 Max: " & Float_64'Image(MaxF64));


 else

 -- 32 bit Physical data

 
 if(BITPIX = -32)
 then
   F32Keys := I16F32_V3_Data.Array_Value_Rec(Cards,BITPIX);   
   F32_Checked_MinMax(InFile, DUSize, F32Keys.BZERO, F32Keys.BSCALE);
   Put_Line("UndefVal count: " & Natural'Image(UndefValCnt));

 elsif(BITPIX = 16)
 then

   I16Keys := I16F32_V3_Data.Array_Value_Rec(Cards,BITPIX);   

   if(I16Keys.BLANK_Avail)
   then
     Put_Line("BLANK : " & Integer_16'Image(I16Keys.BLANK));
     I16_Checked_MinMax(InFile, DUSize, I16Keys.BZERO, I16Keys.BSCALE, I16Keys.BLANK);
     Put_Line("UndefVal count: " & Natural'Image(UndefValCnt));
   else
     I16_MinMax(InFile, DUSize, I16Keys.BZERO, I16Keys.BSCALE);
   end if;

   Set_File_Block_Index(InFile,DUStart);

   -- test Int-UInt conversion
   U16_MinMax(InFile, DUSize); 
   Put_Line("U16 Min: " & Unsigned_16'Image(MinU16) & " / "& Unsigned_16'Image(Unsigned_16'First));
   Put_Line("U16 Max: " & Unsigned_16'Image(MaxU16) & " / "& Unsigned_16'Image(Unsigned_16'Last));

 elsif(BITPIX = 8)
 then
   -- FIXME what to do with UI8 I8 ??
   --UI8_MinMax(InFile, DUSize, BZEROF32, BSCALEF32);
   null;
 end if;

 Put_Line("F32 Min: " & Float_32'Image(MinF32));
 Put_Line("F32 Max: " & Float_32'Image(MaxF32));

 end if;

 end; -- declare Cards : ...

 SIO.Close(InFile);

 print_ranges;
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

