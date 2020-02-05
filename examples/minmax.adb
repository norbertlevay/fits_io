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


 BZEROF32  : Float_32 := 0.0;
 BSCALEF32 : Float_32 := 1.0;
 MinF32 : Float_32 := Float_32'Last;
 MaxF32 : Float_32 := Float_32'First;
 procedure F32_ElemMinMax is new ElemMinMax(Float_32, MinF32, MaxF32);


 BZEROF64  : Float_64 := 0.0;
 BSCALEF64 : Float_64 := 1.0;
 MinF64 : Float_64 := Float_64'Last;
 MaxF64 : Float_64 := Float_64'First;
 procedure F64_ElemMinMax is new ElemMinMax(Float_64, MinF64, MaxF64);

 -- example of signed-unsigned conversion

 BZEROU64  : Unsigned_64 := 0;
 BSCALEU64 : Unsigned_64 := 1;
 MinU64 : Unsigned_64 := Unsigned_64'Last;
 MaxU64 : Unsigned_64 := Unsigned_64'First;
 procedure U64_ElemMinMax is new ElemMinMax(Unsigned_64, MinU64, MaxU64);

 BZEROU32  : Unsigned_32 := 0;
 BSCALEU32 : Unsigned_32 := 1;
 MinU32 : Unsigned_32 := Unsigned_32'Last;
 MaxU32 : Unsigned_32 := Unsigned_32'First;
 procedure U32_ElemMinMax is new ElemMinMax(Unsigned_32, MinU32, MaxU32);

 BZEROU16  : Unsigned_16 := 0;
 BSCALEU16 : Unsigned_16 := 1;
 MinU16 : Unsigned_16 := Unsigned_16'Last;
 MaxU16 : Unsigned_16 := Unsigned_16'First;
 procedure U16_ElemMinMax is new ElemMinMax(Unsigned_16, MinU16, MaxU16);

 BZEROI8  : Integer_8 := 0;
 BSCALEI8 : Integer_8 := 1;
 MinI8 : Integer_8 := Integer_8'Last;
 MaxI8 : Integer_8 := Integer_8'First;
 procedure I8_ElemMinMax is new ElemMinMax(Integer_8, MinI8, MaxI8);



 -- MinMax for all array types

 -- integer data if BLANK available (undefined value check)
 
 BLfound  : Boolean := False; -- FIXME must be reset before each Header read
 BLANKI64 : Integer_64;
 BLANKI32 : Integer_32;
 BLANKI16 : Integer_16;
 BLANKUI8 : Unsigned_8;
 UndefValCnt : Natural := 0; -- FIXME must be reset at each Header read start
 procedure UndefVal is begin UndefValCnt := UndefValCnt + 1; end UndefVal;


 procedure I64_Checked_MinMax is new I64F64.Read_Checked_Integers(F64_ElemMinMax, UndefVal);
 procedure I32_Checked_MinMax is new I32F64.Read_Checked_Integers(F64_ElemMinMax, UndefVal);
 procedure I16_Checked_MinMax is new I16F32.Read_Checked_Integers(F32_ElemMinMax, UndefVal);
 procedure UI8_Checked_MinMax is new UI8F32.Read_Checked_Integers(F32_ElemMinMax, UndefVal);

 -- integer data if BLANK not available

 procedure I64_MinMax is new I64F64.Read_Values(F64_ElemMinMax);
 procedure I32_MinMax is new I32F64.Read_Values(F64_ElemMinMax);
 procedure I16_MinMax is new I16F32.Read_Values(F32_ElemMinMax);
 procedure UI8_MinMax is new UI8F32.Read_Values(F32_ElemMinMax);

 -- integer sign conversion variants if BZERO BSCALE = Tab11

 procedure U64_MinMax is new I64U64.Read_Values(U64_ElemMinMax);
 procedure U32_MinMax is new I32U32.Read_Values(U32_ElemMinMax);
 procedure U16_MinMax is new I16U16.Read_Values(U16_ElemMinMax);
 procedure I8_MinMax  is new UI8I8.Read_Values ( I8_ElemMinMax);

 -- floats (always with undefined value check)

 procedure F64_Checked_MinMax is new F64F64.Read_Checked_Scaled_Floats(F64_ElemMinMax, UndefVal);
 procedure F32_Checked_MinMax is new F32F32.Read_Checked_Scaled_Floats(F32_ElemMinMax, UndefVal);



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


 InFile      : SIO.File_Type;
 InFileName  : String := Argument(1);
 -- FIXME might raise excpetion before Usage written

 BITPIX : Integer;
 DUSize : Positive;
 DUStart : Positive;

 -- analyze array keys

 ArrKeysGiven : Boolean;

 function Analyze_Array_Keys( Cards : Optional.Card_Arr; 
			      BZERO  : in out Float_32; 
			      BSCALE : in out Float_32 ) return Boolean
 is
  BZfound : Boolean := False;
  BSfound : Boolean := False;
 begin
   BLfound := False;
   UndefValCnt := 0;
   for I in Cards'Range
   loop
     Put_Line("RESKEYS: >" & Cards(I) & "<" );
     if(Cards(I)(1..5) = "BZERO")
     then
	BZERO := Float_32'Value((Cards(I)(11..30)));
	BZEROF64 := Float_64(BZEROF32); -- FIXME is global!!
	BZEROU16 := Unsigned_16(BZEROF32);-- FIXME is global!!
	BZfound := True;
     elsif(Cards(I)(1..6) = "BSCALE")
     then
	BSCALE := Float_32'Value((Cards(I)(11..30)));
	BSCALEF64 := Float_64(BSCALEF32); -- FIXME global var!!
	BSCALEU16 := Unsigned_16(BSCALEF32); -- FIXME global var !!
	BSfound := True;
     elsif(Cards(I)(1..5) = "BLANK")
     then
        -- FIXME global vars!!
        case(BITPIX) is
	when 64 => BLANKI64 := Integer_64'Value((Cards(I)(11..30)));
	when 32 => BLANKI32 := Integer_32'Value((Cards(I)(11..30)));
	when 16 => BLANKI16 := Integer_16'Value((Cards(I)(11..30)));
	when  8 => BLANKUI8 := Unsigned_8'Value((Cards(I)(11..30)));
	when others => 
		Put_Line("BLANK card in Float data !");
	end case;
	BLfound := True;
     end if;
   end loop;

  return BZfound AND BSfound;
 end Analyze_Array_Keys;

 HDUStart : Positive := 1;
 -- FIXME now only Primary HDU, later consider other HDUs

-------------------------------------------------------------------------------
-- MAIN -------------------------------------------- MAIN ---------------------
begin 

 Put_Line("Usage  " & Command_Name & " <file name>");

 SIO.Open(InFile,  SIO.In_File,  InFileName);

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

 -- reset to HDU start
 Set_File_Block_Index(InFile,HDUStart);

 declare
   Cards : Optional.Card_Arr := Read_Header(InFile, Optional.Reserved.Array_Keys);
 begin
   ArrKeysGiven := Analyze_Array_Keys(Cards, BZEROF32, BSCALEF32);
 end;

 if(BLfound = True)
 then
   Put_Line("BLANK  :" & Integer_16'Image(BLANKI16)); 
 end if;
 Put_Line("BZERO  :" & Float_32'Image(BZEROF32)); 
 Put_Line("BSCALE :" & Float_32'Image(BSCALEF32)); 
 
 -- read data sequentially by blocks

 if ( abs BITPIX > 32 )
 then
 
 if(BITPIX = -64)
 then
   F64_Checked_MinMax(InFile, DUSize, BZEROF64, BSCALEF64);
   Put_Line("UndefVal count: " & Natural'Image(UndefValCnt));
 elsif(BITPIX = 64)
 then
   I64_MinMax(InFile, DUSize, BZEROF64, BSCALEF64);
 end if;

 Put_Line("F64 Min: " & Float_64'Image(MinF64));
 Put_Line("F64 Max: " & Float_64'Image(MaxF64));

 else
 
 if(BITPIX = -32)
 then
   F32_Checked_MinMax(InFile, DUSize, BZEROF32, BSCALEF32);
   Put_Line("UndefVal count: " & Natural'Image(UndefValCnt));
 elsif(BITPIX = 32)
 then
   I32_MinMax(InFile, DUSize, BZEROF64, BSCALEF64);
 elsif(BLfound AND (BITPIX = 16))
 then
   --I16_MinMax(InFile, DUSize, BZEROF32, BSCALEF32);
   I16_Checked_MinMax(InFile, DUSize, BZEROF32, BSCALEF32, BLANKI16);
   Put_Line("UndefVal count: " & Natural'Image(UndefValCnt));
 elsif(not BLfound AND (BITPIX = 16))
 then
   I16_MinMax(InFile, DUSize, BZEROF32, BSCALEF32);
   Set_File_Block_Index(InFile,DUStart); -- reset to DUStart and read again
   U16_MinMax(InFile, DUSize, BZEROU16, BSCALEU16); 
   Put_Line("U16 Min: " & Unsigned_16'Image(MinU16) & " / "& Unsigned_16'Image(Unsigned_16'First));
   Put_Line("U16 Max: " & Unsigned_16'Image(MaxU16) & " / "& Unsigned_16'Image(Unsigned_16'Last));
 elsif(BITPIX = 8)
 then
   UI8_MinMax(InFile, DUSize, BZEROF32, BSCALEF32);
 end if;

 Put_Line("F32 Min: " & Float_32'Image(MinF32));
 Put_Line("F32 Max: " & Float_32'Image(MaxF32));

 end if;


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

