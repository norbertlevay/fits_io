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

 procedure print_ranges
 is
 begin
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
  with function "<" (L,R : in T) return Boolean;
  with function ">" (L,R : in T) return Boolean;
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
 procedure F32_ElemMinMax is new ElemMinMax(Float_32, MinF32, MaxF32, "<", ">");


 BZEROF64  : Float_64 := 0.0;
 BSCALEF64 : Float_64 := 1.0;
 MinF64 : Float_64 := Float_64'Last;
 MaxF64 : Float_64 := Float_64'First;
 procedure F64_ElemMinMax is new ElemMinMax(Float_64, MinF64, MaxF64, "<", ">");

 -- Data Unit for Arra types
 package F64_DU is new Generic_Data_Unit(Float_64);
 package I64_DU is new Generic_Data_Unit(Integer_64);
 package F32_DU is new Generic_Data_Unit(Float_32);
 package I32_DU is new Generic_Data_Unit(Integer_32);
 package I16_DU is new Generic_Data_Unit(Integer_16);
 package UI8_DU is new Generic_Data_Unit(Unsigned_8);
 -- FIXME reconsider: all package generic or each procedure generic but not őackage

 -- NOTE on conversions: 
 --
 -- Integers -> Float : no range and no loss-of-precision problems (except I64) (?)
 -- UI8..I16  (3 and 5 digits) -> F32 (6 digits)
 -- I32 (10 digits)            -> F64 (15 digits)
 -- I64 (19 digits)            -> F64 : range ok, but 4-digits lost
 -- 
 -- Float -> Integers : always check range -> or handle Constraint_Error
 -- if range ok then:
 -- F64 (15 digits) -> I64 (19 digits) 
 -- F32 (6 digits)  -> I32 (10 digits) 
 -- F32 (6 digits)  -> I16 ( 5 digits) : ~ 1 digit lost but half memory space needed
 
 -- MinMax for all array types

 procedure F64_MinMax is
  new F64_DU.Read_Physical_Values(Float_64, F64_ElemMinMax, "+","*","+");

 procedure F32_MinMax is
  new F32_DU.Read_Physical_Values(Float_32, F32_ElemMinMax, "+","*","+");

 function "+" (R : Integer_64) return Float_64 is begin return Float_64(R); end "+";
 procedure I64_MinMax is
  new I64_DU.Read_Physical_Values(Float_64, F64_ElemMinMax, "+","*","+");

 function "+" (R : Integer_32) return Float_32 is begin return Float_32(R); end "+";
 procedure I32_MinMax is
  new I32_DU.Read_Physical_Values(Float_32, F32_ElemMinMax, "+","*","+");

 function "+" (R : Integer_16) return Float_32 is begin return Float_32(R); end "+";
 procedure I16_MinMax is
  new I16_DU.Read_Physical_Values(Float_32, F32_ElemMinMax, "+","*","+");

 function "+" (R : Unsigned_8) return Float_32 is begin return Float_32(R); end "+";
 procedure U8_MinMax is
  new UI8_DU.Read_Physical_Values(Float_32, F32_ElemMinMax, "+","*","+");

 -- example handling undefined value (I16 only)

 BLfound  : Boolean := False; -- FIXME must be reset before each Header read
 BLANKI16 : Integer_16;
 UndefValCnt : Natural := 0; -- FIXME miust be reset at each Header read start

 procedure I16_UndefVal(V : in Integer_16)
 is
 begin
  UndefValCnt := UndefValCnt + 1;
 end I16_UndefVal;

 function I16_Is_BLANK(V : Integer_16) return Boolean
 is begin return V = BLANKI16; end I16_Is_BLANK;

 procedure I16_Checked_MinMax is
  new I16_DU.Read_Checked_Physical_Values(Float_32, 
      I16_Is_BLANK, I16_UndefVal, F32_ElemMinMax, "+","*","+");

 -- example undefined Float value

 procedure F32_UndefVal(V : in Float_32)
 is
 begin
  UndefValCnt := UndefValCnt + 1;
 end F32_UndefVal;

 function F32_Is_NaN(V : Float_32) return Boolean
 is
  function F32_To_U32 is new Ada.Unchecked_Conversion(Float_32, Unsigned_32);
  VM : Unsigned_32 := F32_To_U32(V);
-- NOTE attribs exits: T'Exponent T'Fraction
  NaN_Exp : constant Unsigned_32 := 16#7F800000#;
  Exp   : Unsigned_32 := VM and 16#7F800000#;
  Fract : Unsigned_32 := VM and 16#007FFFFF#;
 begin
  -- IEEE NaN : signbit=dont care & exponent= all ones & fraction= any but not all zeros
  if((Exp = Nan_Exp) AND (Fract /= 16#00000000#)) 
  then return True;  -- NaN
  else return False; -- not NaN
  end if; 
 end F32_Is_NaN;
 -- FIXME do this as generic

 procedure F32_Checked_MinMax is
  new F32_DU.Read_Checked_Physical_Values(Float_32, 
      F32_Is_NaN, F32_UndefVal, F32_ElemMinMax, "+","*","+");


 -- example of signed-unsigned conversion

 BZEROU16  : Unsigned_16 := 0;
 BSCALEU16 : Unsigned_16 := 1;
 MinU16 : Unsigned_16 := Unsigned_16'Last;
 MaxU16 : Unsigned_16 := Unsigned_16'First;
 procedure U16_ElemMinMax is new ElemMinMax(Unsigned_16, MinU16, MaxU16, "<", ">");

 function "+" (R : Integer_16) return Unsigned_16
 is 
 begin
   if(R < 0) then return (Unsigned_16'Last + 1) - Unsigned_16(abs R);
   else           return Unsigned_16(R);
   end if;
 end "+";
 -- NOTE map Int-negative values to 'upper-half'/mid-last Unsigned range
 -- First: -1 -> 64353  Last: -32768->32768
 procedure I16_MinMax_U16 is
  new I16_DU.Read_Physical_Values(Unsigned_16, U16_ElemMinMax, "+","*","+");

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
	BLANKI16 := Integer_16'Value((Cards(I)(11..30))); -- FIXME not ok for UI8 data
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
   F64_MinMax(InFile, DUSize, BZEROF64, BSCALEF64);
 elsif(BITPIX = 64)
 then
   I64_MinMax(InFile, DUSize, BZEROF64, BSCALEF64);
 end if;

 Put_Line("F64 Min: " & Float_64'Image(MinF64));
 Put_Line("F64 Max: " & Float_64'Image(MaxF64));

 else
 
 if(BITPIX = -32)
 then
   -- F32_MinMax(InFile, DUSize, BZEROF32, BSCALEF32);
   F32_Checked_MinMax(InFile, DUSize, BZEROF32, BSCALEF32);
   Put_Line("UndefVal count: " & Natural'Image(UndefValCnt));
 elsif(BITPIX = 32)
 then
   I32_MinMax(InFile, DUSize, BZEROF32, BSCALEF32);
 elsif(BLfound AND (BITPIX = 16))
 then
   --I16_MinMax(InFile, DUSize, BZEROF32, BSCALEF32);
   I16_Checked_MinMax(InFile, DUSize, BZEROF32, BSCALEF32);
   Put_Line("UndefVal count: " & Natural'Image(UndefValCnt));
 elsif(not BLfound AND (BITPIX = 16))
 then
   I16_MinMax(InFile, DUSize, BZEROF32, BSCALEF32);
   Set_File_Block_Index(InFile,DUStart); -- reset to DUStart and read again
   I16_MinMax_U16(InFile, DUSize, BZEROU16, BSCALEU16); 
   Put_Line("U16 Min: " & Unsigned_16'Image(MinU16) & " / "& Unsigned_16'Image(Unsigned_16'First));
   Put_Line("U16 Max: " & Unsigned_16'Image(MaxU16) & " / "& Unsigned_16'Image(Unsigned_16'Last));
 elsif(BITPIX = 8)
 then
   U8_MinMax(InFile, DUSize, BZEROF32, BSCALEF32);
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

