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
with Generic_Floats_Data_Unit;
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

-- FIXME Ti range <> -> Ti'Value 'Image for BLANK available but no UI8 Unsigned vals
--       Ti private  -> Unsigned available but Ti'Value Ti'Image for BLANK not available
 generic
    type Ti is range <>; -- type in file FIXME gooe for Ti'Value for BLANK but no Unsigned 
--    type Ti is private;  -- type in file FIXME good also for UI8 but not Ti'Value for BLANK
    type Tf is digits <>; -- physical value type
    with function "+" (R : in Ti) return Tf is <>;
 package V3_Data is

  procedure FloatsMinMax(F : SIO.File_Type; DUSize : Positive; Cards : Optional.Card_Arr);
  procedure IntsMinMax(F : SIO.File_Type; DUSize : Positive; Cards : Optional.Card_Arr);

 end V3_Data; 

 -- Body

 package body V3_Data is

  type Array_Keys_Rec is
    record
	DATAMIN_Avail : Boolean;
	DATAMAX_Avail : Boolean;
 	BLANK_Avail   : Boolean;
 	BZERO,BSCALE    : Tf;
	DATAMIN,DATAMAX : Tf;
	BLANK 		: Ti;-- FIXME how to handle UInt8
  end record;
 -- BZERO BSCALE
 -- Tab11 vals -> ZERO_SHIFT   -> Int only, FlipSign (BLANK also flip)
 -- 0.0 1.0    -> UNITY        -> call Checked no scaling (if BLANK given)
 -- other      -> SCALING      -> Floats: checked scaling  Ints: scaling with BLANK 
 type Array_Keys_Category is (UNITY, ZERO_SHIFT, SCALING);


 function Array_Value_Rec(Cards : Optional.Card_Arr) return Array_Keys_Rec;
 function Analyze( R : in Array_Keys_Rec ) return Array_Keys_Category;




 -- the callback
 
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


 Min : Tf := Tf'Last;
 Max : Tf := Tf'First;
 procedure Tf_ElemMinMax is new ElemMinMax(Tf, Min, Max);


 UndefValCnt : Natural := 0; -- FIXME must be reset at each Header read start
 procedure UndefVal is begin UndefValCnt := UndefValCnt + 1; end UndefVal;

 -- integer data if BLANK is available
 package Ti_DU is new Generic_Data_Unit(Ti);
 package Tf_fDU is new Generic_Data_Unit(Tf);
 package TiTf is new Ti_DU.Physical(Tf,"+","*","+");

 package Tf_DU is new Generic_Floats_Data_Unit(Tf,Tf,Tf_fDU);

 procedure Ti_Checked_MinMax 	is new TiTf.Read_Checked_Scaled_Integers(Tf_ElemMinMax, UndefVal);
 procedure Ti_MinMax   		is new TiTf.Read_Scaled_Values(Tf_ElemMinMax);
-- procedure U_MinMax 		is new TiTf.Read_Sign_Converted_Integers(Tf_ElemMinMax);
 procedure Tf_Checked_MinMax 	is new Tf_DU.Read_Checked_Scaled_Floats(Tf_ElemMinMax, UndefVal);
 



  procedure FloatsMinMax(F : SIO.File_Type; DUSize : Positive; Cards : Optional.Card_Arr)
  is
   Keys : Array_Keys_Rec;
  begin
   Keys := Array_Value_Rec(Cards);   
   Tf_Checked_MinMax(F, DUSize, Keys.BZERO, Keys.BSCALE);
   Put_Line("UndefVal count: " & Natural'Image(UndefValCnt));
   Put_Line("F Min: " & Tf'Image(Min));
   Put_Line("F Max: " & Tf'Image(Max));
  end FloatsMinMax;


  procedure IntsMinMax(F : SIO.File_Type; DUSize : Positive; Cards : Optional.Card_Arr)
  is
   Keys : Array_Keys_Rec := Array_Value_Rec(Cards);   
  begin
 
   case (Analyze(Keys))
   is
   when ZERO_SHIFT => null;
	--U_MinMax(F, DUSize);
	--Put_Line("U64 Min: " & Unsigned_64'Image(MinU64));
   	--Put_Line("U64 Max: " & Unsigned_64'Image(MaxU64));

   when UNITY | SCALING =>
    if(Keys.BLANK_Avail)
    then
      Put_Line("BLANK : " & Ti'Image(Keys.BLANK));
      Ti_Checked_MinMax(F, DUSize, Keys.BZERO, Keys.BSCALE,Keys.BLANK);
      Put_Line("UndefVal count: " & Natural'Image(UndefValCnt));
    else
      Ti_MinMax(F, DUSize, Keys.BZERO, Keys.BSCALE);
    end if;
    Put_Line("F Min: " & Tf'Image(Min));
    Put_Line("F Max: " & Tf'Image(Max));
  end case;

  end IntsMinMax;


 function Analyze( R : in Array_Keys_Rec ) return Array_Keys_Category
 is
  Cat : Array_Keys_Category;
  Shift_Val : Tf := 2.0**(Ti'Size - 1); -- FIXME is this safe ??
 begin
  
   if((R.BZERO = 0.0) AND (R.BSCALE = 1.0))
   then
      Cat := UNITY;
   elsif((R.BSCALE = 1.0) AND (R.BZERO = Shift_Val))
   then
      Cat := ZERO_SHIFT;
   else
      Cat := SCALING;
   end if;

  return Cat;
 end Analyze;




 function To_Ti(S : String) return Ti
 is
  V : Ti;
 begin
  for I in S'Range
  loop
    null; -- FIXME implement all set of CardString to FITS-Integer conversions
          -- it should be in Keyword_Record module and passed as generic parameter here
  end loop;
  return V;
 end To_Ti;


 function Array_Value_Rec(Cards : Optional.Card_Arr) return Array_Keys_Rec
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
--	V.BLANK := To_Ti(Cards(I)(11..30));
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

 function "+" (R : in Integer_16) return Float_64
 is begin return Float_64(R); end "+";
 package I16F64_V3_Data is new V3_Data(Integer_16, Float_64);
 -- only test not in use

 --package UI8F32_V3_Data is new V3_Data(Unsigned_8, Float_32);
 --FIXME Ti must be mod or private

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------

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
 begin
 -- read data
 if(BITPIX = -64)    then I32F64_V3_Data.FloatsMinMax(InFile, DUSize, Cards);
 elsif(BITPIX = -32) then I16F32_V3_Data.FloatsMinMax(InFile, DUSize, Cards);
 elsif(BITPIX = 64)  then I64F64_V3_Data.IntsMinMax(InFile, DUSize, Cards);
 elsif(BITPIX = 32)  then I32F64_V3_Data.IntsMinMax(InFile, DUSize, Cards);
 elsif(BITPIX = 16)  then I16F32_V3_Data.IntsMinMax(InFile, DUSize, Cards);
 --elsif(BITPIX = 8)   then UI8F32_V3_Data.IntsMinMax(InFile, DUSize, Cards);
 end if;
 end;

 SIO.Close(InFile);

-- print_ranges;
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

