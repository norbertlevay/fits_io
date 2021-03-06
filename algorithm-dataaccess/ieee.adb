with Ada.Text_IO; use Ada.Text_IO;

with Interfaces; use Interfaces;
 
procedure IEEE is -- Non portable, bad, never do this!
   Zero  : Float := 0.0;
   Zeroi  : IEEE_Float_32 := 0.0;
   PInf  : Float := 1.0 / Zero;
   NInf  : Float := -PInf;
   PZero : Float := 1.0 / PInf;
   NZero : Float := 1.0 / NInf;
   NaN   : Float := 0.0 / Zero; 
   NaNi   : IEEE_Float_32 := 0.0 / Zeroi; 

   VF32 : IEEE_Float_32 := -1.23457;
   VInt : Integer := Integer(VF32);
-- unsigned
 type Unsigned_8  is  mod 2**8;

   VUInt : Unsigned_8 := Unsigned_8(VF32);

begin
   Put_Line (" -oo = " & Float'Image (NInf));
   Put_Line (" +oo = " & Float'Image (PInf));
   Put_Line (" NaN = " & Float'Image (NaN));
   Put_Line (" NaNi = " & IEEE_Float_32'Image (NaNi));
   Put_Line ("  -0 = " & Float'Image (NZero));
 
   Put_Line (" -oo < first " & Boolean'Image (NInf < Float'First));
   Put_Line (" +oo > last  " & Boolean'Image (PInf > Float'Last));
   Put_Line (" NaN = NaN   " & Boolean'Image (NaN = NaN));
   Put_Line ("  -0 = 0     " & Boolean'Image (NZero = 0.0));
   Put_Line ("  +0 = 0     " & Boolean'Image (PZero = 0.0));
   Put_Line ("  +0 < least positive   " & Boolean'Image (PZero < Float'Succ (Zero)));
   Put_Line ("  -0 > biggest negative " & Boolean'Image (NZero > Float'Pred (Zero)));
 
      -- Validness checks
   Put_Line ("Valid -oo is " & Boolean'Image (NInf'Valid));
   Put_Line ("Valid +oo is " & Boolean'Image (PInf'Valid));
   Put_Line ("Valid NaN is " & Boolean'Image (NaN'Valid));
   Put_Line ("Valid NaNi is " & Boolean'Image (NaNi'Valid));
--   Put_Line ("Conv NaNi to Int is " & Integer'Image (Integer(NaNi)));
		-- <- raises Constraint_Error overflow check failed
   Put_Line ("Conv NaNi to Float64 is " & IEEE_Float_64'Image (IEEE_Float_64(NaNi)));-- stays NaN


  Put_Line ("Conv Float_32 -> Int: " & IEEE_Float_32'Image(VF32) &" -> "& Integer'Image(VInt) );  
  Put_Line ("Conv Float_32 -> UInt8: " & IEEE_Float_32'Image(VF32) &" -> "& Unsigned_8'Image(VUInt) );  
 
end IEEE;
 
