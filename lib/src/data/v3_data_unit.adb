

with Generic_Data_Unit;

package body V3_Data_Unit is

-- Floats

procedure F64F64_Read_Physical_Values
                (F : SIO.File_Type; 
                DUSize : in Positive;
                BZERO  : in Float_64;
                BSCALE : in Float_64)
is
 procedure F64_Read is
  new F64_DU.Read_Physical_Values(Float_64, Element, "+","*","+");
begin
 F64_Read(F, DUSize, BZERO, BSCALE);
end F64F64_Read_Physical_Values;

procedure F32F32_Read_Physical_Values
                (F : SIO.File_Type; 
                DUSize : in Positive;
                BZERO  : in Float_32;
                BSCALE : in Float_32)
is
 procedure F32_Read is
  new F32_DU.Read_Physical_Values(Float_32, Element, "+","*","+");
begin
 F32_Read(F, DUSize, BZERO, BSCALE);
end F32F32_Read_Physical_Values;



-- Conversions Int -> Float

-- 64bit

procedure I64F64_Read_Physical_Values
                (F : SIO.File_Type; 
                DUSize : in Positive;
                BZERO  : in Float_64;
                BSCALE : in Float_64)
is
 function "+" (R : Integer_64) return Float_64 is begin return Float_64(R); end "+";
 procedure F64_Read is
  new I64_DU.Read_Physical_Values(Float_64, Element, "+","*","+");
begin
 F64_Read(F, DUSize, BZERO, BSCALE);
end I64F64_Read_Physical_Values;


procedure I32F64_Read_Physical_Values
                (F : SIO.File_Type; 
                DUSize : in Positive;
                BZERO  : in Float_64;
                BSCALE : in Float_64)
is
 function "+" (R : Integer_32) return Float_64 is begin return Float_64(R); end "+";
 procedure F64_Read is
  new I32_DU.Read_Physical_Values(Float_64, Element, "+","*","+");
begin
 F64_Read(F, DUSize, BZERO, BSCALE);
end I32F64_Read_Physical_Values;


-- 32bit

procedure I16F32_Read_Physical_Values
                (F : SIO.File_Type; 
                DUSize : in Positive;
                BZERO  : in Float_32;
                BSCALE : in Float_32)
is
 function "+" (R : Integer_16) return Float_32 is begin return Float_32(R); end "+";
 procedure F32_Read is
  new I16_DU.Read_Physical_Values(Float_32, Element, "+","*","+");
begin
 F32_Read(F, DUSize, BZERO, BSCALE);
end I16F32_Read_Physical_Values;



procedure UI8F32_Read_Physical_Values
                (F : SIO.File_Type; 
                DUSize : in Positive;
                BZERO  : in Float_32;
                BSCALE : in Float_32)
is
 function "+" (R : Unsigned_8) return Float_32 is begin return Float_32(R); end "+";
 procedure F32_Read is
  new UI8_DU.Read_Physical_Values(Float_32, Element, "+","*","+");
begin
 F32_Read(F, DUSize, BZERO, BSCALE);
end UI8F32_Read_Physical_Values;


-- sign conversions
procedure I16U16_Read_Physical_Values
        (F : SIO.File_Type;
	DUSize : in Positive;
	BZERO  : in Unsigned_16;
	BSCALE : in Unsigned_16)
is
function "+" (R : Integer_16) return Unsigned_16
 is  
 begin
   if(R < 0) then return (Unsigned_16'Last + 1) - Unsigned_16(abs R); 
   else           return Unsigned_16(R);
   end if;
 end "+";
 -- NOTE map Int-negative values to 'upper-half'/mid-last Unsigned range
 -- First: -1 -> 64353  Last: -32768->32768
 procedure U16_Read is
  new I16_DU.Read_Physical_Values(Unsigned_16, Element, "+","*","+");
begin
 U16_Read(F, DUSize, BZERO, BSCALE);
end I16U16_Read_Physical_Values;




end V3_Data_Unit;

