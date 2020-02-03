

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




end V3_Data_Unit;

