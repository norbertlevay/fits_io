
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





with Ada.Streams.Stream_IO;

with V3_Types; use V3_Types;
with Generic_Data_Unit;

package V3_Data_Unit is
 package SIO renames Ada.Streams.Stream_IO;

package F64_DU is new Generic_Data_Unit(Float_64);
package I64_DU is new Generic_Data_Unit(Integer_64);
package F32_DU is new Generic_Data_Unit(Float_32);
package I32_DU is new Generic_Data_Unit(Integer_32);
package I16_DU is new Generic_Data_Unit(Integer_16);
package UI8_DU is new Generic_Data_Unit(Unsigned_8);


-- Floats, scaling Vphys = BZERO + BSCALE * Varr

generic
 with procedure Element(V : in Float_64);
procedure F64F64_Read_Physical_Values
	(F : SIO.File_Type; DUSize : in Positive; BZERO  : in Float_64; BSCALE : in Float_64);

generic
 with procedure Element(V : in Float_32);
procedure F32F32_Read_Physical_Values
	(F : SIO.File_Type; DUSize : in Positive; BZERO  : in Float_32; BSCALE : in Float_32);


-- Integer conversions

-- 64bit

generic
 with procedure Element(V : in Float_64);
procedure I64F64_Read_Physical_Values
	(F : SIO.File_Type; DUSize : in Positive; BZERO  : in Float_64; BSCALE : in Float_64);
-- looses ~4 digits in precision

generic
 with procedure Element(V : in Float_64);
procedure I32F64_Read_Physical_Values
	(F : SIO.File_Type; DUSize : in Positive; BZERO  : in Float_64; BSCALE : in Float_64);

-- 32bit

generic
 with procedure Element(V : in Float_32);
procedure I16F32_Read_Physical_Values
	(F : SIO.File_Type; DUSize : in Positive; BZERO  : in Float_32; BSCALE : in Float_32);

generic
 with procedure Element(V : in Float_32);
procedure UI8F32_Read_Physical_Values
	(F : SIO.File_Type; DUSize : in Positive; BZERO  : in Float_32; BSCALE : in Float_32);


-- sign conversions

generic
 with procedure Element(V : in Unsigned_16);
procedure I16U16_Read_Physical_Values
	(F : SIO.File_Type; DUSize : in Positive; BZERO  : in Unsigned_16; BSCALE : in Unsigned_16);

-- FIXME add I32U32 I64U64 UI8I8


end V3_Data_Unit;

