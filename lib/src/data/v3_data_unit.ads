
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

with V3_Types; use V3_Types;
with Generic_Data_Unit;

package V3_Data_Unit is

 -- Data Units per ArrType

package F64_DU is new Generic_Data_Unit(Float_64);
package F32_DU is new Generic_Data_Unit(Float_32);
package I64_DU is new Generic_Data_Unit(Integer_64);
package I32_DU is new Generic_Data_Unit(Integer_32);
package I16_DU is new Generic_Data_Unit(Integer_16);
package UI8_DU is new Generic_Data_Unit(Unsigned_8);

-- Data Units per ArraType / PhysType

-- scaling float values Vout = BZERO + BSCALE * Vin

package F64F64 is new F64_DU.Physical_Float(Float_64);
package F32F32 is new F32_DU.Physical_Float(Float_32);

-- scaling and int->float conversions

package I64F64 is new I64_DU.Physical(Float_64);
package I32F64 is new I32_DU.Physical(Float_64);
package I16F32 is new I16_DU.Physical(Float_32);
package UI8F32 is new UI8_DU.Physical(Float_32);

-- sign conversions

package I64U64 is new I64_DU.Physical(Unsigned_64);
package I32U32 is new I32_DU.Physical(Unsigned_32);
package I16U16 is new I16_DU.Physical(Unsigned_16);
package UI8I8  is new UI8_DU.Physical(Integer_8);

end V3_Data_Unit;

