-- TODO
-- Physical_Value type Integer: implement Signed-Unsigned conversion [FITS Tab 11]


package Generic_Value is

-- A, [FITS Eq(3)] PhysVal type Float: below is Int-Float Float-Float conversions
generic
  type TD is range <>;  -- any signed integer type
  type TF is digits <>; -- any floating point type
function Physical_From_Int(BZERO : in TF; BSCALE : in TF; BLANK : in TD; Data : in TD)
	return TF;
-- when HDU-array type is Integer

generic
  type TF is digits <>; -- any floating point type
function Physical_From_Float(BZERO : in TF; BSCALE : in TF; Data : in TF)
	return TF;
-- whenHDU-array type is Float


-- B, [FITS Tab 11] PhysVal type Integer: Integer-data signed-unsigned converions

-- FIXME not yet implemented

end Generic_Value;

