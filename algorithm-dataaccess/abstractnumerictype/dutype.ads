
-- Numeric data type which
-- * unifies Unsigned Integer and Float and
-- * provides Null value (optionally)

-- It is based on private generic meta-type, however
-- the provided operations restrict it to instances
-- by numeric types only.
-- Operations are:
-- * To_Numeric() To_<ada-numeric-type> conversions
--   (implemented using Ada type conversion operator type())
-- * Is_Null() for Floats Nul is NaN, for discrete
--   (Int, UInt) it is one value taken from the full range of (U)Int


-- Alternatively, distuinguish by has or not Null-value:

-- package DUType           - does conversions
-- package DUType.With_Null - adds Null-value handling

-- then use 'with'-clause to include into Physical, Raw, Scaling, examples... POSSIBLE ??
-- NO! - all with-clauses wuill need to be repeated in Physical ...

-- package DUType.With_Null.Physical <-- now Physical has alternative access
-- to both: DUType without Null, as well as DUType.With_Null !!
-- Header has BLANK (Read or Created)    -> instantiate DUType.With_Null
-- Header has no BLANK (Read or Created) -> instantiate DUType only
-- This eliminates the need for Null_Valid-flag


-- Usage:
-- 1, have Header        (read from fits-file or created to be written to fits-file)
-- 2, calculate UndefOut (if BLANK given)
-- 3, calculate DataOut  (2 ways: with or without Undef conversion)


generic
  type T is private;
  -- explicit conversions
  with function To_Unsigned_8(V : in T) return Unsigned_8 is <>;
  with function To_Numeric(V : in Unsigned_8) return T is <>;
  -- sign-operator (internally calls explicit conversion func)
  with function "+"(V : in T) return Unsigned_8 is <>;
  with function "+"(V : in Unsigned_8) return T is <>;
package DUType is
end DUType;


-- TO AVOID CONFUSION WITH Ada null-keyword IS BETTER TO STAY WITH
-- FITS-TERMINOLOGY "UNDEFINED VALUE" -> Undef ??
-- DUType.Undefined_Value
-- Float - no BLANK, Undef is NaN implicitely
-- (U)Int - optional BLANK, Undef is value of BLANK (always Raw value)

generic
  Null_Value : in T;
  with function Is_Null(V : in T) return Boolean is <>;
package DUType.With_Null / .Undefined_Value is

-- AVOID appearing two meta-Types (Tf Tm) in generic params of a pack.

-- HOW TO MAKE SURE OTHER UndefOut IS INITED BEFORE Scaling.Linear IS CALLED ? ONE OF THE TWO UndefIn/UndefOut is always NaN!!!
    -- Now Scaling goes to Float: e.g. one side of Undef's is alway NaN !!
    -- So we need only NaN <-> UndefX  conversion (where X= In / Out depending on Read/Write)

-- see examples/minmax call sequence:
-- with BLANK: Header_Info(Cards, Undef):True  -> XX.Init_Undef -> Physical.Read/Write_Array(..,A,B)
--   no BLANK: Header_Info(Cards, Undef):False         ->          Physical.Read/Write_Array(..,A,B)

-- MUST resolve in Physical: so it is outside of the loops : but Scaling can provide the info, whether Udef is present or not !!
    -- e.g. instantiatate Scaling or Scaling.With_Undef and both must have Has_Undef() ret Boolean <- called inside Physical before
    -- entering the loops to convert that data by Linear or Linear_With_Undef



end DUType.With_Null;

-- FIXME I used 2-types to distinguish implementation of Scaling,
-- see: Scaling_Impl.ad?  <- those diffs are related to Undef-handling
-- (not Linear-calc) --> all that now moves here to DUType?!
-- Is possible to distuinguish based on one type ?? : yes,
-- one of the two types is explicit here, like in To_Unsigned_8()
-- or To_Numeric(V : Unsigned_8) ... 
-- E.g. half what used to be in V3_Pool_* goes here and other half
-- remains in Pool.
--
-- Scaling_Impl implements:
-- * initialization of Null values - depends on Type-combinations
-- * Is_Undef() func - depends whether Float or not

-- E.g: rename Scaling_Impl.adb -> DUType.adb and adjust fro thid DUType.ads
-- Should Null be inited as generic param at instantiation OR as explicit
-- Init_Null-func to be called _after_ instantiation ? The second would allow
-- to implementa Null to be optional (defaulr Has_Null in geeneric params
-- defaults to False)


