
-- 2 Rules:
-- Raw-side is defined by Header in _both_ cases Read/Write
-- each invocation of DataUnit.Read/Write only shifts File.Index
-- as it goes through the Data_Unit in sequentila manner
--
-- ergo:
-- there needs to be a Data_Unit.Init/Open... func AFTER Header is ready
-- and T is known (Data_Unit instantiated), but BEFORE repeated calls to
-- DU Read/Write start
--
-- Additional 2 Rules:
-- Header is DU-Type independent (Strings,?Floats?)
-- only Data_Unit package is generic by T and defines Physical-side


-- NOTE Read/Write call should _only_ shift File-index as it goes through Data Unit
-- no other state (ScalingRec & local vars) should change, (besides data moved of course)

-- NOTE on higher-level API: what if Header would be generic by T_Raw ?
-- just as Data_Unit is generic by T_Physical ?
-- (Header type-indepence would apply only to proprietary cards
-- Mandatory and Reserved would be Ada-types)

-- e.g. T_Raw generic Header would need conversion routines Header-Types/any T_Raw  <-> String
-- just like Data_Unit (through 2 Numeric Types) is converting  DU-Types/any T_Phys <-> Float
-- NOTE !!! except of BITPIX any other keys affected by choice of T_Raw ??
-- Header is metadata -> as such it _describes_ Raw-side
-- DataUnit is the data itself -> 
-- must be implemented as such (T-defined not described by BITPIX key)


with Init; use Init;


generic
type T is private;

with function "+"(V : in Float)   return T is <>; 
with function "+"(V : in T) return Float   is <>; 
with function Is_Undef  (V,U : in T) return Boolean is <>; 
with function To_BITPIX (V   : in T) return Integer is <>; 

package FITS_IO.Data_Unit is

   type T_Arr is array (Positive_Count range <>) of T;
   subtype Buffer is T_Arr;



   -- Xfer params: READ IS FULLY SPECIFIED
   -- Physical.BITPIX = func(T) (choice: user application dictates)
   -- all Raw : from Header
   -- -- Physical.Undef (optional override)
  procedure Read
     (File    : File_Type;
      Scaling : Access_Rec;
      Item : out T_Arr;
      Last : out Count);



   -- Xfer params: WRITE leaves free selection for triple Raw.[BITPIX; A,B]
   -- all Physical (choice: user data dictates) Phys.BITPIX = func(T)
   -- Raw.BITPIX
   -- Raw.[A,B]
   -- -- Raw.Undef (optional override)
  procedure Write
     (File    : File_Type;
      Scaling : Access_Rec;
      Item : T_Arr);

  -- NOTE should [BITPIX A,B] :
  -- 1, use default Raw.BITPIX = Phys.BITPIX A=0.0 B=1.0
  -- 2, use Tab11 when UInt<->Int conversion
  -- 3, optionally parametrize to depend on Physical.Min/Max of the Data

end FITS_IO.Data_Unit;

