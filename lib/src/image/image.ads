
-- internal model of FITS-image for creating Headers:
-- user chooses type T in which he has created tha data
-- user provides the dimesnions NAXISn
-- optionally, user can add any reserved or proprietary Cards

-- FIXME how to deal with Reserved and proprietary cards ?
-- A, generic params for the -> can generics params be null if not needed ?
-- B, use child-package(s) ? How many ?

-- Reserved support: should optionally allow to add info like Biblio, Observation...
-- (see Optional-Reserved.ads in lib/src/parser) (implement as array-of-cards?)
-- Optional (proprietary): should allow to add array-of arbitrary cards created by user
-- at write (?) chwck that all FITS-standard rules are satisfied

-- NOTE FITS v3 the user data type is one of:
-- Unsigned_8 .. Unsigned_64
-- Signed_8 .. Signed_64
-- Float_32 and Float_64

-- It is responsibility of the Fits_IO package to convert
-- any of those type to raw-type defined in FITS v3 (U8, I16..I64, F32,F64)
-- which is a restricted set of the above types

-- FIXME how to convert generic T to BITPIX ?
-- we need to: 
-- recognize UInt_8 -> raw but other UInt_n must be converted to Int_n (for v3 FITS by Tab 11)
-- recognize Int_8 and convert it to U8, but other Int_n -> raw
-- recognize Float -> written directly as raw
-- A, use three implementations for Modular Integer Float
-- B, use private and pull-in mechanism (like in read) for those parts of code 
-- which differ by meta-type (modular, integer, float)



with Mandatory; -- NAXIS_Arr needed
with Optional;  -- Card_Arr needed


generic
type T is private;
NAXISn : Mandatory.NAXIS_Arr;
Cards  : Optional.Card_Arr := Optional.Null_Card_Arr;
package Image is

    type Data_Model is tagged null record;

    function To_Cards(BITPIX : in Integer) return Optional.Card_Arr;
    -- convert [T,NAXISn,Cards] -> Card_Arr
    -- FIXME unylear hoe convert T <-> BITPIX

end Image;


