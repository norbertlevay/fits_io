

-- internal model of FITS-image for creating Headers:
-- user chooses type T in which he has created tha data
-- user provides the dimesnions NAXISn

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

-- FIXME how to deal with Reserved and proprietary cards ?
-- A, generic params for the -> can generics params be null if not needed ?
-- B, use child-package(s) ? How many ?

-- Reserved support: should optionally allow to add info like Biblio, Observation...
-- (see Optional-Reserved.ads in lib/src/parser) (implement as array-of-cards?)
-- Optional (proprietary): should allow to add array-of arbitrary cards created by user
-- at write (?) chwck that all FITS-standard rules are satisfied


generic
N : Positive := 1; -- number of buffer-blocks for Card-Array and Data-Array writing
type T is private;
NAXISn : Mandatory.NAXIS_Arr;
Cards  : Optional.Card_Arr; -- FIXME what if the needed Card-array is too long for memory ? Cannot be available at one shot; but like in Write_Data, in pieces (for instance by buffer on n-blocks/36-cards)
package Image is

-- NOTE who writes the first card ? (SIMPLE / XTENSION / <other>)
procedure Write_Header(F : in File_Type) is null;
-- converts T and NAXISn to Card array
-- writes mandatory card-array
-- starts writing optional Cards (by buffer of n-blocks) FIXME how is the card-array provided? 
-- adds padding and END card


procedure Write_Data(F : in File_Type) is null;
-- starts write into block after header
-- ends with padding
-- FIXME how is the Data-array provided ?



end Image;


