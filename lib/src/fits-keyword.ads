
with Ada.Strings.Bounded;
use Ada.Strings.Bounded;


package FITS.Keyword is

   package Max_8 is
       new Ada.Strings.Bounded.Generic_Bounded_Length (Max =>  8);
   package Max20 is
       new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 20);
   package Max48 is
       new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 48);
   package Max70 is
       new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 70);






   type Keyword_Type is tagged
    record
	Name    : Max_8.Bounded_String;
    end record;

   function Match(Key  : in Keyword_Type;
                  Card : in Card_Type) return Boolean;


   type Indexed_Keyword_Type is new Keyword_Type with
    record
        Index_First : Natural;
        Index_Last  : Positive;
        Index       : Natural;
    end record;

   function Match(Key  : in Indexed_Keyword_Type;
                  Card : in Card_Type) return Boolean;

end FITS.Keyword;

-- Fits file is PrimaryHDU-ConfExtens-SpecialRecords:

-- [FITS 3.1] The primary HDU shall start with
-- the first FITS block of the FITS file.
-- [FITS 4.4.1.1] The SIMPLE keyword is required to
-- be the first keyword in the primary header of all FITS files.
-- [FITS Table 7] all mandatory keys in primary HDU

-- [FITS 3.4.1.1] Each extension type shall have a unique
-- type name, specified in the header by the XTENSION keyword.
-- [FITS Table 10] all mandatory keys for conforming extensions
-- [FITS 4.4.1.2] XTENSION key: This keyword is
-- mandatory for an extension header and must not appear in the
-- primary header.

-- [FITS 3.5] Special records are 2880-byte FITS blocks following the last
-- HDU of the FITS file that have an unspecified structure that
-- does not meet the requirements of a conforming extension. The
-- first 8 bytes of the special records must not contain the string
-- "XTENSION". It is recommended that they do not contain the
-- string "SIMPLE   ". The contents of special records are not oth-
-- erwise specified by this standard.

-- [FITS 6] Random Groups:
-- ... consists of a subarray of data and a set of asso-
-- ciated parameter values, to be stored within the FITS primary
-- data array.
-- ... the random groups structure is DEPRECATED and should not
-- be further used.
-- [FITS 6.1.1] The SIMPLE keyword is required to be the first keyword in the
-- primary header of all FITS files, including those with random
-- groups records.
-- ...
-- GROUPS keyword. The value field shall contain the logical con-
-- stant T. The value T associated with this keyword implies that
-- random groups records are present.
-- [FITS Table 12] all mandatory keys for random groups

-- Keyword types:
-- [FITS Tables C1 C.2 C.3] Keyword groups:
-- Mandatory, Reserved, General Reserved, and
-- WCS keywords [FITS Section 8 WCS]

-- [FITS Appendix A: Syntax of keyword records] 2 forms:
-- Commentary and Valued keyword record (=card)

-- [FITS 2.1] Indexed keyword. A keyword name that is of the form of a
-- fixed root with an appended positive integer index number.

-- [FITS 8.2] Note: PCi_j CDi_j see eq (9) and (10) where i,j is NxN matrix
--                  of max world coordinates
-- [FITS 8.2] Note: PVi_m PSi_m where i coordinate index and m = 0..99

-- [FITS 8.2.1 Table 22 suffix 'a']
--  Alterantive WCS axis description : append 'A'...'Z' to keyword name
-- All implied keywords with this encoding
-- are reserved keywords, and must only be used in FITS HDUs
-- as specified in this Standard. The axis numbers must lie in the
-- range 1 through 99, and the coordinate parameter m must lie in
-- the range 0 through 99, both with no leading zeros.

