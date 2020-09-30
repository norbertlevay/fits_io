
-- internal model of FITS-image for creating Headers:
-- user chooses type T in which he has created tha data
-- user provides the dimesnions NAXISn and the Undefined value, if applicable
-- optionally, user can add any reserved Cards
--
-- This model is final-size: NAXISn is final (NAXIS-card), and count of
-- reserved cards is final as they are predefined:
-- Image_Model'Input  -> will load Mandatory and Reserved cards
-- Image_Model'Output -> will write Mandatory and Reserved cards
--
-- Array NAXISn and Array of Reserved cards has unknown,
-- but _limited_ length:
-- use 'Input instead of 'Read: card NAXIS and count of
-- pre-defined Reserved cards provide max limit
--
-- Count of Proprietars cards is unknown and _unlimited_
-- so use buffer-approach to read them (like the data).
-- Optional-Proprietary cards are read/parsed and written separately
-- as array of generic cards.


-- NOTE on 'Class'Write attribe and dispatching:
-- (http://ada-auth.org/standards/12rm/html/RM-13-13-2.html)
-- Dispatches to the subprogram denoted by the Write attribute of the
-- specific type identified by the tag of Item.
-- 
-- procedure S'Class'Write(
--   Stream : not null access Ada.Streams.Root_Stream_Type'Class;
--   Item   : in T'Class)



-- NOTE FITS v3 the user data type is one of:
-- Unsigned_8 .. Unsigned_64
-- Signed_8 .. Signed_64
-- Float_32 and Float_64
-- It is responsibility of the Fits_IO package to convert
-- any of those type to raw-type defined in FITS v3 (U8, I16..I64, F32,F64)
-- which is a restricted set of the above types

with Ada.Streams.Stream_IO; -- Stream needed

with Mandatory; -- NAXIS_Arr needed
with Optional;  -- Card_Arr needed


generic
 type T is private;
 Dummy_Undef_Val : T; -- FIXME hm... only for init: Valid is FAlse
package Image is

    package SIO renames Ada.Streams.Stream_IO;

    type Valued_Key_Record_Arr is array (Natural range <>) of Optional.Valued_Key_Record;

    type Image_Rec(NAXIS : Natural; Key_Count : Natural) is
        record
            NAXISn          : Mandatory.NAXIS_Arr(1 .. NAXIS);
            Undefined_Valid : Boolean;
            Undefined_Value : T;
            Valued_Keys     : Valued_Key_Record_Arr(0 .. Key_Count);
            Target_BITPIX   : Integer; -- FIXME not here: not Metadata but transfer-param
        end record;

    function Metadata
        (NAXISn     : Mandatory.NAXIS_Arr;
        Valued_Keys : Valued_Key_Record_Arr) return Image_Rec;


        -- FIXME when use tagged-records to cover Undef vals, change both
        -- Image_Rec'Input 'Output to -> 'Class'Input 'Class'Output
        -- where Class is some Image_Root <- Record with only NAXISn in it ?

        -- FIXME change Image_Write -> Image_Output

    procedure Image_Write (
                 Stream : not null access Ada.Streams.Root_Stream_Type'Class;
                 Item   : in  Image_Rec);

    for Image_Rec'Write use Image_Write;



    function Image_Input(
                Stream : not null access Ada.Streams.Root_Stream_Type'Class)
                return Image_Rec;


    for Image_Rec'Input use Image_Input;




end Image;


