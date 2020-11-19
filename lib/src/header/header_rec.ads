
with Ada.Streams.Stream_IO;
with FITS_IO; use FITS_IO; -- NAXIS_Array needed
with Mandatory;-- Result_Rec needed
with Ada.Strings.Bounded; use Ada.Strings.Bounded; -- Max20 only FIXME !!
with Optional; use Optional;-- Bounded_String_8_Arr & Card_Arr needed 

package Header_Rec is

    package SIO renames FITS_IO;



    type Valued_Key_Record_Arr is array (Natural range <>) of Optional.Valued_Key_Record;

    type Header_Rec(NAXIS, Desc_Last, Obs_Last, Bib_Last,
                    Comms_Last, Arrs_Last, Wcs_Last, Ext_Last,
                    Prop_Last : Natural) is
        record
            BITPIX : Integer; -- BITPIX in FIle
  --          NAXISn : NAXIS_Array(1 .. NAXIS);
            Desc   : Valued_Key_Record_Arr(1 .. Desc_Last);
            Obs    : Valued_Key_Record_Arr(1 .. Obs_Last);
            Biblio : Valued_Key_Record_Arr(1 .. Bib_Last);
            Arrs   : Valued_Key_Record_Arr(1 .. Arrs_Last);
            Wcs    : Valued_Key_Record_Arr(1 .. Wcs_Last);
            Ext    : Valued_Key_Record_Arr(1 .. Ext_Last);
            Prop   : Valued_Key_Record_Arr(1 .. Prop_Last); -- proprietary cards
        end record;
    -- read/parse all keys as defined by standard

    -- or alternatively

--    function  Read_Mandatory (FitsFile : in SIO.File_Type; Keys : Key_Arr := Null_Keys)
--        return Mand_And_Reserved_Rec; -- or Header_Rec above
    -- read/parse mandatory and selected keys (by 'Keys' param)

    -- NOTE the Header_Rec_Input used for 'Input internally calls Read_Mandatory
    -- below with 'Keys' set to all reserved as defined by standard

    -- FIXME  Comments : Valued_Key_Record_Arr(1 .. Comms_Last);-- FIXME is not Valued_Key !
    -- also max count of commnets is not known - does not belong here

    -- FIXME check, that all other reserved card-groups are final/bounded in count-of-cards




end Header_Rec;

