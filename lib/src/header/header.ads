
with Ada.Streams.Stream_IO;
with FITS_IO; use FITS_IO; -- NAXIS_Array needed
with Mandatory;-- Result_Rec needed
with Ada.Strings.Bounded; use Ada.Strings.Bounded; -- Max20 only FIXME !!
--with Keyword_Record; use Keyword_Record; -- String_80 needed
with Optional; use Optional;-- Bounded_String_8_Arr & Card_Arr needed 

package Header is

--    package SIO renames Ada.Streams.Stream_IO;
    package SIO renames FITS_IO;


    -- BEGIN new ------------------------------------------------------------------------------

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

    -- END new ------------------------------------------------------------------------------




    -- Read funcs below always read all header, e.g.
    -- leave File_Index pointing to Data Unit start
    -- FIXME how to make this explicit ? func name, some param??

    -- read Mandatory cards

    function  Read_Mandatory (FitsFile : in SIO.File_Type)
        return Mandatory.Result_Rec;

   -- read optional/reserved cards given by Keys

   function  Read_Optional (FitsFile : in  SIO.File_Type;
                            Keys : in Optional.Bounded_String_8_Arr)
      return Card_Arr;

-- FIXME consider returning also position at which the card was in the Header
-- and hen consider returning also separately key and value/comment??

    function Has_Card(Cards : Card_Arr; Key : String; Value : out String) return Boolean;


function To_Value_String( V : in Integer) return String;
function To_Value_String( V : in String) return String;
function To_Value_String( V : in Boolean) return String;

function Create_Card(Key : in String; Value : in String) return String_80;
function Create_Mandatory_Card(Key : in String; Value : in String) return String_80;
function Create_NAXIS_Card_Arr(NAXISn : in NAXIS_Array) return Card_Arr;
function Create_NAXIS_Card_Arr(NAXISn : in NAXIS_Array) return Card_Array;


-- constructing Header

procedure Write_Card_SIMPLE(F : in SIO.File_Type; Value : in Boolean);
procedure Write_Card_XTENSION(F : in SIO.File_Type; Ext_Name : in String);

procedure Write_Cards(F : in SIO.File_Type; Cards : in Card_Arr);
-- adds cards after last written; call several times until header completed

procedure Close(F : in SIO.File_Type);
-- writes last END-card and padding


end Header;

