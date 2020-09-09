
with Ada.Streams.Stream_IO;

with Ada.Strings.Bounded; use Ada.Strings.Bounded; -- Max20 only FIXME !!
with Keyword_Record; use Keyword_Record; -- String_80 needed
with Mandatory; use Mandatory;-- NAXIS_Arr needed
with Optional; use Optional;-- Bounded_String_8_Arr & Card_Arr needed 

package Header is

    package SIO renames Ada.Streams.Stream_IO;

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


function Create_Card(Key : in String; Value : in String) return String_80;
function Create_Mandatory_Card(Key : in String; Value : in String) return String_80;
function Create_NAXIS_Card_Arr(NAXISn : in NAXIS_Arr) return Card_Arr;


-- constructing Header

procedure Write_Card_SIMPLE(F : in SIO.File_Type; Value : in Boolean);
procedure Write_Card_XTENSION(F : in SIO.File_Type; Ext_Name : in String);

procedure Write_Cards(F : in SIO.File_Type; Cards : in Card_Arr);
-- adds cards after last written; call several times until header completed

procedure Close(F : in SIO.File_Type);
-- writes last END-card and padding


end Header;

