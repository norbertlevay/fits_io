
with Ada.Streams.Stream_IO;

with Ada.Strings.Bounded; use Ada.Strings.Bounded; -- Max20 only FIXME !!
with Keyword_Record; use Keyword_Record; -- String_80 needed
with Mandatory; -- NAXIS_Arr needed
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

end Header;
