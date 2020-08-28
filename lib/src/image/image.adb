

with Ada.Streams.Stream_IO;
with Mandatory; -- NAXIS_Arr needed
with Optional;  -- Card_Arr needed
with Header;    -- Image_Rec needed
with Keyword_Record; -- ENDCard needed
with V3_Types; -- FIXME only temporaly here; goes with To_BITPIX
with File.Misc; -- needs Write_Padding for Header

--generic
--type T is private;
--NAXISn : Mandatory.NAXIS_Arr;
--Cards  : Optional.Card_Arr := Optional.Null_Card_Arr; 
--N : Positive := 1; -- number of buffer-blocks for Card-Array and Data-Array writing
package body Image is

-- FIXME pull-in correct type-dependent implementation - HOW has no params!! T only in body
    function T_To_BITPIX return Integer is begin return T'Size; end T_To_BITPIX;


-- NOTE who writes the first card ? (SIMPLE / XTENSION / <other>)
-- converts T and NAXISn to Card array
-- writes mandatory card-array
-- starts writing optional Cards (by buffer of n-blocks) FIXME how is the card-array provided? 
-- adds padding and END card
procedure Write_Header(F : in SIO.File_Type)
is
    BITPIX : Integer := T_To_BITPIX;
    Mand   : Header.Image_Rec := (NAXISn'Length, BITPIX, NAXISn);
    MandCards : Optional.Card_Arr := Header.To_Primary_Cards(Mand);
    use Optional;-- needed operator &
    AllCards  : Optional.Card_Arr := (MandCards & Cards & Keyword_Record.ENDCard);
begin
    Optional.Card_Arr'Write(SIO.Stream(F), AllCards);
    File.Misc.Write_Padding(F,SIO.Index(F),File.Misc.HeaderPadValue);
    -- FIXME get rid of this, 
    -- rather calc reminder to 36 and write that many (=EmptyCards*CardLength) HeaderPad-values
end Write_Header;





--procedure Write_Data(F : in SIO.File_Type) is null;
-- starts write into block after header
-- ends with padding
-- FIXME how is the Data-array provided ?



end Image;


