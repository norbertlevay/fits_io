

with Ada.Streams.Stream_IO;
with Mandatory; -- NAXIS_Arr needed
with Optional;  -- Card_Arr needed
with Header;    -- Image_Rec needed
--with V3_Types; -- FIXME only temporaly here; goes with To_BITPIX

package body Image is

-- FIXME pull-in correct type-dependent implementation - HOW has no params!! T only in body
    function T_To_BITPIX return Integer is begin return T'Size; end T_To_BITPIX;


function To_Cards( Im : in Image_Rec ) return Optional.Card_Arr
is
    Cards : Optional.Card_Arr(1 .. (2 + Im.NAXISn'Length));
begin
    Cards(1) := Header.Create_Mandatory_Card("BITPIX",  Header.To_Value_String(Im.BITPIX));
    Cards(2) := Header.Create_Mandatory_Card("NAXIS",   Header.To_Value_String(Im.NAXIS));
    Cards(3 .. (3 + Im.NAXISn'Length) - 1) := Header.Create_NAXIS_Card_Arr(Im.NAXISn);
    return Cards;
end To_Cards;




function To_Cards(BITPIX : in Integer) return Optional.Card_Arr
is
--    BITPIX : Integer := T_To_BITPIX;
    Mand   : Image_Rec := (NAXISn'Length, BITPIX, NAXISn);
    MandCards : Optional.Card_Arr := To_Cards(Mand);
    use Optional;-- needed operator &
    AllCards  : Optional.Card_Arr := (MandCards & Cards);
begin
    return AllCards;
end To_Cards;

-- Image


 procedure Image_Write (
               Stream : not null access Ada.Streams.Root_Stream_Type'Class;
               Item   : in  Image_Rec)
 is
     Target_Im : Image_Rec :=
                            (NAXIS => Item.NAXISn'Length, 
                            BITPIX => Target_BITPIX, 
                            NAXISn => Item.NAXISn);
     MandCards : Optional.Card_Arr := To_Cards(Target_Im);
 begin
     Optional.Card_Arr'Write(Stream, MandCards);
 end Image_Write;






end Image;

