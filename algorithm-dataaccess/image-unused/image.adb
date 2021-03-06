

with Mandatory;
with Optional;  -- Card_Arr needed
with Header;    -- Image_Rec needed
with Card;
with FITS_IO; use FITS_IO;

package body Image is

-- FIXME pull-in correct type-dependent implementation - HOW has no params!! T only in body
    function T_To_BITPIX return Integer is begin return T'Size; end T_To_BITPIX;


    function Metadata
        (NAXISn      : NAXIS_Array;
         Valued_Keys : Valued_Key_Record_Arr) return Image_Rec
    is
        Im : Image_Rec(NAXISn'Last, Valued_Keys'Last);
    begin
        Im.NAXISn           := NAXISn;
        Im.Undefined_Valid  := False;
--        Im.Undefined_Value  := Dummy_Undef_Val;
        Im.Valued_Keys      := Valued_Keys;
        Im.BITPIX := T_To_BITPIX;
        return Im;
    end Metadata;



    -- Write to stream


-- new for FITS_IO API

function To_Cards( Im : in Image_Rec ) return FITS_IO.String_80_Array
is
    Cards : FITS_IO.String_80_Array(1 .. (2 + Im.NAXISn'Length + Im.Valued_Keys'Length) );
    Ix : Positive_Count;
begin
    Cards(1) := Card.Create_Mandatory_Card("BITPIX",  Card.To_Value_String(Im.BITPIX));
    Cards(2) := Card.Create_Mandatory_Card("NAXIS",   Card.To_Value_String(Im.NAXIS));
    Cards(3 .. (3 + Im.NAXISn'Length) - 1) := Card.Create_NAXIS_Card_Arr(Im.NAXISn);
    -- here write Valued_Keys.. FIXME
    Ix := (3 + Im.NAXISn'Length) - 1;
    -- next position
    for I in Im.Valued_Keys'Range
    loop
        Ix := Ix + 1;
        Cards(Ix) := Card.Create_Mandatory_Card
                            (Optional.BS_8.To_String(Im.Valued_Keys(I).Key),
                             Card.To_Value_String(
                                 Optional.BS70.To_String(Im.Valued_Keys(I).Value)));
    end loop;
    return Cards;
end To_Cards;




-- Image


 procedure Image_Output (
               Stream : not null access Ada.Streams.Root_Stream_Type'Class;
               Item   : in  Image_Rec)
 is
    MandCards : Optional.Card_Arr := To_Cards(Item);
 begin
     Optional.Card_Arr'Write(Stream, MandCards);
 end Image_Output;



-- Read/Parse header
 function  Read_Mandatory (Fits_Stream : in SIO.Stream_Access) return Mandatory.Result_Rec
 is
     Card : String(1..80);
     CardNum : Count;
    --use SIO;
 begin
     CardNum := Mandatory.Reset_State;
     loop
         String'Read(Fits_Stream, Card);
         CardNum := Mandatory.Next(CardNum, Card);
         exit when (CardNum = 0);
     end loop;

     declare
         Res : Mandatory.Result_Rec := Mandatory.Get;
     begin
         return Res;
     end;
 end Read_Mandatory;


     function Image_Input(
                 Stream : not null access Ada.Streams.Root_Stream_Type'Class)
                 return Image_Rec
     is
         Mand : Mandatory.Result_Rec := Read_Mandatory(SIO.Stream_Access(Stream));
     begin
        declare
         Im : Image_Rec(Mand.NAXISn'Length, 0);
        begin
            Im.NAXISn := Mand.NAXISn;
            Im.BITPIX := Mand.BITPIX;
            return Im;
        end;
    end Image_Input;


end Image;

