
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO; -- (Positive_)Count needed
with Mandatory;

procedure scopium
is

    -- FIXME also see Header.Image_Rec & To_Primary_Cards (like in create.adb)

    NAXISLast : constant Positive := 2;
    NAXISn    : Mandatory.NAXIS_Arr(1 .. NAXISLast) := (640,512);
    CardsCnt  : Positive_Count := 6; -- SIMPLE BITPIX NAXIS NAXIS1 NAXIS2 END

    MetaData  : Mandatory.Result_Rec(Mandatory.IMAGE, NAXISLast, 0);

    procedure Write_Primary_Header(MetaData : in Mandatory.Result_Rec)
    is
    begin
        -- reserve block (36 cards)
        null;
    end;

begin
    -- create FITS-file for writing
    MetaData.CardsCount := CardsCnt;
    MetaData.BITPIX     := 8;
    MetaData.NAXISn     := NAXISn;
    Write_Primary_Header(MetaData);
    -- add other Reserved and/or proprietary keys to describe the raw-data
    ------ (Bayer RGGB, date/time, gain, exposure, instrument...)
    -- open scopium-raw-frame file -> raw-frame
    -- write to FITS_file DataUnit <- raw-frame
    -- add padding and close FITS-file
end scopium;

