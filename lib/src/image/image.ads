
-- NOTE 
-- conversions: V3_Types -> Value-string (both: fixed- & free-format)
-- write padding (?when EndCard written?)
-- or introduce some OpenHDU/CreateHDU - CloseHDU:
-- @Open/Create write Mandatory, @Close write ENDCard & padding (or padding only?)
--
-- Loweset level:
-- allow writing whatever card (?) or even whatever String_80
-- allow writing non-standard header (SIMPLE=F)
-- allow writig standard header (SIMPLE=T)
-- allow writing extensions (XTENSION=...)
--
-- Higher Level:
-- Image_Rec'Write
-- Reserved_Biblio_Card_Arr'Write
-- Reserved_Image_Card_Arr'Write
-- ...
-- Optional_Card_Arr'Write : write proprietary cards not defined by Standard
-- ...

with Mandatory; use Mandatory; -- NAXISn_Arr needed
with Optional; use Optional; -- Card_Arr & ENDCard needed

package Image is

type Image_Rec(NAXIS : Natural) is
    record
        BITPIX : Integer;
        NAXISn : NAXIS_Arr(1 .. NAXIS);
    end record;

function To_Primary_Cards( Im : in Image_Rec ) return Card_Arr;

function To_Extension_Cards( Im : in Image_Rec ) return Card_Arr;


end Image;
