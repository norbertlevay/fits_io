
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
