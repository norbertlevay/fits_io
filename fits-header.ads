--
-- Notes:
--
-- FIXME make sure Ada Character type [Ada?][GNAT?]
-- is of same size as FITS Standard [FITS?] header-character

with FITS.Size;  use FITS.Size;

package FITS.Header is

   ------------------
   -- Parse Header --
   ------------------

   function To_Card(KeyName  : in String;
                    KeyValue : in String;
                    Comment  : in String) return Card_Type;

   --------------------------------
   -- Heade types for Read/Write --
   --------------------------------


   CardsCntInBlock : constant Positive := 36;
   type HeaderBlock_Type is array (1 .. CardsCntInBlock) of Card_Type;
   for  HeaderBlock_Type'Size use (2880 * Byte'Size);
   -- Access Header

   type HBlockArr_Type  is array ( Positive range <> ) of HeaderBlock_Type;
   type CardArr_Type    is array ( Positive range <> ) of Card_Type;
   type CharArr_Type    is array ( Positive range <> ) of Character;

   -- Header arrays

   type FitsData_Type is
       (HBlock, Card, Char); -- Header types

   type DataArray_Type ( FitsType : FitsData_Type ;
                         Length   : Positive ) is
     record
       case FitsType is
       when HBlock =>  HBlockArr  : HBlockArr_Type (1 .. Length);
       when Card  =>   CardArr    : CardArr_Type   (1 .. Length);
       when Char  =>   CharArr    : CharArr_Type   (1 .. Length);
      end case;
     end record;

   -- in file all data are packed
   -- FIXME pragma Pack is only hint for compiler
   -- to optimize for size on expense of speed,
   -- use 'Size to guarantee that array is packed
   pragma Pack (HBlockArr_Type);
   pragma Pack (CardArr_Type);
   pragma Pack (CharArr_Type);
   pragma Pack (DataArray_Type);

   --------------------------
   -- Parsing DU_Size_Type --
   --------------------------

   procedure Parse_Card_For_Size
              (Card          : in  Card_Type;
               DUSizeKeyVals : out DU_Size_Type);

   function  Write_Cards_For_Size
              (BITPIX : Integer;
               Dim    : AxesLengths_Arr ) return Card_Arr;

end FITS.Header;
