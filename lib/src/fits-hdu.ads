
-- stores hiarerchy of various HDU structures
-- (Primary, binary table extension, random groups,...)

-- Implements [FITS Table C.1]

package FITS.HDU is

   type NAXIS_Arr is array (Natural range <>) of Positive;

   -- Primary HDU's

   type Primary_Type(NAXIS : Positive) is tagged
    record
      BITPIX   : Integer;
      NAXISArr : NAXIS_Arr(1..NAXIS);
    end record;

--   procedure InitList(List : in In_Key_List);
   function DUSize(Data : in Primary_Type) return FNatural;


   type Random_Groups_Records is new Primary_Type with
    record
      GROUPS : Boolean;
      PCOUNT : FNatural;
      GCOUNT : FPositive;
    end record;

   function DUSize(Data : in Random_Groups_Records) return FNatural;


   -- Extensions

   type Conforming_Extension is new Primary_Type with
    record
      PCOUNT : FNatural;
      GCOUNT : FPositive;
    end record;

   function DUSize(Data : in Conforming_Extension) return FNatural;


   type Image_Extension is new Conforming_Extension with null record;

   function DUSize(Data : in Image_Extension) return FNatural;


   type ASCII_Table_Extension(NAXIS : Positive) is
    new Conforming_Extension with
    record
      TFIELDS  : FNatural;
      TFORMArr : NAXIS_Arr(1..NAXIS); -- FIXME check what array
      TBCOLArr : NAXIS_Arr(1..NAXIS); -- FIXME check what array
    end record;

   function DUSize(Data : in ASCII_Table_Extension) return FNatural;


   -- skip here BinTable for testing extendeability

end FITS.HDU;

