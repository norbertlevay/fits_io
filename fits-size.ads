
--------------------------------
-- Header and Data unit Sizes --
--------------------------------

package FITS.Size is

   -- FITS numeric types are prefixed with F...
   --
   -- 1. deriving from file-system representation (Stream_IO):
   -- subtype FNatural  is SIO.Count;
   -- subtype FPositive is SIO.Positive_Count;
   -- FIXME check-out difference; this also possible:
   -- type FPositive is new SIO.Count
   --

   MaxAxes : constant Positive := 999; -- [FITS, Sect 4.4.1]
   subtype NAXIS_Type is Natural range 0 .. MaxAxes;
   -- [FITS 4.4.1.1 Primary Header] "A value of zero signifies
   -- that no data follow the header in the HDU."


   type AxesLengths_Arr is array (Positive range <>) of FPositive;
--   type Dims_Type is array (1..MaxAxes) of FPositive;
   -- [FITS 4.2.3 Integer number]:
   -- FITS poses no limit on max value of Integer / NAXISn.
   -- So max value NAXISn will be implementation limited:
   -- 0 .. FPositive'Last

   type DU_Size_Type is record
      -- Primary HDU:
      BITPIX : Integer;     -- BITPIX from header (data size in bits)
      NAXIS  : NAXIS_Type;  -- NAXIS  from header
      NAXISn : NAXISn_Type;   -- NAXISn from header, 0 means dimension not in use
      -- Conforming extensions:
      PCOUNT : FNatural;    -- BINTABLE: size of heap OR Random Groups: param count preceding each group
      GCOUNT : FPositive;   -- Number of Random Groups present
      -- FIXME what type to use for P/GCOUNT ? -> implementation limited?
   end record;
   -- collects keyword values which define DataUnit size

   type HDU_Size_Type is record
      XTENSION      : String(1..10); -- XTENSION type string or empty
      CardsCnt      : FPositive;     -- number of cards in this Header (gives Header-size)
      DUSizeKeyVals : DU_Size_Type;  -- keyword values to calc DataUnit-size
   end record;


   -----------------------
   -- Size computations --
   -----------------------

   --
   -- calculate Header size in FITS Blocks
   --
   function  Size_blocks (CardsCnt      : in FPositive   ) return FPositive;

   --
   -- calculate DataUnit size in FITS Blocks
   --
   function  Size_blocks (DUSizeKeyVals : in DU_Size_Type) return FPositive;
   -- implements Eq(1), (2) and (4) from [FITS]
   -- However we should parse other keys (SIMPLE, XTENSION, GROUPS) to
   -- establish HDU type


   --
   -- calc number of free cards to fill up HeaderBlock
   --
   function  Free_Card_Slots (CardsCnt : in FPositive ) return Natural;
   --  always 0..35 < 36(=Cards per Block)

   --
   -- calc number of free data array slots to fill up DataBlock
   --
   procedure Free_Data_Slots (DataCnt :  in FPositive; FreeDataCnt: out Natural) is null;
   -- FIXME add (as function) later when needed

end FITS.Size;


