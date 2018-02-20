

with Ada.Streams.Stream_IO;

package FITS.File is

   package SIO renames Ada.Streams.Stream_IO;

   -- FITS numeric types are prefixed with F...
   --
   -- 1. deriving from file-system representation (Stream_IO):
   -- subtype FNatural  is SIO.Count;
   -- subtype FPositive is SIO.Positive_Count;
   -- FIXME check-out difference; this also possible:
   -- type FPositive is new SIO.Count
   --
   -- 2. deriving from FITS-Standard:
   type    FInteger  is new Long_Long_Integer;
   subtype FNatural  is FInteger range 0 .. FInteger'Last;
   subtype FPositive is FNatural range 1 .. FNatural'Last;
   -- note: package division into FITS and FITS.File
   --       favours 2. (from FITS Standard)

   MaxAxes : constant Positive := 999; -- [FITS, Sect 4.4.1]
   subtype NAXIS_Type is Natural range 0 .. MaxAxes;
   -- [FITS 4.4.1.1 Primary Header] "A value of zero signifies
   -- that no data follow the header in the HDU."

   type Dims_Type is array (1..MaxAxes) of FPositive;
   -- [FITS 4.2.3 Integer number]:
   -- FITS poses no limit on max value of Integer / NAXISn.
   -- So max value NAXISn will be implementation limited:
   -- 0 .. FPositive'Last

   type DU_Size_Type is record
      -- Primary HDU:
      BITPIX : Integer;     -- BITPIX from header (data size in bits)
      NAXIS  : NAXIS_Type;  -- NAXIS  from header
      NAXISn : Dims_Type;   -- NAXISn from header, 0 means dimension not in use
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


   procedure Parse_HeaderBlocks (FitsFile : in SIO.File_Type;
                                 HDUSize  : in out HDU_Size_Type);
    -- extract HDU-size information: read by Blocks.
    -- After this call file-pointer points to DU (or next HDU)

   function  DU_Size_blocks  (InFits  : in SIO.File_Type) return FNatural;
    -- calls Parse_HeaderBlocks & FITS.Size_blocks

   procedure List_Content (FitsFile : in SIO.File_Type;
                           Print : not null access
                           procedure(HDUNum : Positive;
                                     HDUSize : HDU_Size_Type) );
   -- list each HDU's size related parameters

   procedure Set_Index (FitsFile : in SIO.File_Type;
                        HDUNum   : in Positive);
   -- set file-index to correct position before 'Read/'Write

   --
   -- copy NBlocks from current index position in chunks of ChunkSize_blocks
   --
   procedure Copy_Blocks (InFits  : in SIO.File_Type;
                          OutFits : in SIO.File_Type;
                          NBlocks : in FPositive;
                          ChunkSize_blocks : in Positive := 10);

   --
   -- copy HDU from InFile to OutFile: both file-pointers must be correctly positioned
   --
   procedure Copy_HDU (InFits  : in SIO.File_Type;
                       OutFits : in SIO.File_Type;
                       HDUNum  : in Positive;
                       ChunkSize_blocks : in Positive := 10);


   -----------------------
   -- Size computations --
   -----------------------

   function  Free_Card_Slots (CardsCnt : in FPositive ) return Natural;
   -- calc number of free cards to fill up HeaderBlock (always 0..35 < Cards/Block)
   procedure Free_Data_Slots (DataCnt :  in FPositive; FreeDataCnt: out Natural) is null;
   -- FIXME add (as function) later when needed: calc number of free data array slots to fill up DataBlock

   function  Size_blocks (DUSizeKeyVals : in DU_Size_Type) return FPositive;
   -- size of Header and DataUnit respectively, counted in FITS-blocks
private
   function  Size_blocks (CardsCnt      : in FPositive   ) return FPositive;


end FITS.File;

