

with Ada.Streams.Stream_IO;

package FITS.File is

   package SIO renames Ada.Streams.Stream_IO;

   ------------------------------------------
   -- List FITS-file content : HDU params  --
   ------------------------------------------

   MaxAxes : constant Positive := 999; -- [FITS, Sect 4.4.1]
   subtype NAXIS_Type is Natural range 0 .. MaxAxes;
   -- [FITS 4.4.1.1 Primary Header] "A value of zero signifies
   -- that no data follow the header in the HDU."

   type Dim_Type is array (1..MaxAxes) of FPositive;
   -- FITS poses no limit on max value of NAXISi
   -- except the space in Card: 11..30 columns:
   -- 19 decimal digits (called by FITS 'fixed integer')
   -- That is slightly over Long_Long_Integer'Last ~ 9.2 x 10**19
   -- vs 19 digits of 9: 9.9999..x10**19.
   -- So max value NAXISn will be implementation limited.
   -- derived from Count which is derived from Address_Size:
   --  e.g. NAXISn will be 32bit or 64bit depending on the machine

   type DUSizeParam_Type is record
      Data     : FitsData_Type; -- data type as given by BITPIX
      BITPIX   : Integer;       -- BITPIX from Header (data size in bits)
      Naxes    : NAXIS_Type;  -- NAXIS  from header
      Naxis    : Dim_Type;    -- NAXISi from header, 0 means dimension not in use
   end record;
   -- collects data which defines DataUnit size

   type HDU_Size_Type is record
      CardsCnt    : Positive;    -- number of cards in this Header
      DUSizeParam : DUSizeParam_Type; -- data type as given by BITPIX
   end record;

   procedure List_Content (FitsFile : in SIO.File_Type;
                           Print : not null access
                           procedure(HDUNum : Positive;
                                     HDUSize : HDU_Size_Type) );

   ------------------------------
   -- Positioning in FITS-file --
   ------------------------------

   procedure Set_Index(FitsFile : in SIO.File_Type;
                       HDUNum   : in Positive;      -- which HDU
                       DataType : in FitsData_Type; -- decide to position to start of HeaderUnit or DataUnit
                       Offset   : in FNatural := 0); -- offset within the Unit (in units of FitsData_Type)
   -- set file-index to correct position before 'Read/'Write


   function DU_Size_blocks  (InFits  : in SIO.File_Type) return FNatural;

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

end FITS.File;

