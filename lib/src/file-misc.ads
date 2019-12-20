

with Image; use Image;

package File.Misc is

    BlockSize_bits : constant FPositive := 2880 * Byte'Size; -- 23040 bits
    -- [FITS 3.1 Overall file structure]

   -------------------------------
   -- Write Header/Data Padding --
   -------------------------------

   HeaderPadValue : constant Unsigned_8 := 32; -- Space ASCII value
   DataPadValue   : constant Unsigned_8 :=  0;

   procedure Write_Padding(FitsFile : in SIO.File_Type;
                           From     : in SIO.Positive_Count;
                           PadValue : in Unsigned_8);
   -- [FITS ??]: FITS file consists of 2880-bytes long blocks.
   -- If last Header- or Data-block is not filled up,
   -- Write_Padding puts PadValue from FileOffset until end of the block.
   -- If Block is filled up, Write_Padding does nothing.

   ---------------------------
   -- Read/Write Image Data --
   ---------------------------

   -- FIXME notes on Item (Item_Arr) below:
   --       e.g. no need to define arrays as in FITS.ads
   --       instantation can be done by types rather then array types
   --       Q: How to guarantee then that the Item_Arr array would be packed?
   --       A: At instation the supplied array must be packed.
   --          No represantation clauses allowed in generic
   --
   generic
    type Item is private;
--    type Item_Arr is array (FPositive range <>) of Item;
    with function Element (Coord : in Strict.Positive_Arr) return Item;
   procedure Write_DataUnit (FitsFile  : in  SIO.File_Type;
                             MaxCoords : in  Strict.Positive_Arr);
   -- Item is any of FITS.UInt8 ... FITS.Float64
   -- Write_Data adds DataUnit padding after last Item written




  function  DU_Size_blocks (FitsFile : in SIO.File_Type) return FNatural; 
        -- used in commands




   ---------------------------
   -- Ops on two FITS-files --
   ---------------------------

   -- high level block/HDU copying

   --  
   -- copy NBlocks from current index position in chunks of ChunkSize_blocks
   --  
   procedure Copy_Blocks (InFits  : in SIO.File_Type;
                          OutFits : in SIO.File_Type;
                          NBlocks : in FPositive;
                          ChunkSize_blocks : in Positive := 10);
   -- FIXME is this needed ? maybe should be internal only
   --       Ext API: Copy_HDU ok.

   --  
   -- copy HDU from InFile to OutFile: both file-pointers must be correctly positioned
   --  
   procedure Copy_HDU (InFits  : in SIO.File_Type;
                       OutFits : in SIO.File_Type;
                       HDUNum  : in Positive;
                       ChunkSize_blocks : in Positive := 10);



end File.Misc;
