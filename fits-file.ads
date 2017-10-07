

with Ada.Streams.Stream_IO;

package FITS.File is

   package SIO renames Ada.Streams.Stream_IO;

   procedure Parse_Header (FitsFile : in SIO.File_Type;
                           HDUSize  : in out HDU_Size_Type);
    -- extract HDU-size information

   function DU_Size_blocks  (InFits  : in SIO.File_Type) return FNatural;

   ------------------------------------------
   -- List FITS-file content : HDU params  --
   ------------------------------------------

   procedure List_Content (FitsFile : in SIO.File_Type;
                           Print : not null access
                           procedure(HDUNum : Positive;
                                     HDUSize : HDU_Size_Type) );

   ------------------------------
   -- Positioning in FITS-file --
   ------------------------------

   procedure Set_Index(FitsFile : in SIO.File_Type;
                       HDUNum   : in Positive;      -- which HDU
                       DataType : in FitsData_Type := Card; -- decide to position to start of HeaderUnit or DataUnit
                       Offset   : in FNatural := 0); -- offset within the Unit (in units of FitsData_Type)
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

end FITS.File;

