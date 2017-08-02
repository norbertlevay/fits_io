
 ---------------------------------------------------
 -- Low level FITS-Headers access by HeaderBlocks --
 ---------------------------------------------------

-- Low-level FITS file is viewed as array of blocks of size 2880 octets.
--
-- This module guarantees correct FITS-file size and HDU-offsets,
-- both always being multiple of 2880 octets as required by the standard [FITS].
--
-- This module is used to read/write the FITS-Headers (not FITS-Data),
-- because the Header defines the FITS files/HDU sizes.
-- Any Data-Unit is then filled in by data within the limits defined by its Header.
--
-- Currently implemented with Stream_IO:
-- Possible with Direct_IO which would eliminate block position calculations.
-- However Direct_IO does not allow to write more blocks with one write so needs cycle,
-- and Read is procedure not a function, then cannot initialize unconstrained arrays,
-- and so also needs cycle by Block.
-- Decided in favour of Stream_IO.

with Ada.Text_IO;-- for debug only
with Ada.Streams.Stream_IO; --use Ada.Streams.Stream_IO;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

with Ada.Containers.Vectors; -- since Ada 2005
 -- for HDU dynamic vector implementation; unlike static array,
 -- dynamic vector allows unlimited number of HDU's in FITS-file
 -- as required in FITS standard

package body FITS_IO.Block_IO is

 --
 -- Positioning in file
 --
 function  To_BlockIndex( OctetIndex : in  Positive_Count ) return Positive_Count is
  begin
   return (OctetIndex - 1) / BlockSize + 1;
  end To_BlockIndex;

 function  To_OctetIndex( BlockIndex : in  Positive_Count ) return Positive_Count is
  begin
   return (BlockIndex - 1) * BlockSize + 1;
  end To_OctetIndex;

 pragma Inline (To_BlockIndex);
 pragma Inline (To_OctetIndex);
 -- Note: since Ada2012 pragma Inline obsolete, add anyway for older compilers

 -- Fits_IO.Count inhereted from SIO.Count:
 -- Index() and Set_Index() are inherited from SIO.[Set_]Index()
 function  BlockIndex ( File  : in SIO.File_Type ) return Positive_Count
 is
 begin
  return To_BlockIndex(Index( File ));
 end BlockIndex;

 procedure Set_BlockIndex ( File  : in SIO.File_Type;
                            Index : in Positive_Count ) -- Block Index
 is
 begin
  Set_Index( File, To_OctetIndex(Index) );
 end Set_BlockIndex;

 pragma Inline (BlockIndex);
 pragma Inline (Set_BlockIndex);

 --
 -- Header access
 --
 procedure Write(File    : in SIO.File_Type;
                 Blocks  : in HeaderBlockArray_Type)
 is
 begin
   HeaderBlockArray_Type'Write( SIO.Stream(File), Blocks );
 end Write;

 function  Read (File    : in  SIO.File_Type;
                 NBlocks : in  Positive_Count := 1) return HeaderBlockArray_Type
 is
   Blocks : HeaderBlockArray_Type( 1 .. NBlocks );
 begin
   HeaderBlockArray_Type'Read( SIO.Stream(File), Blocks );
   return Blocks;
 end Read;

end FITS_IO.Block_IO;
