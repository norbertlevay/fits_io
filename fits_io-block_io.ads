
with Ada.Streams.Stream_IO;
with Ada.Unchecked_Conversion;

 ---------------------------------------------------
 -- Low level FITS-Headers access by HeaderBlocks --
 ---------------------------------------------------

package FITS_IO.Block_IO is

 package SIO renames Ada.Streams.Stream_IO;

 BlockSize       : constant Positive_Count := 2880; -- [FITS, Sect 3.1]
 CardsCntInBlock : constant Positive_Count := BlockSize / Positive_Count(CardSize);
 -- [FITS 3.3.1 Primary Header] 36 cards per block        -- conv ok, CardSize 80 const

 subtype Card_Type is String(1..CardSize);
 -- makes sure index start with 1
 type HeaderBlock_Type is array (1 .. CardsCntInBlock) of Card_Type;
 type HeaderBlockArray_Type is array (Positive_Count range <>) of HeaderBlock_Type;
 -- Header format inside FITS-file

 ENDCard    : constant Card_Type := "END                                                                             ";
 EmptyCard  : constant Card_Type := (others => ' ');
 EmptyBlock : constant HeaderBlock_Type := (others => EmptyCard);

 function  To_SIO is new Ada.Unchecked_Conversion (File_Mode, SIO.File_Mode);
 -- function  To_FITS_IO is new Ada.Unchecked_Conversion (SIO.File_Mode, File_Mode);

 -- positioning in file

 function  To_BlockIndex( OctetIndex : in  Positive_Count ) return Positive_Count;
 function  To_OctetIndex( BlockIndex : in  Positive_Count ) return Positive_Count;

 -- Fits_IO.Count inhereted from SIO.Count:
 -- Index() and Set_Index() are inherited from SIO.[Set_]Index()
 function  BlockIndex ( File  : in SIO.File_Type ) return Positive_Count;

 procedure Set_BlockIndex ( File  : in SIO.File_Type;
                            Index : in Positive_Count ); -- Block Index

 -------------------------
 -- Read / Write Header --
 -------------------------

 procedure Write(File    : in SIO.File_Type;
                 Blocks  : in HeaderBlockArray_Type);

 function  Read (File    : in  SIO.File_Type;
                 NBlocks : in  Positive_Count := 1) return HeaderBlockArray_Type;

end FITS_IO.Block_IO;
