
with Ada.IO_Exceptions;
with Ada.Direct_IO;
--use Ada.Direct_IO;

with FITS; use FITS;

package FITS_IF.Header is
      
   -- random access

    procedure Read
     (File : File_Type;
      Card : out Card_Type;
      From : Positive_Count) is null;

   procedure Write
     (File : File_Type;
      Card : Card_Type;
      To   : Positive_Count) is null;

   -- and other: like access by card key, number of cards...
      -- buffered access where caller supplies block-sized buffer,
      -- access many consecutive cards/card arrays: Card(1..5) Card_Arr


end FITS_IF.Header;
