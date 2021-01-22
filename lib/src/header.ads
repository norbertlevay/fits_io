
-- Funcs in this module always operate (read/write) on all Header:
-- Read funcs below always read all header, e.g.
-- leave File_Index pointing to Data Unit start
-- FIXME how to make this explicit ? func name, some param??


-- Parser and Composer implement the rules, how groups of cards are related in Header

   -- Distinguish Primary and Conforming Extension (SIMPLE vs XTENSION card)
   -- * RULE first HDU always starts with SIMPLE-card
   -- * RULE conforming extension always start with XTENSION-Card
   -- * RULE if not File-End and not SIMPLE and not XTENSION -> then RandomRecords until EOF
   -- Mandatory cards (Image,Table,BinTable) are first (no other cards may interleave)
   -- * RULE ...
   -- Reserved cards
   -- * RULE ...
   -- Closing the Header
   -- * RULE ENDCard is last and follwed by padding

with Ada.Streams.Stream_IO;
--with FITS_IO; use FITS_IO; -- NAXIS_Array needed
with Mandatory;-- Result_Rec needed
--with Ada.Strings.Bounded; use Ada.Strings.Bounded; -- Max20 only FIXME !!
with Optional; use Optional;-- Bounded_String_8_Arr & Card_Arr needed 


package Header is

   -- package SIO renames FITS_IO;
   package SIO renames Ada.Streams.Stream_IO;


   -- API read Header: run the Parser, supplying one card from File-Stream

   function  Read_Mandatory
       (FitsFile : in SIO.File_Type) return Mandatory.Result_Rec;

   function  Read_Optional
      (FitsFile : in  SIO.File_Type;
      Keys : in Optional.Bounded_String_8_Arr) return Card_Arr;


   -- API constructing Header: use the Composer, supplying Card-groups

   procedure Write_ENDCard_With_Padding(FFile : SIO.File_Type);
 
   -- procedure Write_Cards(F : in SIO.File_Type; Cards : in Card_Arr);
      -- adds cards after last written; call several times until header completed

   -- procedure Close(F : in FITS_IO.File_Type);
      -- writes last END-card and padding


end Header;

