

with Ada.Streams.Stream_IO;
with FITSlib.Header; use FITSlib.Header; -- HDU_Variant needed

package FITSlib.File is

   package SIO renames Ada.Streams.Stream_IO;

   -- Peek will _not_ move File Index
   -- Read will move File Index



   function Peek (File : in SIO.File_Type) return HDU_Variant;


  function Read_DataSize_bits 
	   (FitsFile : in SIO.File_Type) return Natural;



  type Data_Dimensions_Type is
                record
                        HDUVar     : HDU_Variant;
                        CardsCount : Positive;
                        BITPIX     : Integer;
                        NAXIS      : Natural;
                        NAXISn     : NAXIS_Arr(NAXIS_Range);
                end record;

 procedure Read_Data_Dimensions
                (Source  : SIO.File_Type;
                 DDims   : out Data_Dimensions_Type);


 -- -------------------------------------------------
 -- Experimental
 -- -------------------------------------------------

         type Exp_Type is
                record
                        CardsCount : Positive;
                        BITPIX     : Integer;
                end record;

    procedure Read_Exp
	    (File : SIO.File_Type; 
	     Exp  : out Exp_Type);
     



end FITSlib.File;
