
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








  --------------------------------------------
  -- Experimentation on Mandatory Keys reading 
  -- for DU size calc/positioning in FITS file
  --------------------------------------------

   -- package FITS.Media is ...

   Limit999 : constant := 999;

   type PosCnt_Arr is array (Positive_Count range <>) of Positive_Count;

   type HDUPos_Type is (PRIMARY, RANDGROUP, CONF_EXT);
     -- HDUPos type absorps SIMPLE/GROUPS/XTENSION cards

   -- FIXME Use better name: Positioning --> DataSize_Keys : exact size in bits (without padding)

   -- NOTE DU_Size is then number of blocks (includes padding)

   -- these are subset of mandatory keys, needed to calc Data-size, 
   -- which is bases for positioning in the FITS file
   type Positioning_Keys ( HDUType : HDUPos_Type;
                           Last    : Positive_Count ) is
	   record
		   BITPIX : Positive_Count;
		   NAXIS  : Positive_Count;
		   NAXISn : PosCnt_Arr(1..Last);
		   case HDUType is
			   when PRIMARY => -- formula (1)
				   null;
			   when RANDGROUP .. CONF_EXT => -- formula (4)+(6) and (2)
	        	           PCOUNT   : Positive_Count;
        	        	   GCOUNT   : Positive_Count;
   	   	    end case;
	   end record;

     -- ?? or should these two remain Read/Write_Mandatory_Keys ??
     -- all positioning keys are mandatory - must be available, otherwise FITS file is not readable
     procedure Read_Positioning_Keys
	        (File : File_Type;
		 Keys : out Positioning_Keys) is null;

     procedure Write_Positioning_Keys
	        (File : File_Type;
		 Keys : Positioning_Keys) is null;
		 -- NOTE when writing, Mandatory_Keys would be better: FITS standard prescribes 
		 -- certain order of the mandatory keys depending on HDU type and all must be
		 -- present/written.

   -- end FITS.Media;





   -- package FITS.Formulas is ...
		 
         -- formulas to calculate DataUnit size (Nbits): FITS (1) (2) (4)+(6) 
	 -- should be function instead procedure, but we need "is null;"
	 -- consider defining own data types to hold the calc (NAXIS1xNAXIS2x...) can grow big

    procedure PrimaryDU_Nbits( -- FITS (1)
	     BITPIX : Positive_Count;
	     NAXISn : PosCnt_Arr;
	     Nbits  : out Positive_Count) is null;

    procedure ConfExtDU_Nbits( -- FITS (2)
	     BITPIX : Positive_Count;
	     GCOUNT : Positive_Count;
	     PCOUNT : Positive_Count;
	     NAXISn : PosCnt_Arr;
	     Nbits  : out Positive_Count) is null;

   procedure RandGroupsDU_Nbits( -- FITS (4)
	     BITPIX : Positive_Count;
	     GCOUNT : Positive_Count;
	     PCOUNT : Positive_Count;
	     NAXISn : PosCnt_Arr; -- has NAXIS1 = 0
	     Nbits  : out Positive_Count) is null;

   procedure RandGroupsDU_Nelem( -- FITS (6)
	     NAXISn : PosCnt_Arr; -- has NAXIS1 = 0
	     Nelem  : out Positive_Count) is null;

    -- end FITS.Formulas;



	     -- OVERALL view:
	     -- bottom packages: Keys, Formulas
	     -- package FITS.Media:: uses Keys and Formulas
	     -- -- Read/Write_Positioning_Keys, 
	     -- -- ?? Read/Write_Reserved_Keys (optional keys)
	     -- highest level (user interface): 
	     -- FITS_IO::Set_HDU() - will use Read_Positioning_Keys() and Formulas()




    -- package FITS.Key is ...
	     
	     type Indexed_Key is
		     record
			     Root  : String(1..7); -- FIXME proper type ? Unbounded_String?
			     Index : Positive range 1 .. 999;
		     end record;
	     -- all Key must fit into FITS-keyword length (8? chars)

	procedure Encode_Indexed_Key
		      (Root    : String;
		       Index   : Positive;
		       Keyword : out String) is null;

        procedure Decode_Indexed_Key
		      (Keyword : String;
		       Root    : String;
		       Index   : out Positive) is null;


        -- later if needed introduce Matrix_Key for large matrices (small matrices 2x2, 3x3 
		     -- handle as scalar Keys)

    -- end FITS.Key;


end FITS_IF.Header;
