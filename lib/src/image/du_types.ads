

--with FITS_IO; use FITS_IO;
with FITS; use FITS;

package DU_Types is

   subtype DU_Int_Type is DU_Type range Int8 .. Int64;

   function BITPIX_To_DU_Type(BITPIX : Integer) return DU_Type;


   -- implements signed-unsigned conversions for integer types as of Tab11
   procedure DU_Type_To_BITPIX
      (DUType : DU_Type;
      BITPIX  : out Integer;
      Aui     : out Float);

   -- Array_Keys: BZERO BSCALE BUNIT BLANK DATAMIN DATAMAX

   procedure Linear_Scale
      (DUIntType : in DU_Int_Type;
      DATAMIN, DATAMAX : in Float; -- DATAMAX /= DATAMIN
      A : out Float; B : out Float);

   -- FIXME Linear_Scale() includes the Tab11 A-shift; DU_Types_Reads/Writes should not
   -- shift again !!
   -- ?? remove from inside DU_Types_Reads/Writes the Tab11-shift and provide - as Linear_Scale -
   -- as separate API element ?? Then DU_Types_Reads/Writes depends on BITPIX again->remove BITPIX
   -- from DU_Types_Reads and Access_Rec ??

end DU_Types;
