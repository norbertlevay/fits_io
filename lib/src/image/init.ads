

--with Header; -- Valued_Key_Record_Arr needed

with FITS_IO; use FITS_IO;

package Init is

   subtype DU_Int_Type is DU_Type range Int8 .. Int64;

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

   -- FIXME Linear_Scale() includes the Tab11 A-shift; Init_Reads/Writes should not shift again !!
   -- ?? remove from inside Init_Reads/Writes the Tab11-shift and provide - as Linear_Scale -
   -- as separate API element ?? Then Init_Reads/Writes depends on BITPIX again -> remove BITPIX
   -- from Init_Reads and Access_Rec ??

end Init;
