

with Data_Value; use Data_Value;


package Cache is

   -- Cache

   -- cache of values filled in during manipulating the Header (reading or creating it)
   -- and by User API calls Set_*(File : in out File_Type, <param to set>)
   -- Cache is initialized at Create/Open and reset at Close.

   type Cache_Rec is
      record
         BITPIX : Integer; -- from Header BITPIX
         Aui, Ah, Bh, Au, Bu : Float;
         -- Aui - Tab11 shift Int-UInt conversions
         -- Ah Bh - values from Header BZERO BSCALE
         -- Au Bu - values from User calling Set_Linear_Scaling()
         Physical_Undef_Valid : Boolean;-- set from Set_Undefined_Physical()
         Physical_Undef_Value : Float;
         Raw_Undef_Valid : Boolean;-- set from Header-BLANK
         Raw_Undef_Value : Float;
      end record;


   F_Zero : Float := 0.0;
   F_NaN : constant Float := 0.0/F_Zero;
   -- FIXME NaN for Floats in Ada -> how?

   Null_Cache_Rec : constant Cache_Rec := (
      BITPIX => 0, Aui => 0.0, Ah => 0.0, Bh => 1.0, Au => 0.0, Bu => 1.0,
      Physical_Undef_Valid => False, Physical_Undef_Value => F_NaN, 
      Raw_Undef_Valid => False, Raw_Undef_Value => F_NaN);


   -- load from Cache -> Scaling : Access_Rec

   procedure Load_BITPIX_And_Scaling_AB(Scaling : in out Access_Rec; Cache : Cache_Rec);

   procedure Load_Undef_Vals_At_Write  (Scaling : in out Access_Rec; Cache : in out Cache_Rec);
   -- UndefRaw  = ( UndefPhys - A ) / B

   procedure Load_Undef_Vals_At_Read   (Scaling : in out Access_Rec; Cache : in out Cache_Rec);
   -- UndefPhys = A + B * UndefRaw

   procedure Put_Access_Rec(AccRec : Access_Rec; Prefix : String := "");

end Cache;



