


package body FITS_IO is









   procedure Read_Header
     (File    : SIO.File_Type;
      Scaling : out Scaling_Rec;
      NAXISn : out NAXIS_Array;
      Undef  : in out BS70.Bounded_String) is begin null; end;

   procedure Write_Header
      (File    : SIO.File_Type;
       Scaling : Scaling_Rec;
       NAXISn : NAXIS_Array;
       Undef  : BS70.Bounded_String := Null_Undefined_Value) is begin null; end;





end FITS_IO;

