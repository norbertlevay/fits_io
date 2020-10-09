


package body FITS_IO is









   procedure Read_Header
     (File    : SIO.File_Type;
      Scaling : out Scaling_Rec;
      NAXISn : out NAXIS_Arr;
      Undef  : in out Optional.BS70.Bounded_String) is begin null; end;

   procedure Write_Header
      (File    : SIO.File_Type;
       Scaling : Scaling_Rec;
       NAXISn : NAXIS_Arr;
       Undef  : Optional.BS70.Bounded_String := Null_Undefined_Value) is begin null; end;





end FITS_IO;

