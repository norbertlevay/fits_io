
with Pool_For_Numeric_Type; use Pool_For_Numeric_Type;
with Array_IO;

package body FITS_IO is

   -- FIXME index is Positive but shopuld be Positive_Count
   -- - caused circular include of FITS and Numeric_Type
   type Float_Arr is array (Positive_Count range <>) of Float;
   type I16_Arr   is array (Positive_Count range <>) of Short_Integer;

   package I16Raw  is new Numeric_Type(Short_Integer, I16_Arr,    Float_Arr);
   package F32Raw  is new Numeric_Type(Float,         Float_Arr,  Float_Arr);

   package I16_AIO is new Array_IO(I16Raw, Physical);
   package F32_AIO is new Array_IO(F32Raw, Physical);












   procedure Read_Header
     (File    : SIO.File_Type;
      Scaling : out Scaling_Rec;
      NAXISn : out NAXIS_Arr;
      Undef  : in out Physical.Numeric) is begin null; end;

   procedure Write_Header
      (File    : SIO.File_Type;
       Scaling : Scaling_Rec;
       NAXISn : NAXIS_Arr;
       Undef  : Physical.Numeric) is begin null; end;-- := Physical.Null_Numeric) is null;









   procedure Read
     (File    : SIO.File_Type;
      Scaling : Scaling_Rec;
      Item : out Physical.Numeric_Arr;
      Last : out Count)
   is
   begin

    if(Scaling.File_Undefined_Valid)
    then

        I16Raw.Set_Undefined(+(Scaling.File_Undefined_Value));

        -- did user force undefined value ? it has precedence
        if(Scaling.Memory_Undefined_Valid)
        then
             case(Scaling.Memory_BITPIX) is
                when  0 | 8 | 16 | 32 | 64 | -32 | -64 =>  
                    Physical.Set_Undefined(Scaling.Memory_Undefined_Value);
                when others => null; -- Error: "Invalid BITPIX for target"
            end case;
        else
            null; -- lib calculates
        end if;

    end if;


    case(Scaling.File_BITPIX) is
        when  16 => I16_AIO.Read(SIO.Stream(File), Scaling.A,Scaling.B, Item);
        when -32 => F32_AIO.Read(SIO.Stream(File), Scaling.A,Scaling.B, Item);
        when  8 | 32 | 64 | -64 => null; -- Warn: "Not implemented"
        when others => null; -- Error: "Invalid BITPIX for target"
    end case;

   end Read;






   procedure Write
     (File    : SIO.File_Type;
      Scaling : Scaling_Rec;
      Item : Physical.Numeric_Arr)
   is
   begin

    if(Scaling.Memory_Undefined_Valid)
    then
        Physical.Set_Undefined(Scaling.Memory_Undefined_Value);

        -- did caller force undefined value ? it has precedence
        if(Scaling.File_Undefined_Valid)
        then
            case(Scaling.File_BITPIX) is
                when  16=> I16Raw.Set_Undefined(I16Raw.To_Numeric(+Scaling.File_Undefined_Value));
                when -32=> F32Raw.Set_Undefined(F32Raw.To_Numeric(+Scaling.File_Undefined_Value));
                when  8 | 32 | 64 | -64 => null; -- Warn: "Not implemented"
                when others => null; -- Error: "Invalid BITPIX for target"
            end case;
        end if;

    end if;


    case(Scaling.File_BITPIX) is
        when  16 => I16_AIO.Write(SIO.Stream(File), Scaling.A,Scaling.B, Item);
        when -32 => F32_AIO.Write(SIO.Stream(File), Scaling.A,Scaling.B, Item);
        when  8 | 32 | 64 | -64 => null; -- Warn: "Not implemented"
        when others => null; -- Error: "Invalid BITPIX for target"
    end case;


   end Write;

end FITS_IO;

