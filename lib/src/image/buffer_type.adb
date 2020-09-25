
with Numeric_Type;
with Array_IO;

with Ada.Text_IO;
use Ada.Text_IO;

package body Buffer_Type is

 type Float_Arr is array (SIO.Positive_Count range <>) of Float;
 type T_Arr is array (SIO.Positive_Count range <>) of T;
 -- FIXME replace Short_Integer with Integer_16
 -- use here only values allowed inside FITS files
 type I16_Arr is array (SIO.Positive_Count range <>) of Short_Integer;

 function "+"(V : in Float) return Short_Integer is begin return Short_Integer(V); end "+";
 function "+"(V : in Short_Integer)     return Float is begin return Float(V); end "+";

 function Is_Undef(V,U : in Short_Integer) return Boolean is begin return (V=U); end Is_Undef;
 function To_BITPIX(V : in Short_Integer) return Integer is begin return V'Size; end To_BITPIX;

 -- prepare the conversion arrays

 package Phys is new Numeric_Type(T, Buffer, Float_Arr);

 package TRaw  is new Numeric_Type(T, T_Arr, Float_Arr);
 package T_AIO is new Array_IO(TRaw,Phys);

 package I16Raw  is new Numeric_Type(Short_Integer, I16_Arr, Float_Arr);
 package I16_AIO is new Array_IO(I16Raw,Phys);




procedure Read_Buffer(S: not null access Ada.Streams.Root_Stream_Type'Class; Item : out Buffer)
is
begin

    if(Undefined_Valid)
    then

        Phys.Set_Undefined(Undefined_Value);

    end if;


    case(Target_BITPIX) is
        when  0 =>   T_AIO.Read(S, A,B, Item);
        when 16 => I16_AIO.Read(S, A,B, Item);
        when  8 | 32 | 64 | -32 | -64 => null; -- Warn: "Not implemented"
        when others => null; -- Error: "Invalid BITPIX for target"
    end case;
end Read_Buffer;



procedure Write_Buffer(S: not null access Ada.Streams.Root_Stream_Type'Class; Item : in Buffer)
is
begin

    if(Undefined_Valid)
    then
        Phys.Set_Undefined(Undefined_Value);

        -- caller has supplied Undefined value for target
        -- it has precedent, so set it (Undef value can be set only
        -- once after init of Numeric_Type see Set_Undefined)

        if(Target_Undefined_Valid)
        then
            case(Target_BITPIX) is
                when  0 =>   TRaw.Set_Undefined(  TRaw.To_Numeric(Target_Undefined_Value));
                when 16 => I16Raw.Set_Undefined(I16Raw.To_Numeric(Target_Undefined_Value));
                when  8 | 32 | 64 | -32 | -64 => null; -- Warn: "Not implemented"
                when others => null; -- Error: "Invalid BITPIX for target"
            end case;
        end if;

    end if;


    case(Target_BITPIX) is
        when  0 =>   T_AIO.Write(S, A,B, Item);
        when 16 => I16_AIO.Write(S, A,B, Item);
        when  8 | 32 | 64 | -32 | -64 => null; -- Warn: "Not implemented"
        when others => null; -- Error: "Invalid BITPIX for target"
    end case;
end Write_Buffer;



end Buffer_Type;

