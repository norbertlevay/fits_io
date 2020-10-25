

with Numeric_Type;
with Pool_For_Numeric_Type; use Pool_For_Numeric_Type;
with Array_IO;




package body FITS_IO.Data_Unit is

   -- FIXME index is Positive but shopuld be Positive_Count
   -- - caused circular include of FITS and Numeric_Type
   type Float_Arr is array (Positive_Count range <>) of Float;
   type I16_Arr   is array (Positive_Count range <>) of Short_Integer;

   package Physical is new Numeric_Type(T, T_Arr, Float_Arr);

   package I16Raw  is new Numeric_Type(Short_Integer, I16_Arr,    Float_Arr);
   package F32Raw  is new Numeric_Type(Float,         Float_Arr,  Float_Arr);

   package I16_AIO is new Array_IO(I16Raw, Physical);
   package F32_AIO is new Array_IO(F32Raw, Physical);

   -- Ada will not convert NaN****** string into NaN value
       -- FIXME later to be part of Keyword_Record Value conversion/parsing routines
   function To_Undef_Value(S : String) return Float
   is  
      Zero : Float := 0.0;
      Loc_NaN : Float := 0.0/Zero;
   begin
      if(S = Float'Image(Loc_NaN))
      then
         return Loc_NaN;
      else
         return Float'Value(S);
      end if;
   end To_Undef_Value;

   --
   -- FIXME Undef value should be:
   --
   -- Undef_Valid : Boolean
   -- Undef_Raw   : T_Raw  := converted or user-defined value @Write
   -- Undef_Phys  : T_Phys := converted or user-defined value @Read
   --
   -- if Valid False -> values irrelevant
   -- if Valid True  -> _both_ values must be defined
   --
   -- ideally values should be in their own Type (T_Phys and T_Raw), but
   -- both types available only here: -> Undef calc/conversion must be in
   -- this module/package based on this input:
   --
   -- @Read  -> BLANK : String
   -- @Write -> Undef : T_Phys  <- be generic param () ?
   --
   -- @Read  Undef_Valid True -> if BLANK found in Header
   -- @Write Undef_Valid True -> if caller has defined for his data
   -- 
   -- FIXME below in Read/Write Undef handling overcomplicated; 
   -- valuse should be converted/calculed earlier not in Read/Write;
   -- it should be only (for both Read and Write the same):
   --
   -- if(Undef_Valid)
   --    Physical.Set_Undefined(Undef_Phys);
   --    case(RawBITPIX)
   --       TT.Raw.Set_Undefined(+(BLANK));
   --    end case
   --  end if;
   --

  procedure Read
     (File    : SIO.File_Type;
      Scaling : Scaling_Rec;
      Item : out T_Arr;
      Last : out Count)
   is
      use BS70; -- BS70 op needed
      Memory_Undefined_Valid : Boolean := Not (Scaling.Physical.Undef  = Null_Undefined_Value);
      Memory_Undefined_Value : Float;
      File_Undefined_Valid : Boolean   := Not (Scaling.Raw.Undef       = Null_Undefined_Value);
      File_Undefined_Value : Float;
   begin

   -- Set Undefined value

    if(File_Undefined_Valid)
    then

        File_Undefined_Value := To_Undef_Value(To_String(Scaling.Raw.Undef));

        case(Scaling.Raw.BITPIX) is
             when  16=> I16Raw.Set_Undefined(I16Raw.To_Numeric(+File_Undefined_Value));
             when -32=> F32Raw.Set_Undefined(F32Raw.To_Numeric(+File_Undefined_Value));
             when  8 | 32 | 64 | -64 => null; -- Warn: "Not implemented"
             when others => null; -- Error: "Invalid BITPIX for target"
        end case;

        -- did user force undefined value ? it has precedence
        if(Memory_Undefined_Valid)
        then

           Memory_Undefined_Value := To_Undef_Value(To_String(Scaling.Physical.Undef));
           Physical.Set_Undefined(+(Memory_Undefined_Value));

        else
           null; -- FIXME calc after Header read/written, not here; NOTE also calc'd in Array_IO
        end if;

    end if;

    -- Scaling

    case(Scaling.Raw.BITPIX) is
        when  16 => I16_AIO.Read(SIO.Stream(File), Scaling.Raw.A,Scaling.Raw.B, Item);
        when -32 => F32_AIO.Read(SIO.Stream(File), Scaling.Raw.A,Scaling.Raw.B, Item);
        when  8 | 32 | 64 | -64 => null; -- Warn: "Not implemented"
        when others => null; -- Error: "Invalid BITPIX for target"
    end case;

   end Read;


  procedure Write
     (File    : SIO.File_Type;
      Scaling : Scaling_Rec;
      Item : T_Arr)
   is
      use BS70; -- BS70 op needed
      Memory_Undefined_Valid : Boolean := Not (Scaling.Physical.Undef = Null_Undefined_Value);
      Memory_Undefined_Value : Float;
      File_Undefined_Valid : Boolean := Not (Scaling.Raw.Undef = Null_Undefined_Value);
      File_Undefined_Value : Float;
   begin

   -- Set Undefined value

    if(Memory_Undefined_Valid)
    then

        Memory_Undefined_Value := To_Undef_Value(To_String(Scaling.Physical.Undef));
        Physical.Set_Undefined(+Memory_Undefined_Value);

        -- did caller force undefined value ? it has precedence
        -- FIXME can this be generalized and set in Array_IO ? rather then here
        -- separately for each instance ? : Should Undef val be given Float
        if(File_Undefined_Valid)
        then

           File_Undefined_Value := To_Undef_Value(To_String(Scaling.Raw.Undef));

           case(Scaling.Raw.BITPIX) is
                when  16=> I16Raw.Set_Undefined(I16Raw.To_Numeric(+File_Undefined_Value));
                when -32=> F32Raw.Set_Undefined(F32Raw.To_Numeric(+File_Undefined_Value));
                when  8 | 32 | 64 | -64 => null; -- Warn: "Not implemented"
                when others => null; -- Error: "Invalid BITPIX for target"
           end case;

        else
           null; -- FIXME calc after Header read/written, not here; NOTE also calc'd in Array_IO
        end if;

    end if;

    -- Scaling

    case(Scaling.Raw.BITPIX) is    -- FIXME should be Scaling.Physical.A.B ?$
        when  16 => I16_AIO.Write(SIO.Stream(File), Scaling.Raw.A,Scaling.Raw.B, Item);
        when -32 => F32_AIO.Write(SIO.Stream(File), Scaling.Raw.A,Scaling.Raw.B, Item);
        when  8 | 32 | 64 | -64 => null; -- Warn: "Not implemented"
        when others => null; -- Error: "Invalid BITPIX for target"
    end case;

   end Write;


end FITS_IO.Data_Unit;
