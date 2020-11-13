

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


   Null_String : constant String := "";

   type Scaling_Rec is record
      A,B : Float;
   end record;

   type Undef_Rec is record
      Valid : Boolean;
      Raw   : Float;
      Phys  : T;
   end record;



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



   procedure Read
     (File    : SIO.File_Type;
      Scaling : Access_Rec;
      Item : out T_Arr;
      Last : out Count)
   is
      use BS70;
      File_Undefined_Value : Float;
   begin

   -- Set Undefined value

    if(Scaling.Undef_Used)
    then

        Physical.Set_Undefined(+Scaling.Undef_Phys);

        File_Undefined_Value := Scaling.Undef_Raw;
        --File_Undefined_Value := To_Undef_Value(To_String(Scaling.Undef_Raw));
        case(Scaling.BITPIX) is
             when  16=> I16Raw.Set_Undefined(I16Raw.To_Numeric(+File_Undefined_Value));
             when -32=> F32Raw.Set_Undefined(F32Raw.To_Numeric(+File_Undefined_Value));
             when  8 | 32 | 64 | -64 => null;
             when others => null;
        end case;

    end if;

    -- Scaling

    case(Scaling.BITPIX) is
        when  16 => I16_AIO.Read(SIO.Stream(File), Scaling.A,Scaling.B, Item);
        when -32 => F32_AIO.Read(SIO.Stream(File), Scaling.A,Scaling.B, Item);
        when  8 | 32 | 64 | -64 => null;
        when others => null;
    end case;

   end Read;


  procedure Write
     (File    : SIO.File_Type;
      Scaling : Access_Rec;
      Item : T_Arr)
   is
      use BS70;
      File_Undefined_Value : Float;
   begin

   -- Set Undefined value

    if(Scaling.Undef_Used)
    then

        Physical.Set_Undefined(+Scaling.Undef_Phys);

        File_Undefined_Value := Scaling.Undef_Raw;
        --File_Undefined_Value := To_Undef_Value(To_String(Scaling.Undef_Raw));
        case(Scaling.BITPIX) is
             when  16=> I16Raw.Set_Undefined(I16Raw.To_Numeric(+File_Undefined_Value));
             when -32=> F32Raw.Set_Undefined(F32Raw.To_Numeric(+File_Undefined_Value));
             when  8 | 32 | 64 | -64 => null;
             when others => null;
        end case;

    end if;

    -- Scaling

    case(Scaling.BITPIX) is
        when  16 => I16_AIO.Write(SIO.Stream(File), Scaling.A,Scaling.B, Item);
        when -32 => F32_AIO.Write(SIO.Stream(File), Scaling.A,Scaling.B, Item);
        when  8 | 32 | 64 | -64 => null;
        when others => null;
    end case;

   end Write;


end FITS_IO.Data_Unit;
