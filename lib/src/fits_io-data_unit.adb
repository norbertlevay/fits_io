

-- Read():
   -- Xfer params: READ IS FULLY SPECIFIED
   -- Physical.BITPIX = func(T) (choice: user application dictates)
   -- all Raw : from Header
   -- -- Physical.Undef (optional override)

-- Write():
   -- Xfer params: WRITE leaves free selection for triple Raw.[BITPIX; A,B]
   -- all Physical (choice: user data dictates) Phys.BITPIX = func(T)
   -- Raw.BITPIX
   -- Raw.[A,B]
   -- -- Raw.Undef (optional override)



-- 2 Rules:
-- Raw-side is defined by Header in _both_ cases Read/Write
-- each invocation of DataUnit.Read/Write only shifts File.Index
-- as it goes through the Data_Unit in sequentila manner
--
-- ergo:
-- there needs to be a Data_Unit.Init/Open... func AFTER Header is ready
-- and T is known (Data_Unit instantiated), but BEFORE repeated calls to
-- DU Read/Write start
--
-- Additional 2 Rules:
-- Header is DU-Type independent (Strings,?Floats?)
-- only Data_Unit package is generic by T and defines Physical-side


-- NOTE Read/Write call should _only_ shift File-index as it goes through Data Unit
-- no other state (ScalingRec & local vars) should change, (besides data moved of course)

-- NOTE on higher-level API: what if Header would be generic by T_Raw ?
-- just as Data_Unit is generic by T_Physical ?
-- (Header type-indepence would apply only to proprietary cards
-- Mandatory and Reserved would be Ada-types)

-- e.g. T_Raw generic Header would need conversion routines Header-Types/any T_Raw  <-> String
-- just like Data_Unit (through 2 Numeric Types) is converting  DU-Types/any T_Phys <-> Float
-- NOTE !!! except of BITPIX any other keys affected by choice of T_Raw ??
-- Header is metadata -> as such it _describes_ Raw-side
-- DataUnit is the data itself -> 
-- must be implemented as such (T-defined not described by BITPIX key)




-- NOTE should [BITPIX A,B] :
-- 1, use default Raw.BITPIX = Phys.BITPIX A=0.0 B=1.0
-- 2, use Tab11 when UInt<->Int conversion
-- 3, optionally parametrize to depend on Physical.Min/Max of the Data








with Numeric_Type;
with Pool_For_Numeric_Type; use Pool_For_Numeric_Type;
with Array_IO;




package body FITS_IO.Data_Unit is

   package SIO renames Ada.Streams.Stream_IO;

   type Float_Arr is array (Positive_Count range <>) of Float;
   type I16_Arr   is array (Positive_Count range <>) of Short_Integer;

   package Physical is new Numeric_Type(T, T_Arr, Float_Arr);

   package I16Raw  is new Numeric_Type(Short_Integer, I16_Arr,    Float_Arr);
   package F32Raw  is new Numeric_Type(Float,         Float_Arr,  Float_Arr);

   package I16_AIO is new Array_IO(I16Raw, Physical);
   package F32_AIO is new Array_IO(F32Raw, Physical);


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
     (File    : File_Type;
      DU : Data_Unit_Type;
      --Scaling : Access_Rec;
      Item : out T_Arr;
      Last : out Count)
   is
      Scaling : Access_Rec := DU.Scaling;
   begin

   -- Set Undefined value

    if(Scaling.Undef_Used)
    then

        Physical.Set_Undefined(+Scaling.Undef_Phys);

        case(Scaling.BITPIX) is
             when  16=> I16Raw.Set_Undefined(I16Raw.To_Numeric(+Scaling.Undef_Raw));
             when -32=> F32Raw.Set_Undefined(F32Raw.To_Numeric(+Scaling.Undef_Raw));
             when  8 | 32 | 64 | -64 => null;
             when others => null;
        end case;

    end if;

    -- Scaling

    case(Scaling.BITPIX) is
        when  16 => I16_AIO.Read(Stream(File), Scaling.A,Scaling.B, Item);
        when -32 => F32_AIO.Read(Stream(File), Scaling.A,Scaling.B, Item);
        when  8 | 32 | 64 | -64 => null;
        when others => null;
    end case;

   end Read;


  procedure Write
     (File    : File_Type;
      DU : Data_Unit_Type;
      --Scaling : Access_Rec;
      Item : T_Arr)
   is
      Scaling : Access_Rec := DU.Scaling;
   begin

   -- Set Undefined value

    if(Scaling.Undef_Used)
    then

        Physical.Set_Undefined(+Scaling.Undef_Phys);

        case(Scaling.BITPIX) is
             when  16=> I16Raw.Set_Undefined(I16Raw.To_Numeric(+Scaling.Undef_Raw));
             when -32=> F32Raw.Set_Undefined(F32Raw.To_Numeric(+Scaling.Undef_Raw));
             when  8 | 32 | 64 | -64 => null;
             when others => null;
        end case;

    end if;

    -- Scaling

    case(Scaling.BITPIX) is
        when  16 => I16_AIO.Write(Stream(File), Scaling.A,Scaling.B, Item);
        when -32 => F32_AIO.Write(Stream(File), Scaling.A,Scaling.B, Item);
        when  8 | 32 | 64 | -64 => null;
        when others => null;
    end case;

   end Write;


end FITS_IO.Data_Unit;
