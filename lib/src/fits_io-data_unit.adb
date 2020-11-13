

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

   type Scaling_Rec is
      A,B : Float;
   end record;

   type Undef_Rec is
      Valid : Boolean;
      Raw   : Float;
      Phys  : T;
   end record;

   type Access_Rec is
      record
         BITPIX  : Integer;
         Scaling : Scaling_Rec;
         Undef   : Undef_Rec;
     end record;

   procedure Init_DU_Access
      (BITPIX        : String;
      BZERO, BSCALE  : String;
      User_Scaling   : Scaling_Rec;
      BLANK            : in out String;
      Undef_Phys_Valid : in out Boolean;
      Undef_Phys_Value : in out T;
      DU_Access      : out Access_Rec)
   is
      -- FIXME conversions properly (parsing?)
      Raw_BITPIX : Integer := Integer'Value(BITPIX);
      Scaling         : Scaling_Rec := (0.0, 1.0);
      Undef           : Undef_Rec;
   begin

      Scaling := Init_Scaling(Raw_BITPIX, BZERO, BSCALE);
      Undef   := Init_Undef(Scaling, BLANK, User_Undef);

      return (Bpix, Scaling, Undef);
   end Init_DU_Access;

   Null_Scaling : constant Scaling_Rec := (0.0, 1.0);

   function Init_Scaling
      (Raw_BITPIX   : Integer;
      BZERO, BSCALE : String;
      User_Scaling  : Scaling_Rec := Null_Scaling)
      return Scaling_Rec
   is
      V : T; -- dummy value, only Type needed
      Phys_BITPIX : Integer := To_BITPIX(V);
      Is_Float_Raw  : Boolean := (Raw_BITPIX  < 0);
      Is_Float_Phys : Boolean := (Phys_BITPIX < 0);
      Araw,Braw : Float;
      Aui, Bui  : Float;
   begin

      if(BZERO = Null_String)
      then Araw := 0.0;
      else Araw := Float'Value(BZERO);
      end if;

      if(BSCALE = Null_String)
      then Braw := 1.0;
      else Braw := Float'Value(BSCALE);
      end if;

      -- calc Tab11 UInt-Int conversion shift

      if((Not Is_Float_Raw) AND (Not Is_Float_Phys))
      then
         Scale_AB(V, Raw_BITPIX, Aui, Bui);
      end if;

      A := User_Scaling.A + Aui + Araw;
      B := User_Scaling.B * Bui * Braw;

      return (A,B);

   end Init_Scaling;


   Null_Undef : constant Undef_Record := (Valid => False);

   function Init_Undef
      (Scaling   : Scaling_Rec;
      BLANK      : String;
      User_Undef : Undef_Rec) --- ???? user"s preference should be only the "other side"
      return Undef_Rec
   is
      Undef : Undef_Rec := Undef_Null;
      Undef_Phys : Float;
   begin

      -- FIXME we do not care is this called in Read or Write: 
      -- if one side is missing calc the other

      -- if BLANK /= Null and Phys is Valid  -> calc Undef.Raw  = Float'Value(BLANK)
      -- if BLANK = Null and Phys is Valid   -> calc Undef.Raw  = f(inv[A,B], Undef.Phys) OR User
      -- if Phys not Valid and BLANK /= Null -> calc Undef.Phys = f(A,B,Undef.Raw)        OR User

      -- User== user supplied Undef value for the "other side" - overrides calculated value

      if((BLANK /= Undef_String) AND (Not Undef_Phys_Valid))
      then

         Undef.Raw   := Float'Value(BLANK);
         Undef.Valid := True;

         if(User_Undef = Null_Undef)
         then
            Undef_Phys := Scaling.A + Scaling.B * Undef.Raw;
         else
            Undef_Phys := User_Undef.Phys;
         end if;

      end if;

      return Undef;
   end Init_Undef;
   
   
   -- ------------------------------------------------------------------

   function Init_Scaling_For_Reads
      (Raw_BITPIX : in Integer;
      A,B         : in Float;
      BZERO, BSCALE : in String;
      BLANK       : in String;
      Undef_Phys  : in T) return Scaling_Re
   is
      Scaling : Scaling_Rec;
   begin

      Init_Reads(Raw_BITPIX, BZERO, BSCALE, A,B);

      Scaling.Raw_BITPIX := Raw_BITPIX;
      Scaling.A := A;
      Scaling.B := B;

      -- compute one of the Undefs if User did not provide
      -- Init_Undefs_For_Reads
      --             (Valid      : in Boolean;
      --             A,B :       : int Float;
      --             Undef_Raw   : in out String;
      --             Undef_Phys  : in out T)

      Scaling.Undef_Valid : (BLANK /= "");
      Scaling.Undef_Raw   := BLANK;
      Scaling:Undef_Phys  := Undef_Phys;
      return Scaling;
   end Init_Scaling_For_Reads;


   -- BZERO BSCALE must be the same as those written to Header
   procedure Init_Writes
      (Raw_BITPIX   : Integer;
      BZERO, BSCALE : String; -- in Header
      A,B : out Float) -- used in Write-Scaling
   is
      V : T; --dummy, only Type needed
      Phys_BITPIX : Integer := To_BITPIX(V);
      Is_Float_Raw  : Boolean := (Raw_BITPIX  < 0);
      Is_Float_Phys : Boolean := (Phys_BITPIX < 0);
      Araw, Braw : Float;
   begin

      if(BZERO = Null_String)
      then Araw := 0.0;
      else Araw := Float'Value(BZERO);
      end if;
      if(BSCALE = Null_String)
      then Braw := 1.0;
      else Braw := Float'Value(BSCALE);
      end if;

      if((Not Is_Float_Raw) AND (Not Is_Float_Phys))
      then
         -- calc Tab11 UInt-Int conversion shift
         Scale_AB(V : T; Raw_BITPIX : Integer; A : out Float; B : out Float);
      end if;

      -- invert raw and add U-Int conversion offset

      A := A + (-Araw / Braw);
      B := B + (  1.0 / Braw);
      -- FIXME what if Braw = 0.0

   end Init_Write;









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
      Scaling : Scaling_Rec;
      Item : out T_Arr;
      Last : out Count)
   is
      use BS70;
      File_Undefined_Value : Float;
   begin

   -- Set Undefined value

    if(Scaling.Undef_Valid)
    then

        Physical.Set_Undefined(Scaling.Undef_Phys);

        File_Undefined_Value := To_Undef_Value(To_String(Scaling.Undef_Raw));
        case(Scaling.Raw_BITPIX) is
             when  16=> I16Raw.Set_Undefined(I16Raw.To_Numeric(+File_Undefined_Value));
             when -32=> F32Raw.Set_Undefined(F32Raw.To_Numeric(+File_Undefined_Value));
             when  8 | 32 | 64 | -64 => null;
             when others => null;
        end case;

    end if;

    -- Scaling

    case(Scaling.Raw_BITPIX) is
        when  16 => I16_AIO.Read(SIO.Stream(File), Scaling.A,Scaling.B, Item);
        when -32 => F32_AIO.Read(SIO.Stream(File), Scaling.A,Scaling.B, Item);
        when  8 | 32 | 64 | -64 => null;
        when others => null;
    end case;

   end Read;


  procedure Write
     (File    : SIO.File_Type;
      Scaling : Scaling_Rec;
      Item : T_Arr)
   is
      use BS70;
      File_Undefined_Value : Float;
   begin

   -- Set Undefined value

    if(Scaling.Undef_Valid)
    then

        Physical.Set_Undefined(Scaling.Undef_Phys);

        File_Undefined_Value := To_Undef_Value(To_String(Scaling.Undef_Raw));
        case(Scaling.Raw_BITPIX) is
             when  16=> I16Raw.Set_Undefined(I16Raw.To_Numeric(+File_Undefined_Value));
             when -32=> F32Raw.Set_Undefined(F32Raw.To_Numeric(+File_Undefined_Value));
             when  8 | 32 | 64 | -64 => null;
             when others => null;
        end case;

    end if;

    -- Scaling

    case(Scaling.Raw_BITPIX) is
        when  16 => I16_AIO.Write(SIO.Stream(File), Scaling.A,Scaling.B, Item);
        when -32 => F32_AIO.Write(SIO.Stream(File), Scaling.A,Scaling.B, Item);
        when  8 | 32 | 64 | -64 => null;
        when others => null;
    end case;

   end Write;


end FITS_IO.Data_Unit;
