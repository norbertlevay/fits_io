
package body Numeric_Type is

function Bit_Count return Positive
is
V : T := To_Numeric(0.0);
begin
    return V'Size;
end Bit_Count;

function BITPIX return Integer
is
V : T := To_Numeric(0.0);
begin
    return To_BITPIX(V);
end BITPIX;



  -- conversions to/form Float with undef handling

    Undef : Numeric;
    Undef_Valid : Boolean := False;

    Zero : Float := 0.0;
    NaN  : Float := 0.0 / Zero; -- FIXME see alternatives to encode NaN


 procedure Set_Undefined(U : in Numeric)
 is
 begin
     -- set only if not set yet
     -- API caller might set it
     -- and must set it when source data is Float
     -- and requests conversion to (U)Int
--     if(Not Undef_Valid)
--     then
        Undef       := U;
        Undef_Valid := True;
--     end if;
 end Set_Undefined;

 function  Is_Undefined_Valid return Boolean
 is
 begin
     return Undef_Valid;
 end Is_Undefined_Valid;


 function  Get_Undefined return Numeric
 is
     Dummy : Numeric;
 begin
     if(Undef_Valid)
     then return Undef;
     else return Dummy; -- Error: "Undef is not valid"
     end if;
 end Get_Undefined;



  -- conversions to/from Float

 function Is_Undef_Float(F : Float) return Boolean
 is
 begin
     return (Not (F = F));
 end Is_Undef_Float;


  function To_Numeric(V : in Float) return Numeric
  is
      Dummy : Numeric; -- replace with Exception
      Vn : Numeric;
  begin

      if(Undef_Valid)
      then

        -- Undefined values in use

          if(Is_Undef_Float(V))
          then
              return Undef;
          else

              Vn := +V; 

              if(Is_Undef(Vn,Undef))-- = Undef)
              then
                  return Dummy; -- Error: "Vout is Undef but Vin was not" (V would be NaN)
              else
                  return Vn;
              end if;

          end if;

      else

        -- Undefined values not in use, all values are valid

        return +V;

      end if;

  end To_Numeric;


  function To_Float  (V : in Numeric) return Float
  is
  begin
      if(Undef_Valid AND Is_Undef(V,Undef))
      then return NaN;
      else return +V;
      end if;
  end To_Float;




 function To_Numeric(Af : in Float_Arr) return Numeric_Arr
 is
     An : Numeric_Arr(Af'Range);
    Dummy : Numeric; -- FIXME replace with exceoption
 begin

     if(Undef_Valid)
     then

        -- Undefined values in use

        for I in Af'Range
        loop

            if(Is_Undef_Float(Af(I)))
            then
                An(I) := Undef;
            else
                An(I) := +Af(I);

                if(Is_Undef(An(I),Undef))
                then
                    An(I) := Dummy; -- Error: "Vout is Undef but Vin was not" (Af(I) is not Undef)
                end if;

            end if;

        end loop;

     else

        -- Undefined values not in use, all values valid

        for I in Af'Range
        loop
            An(I) := +Af(I);
        end loop;

     end if;

     return An;

 end To_Numeric;



 function To_Float(An : in Numeric_Arr) return Float_Arr
 is
     Af : Float_Arr(An'Range);
 begin

     if(Undef_Valid)
     then

         for I in An'Range
         loop
            if(Is_Undef(An(I),Undef))
            then Af(I) := NaN;
            else Af(I) := +An(I);
            end if;
         end loop;

         return Af;

     else

         for I in An'Range
         loop
             Af(I) := +An(I);
         end loop;

         return Af;

     end if;
 
 end To_Float;

end Numeric_Type;

