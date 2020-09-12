
-- data type used in DataUnit

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;-- Positive_Count needed$

package body DU_Type is

  Tm_Null       : Tm;
  Tm_Null_Valid : Boolean;
  Tf_Null       : Tf;
  Tf_Null_Valid : Boolean;

    procedure Init_Undef_For_Read
        (UInValid : in     Boolean; UIn  : in     Tf; 
        UOutValid : in out Boolean; UOut : in out Tm)
    is
    begin
        Tf_Null       := UIn;
        Tf_Null_Valid := UInValid;
        Tm_Null       := UOut;
        Tm_Null_Valid := UOutValid;
    end Init_Undef_For_Read;

    procedure Init_Undef_For_Write
        (UInValid : in     Boolean; UIn  : in     Tm; 
        UOutValid : in out Boolean; UOut : in out Tf)
    is
    begin
        Tf_Null       := UOut;
        Tf_Null_Valid := UOutValid;
        Tm_Null       := UIn;
        Tm_Null_Valid := UInValid;
    end Init_Undef_For_Write;

function Is_Undef_Inited return Boolean
is
begin
  return (Tf_Null_Valid OR Tm_Null_Valid);
end Is_Undef_Inited;


end DU_Type;

