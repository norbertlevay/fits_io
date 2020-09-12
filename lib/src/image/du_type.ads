
-- data type used in DataUnit

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;-- Positive_Count needed$

generic
  type Tm is private;   -- type in memory
  type Tm_Arr is array (Positive_Count range <>) of Tm; 
  type Tc is digits <>; -- type in which scaling is calculated
  type Tf is private;   -- type in fits-file$
--  Tm_Null       : in out Tm;
--  Tm_Null_Valid : in out Boolean;
--  Tf_Null       : in out Tm;
--  Tf_Null_Valid : in out Boolean;
 with function Is_Undef(V,U : Tf; UValid : Boolean) return Boolean is <>;
 with function Is_Undef(V,U : Tm; UValid : Boolean) return Boolean is <>;
-- these are implemented in scaling.adb and offered in v3/v3_pool_scaling


-- for Read direction:$
with function "+"(R : Tf) return Tc is <>;
with function "+"(R : Tc) return Tm is <>;
-- for Write direction:$
 with function "+"(R : Tm) return Tc is <>;
 with function "+"(R : Tc) return Tf is <>;


with function To_V3Type(Arg : String) return Tc is <>;
with function To_V3Type(Arg : String) return Tf is <>;
with function To_V3Type(Arg : String) return Tm is <>;







package DU_Type is

    procedure Init_Undef_For_Read
        (UInValid : in     Boolean; UIn  : in     Tf; 
        UOutValid : in out Boolean; UOut : in out Tm);

    procedure Init_Undef_For_Write
        (UInValid : in     Boolean; UIn  : in     Tm; 
        UOutValid : in out Boolean; UOut : in out Tf);

    function Is_Undef_Inited return Boolean;


end DU_Type;

