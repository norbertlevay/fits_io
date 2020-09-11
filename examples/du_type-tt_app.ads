
-- empty package collects all type dependent functions for data access
--
-- Applications which use Physical and V3_Image modules
-- should be children of this package (to avoid re-typing these with-functions)

-- FIXME actually - should the Physical, V3_Image be also children of this pack ?
-- and all data-access packages (whether fitslib or user-program) should be rooted
-- in this ads-module?


with Ada.Streams.Stream_IO;


  generic
--    type Tm is private;
--    type Tm_Arr is array (Ada.Streams.Stream_IO.Positive_Count range <>) of Tm;
--    type Tc is digits <>;
    type Tf is private;

    with function To_V3Type(Arg : String) return Tf is <>;

    with function Init_UOut(UInValid : in Boolean; UIn : in Tf;
            UOutValid : in out Boolean; UOut : in out Tm) return Boolean is <>;
    with function Init_UOut(UInValid : in Boolean; UIn : in Tm;
            UOutValid : in out Boolean; UOut : in out Tf) return Boolean is <>;

    with function Is_Undef(V,U : Tf; UValid : Boolean) return Boolean is <>;
    with function Is_Undef(V,U : Tm; UValid : Boolean) return Boolean is <>;

    with function "+"(R : Tf) return Tc is <>;
    with function "+"(R : Tc) return Tm is <>;
    with function "+"(R : Tm) return Tc is <>;
    with function "+"(R : Tc) return Tf is <>;

    with function T_First return Tm is <>;
    with function T_Last  return Tm is <>;
    with function T_Image(V: Tm) return String is <>;
    with function T_Valid(V: Tm) return Boolean is <>;
    with function ">"(L,R : Tm)  return Boolean is <>;
    with function "<"(L,R : Tm)  return Boolean is <>;
    with function To_V3Type(S : String) return Tm is <>;
  package DU_Type.TT_App is
  end DU_Type.TT_App;


