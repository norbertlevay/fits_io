


package Linear_Conv is

--  FI :  Tout = F   Tin = I
--
--
--                  Tin
--             F       I       U
--           ---------------------
--        F |  FF    FI/B    FU/B
--          |
--   Tout I | IF/B    II      IU
--          |
--        U | UF/B    UI      UU
--
--
-- IF UF FI FU have two variants: with and without BLANK
--


-- convert to Floats: FF FI FU

generic type Tin is digits <>; type Tc is digits <>; type Tout is digits <>; A,B: in out Tc; ToutNaN : Tout;   function FF(Vin : Tin) return Tout;
generic type Tin is range  <>; type Tc is digits <>; type Tout is digits <>; A,B: in out Tc;                   function FI(Vin : Tin) return Tout;
generic type Tin is mod    <>; type Tc is digits <>; type Tout is digits <>; A,B: in out Tc;                   function FU(Vin : Tin) return Tout;

-- convert to Ints: IF II IU

generic type Tin is digits <>; type Tc is digits <>; type Tout is range <>; A,B: in out Tc;   function rF(Vin : Tin) return Tout;-- Excpetion: No BLANK given but NaN encoutered
generic type Tin is range  <>; type Tc is digits <>; type Tout is range <>; A,B: in out Tc;   function II(Vin : Tin) return Tout;
generic type Tin is mod    <>; type Tc is digits <>; type Tout is range <>; A,B: in out Tc;   function IU(Vin : Tin) return Tout;

-- convert to UInt: UF UI UU

generic type Tin is digits <>; type Tc is digits <>; type Tout is mod   <>; A,B: in out Tc;   function UF(Vin : Tin) return Tout;-- Excpetion: No BLANK given but NaN encoutered
generic type Tin is range  <>; type Tc is digits <>; type Tout is mod   <>; A,B: in out Tc;   function UI(Vin : Tin) return Tout;
generic type Tin is mod    <>; type Tc is digits <>; type Tout is mod   <>; A,B: in out Tc;   function UU(Vin : Tin) return Tout;


-- FI FU variants with BLANK

generic type Tin is range  <>; type Tc is digits <>; type Tout is digits <>; BLANKin: in out Tin; A,B: in out Tc; ToutNaN : Tout;   function FI_BLANK(Vin : Tin) return Tout;
generic type Tin is mod    <>; type Tc is digits <>; type Tout is digits <>; BLANKin: in out Tin; A,B: in out Tc; ToutNaN : Tout;   function FU_BLANK(Vin : Tin) return Tout;

-- IF UF variants with BLANK

generic type Tin is digits <>; type Tc is digits <>; type Tout is range <>; BLANKout: in out Tout; A,B: in out Tc;  function rF_NaN(Vin : Tin) return Tout;-- Exception: valid input produces BLANKour
generic type Tin is digits <>; type Tc is digits <>; type Tout is mod   <>; BLANKout: in out Tout; A,B: in out Tc;  function UF_NaN(Vin : Tin) return Tout;-- Exception: valid input produces BLANKout


end Linear_Conv;
