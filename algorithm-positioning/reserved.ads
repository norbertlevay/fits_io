
with Ada.Exceptions; use Ada.Exceptions;
with FITS; use FITS; -- Card_Type needed


package Reserved is

EmptyVal : constant String(1..20) := (others => ' ');


type CardValue is
        record
                Value : String(1..20);
                Read  : Boolean;
        end record;

InitVal  : constant CardValue := (EmptyVal,False);
-- FIXME these above, we want hidden, but needed in 3 packs: here and FA_Primary (also FA_Ext)


-- Production keys
type Prod_Type is
        record
                DATE    : CardValue;
                ORIGIN  : CardValue;
                BLOCKED : CardValue;
        end record;

InitProd : constant Prod_Type := (InitVal,InitVal,InitVal);

procedure DBG_Print(Prod : in Prod_Type);

function Match_Any_Prod(Flag : Boolean; 
		        Card : in Card_Type;
                        Prod : in out Prod_Type) return Boolean;



-- Biblioghapic keys
type Biblio_Type is
        record
                AUTHOR   : CardValue;
                REFERENC : CardValue;
        end record;

InitBiblio : constant Biblio_Type := (InitVal,InitVal);

procedure DBG_Print(Biblio : in Biblio_Type);

function Match_Any_Biblio(Flag   : Boolean; 
			  Card   : in Card_Type;
                          Biblio : in out Biblio_Type) return Boolean;



-- Observation related keys
type Obs_Type is
        record
                DATEOBS : CardValue;
                --DATExxxx : CardValue; -- FIXME what to do ? -> return List and store also KeyName
                TELESCOP : CardValue;
                INSTRUME : CardValue;
                OBSERVER : CardValue;
                OBJECT   : CardValue;
        end record;

InitObs : constant Obs_Type := (InitVal,   InitVal,InitVal,InitVal,InitVal);

procedure DBG_Print(Obs : in Obs_Type);

function Match_Any_Obs(Flag : Boolean; 
			Card : in Card_Type;
                       Obs  : in out Obs_Type) return Boolean;


-- all generic Reserved keys [FITS Table C.2]

type Reserved_Type is
        record
                Prod   : Prod_Type;
		Biblio : Biblio_Type;
                Obs    : Obs_Type;
        end record;

Init : constant Reserved_Type := (InitProd,InitBiblio,InitObs);

procedure DBG_Print(Res : in Reserved_Type);

function Match_Any(Flag   : Boolean; 
		   Card   : in Card_Type;
                   Reserved : in out Reserved_Type) return Boolean;




----------------------------------------------------------------------------------------

-- Array structures (only in IMAGE or IMAGE-like HDUs, see: [FITS Table C.2 comment(2)])

type Arr_Type is
	record
		BSCALE : CardValue;
		BZERO  : CardValue;
		BUNIT  : CardValue;
		BLANK  : CardValue;
		DATAMAX : CardValue;
		DATAMIN : CardValue;
	end record;

InitArr : constant Arr_Type := (others => InitVal);

procedure DBG_Print(Arr : Arr_Type);

function Match_Any_Arr(Flag : Boolean;
                       Card : in Card_Type;
                       Arr  : in out Arr_Type) return Boolean;








-- Exceptions

		   Duplicate_Card : exception;



end Reserved;

