
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





end Reserved;

