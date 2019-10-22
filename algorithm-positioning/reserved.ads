
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
type Prod_Key is (DATE, ORIGIN, BLOCKED, EXTEND);
-- BLOCKED & EXTEND only in Primary (BLOCKED only within 1st 36 cards) 
-- check in Extension that not present, otherwise raise exception(?)
type Prod_Type is array (Prod_Key) of CardValue;
InitProd : constant Prod_Type := (others => InitVal);

procedure DBG_Print(Prod : in Prod_Type);

function Match_Any_Prod(Flag : Boolean; 
		        Card : in Card_Type;
                        Prod : in out Prod_Type) return Boolean;



-- Bibliographic keys
type Biblio_Key is (AUTHOR, REFERENC);
type Biblio_Type is array (Biblio_Key) of CardValue;
InitBiblio : constant Biblio_Type := (others => InitVal);

procedure DBG_Print(Biblio : in Biblio_Type);

function Match_Any_Biblio(Flag   : Boolean; 
			  Card   : in Card_Type;
                          Biblio : in out Biblio_Type) return Boolean;




-- Commentary keys
subtype Comment_Str is String(1..72);
NullCommStr : constant Comment_Str := (others => ' ');

type Comment_Rec is
	record
		Pos : Natural;
		Str : Comment_Str;
	end record;
InitCommentRec : constant Comment_Rec := (0,NullCommStr);

type Comm_Arr is array (1 .. Comments_Max) of Comment_Rec;
type Comment_Arr is 
	record
		Last  : Natural;
		Value : Comm_Arr; 
	end record;

InitCommentArr : constant Comment_Arr := (Last => 0, Value => (others => InitCommentRec) );

type Comment_Type is
        record
                COMMENT   : Comment_Arr;
                HISTORY   : Comment_Arr;
		empty_key : Comment_Arr;
        end record;

InitComments : constant Comment_Type := (others => InitCommentArr);

procedure DBG_Print(Comments : in Comment_Type);

function Match_Any_Comment(Flag    : Boolean;
       			   Pos     : in Positive;	
			   Card    : in Card_Type;
                           Comment : in out Comment_Type) return Boolean;



-- Observation related keys

--DATExxxx : CardValue; -- FIXME what to do ? -> return List and store also KeyName
type Obs_Key is (DATEOBS, TELESCOP, INSTRUME, OBSERVER, OBJECT);
type Obs_Type is array (Obs_Key) of CardValue;
InitObs : constant Obs_Type := (others => InitVal);

procedure DBG_Print(Obs : in Obs_Type);

function Match_Any_Obs(Flag : Boolean;
                       Card : in Card_Type;
                       Obs  : in out Obs_Type) return Boolean;



-- all generic Reserved keys [FITS Table C.2]

type Reserved_Type is
        record
                Prod   : Prod_Type;
		Biblio : Biblio_Type;
		Comments : Comment_Type;
                Obs    : Obs_Type;
        end record;

Init : constant Reserved_Type := (InitProd,InitBiblio,InitComments,InitObs);

procedure DBG_Print(Res : in Reserved_Type);

function Match_Any(Flag   : Boolean;
       		   Pos    : in Positive;	
		   Card   : in Card_Type;
                   Reserved : in out Reserved_Type) return Boolean;




----------------------------------------------------------------------------------------

-- Array structures (only in IMAGE or IMAGE-like HDUs, see: [FITS Table C.2 comment(2)])
type DataArr_Key is (BSCALE,BZERO,BUNIT,BLANK,DATAMAX,DATAMIN);
type DataArr_Type is array (DataArr_Key) of CardValue;
InitDataArr : constant DataArr_Type := (others => InitVal);

procedure DBG_Print(DataArr : DataArr_Type);

function Match_Any_DataArr(Flag : Boolean;
                       Card : in Card_Type;
                       DataArr  : in out DataArr_Type) return Boolean;








-- Exceptions

		   Duplicate_Card : exception;



end Reserved;

