
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

-- FIXME DATEOBS should be DATE-OBS as key-string
type Reserved_Key is (
	DATE, ORIGIN, BLOCKED, EXTEND, 			-- production keys, all HDUs
	AUTHOR, REFERENC,				-- bibliographic keys, all HDUs
	DATEOBS, TELESCOP, INSTRUME, OBSERVER, OBJECT,	-- observation keys, all HDUs
	EQUINOX, EOPCH,					-- observation keys / WCS ?, all HDUs
	BSCALE,BZERO,BUNIT,BLANK,DATAMAX,DATAMIN	-- data-array keys, IMAGES-like only
	);
type Ext_Reserved_Key is (
	EXTNAME, EXTVER, EXTLEVEL,			-- conforming extensions only
	THEAP						-- BINTABLE only
	);

subtype Prod_Key is Reserved_Key range DATE .. EXTEND;
-- BLOCKED & EXTEND only in Primary (BLOCKED only within 1st 36 cards) 
-- check in Extension that not present, otherwise raise exception(?)
subtype Biblio_Key is Reserved_Key range AUTHOR .. REFERENC;
--DATExxxx : CardValue; -- FIXME what to do ? -> return List and store also KeyName
subtype Obs_Key is Reserved_Key range DATEOBS .. OBJECT;
-- Array structures (only in IMAGE or IMAGE-like HDUs, see: [FITS Table C.2 comment(2)])
subtype DataArr_Key is Reserved_Key range BSCALE .. DATAMIN;

type Reserved_Key_Cards is array (Reserved_Key) of CardValue;
InitResKeyCards : constant Reserved_Key_Cards := (others => InitVal);

type Ext_Reserved_Cards is array (Ext_Reserved_Key) of CardValue;
InitResExtCards : constant Ext_Reserved_Cards := (others => InitVal);




-- Indexed Arrays

type Prim_Reserved_Root is (
	PTYPE, PSCAL, PZERO				-- Random groups only
	);
type Reserved_RG_Cards is array (Prim_Reserved_Root) of CardValue;
InitResRGCards : constant Reserved_RG_Cards := (others => InitVal);


type Ext_Reserved_Root is (
	TSCAL, TZERO, TNULL, TTYPE, TUNIT, TDISP,	-- tables only
	TDIM						-- BINTABLE only
	);
subtype Tab_Root    is Ext_Reserved_Root range TSCAL .. TDISP;
subtype BinTab_Root is Ext_Reserved_Root range TSCAL .. TDIM;

type Reserved_Ext_Root_Arr is array (Ext_Reserved_Root) of CardValue;
InitResExtRootArrs : constant Reserved_Ext_Root_Arr := (others => InitVal);




-- selector funcs

function Match_Any_Prod(Flag : Boolean; 
		        Card : in Card_Type;
			Res  : in out Reserved_Key_Cards) return Boolean;

function Match_Any_Biblio(Flag : Boolean; 
			  Card : in Card_Type;
			  Res  : in out Reserved_Key_Cards) return Boolean;

function Match_Any_Obs(Flag : Boolean;
                       Card : in Card_Type;
		       Res  : in out Reserved_Key_Cards) return Boolean;

function Match_Any_DataArr(Flag : Boolean;
                       Card : in Card_Type;
		       Res  : in out Reserved_Key_Cards) return Boolean;



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



--
-- all generic Reserved keys [FITS Table C.2]
-- 

type Reserved_Type is
        record
		Comments : Comment_Type;
		Res      : Reserved_Key_Cards;
        end record;
Init : constant Reserved_Type := (InitComments,InitResKeyCards);

procedure DBG_Print(Res : in Reserved_Type);

function Match_Any(Flag   : Boolean;
       		   Pos    : in Positive;	
		   Card   : in Card_Type;
                   Reserved : in out Reserved_Type) return Boolean;



-- Exceptions

		   Duplicate_Card : exception;

end Reserved;


