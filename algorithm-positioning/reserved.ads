
-- FIXME CardValue & InitVal & EmpptyVal, we want hidden, but needed in 3 packs: here and FA_Primary (also FA_Ext)
-- FIXME DATEOBS should be DATE-OBS as key-string
-- FIXME Prod_Keys BLOCKED & EXTEND only in Primary (BLOCKED only within 1st 36 cards) 
-- check in Extension that not present, otherwise raise exception(?)
-- FIXME DATExxxx : CardValue; -- FIXME what to do ? -> return List and store also KeyName

-- NOTE Array structures (only in IMAGE or IMAGE-like HDUs, see: [FITS Table C.2 comment(2)])



-- Reserved-Key placement:
--		scalar Keys 	IndexedKeys
-- Common 	yes		none
-- Prim		none		yes
-- Ext		yes		yes



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



--
-- Reserved scalar Keys for Primary or Extension or both ('Common')
--
type Reserved_Key is (
	DATE, ORIGIN, BLOCKED, EXTEND, 			-- production keys, all HDUs
	AUTHOR, REFERENC,				-- bibliographic keys, all HDUs
	DATEOBS, TELESCOP, INSTRUME, OBSERVER, OBJECT,	-- observation keys, all HDUs
	EQUINOX, EOPCH,					-- observation keys / WCS ?, all HDUs
	BSCALE,BZERO,BUNIT,BLANK,DATAMAX,DATAMIN,	-- data-array keys, IMAGES-like only
	EXTNAME, EXTVER, EXTLEVEL,			-- conforming extensions only
	THEAP						-- BINTABLE only
	);


subtype Common_Key    is Reserved_Key range DATE .. DATAMIN;
subtype Extension_Key is Reserved_Key range EXTNAME .. THEAP;

subtype Prod_Key    is Common_Key range DATE .. EXTEND;
subtype Biblio_Key  is Common_Key range AUTHOR .. REFERENC;
subtype Obs_Key     is Common_Key range DATEOBS .. OBJECT;
subtype DataArr_Key is Common_Key range BSCALE .. DATAMIN;


type Common_Key_Values    is array (Common_Key)    of CardValue;
type Extension_Key_Values is array (Extension_Key) of CardValue;

InitCommKeyVals : constant Common_Key_Values    := (others => InitVal);
InitExtKeyVals  : constant Extension_Key_Values := (others => InitVal);




--
-- Reserved Indexed Arrays for Prim, Ext
--
type Primary_Root is (
	PTYPE, PSCAL, PZERO				-- Random groups only
	);
type Extension_Root is (
	TSCAL, TZERO, TNULL, TTYPE, TUNIT, TDISP,	-- tables only
	TDIM						-- BINTABLE only
	);

subtype Tab_Root    is Extension_Root range TSCAL .. TDISP;
subtype BinTab_Root is Extension_Root range TSCAL .. TDIM;


type RANDG_Arr   is array (1 .. RANDG_Max)   of CardValue;
type TFIELDS_Arr is array (1 .. TFIELDS_Max) of CardValue;

type Primary_Root_Values   is array (Primary_Root)   of RANDG_Arr;
type Extension_Root_Values is array (Extension_Root) of TFIELDS_Arr;


InitRANDGArr   : constant RANDG_Arr   := (others => InitVal);
InitTFIELDSArr : constant TFIELDS_Arr := (others => InitVal);
InitPrimArrVals : constant Primary_Root_Values   := (others => InitRANDGArr);
InitExtArrVals  : constant Extension_Root_Values := (others => InitTFIELDSArr);


-- selector funcs, scalar-Keys

function Match_Any_Prod(Flag : Boolean; 
		        Card : in Card_Type;
			Res  : in out Common_Key_Values) return Boolean;

function Match_Any_Biblio(Flag : Boolean; 
			  Card : in Card_Type;
			  Res  : in out Common_Key_Values) return Boolean;

function Match_Any_Obs(Flag : Boolean;
                       Card : in Card_Type;
		       Res  : in out Common_Key_Values) return Boolean;

function Match_Any_DataArr(Flag : Boolean;
                       Card : in Card_Type;
		       Res  : in out Common_Key_Values) return Boolean;



-- selector funcs, indexed-Keys















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
		Res      : Common_Key_Values;
        end record;
Init : constant Reserved_Type := (InitComments,InitCommKeyVals);

procedure DBG_Print(Res : in Reserved_Type);

function Match_Any(Flag   : Boolean;
       		   Pos    : in Positive;	
		   Card   : in Card_Type;
                   Reserved : in out Reserved_Type) return Boolean;



-- Exceptions

		   Duplicate_Card : exception;

end Reserved;


