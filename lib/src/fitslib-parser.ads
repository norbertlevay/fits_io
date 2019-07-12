--
-- This is "strawman's parser" a simple card reader which
-- implements no grammar, but performs only simple card-key matching 
-- and card-vlue (comment) selection.
--

with FITSlib.Header; use FITSlib.Header;

package FITSlib.Parser is

	 generic
                type Source_Type is limited private;
                with function Next(Source : Source_Type) return Card_Block;
                with function First_Block(Blk : Card_Arr) return Boolean;
                with function Parse_Cards(Pos : Positive; Blk : Card_Arr) return Boolean;
                --with function Last_Block(Blk : Card_Arr) return Boolean;
        procedure Read_Cards (Source : Source_Type);

end FITSlib.Parser;
