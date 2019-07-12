


package body FITSlib.Parser is

        procedure Read_Cards (Source : Source_Type)
        is
                Blk  : Card_Block;
                Cont : Boolean := True;
                Pos  : Natural := 0;
        begin

                Blk  := Next(Source);
                Pos  := Pos + 1;

                -- pass the first block to callbacks

                Cont := First_Block(Blk);
                if (not Cont) then
                       return;
                end if;

                Cont := Parse_Cards(Pos, Blk);
                if (not Cont) then
                       return;
                end if;

                -- pass blocks 2 ... to the callback

                loop
                        Blk  := Next(Source);
                        Pos  := Pos + 1;
                        Cont := Parse_Cards(Pos, Blk);
                        exit when not Cont;
                end loop;

-- FIXME do this? -> if yes, caller at
-- instantiation needs to implement Last_Block whether needed or not
-- 
--		Cont := Last_Block(Blk);
--                if (not Cont) then
--                       return;
--                end if;

        end Read_Cards;

end FITSlib.Parser;

