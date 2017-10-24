------------------------------------------------------------------------------
--                                                                          --
--                               Gela Project                               --
--                                                                          --
--                Programming Language Construction Framework               --
--                                                                          --
--                      Incremental Analysis Component                      --
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
-- Copyright © 2017, Maxim Reznik <max@gela.work>                           --
-- All rights reserved.                                                     --
--                                                                          --
-- Redistribution and use in source and binary forms, with or without       --
-- modification, are permitted provided that the following conditions       --
-- are met:                                                                 --
--                                                                          --
--  * Redistributions of source code must retain the above copyright        --
--    notice, this list of conditions and the following disclaimer.         --
--                                                                          --
--  * Redistributions in binary form must reproduce the above copyright     --
--    notice, this list of conditions and the following disclaimer in the   --
--    documentation and/or other materials provided with the distribution.  --
--                                                                          --
--  * Neither the name of the Maxim Reznik, IE nor the names of its         --
--    contributors may be used to endorse or promote products derived from  --
--    this software without specific prior written permission.              --
--                                                                          --
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS      --
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT        --
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR    --
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT     --
-- HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,   --
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED --
-- TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR   --
-- PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF   --
-- LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING     --
-- NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS       --
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.             --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Command_Line;

with Gela.Grammars.Constructors;
with Gela.Grammars.LR.LALR;
with Gela.Grammars.LR_Tables;
with Gela.Grammars.Reader;
with Gela.Grammars_Convertors;
with Gela.Grammars_Debug;

with Gen.Write_Parser_Data;

procedure Gen.Driver is
   File : constant String := Ada.Command_Line.Argument (1);
   G : constant Gela.Grammars.Grammar := Gela.Grammars.Reader.Read (File);
   Plain : constant Gela.Grammars.Grammar :=
     Gela.Grammars_Convertors.Convert (G, False);
   AG : constant Gela.Grammars.Grammar :=
     Gela.Grammars.Constructors.To_Augmented (Plain);
   Table : constant Gela.Grammars.LR_Tables.Table_Access :=
     Gela.Grammars.LR.LALR.Build (AG, False);
begin
   Gen.Write_Parser_Data (Plain, Table.all);

   if Ada.Command_Line.Argument_Count > 1 then
      Gela.Grammars_Debug.Print_Conflicts (AG, Table.all);
   end if;
end Gen.Driver;
