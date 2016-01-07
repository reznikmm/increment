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
-- Copyright © 2015, Maxim Reznik <max@gela.work>                           --
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
--  $Revision$ $Date$
------------------------------------------------------------------------------

with Ada.Wide_Wide_Text_IO;

with League.Strings;

with Incr.Documents;
with Incr.Lexers.Batch_Lexers;
with Incr.Lexers.Incremental;
with Incr.Nodes.Tokens;
with Incr.Parsers.Incremental;
with Incr.Version_Trees;

with Tests.Lexers;
with Tests.Parser_Data;

procedure Tests.Driver is

   procedure Dump
     (Node : Incr.Nodes.Node_Access;
      Indent : Wide_Wide_String);

   ----------
   -- Dump --
   ----------

   procedure Dump
     (Node : Incr.Nodes.Node_Access;
      Indent : Wide_Wide_String)
   is
      procedure Print;

      Now : Incr.Version_Trees.Version;

      procedure Print is
      begin
         Ada.Wide_Wide_Text_IO.Put (" node_kind='");
         Ada.Wide_Wide_Text_IO.Put
           (Incr.Nodes.Node_Kind'Wide_Wide_Image (Node.Kind));
         Ada.Wide_Wide_Text_IO.Put ("' nested_changes='");
         Ada.Wide_Wide_Text_IO.Put
           (Boolean'Wide_Wide_Image (Node.Nested_Changes (Now, Now)));
      end Print;

   begin
      Ada.Wide_Wide_Text_IO.Put (Indent);

      if Node in null then
         Ada.Wide_Wide_Text_IO.Put_Line ("<null/>");
         return;
      end if;

      Now := Node.Document.History.Changing;

      if Node.Is_Token then
         declare
            Token : constant Incr.Nodes.Tokens.Token_Access :=
              Incr.Nodes.Tokens.Token_Access (Node);
         begin
            Ada.Wide_Wide_Text_IO.Put ("<token ");
            Print;
            Ada.Wide_Wide_Text_IO.Put ("'>");
            Ada.Wide_Wide_Text_IO.Put (Token.Text (Now).To_Wide_Wide_String);
            Ada.Wide_Wide_Text_IO.Put_Line ("</token>");
         end;
      else
         Ada.Wide_Wide_Text_IO.Put ("<node ");
         Print;
         Ada.Wide_Wide_Text_IO.Put_Line ("'>");

         for J in 1 .. Node.Arity loop
            Dump (Node.Child (J, Now), Indent & "  ");
         end loop;

         Ada.Wide_Wide_Text_IO.Put (Indent);
         Ada.Wide_Wide_Text_IO.Put_Line ("</node>");
      end if;
   end Dump;

   History : constant Incr.Version_Trees.Version_Tree_Access :=
     new Incr.Version_Trees.Version_Tree;

   Document : constant Incr.Documents.Document_Access :=
     new Incr.Documents.Document (History);

   Batch_Lexer : constant Incr.Lexers.Batch_Lexers.Batch_Lexer_Access :=
     new Tests.Lexers.Test_Lexers.Batch_Lexer;

   Incr_Lexer : constant Incr.Lexers.Incremental.Incremental_Lexer_Access :=
     new Incr.Lexers.Incremental.Incremental_Lexer;

   Incr_Parser : constant Incr.Parsers.Incremental.Incremental_Parser_Access :=
     new Incr.Parsers.Incremental.Incremental_Parser;

   Provider : constant Incr.Parsers.Incremental.Parser_Data_Providers
     .Parser_Data_Provider_Access :=
       new Tests.Parser_Data.Provider;

   Node_Factory : constant Incr.Parsers.Incremental.Parser_Data_Providers
     .Node_Factory_Access :=
       new Tests.Parser_Data.Node_Factory (Document);
begin
   Incr.Documents.Constructors.Initialize (Document.all);
   Incr_Lexer.Set_Batch_Lexer (Batch_Lexer);

   Document.End_Of_Stream.Set_Text
     (League.Strings.To_Universal_String ("a1"));

   Document.Commit;

   Dump (Document.Ultra_Root, "");

   Incr_Parser.Run
     (Lexer     => Incr_Lexer,
      Provider  => Provider,
      Factory   => Node_Factory,
      Document  => Document,
      Reference => History.Prehistoric);

   Dump (Document.Ultra_Root, "");
end Tests.Driver;
