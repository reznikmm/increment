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

with Ada.Containers;
with Ada.Strings.Wide_Wide_Hash;
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

   use type Ada.Containers.Hash_Type;

   procedure Dump
     (Hash   : in out Ada.Containers.Hash_Type;
      Node   : Incr.Nodes.Node_Access;
      Indent : Wide_Wide_String);

   ----------
   -- Dump --
   ----------

   procedure Dump
     (Hash   : in out Ada.Containers.Hash_Type;
      Node   : Incr.Nodes.Node_Access;
      Indent : Wide_Wide_String)
   is
      procedure Print;
      procedure Put (Text : Wide_Wide_String);
      procedure Put_Line (Text : Wide_Wide_String);

      Now : Incr.Version_Trees.Version;

      procedure Print is
      begin
         Put (" node_kind='");
         Put
           (Incr.Nodes.Node_Kind'Wide_Wide_Image (Node.Kind));
         Put ("' nested_changes='");
         Put
           (Boolean'Wide_Wide_Image (Node.Nested_Changes (Now, Now)));
      end Print;

      ---------
      -- Put --
      ---------

      procedure Put (Text : Wide_Wide_String) is
      begin
         Ada.Wide_Wide_Text_IO.Put (Text);
         Hash := Hash + Ada.Strings.Wide_Wide_Hash (Text);
      end Put;

      --------------
      -- Put_Line --
      --------------

      procedure Put_Line (Text : Wide_Wide_String) is
      begin
         Put (Text);
         Ada.Wide_Wide_Text_IO.New_Line;
      end Put_Line;

   begin
      Put (Indent);

      if Node in null then
         Put_Line ("<null/>");
         return;
      end if;

      Now := Node.Document.History.Changing;

      if Node.Is_Token then
         declare
            Token : constant Incr.Nodes.Tokens.Token_Access :=
              Incr.Nodes.Tokens.Token_Access (Node);
         begin
            Put ("<token ");
            Print;
            Put ("'>");
            Put (Token.Text (Now).To_Wide_Wide_String);
            Put_Line ("</token>");
         end;
      else
         Put ("<node ");
         Print;
         Put_Line ("'>");

         for J in 1 .. Node.Arity loop
            Dump (Hash, Node.Child (J, Now), Indent & "  ");
         end loop;

         Put (Indent);
         Put_Line ("</node>");
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

   Hash : Ada.Containers.Hash_Type := 0;
   Ref  : Incr.Version_Trees.Version;
begin
   Incr.Documents.Constructors.Initialize (Document.all);
   Incr_Lexer.Set_Batch_Lexer (Batch_Lexer);

   Document.End_Of_Stream.Set_Text
     (League.Strings.To_Universal_String ("a1"));

   Document.Commit;

   Dump (Hash, Document.Ultra_Root, "");
   pragma Assert (Hash = 2962486295);

   Incr_Parser.Run
     (Lexer     => Incr_Lexer,
      Provider  => Provider,
      Factory   => Node_Factory,
      Document  => Document,
      Reference => History.Prehistoric);

   Dump (Hash, Document.Ultra_Root, "");
   pragma Assert (Hash = 2317793671);

   Ref := History.Changing;
   Document.Commit;

   Document.End_Of_Stream.Set_Text
     (League.Strings.To_Universal_String ("2"));

   Document.Commit;

   Incr_Parser.Run
     (Lexer     => Incr_Lexer,
      Provider  => Provider,
      Factory   => Node_Factory,
      Document  => Document,
      Reference => Ref);

   Dump (Hash, Document.Ultra_Root, "");
   pragma Assert (Hash = 2140449180);

   Ref := History.Changing;
   Document.Commit;

   Document.Start_Of_Stream.Next_Token (Ref).Set_Text
     (League.Strings.Empty_Universal_String);

   Document.End_Of_Stream.Previous_Token (Ref).Set_Text
     (League.Strings.Empty_Universal_String);

   Document.Commit;

   Incr_Parser.Run
     (Lexer     => Incr_Lexer,
      Provider  => Provider,
      Factory   => Node_Factory,
      Document  => Document,
      Reference => Ref);

   Dump (Hash, Document.Ultra_Root, "");
   pragma Assert (Hash = 801605027);

end Tests.Driver;
