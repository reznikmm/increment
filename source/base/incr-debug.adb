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

with League.Strings;

with XML.SAX.Attributes;

with Incr.Nodes.Tokens;
with Incr.Nodes;
with Incr.Version_Trees;

package body Incr.Debug is

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
       renames League.Strings.To_Universal_String;

   Nil_Attributes : XML.SAX.Attributes.SAX_Attributes;

   ----------
   -- Dump --
   ----------

   procedure Dump
     (Doc      : Incr.Documents.Document'Class;
      Provider : P.Parser_Data_Provider'Class;
      Output   : in out XML.SAX.Writers.SAX_Writer'Class)
   is
      procedure Dump (Node : Incr.Nodes.Node_Access);

      procedure Dump (Node : Incr.Nodes.Node_Access)
      is
         function Now return Incr.Version_Trees.Version is
           (Node.Document.History.Changing);

         function Prev return Incr.Version_Trees.Version is
           (Node.Document.History.Parent (Node.Document.History.Parent (Now)));

         function Common_Attributes return XML.SAX.Attributes.SAX_Attributes;

         function Common_Attributes return XML.SAX.Attributes.SAX_Attributes is
            Result : XML.SAX.Attributes.SAX_Attributes;
         begin
            Result.Set_Value
              (Qualified_Name => +"kind",
               Value          => +Provider.Kind_Image (Node.Kind));

            if Node.Nested_Changes (Prev, Now) then
               Result.Set_Value
                 (Qualified_Name => +"nc",
                  Value          => +"y");
            end if;

            if Node.Local_Changes (Prev, Now) then
               Result.Set_Value
                 (Qualified_Name => +"lc",
                  Value          => +"y");
            end if;

            if Node.Nested_Errors (Now) then
               Result.Set_Value
                 (Qualified_Name => +"ne",
                  Value          => +"y");
            end if;

            if Node.Local_Errors (Now) then
               Result.Set_Value
                 (Qualified_Name => +"le",
                  Value          => +"y");
            end if;
            return Result;
         end Common_Attributes;

         Ok : Boolean := True;
      begin
         if Node in null then
            Output.Start_Element
              (Namespace_URI  => +"",
               Local_Name     => +"null",
               Qualified_Name => +"null",
               Attributes     => Nil_Attributes,
               Success        => Ok);
            Output.End_Element
              (Namespace_URI  => +"",
               Local_Name     => +"null",
               Qualified_Name => +"null");

            return;
         elsif Node.Is_Token then
            declare
               Token : constant Incr.Nodes.Tokens.Token_Access :=
                 Incr.Nodes.Tokens.Token_Access (Node);
               Text : constant League.Strings.Universal_String :=
                 Token.Text (Now);
            begin
               Output.Start_Element
                 (Namespace_URI  => +"",
                  Local_Name     => +"token",
                  Qualified_Name => +"token",
                  Attributes     => Common_Attributes,
                  Success        => Ok);
               if not Text.Is_Empty then
                  Output.Characters
                    (Text     => Text,
                     Success  => Ok);
               end if;
               Output.End_Element
                 (Namespace_URI  => +"",
                  Local_Name     => +"token",
                  Qualified_Name => +"token",
                  Success        => Ok);
            end;

         else
            Output.Start_Element
              (Namespace_URI  => +"",
               Local_Name     => +"node",
               Qualified_Name => +"node",
               Attributes     => Common_Attributes,
               Success        => Ok);

            for J in 1 .. Node.Arity loop
               Dump (Node.Child (J, Now));
            end loop;

            Output.End_Element
              (Namespace_URI  => +"",
               Local_Name     => +"node",
               Qualified_Name => +"node",
               Success        => Ok);
         end if;
      end Dump;
   begin
      Output.Start_Document;
      Dump (Doc.Ultra_Root);
      Output.End_Document;
   end Dump;

end Incr.Debug;
