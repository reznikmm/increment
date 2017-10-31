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
-- Copyright © 2015-2017, Maxim Reznik <max@gela.work>                      --
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

with Ada.Command_Line;
with Ada.Wide_Wide_Text_IO;

with League.Application;
with League.Strings;

with XML.SAX.Attributes;
with XML.SAX.Input_Sources.Streams.Files;
with XML.SAX.Pretty_Writers;
with XML.SAX.Simple_Readers;
with XML.SAX.String_Output_Destinations;
with XML.Templates.Streams;

with Incr.Documents;
with Incr.Lexers.Batch_Lexers;
with Incr.Lexers.Incremental;
with Incr.Nodes.Tokens;
with Incr.Parsers.Incremental;
with Incr.Version_Trees;

with Tests.Commands;
with Tests.Lexers;
with Tests.Parser_Data.XML_Reader;

procedure Tests.Driver is

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
       renames League.Strings.To_Universal_String;

   procedure Dump
     (Node   : Incr.Nodes.Node_Access;
      Vector : in out XML.Templates.Streams.XML_Stream_Element_Vectors.Vector);

   function To_String
     (Vector : XML.Templates.Streams.XML_Stream_Element_Vectors.Vector)
       return League.Strings.Universal_String;

   type Provider_Access is access all Tests.Parser_Data.Provider;

   Nil_Location : constant XML.Templates.Streams.Event_Location :=
     (League.Strings.Empty_Universal_String, 0, 0);
   Nil_Attributes : XML.SAX.Attributes.SAX_Attributes;

   History : constant Incr.Version_Trees.Version_Tree_Access :=
     new Incr.Version_Trees.Version_Tree;

   Document : constant Incr.Documents.Document_Access :=
     new Incr.Documents.Document (History);

   Provider : constant Provider_Access :=
       new Tests.Parser_Data.Provider (Document);

   ----------
   -- Dump --
   ----------

   procedure Dump
     (Node   : Incr.Nodes.Node_Access;
      Vector : in out XML.Templates.Streams.XML_Stream_Element_Vectors.Vector)
   is
      use XML.Templates.Streams;

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

   begin
      if Node in null then
         Vector.Append
           ((Kind           => XML.Templates.Streams.Start_Element,
             Namespace_URI  => +"",
             Local_Name     => +"null",
             Qualified_Name => +"null",
             Attributes     => Nil_Attributes,
             Location       => Nil_Location));
         Vector.Append
           ((Kind           => XML.Templates.Streams.End_Element,
             Namespace_URI  => +"",
             Local_Name     => +"null",
             Qualified_Name => +"null",
             Location       => Nil_Location));

         return;
      elsif Node.Is_Token then
         declare
            Token : constant Incr.Nodes.Tokens.Token_Access :=
              Incr.Nodes.Tokens.Token_Access (Node);
            Text : constant League.Strings.Universal_String :=
              Token.Text (Now);
         begin
            Vector.Append
              ((Kind           => XML.Templates.Streams.Start_Element,
                Namespace_URI  => +"",
                Local_Name     => +"token",
                Qualified_Name => +"token",
                Attributes     => Common_Attributes,
                Location       => Nil_Location));
            if not Text.Is_Empty then
               Vector.Append
                 ((Kind     => XML.Templates.Streams.Text,
                   Text     => Text,
                   Location => Nil_Location));
            end if;
            Vector.Append
              ((Kind           => XML.Templates.Streams.End_Element,
                Namespace_URI  => +"",
                Local_Name     => +"token",
                Qualified_Name => +"token",
                Location       => Nil_Location));
         end;

      else
         Vector.Append
           ((Kind           => XML.Templates.Streams.Start_Element,
             Namespace_URI  => +"",
             Local_Name     => +"node",
             Qualified_Name => +"node",
             Attributes     => Common_Attributes,
             Location       => Nil_Location));

         for J in 1 .. Node.Arity loop
            Dump (Node.Child (J, Now), Vector);
         end loop;

         Vector.Append
           ((Kind           => XML.Templates.Streams.End_Element,
             Namespace_URI  => +"",
             Local_Name     => +"node",
             Qualified_Name => +"node",
             Location       => Nil_Location));
      end if;
   end Dump;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Vector : XML.Templates.Streams.XML_Stream_Element_Vectors.Vector)
      return League.Strings.Universal_String
   is
      use XML.Templates.Streams;
      Output : aliased XML.SAX.String_Output_Destinations.
        String_Output_Destination;
      Writer : XML.SAX.Pretty_Writers.XML_Pretty_Writer;
   begin
      Writer.Set_Output_Destination (Output'Unchecked_Access);
      Writer.Set_Offset (2);
      Writer.Start_Document;
      for V of Vector loop
         case V.Kind is
            when Start_Element =>
               Writer.Start_Element
                 (Namespace_URI  => V.Namespace_URI,
                  Qualified_Name => V.Qualified_Name,
                  Local_Name     => V.Local_Name,
                  Attributes     => V.Attributes);
            when End_Element =>
               Writer.End_Element
                 (Namespace_URI  => V.Namespace_URI,
                  Qualified_Name => V.Qualified_Name,
                  Local_Name     => V.Local_Name);
            when Text =>
               Writer.Characters (V.Text);
            when others =>
               null;
         end case;
      end loop;

      Writer.End_Document;
      return Output.Get_Text;
   end To_String;

   Batch_Lexer : constant Incr.Lexers.Batch_Lexers.Batch_Lexer_Access :=
     new Tests.Lexers.Test_Lexers.Batch_Lexer;

   Incr_Lexer : constant Incr.Lexers.Incremental.Incremental_Lexer_Access :=
     new Incr.Lexers.Incremental.Incremental_Lexer;

   Incr_Parser : constant Incr.Parsers.Incremental.Incremental_Parser_Access :=
     new Incr.Parsers.Incremental.Incremental_Parser;

   Ref  : Incr.Version_Trees.Version := History.Parent (History.Changing);

   Input   : aliased XML.SAX.Input_Sources.Streams.Files.File_Input_Source;
   Reader  : XML.SAX.Simple_Readers.Simple_Reader;
   Handler : aliased Tests.Parser_Data.XML_Reader.Reader (Provider);
begin
   Input.Open_By_File_Name (League.Application.Arguments.Element (1));
   Reader.Set_Content_Handler (Handler'Unchecked_Access);
   Reader.Set_Input_Source (Input'Unchecked_Access);
   Reader.Parse;

   Incr.Documents.Constructors.Initialize (Document.all);
   Incr_Lexer.Set_Batch_Lexer (Batch_Lexer);

   for Command of Handler.Get_Commands loop
      case Command.Kind is
         when Tests.Commands.Commit =>
            Document.Commit;

         when Tests.Commands.Set_EOS_Text =>
            Document.End_Of_Stream.Set_Text (Command.Text);

         when Tests.Commands.Set_Token_Text =>
            declare
               Token : Incr.Nodes.Tokens.Token_Access :=
                 Document.Start_Of_Stream;
            begin
               for J in 2 .. Command.Token loop
                  Token := Token.Next_Token (History.Changing);
               end loop;

               Token.Set_Text (Command.Text);
            end;
         when Tests.Commands.Dump_Tree =>
            declare
               use type League.Strings.Universal_String;

               Vector : XML.Templates.Streams.XML_Stream_Element_Vectors.
                 Vector;
               Text   : League.Strings.Universal_String;
               Expect : League.Strings.Universal_String;
            begin
               Dump (Document.Ultra_Root, Vector);
               Text := To_String (Vector);
               Expect := To_String (Command.Dump);
               Ada.Wide_Wide_Text_IO.Put_Line
                 (Text.To_Wide_Wide_String);

               if Text /= Expect then
                  Ada.Wide_Wide_Text_IO.Put_Line ("DOESN'T MATCH!!!");
                  Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
                  return;
               end if;
            end;

         when Tests.Commands.Run =>
            Incr_Parser.Run
              (Lexer     => Incr_Lexer,
               Provider  => Provider.all'Unchecked_Access,
               Factory   => Provider.all'Unchecked_Access,
               Document  => Document,
               Reference => Ref);
            Ref := History.Changing;
      end case;
   end loop;

end Tests.Driver;
