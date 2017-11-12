--  Copyright (c) 2015-2017 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Command_Line;
with Ada.Wide_Wide_Text_IO;

with League.Application;
with League.Strings;

with XML.SAX.Input_Sources.Streams.Files;
with XML.SAX.Pretty_Writers;
with XML.SAX.Simple_Readers;
with XML.SAX.String_Output_Destinations;
with XML.Templates.Streams;
with XML.SAX.Writers;

with Incr.Debug;
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

   procedure Dump
     (Document : Incr.Documents.Document'Class;
      Result   : out League.Strings.Universal_String);

   function To_String
     (Vector : XML.Templates.Streams.XML_Stream_Element_Vectors.Vector)
       return League.Strings.Universal_String;

   type Provider_Access is access all Tests.Parser_Data.Provider;

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
     (Document : Incr.Documents.Document'Class;
      Result   : out League.Strings.Universal_String)
   is
      Output : aliased XML.SAX.String_Output_Destinations.
        String_Output_Destination;
      Writer : XML.SAX.Pretty_Writers.XML_Pretty_Writer;
   begin
      Writer.Set_Output_Destination (Output'Unchecked_Access);
      Writer.Set_Offset (2);
      Incr.Debug.Dump (Document, Provider.all, Writer);
      Result := Output.Get_Text;
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

   Root    : Incr.Nodes.Node_Access;
   Input   : aliased XML.SAX.Input_Sources.Streams.Files.File_Input_Source;
   Reader  : XML.SAX.Simple_Readers.Simple_Reader;
   Handler : aliased Tests.Parser_Data.XML_Reader.Reader (Provider);
begin
   Input.Open_By_File_Name (League.Application.Arguments.Element (1));
   Reader.Set_Content_Handler (Handler'Unchecked_Access);
   Reader.Set_Input_Source (Input'Unchecked_Access);
   Reader.Parse;

   if Provider.Part_Counts (2) = 0 then
      declare
         Kind : Incr.Nodes.Node_Kind;
      begin
         Provider.Create_Node
           (Prod     => 2,
            Children => (1 .. 0 => <>),
            Node     => Root,
            Kind     => Kind);
      end;
   end if;

   Incr.Documents.Constructors.Initialize (Document.all, Root);
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

               Text   : League.Strings.Universal_String;
               Expect : League.Strings.Universal_String;
            begin
               Dump (Document.all, Text);
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
