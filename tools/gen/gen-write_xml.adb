--  Copyright (c) 2015-2017 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Wide_Wide_Text_IO;

with Anagram.Grammars.LR;
with Anagram.Grammars.LR_Tables;

with League.Strings;

with XML.SAX.Attributes;
with XML.SAX.Output_Destinations.Strings;
with XML.SAX.Pretty_Writers;

procedure Gen.Write_XML
  (Name  : String;
   Plain : Anagram.Grammars.Grammar;
   Table : Anagram.Grammars.LR_Tables.Table)
is

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
       renames League.Strings.To_Universal_String;

   procedure Write_Names
     (Writer : in out XML.SAX.Pretty_Writers.XML_Pretty_Writer);
   procedure Write_Counts
     (Writer : in out XML.SAX.Pretty_Writers.XML_Pretty_Writer);
   procedure Write_NT
     (Writer : in out XML.SAX.Pretty_Writers.XML_Pretty_Writer);
   procedure Write_States
     (Writer : in out XML.SAX.Pretty_Writers.XML_Pretty_Writer);
   procedure Write_Actions
     (Writer : in out XML.SAX.Pretty_Writers.XML_Pretty_Writer);

   -------------------
   -- Write_Actions --
   -------------------

   procedure Write_Actions
     (Writer : in out XML.SAX.Pretty_Writers.XML_Pretty_Writer)
   is
      use Anagram.Grammars.LR_Tables;
   begin
      Writer.Start_Element (+"actions");
      for State in 1 .. Last_State (Table) loop
         for Term in 0 .. Plain.Last_Terminal loop
            declare
               S    : constant Anagram.Grammars.LR.State_Count :=
                 Shift (Table, State, Term);
               R    : constant Reduce_Iterator := Reduce (Table, State, Term);
            begin
               if Term in 0 and Finish (Table, State) then
                  Writer.Characters (+" F");
               elsif S not in 0 then
                  declare
                     Text : constant Wide_Wide_String :=
                       Anagram.Grammars.LR.State_Count'Wide_Wide_Image (S);
                  begin
                     Writer.Characters (+" S");
                     Writer.Characters (+Text (2 .. Text'Last));
                  end;
               elsif not Is_Empty (R) then
                  Writer.Characters
                    (Anagram.Grammars.Production_Index'Wide_Wide_Image
                       (Production (R)));
               else
                  Writer.Characters (+" E");
               end if;
            end;
         end loop;

         for NT in 1 .. Plain.Last_Non_Terminal loop
            declare
               S    : constant Anagram.Grammars.LR.State_Count :=
                 Shift (Table, State, NT);
               R    : constant Reduce_Iterator := Reduce (Table, State, NT);
            begin
               if S not in 0 then
                  declare
                     Text : constant Wide_Wide_String :=
                       Anagram.Grammars.LR.State_Count'Wide_Wide_Image (S);
                  begin
                     Writer.Characters (+" S");
                     Writer.Characters (+Text (2 .. Text'Last));
                  end;
               elsif not Is_Empty (R) then
                  Writer.Characters
                    (Anagram.Grammars.Production_Index'Wide_Wide_Image
                       (Production (R)));
               else
                  Writer.Characters (+" E");
               end if;
            end;
         end loop;
      end loop;
      Writer.End_Element (+"actions");
   end Write_Actions;

   ------------------
   -- Write_Counts --
   ------------------

   procedure Write_Counts
     (Writer : in out XML.SAX.Pretty_Writers.XML_Pretty_Writer)
   is
      use type Anagram.Grammars.Part_Count;
   begin
      Writer.Start_Element (+"counts");
      for Prod of Plain.Production loop
         Writer.Characters
           (+Anagram.Grammars.Part_Count'Wide_Wide_Image
              (Prod.Last - Prod.First + 1));
      end loop;
      Writer.End_Element (+"counts");
   end Write_Counts;

   -----------------
   -- Write_Names --
   -----------------

   procedure Write_Names
     (Writer : in out XML.SAX.Pretty_Writers.XML_Pretty_Writer)
   is
      Name  : constant League.Strings.Universal_String := +"name";
      Names : XML.SAX.Attributes.SAX_Attributes;
   begin
      Names.Set_Value
        (+"term",
         +Anagram.Grammars.Terminal_Count'Wide_Wide_Image
           (Plain.Last_Terminal));
      Names.Set_Value
        (+"nt",
         +Anagram.Grammars.Non_Terminal_Count'Wide_Wide_Image
           (Plain.Last_Non_Terminal));
      Writer.Start_Element (+"names", Names);
      Names.Clear;

      for Term in 1 .. Plain.Last_Terminal loop
         Names.Set_Value (Name, Plain.Terminal (Term).Image);
         Writer.Start_Element (Name, Names);
         Writer.End_Element (Name);
      end loop;

      for NT in 1 .. Plain.Last_Non_Terminal loop
         Names.Set_Value (Name, Plain.Non_Terminal (NT).Name);
         Writer.Start_Element (Name, Names);
         Writer.End_Element (Name);
      end loop;

      Writer.End_Element (+"names");
   end Write_Names;

   procedure Write_NT
     (Writer : in out XML.SAX.Pretty_Writers.XML_Pretty_Writer) is
   begin
      Writer.Start_Element (+"nt");
      for NT of Plain.Non_Terminal loop
         Writer.Characters
           (+Anagram.Grammars.Production_Index'Wide_Wide_Image (NT.First));
         Writer.Characters
           (+Integer'Wide_Wide_Image (-Positive (NT.Last)));
      end loop;
      Writer.End_Element (+"nt");
   end Write_NT;

   procedure Write_States
     (Writer : in out XML.SAX.Pretty_Writers.XML_Pretty_Writer)
   is
      use Anagram.Grammars.LR_Tables;
      List : XML.SAX.Attributes.SAX_Attributes;
   begin
      List.Set_Value
        (+"count",
         +Anagram.Grammars.LR.State_Count'Wide_Wide_Image
           (Last_State (Table)));
      Writer.Start_Element (+"states", List);
      for State in 1 .. Last_State (Table) loop
         for NT in 1 .. Plain.Last_Non_Terminal loop
            declare
               S : constant Anagram.Grammars.LR.State_Count :=
                 Shift (Table, State, NT);
            begin
               Writer.Characters
                 (+Anagram.Grammars.LR.State_Count'Wide_Wide_Image (S));
            end;
         end loop;
      end loop;
      Writer.End_Element (+"states");
   end Write_States;

   File   : Ada.Wide_Wide_Text_IO.File_Type;
--   Empty  : XML.SAX.Attributes.SAX_Attributes;
   Writer : XML.SAX.Pretty_Writers.XML_Pretty_Writer;
   Output : aliased
     XML.SAX.Output_Destinations.Strings.String_Output_Destination;
begin
   Writer.Set_Output_Destination (Output'Unchecked_Access);
   Writer.Start_Document;
   Writer.Start_Element (+"incr");
   Write_Names (Writer);
   Write_Counts (Writer);
   Write_NT (Writer);
   Write_States (Writer);
   Write_Actions (Writer);
   Writer.End_Element (+"incr");
   Writer.End_Document;
   Ada.Wide_Wide_Text_IO.Create (File, Name => Name);
   Ada.Wide_Wide_Text_IO.Put_Line (File, Output.Get_Text.To_Wide_Wide_String);
   Ada.Wide_Wide_Text_IO.Close (File);
end Gen.Write_XML;
