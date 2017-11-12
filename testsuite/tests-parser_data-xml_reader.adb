--  Copyright (c) 2015-2017 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Tests.Parser_Data.XML_Reader is

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
       renames League.Strings.To_Universal_String;

   Nil_Location : constant XML.Templates.Streams.Event_Location :=
     (League.Strings.Empty_Universal_String, 0, 0);

   ----------------
   -- Characters --
   ----------------

   overriding procedure Characters
     (Self    : in out Reader;
      Text    : League.Strings.Universal_String;
      Success : in out Boolean)
   is
      pragma Unreferenced (Success);
   begin
      if Self.Collect_Text then
         Self.Text.Append (Text);
      end if;
   end Characters;

   -----------------
   -- End_Element --
   -----------------

   overriding procedure End_Element
     (Self           : in out Reader;
      Namespace_URI  : League.Strings.Universal_String;
      Local_Name     : League.Strings.Universal_String;
      Qualified_Name : League.Strings.Universal_String;
      Success        : in out Boolean)
   is
      pragma Unreferenced (Success);
      use type League.Strings.Universal_String;
      use type Incr.Nodes.Node_Kind;
   begin
      if Local_Name = +"names" then
         Self.Data.Names := Self.List;
         Self.List.Clear;
      elsif Local_Name = +"counts" then
         declare
            List : constant League.String_Vectors.Universal_String_Vector :=
              Self.Text.Split (' ', League.Strings.Skip_Empty);
         begin
            Self.Data.Parts := new P.Parts_Count_Table
              (1 .. P.Production_Index (List.Length));

            for J in Self.Data.Parts'Range loop
               Self.Data.Parts (J) := Natural'Wide_Wide_Value
                 (List (Positive (J)).To_Wide_Wide_String);
            end loop;

            Self.Collect_Text := False;
            Self.Text.Clear;
         end;
      elsif Local_Name = +"nt" then
         declare
            List : constant League.String_Vectors.Universal_String_Vector :=
              Self.Text.Split (' ', League.Strings.Skip_Empty);
         begin
            Self.Data.NT := new Node_Kind_Array (Self.Data.Parts'Range);

            for J in 1 .. List.Length loop
               declare
                  Pair : constant League.String_Vectors.Universal_String_Vector
                    := List (J).Split ('-');
               begin
                  for Prod in P.Production_Index'Wide_Wide_Value
                    (Pair (1).To_Wide_Wide_String)
                      .. P.Production_Index'Wide_Wide_Value
                        (Pair (2).To_Wide_Wide_String)
                  loop
                     Self.Data.NT (Prod) :=
                       Self.Data.Max_Term + Incr.Nodes.Node_Kind (J);
                  end loop;
               end;
            end loop;

            Self.Collect_Text := False;
            Self.Text.Clear;
         end;
      elsif Local_Name = +"states" then
         declare
            Last : Positive := 1;
            List : constant League.String_Vectors.Universal_String_Vector :=
              Self.Text.Split (' ', League.Strings.Skip_Empty);
         begin
            for S in Self.Data.States'Range (1) loop
               for NT in Self.Data.States'Range (2) loop
                  Self.Data.States (S, NT) := P.Parser_State'Wide_Wide_Value
                    (List (Last).To_Wide_Wide_String);
                  Last := Last + 1;
               end loop;
            end loop;

            Self.Collect_Text := False;
            Self.Text.Clear;
         end;
      elsif Local_Name = +"actions" then
         declare
            Last : Positive := 1;
            List : constant League.String_Vectors.Universal_String_Vector :=
              Self.Text.Split (' ', League.Strings.Skip_Empty);
         begin
            for S in Self.Data.Actions'Range (1) loop
               for NT in Self.Data.Actions'Range (2) loop
                  declare
                     Item : constant Wide_Wide_String :=
                       List (Last).To_Wide_Wide_String;
                  begin
                     if Item = "F" then
                        Self.Data.Actions (S, NT) := (Kind => P.Finish);
                     elsif Item = "E" then
                        Self.Data.Actions (S, NT) := (Kind => P.Error);
                     elsif Item (1) = 'S' then
                        Self.Data.Actions (S, NT) :=
                          (P.Shift,
                           P.Parser_State'Wide_Wide_Value
                             (Item (2 .. Item'Last)));
                     else
                        Self.Data.Actions (S, NT) :=
                          (P.Reduce,
                           P.Production_Index'Wide_Wide_Value (Item));
                     end if;

                     Last := Last + 1;
                  end;
               end loop;
            end loop;

            Self.Collect_Text := False;
            Self.Text.Clear;
         end;
      elsif Local_Name = +"set-eos-text" then
         Self.Commands.Append ((Commands.Set_EOS_Text, Self.Text));
         Self.Collect_Text := False;
         Self.Text.Clear;
      elsif Local_Name = +"set-token-text" then
         Self.Commands.Append
           ((Commands.Set_Token_Text, Self.Text, Self.Index));
         Self.Collect_Text := False;
         Self.Text.Clear;
      elsif Local_Name = +"dump-tree" then
         Self.Commands.Append ((Commands.Dump_Tree, Self.Vector));
         Self.Collect_XML := False;
         Self.Vector.Clear;
         Self.Collect_Text := False;
         Self.Text.Clear;
      elsif Self.Collect_XML then
         if not Self.Text.Is_Empty then
            if Local_Name.Starts_With ("token") then
               Self.Vector.Append
                 ((Kind     => XML.Templates.Streams.Text,
                   Text     => Self.Text,
                   Location => Nil_Location));
            end if;

            Self.Text.Clear;
         end if;

         Self.Vector.Append
           ((Kind           => XML.Templates.Streams.End_Element,
             Namespace_URI  => Namespace_URI,
             Local_Name     => Local_Name,
             Qualified_Name => Qualified_Name,
             Location       => Nil_Location));
         return;
      end if;
   end End_Element;

   ------------------
   -- Error_String --
   ------------------

   overriding function Error_String
     (Self : Reader) return League.Strings.Universal_String
   is
      pragma Unreferenced (Self);
   begin
      return League.Strings.Empty_Universal_String;
   end Error_String;

   ------------------
   -- Get_Commands --
   ------------------

   function Get_Commands
     (Self : Reader) return Tests.Commands.Command_Vectors.Vector is
   begin
      return Self.Commands;
   end Get_Commands;

   -------------------
   -- Start_Element --
   -------------------

   overriding procedure Start_Element
     (Self           : in out Reader;
      Namespace_URI  : League.Strings.Universal_String;
      Local_Name     : League.Strings.Universal_String;
      Qualified_Name : League.Strings.Universal_String;
      Attributes     : XML.SAX.Attributes.SAX_Attributes;
      Success        : in out Boolean)
   is
      pragma Unreferenced (Success);
      use type League.Strings.Universal_String;
      use type Incr.Nodes.Node_Kind;

      function "-" (Name : Wide_Wide_String)
        return League.Strings.Universal_String;

      function "-" (Name : Wide_Wide_String) return Incr.Nodes.Node_Kind;
      function "-" (Name : Wide_Wide_String) return P.Parser_State;
      function "-" (Name : Wide_Wide_String) return Integer;

      ---------
      -- "-" --
      ---------

      function "-" (Name : Wide_Wide_String)
        return League.Strings.Universal_String
      is
         Local : constant League.Strings.Universal_String := +Name;
      begin
         for J in 1 .. Attributes.Length loop
            if Attributes.Local_Name (J) = Local then
               return Attributes.Value (J);
            end if;
         end loop;

         raise Constraint_Error;
      end "-";

      function "-" (Name : Wide_Wide_String) return Incr.Nodes.Node_Kind is
      begin
         return Incr.Nodes.Node_Kind'Wide_Wide_Value
           ("-" (Name).To_Wide_Wide_String);
      end "-";

      function "-" (Name : Wide_Wide_String) return P.Parser_State is
      begin
         return P.Parser_State'Wide_Wide_Value
           ("-" (Name).To_Wide_Wide_String);
      end "-";

      function "-" (Name : Wide_Wide_String) return Integer is
      begin
         return Integer'Wide_Wide_Value ("-" (Name).To_Wide_Wide_String);
      end "-";

   begin
      if Self.Collect_XML then
         Self.Vector.Append
           ((Kind           => XML.Templates.Streams.Start_Element,
             Namespace_URI  => Namespace_URI,
             Local_Name     => Local_Name,
             Qualified_Name => Qualified_Name,
             Attributes     => Attributes,
             Location       => Nil_Location));
         Self.Text.Clear;
         return;
      end if;

      if Local_Name = +"name" then
         Self.List.Append (-"name");
      elsif Local_Name = +"names" then
         Self.Data.Max_Term := -"term";
         Self.Data.Max_NT := Self.Data.Max_Term + (-"nt");
         Self.List.Clear;
      elsif Local_Name = +"counts" then
         Self.Collect_Text := True;
         Self.Text.Clear;
      elsif Local_Name = +"nt" then
         Self.Collect_Text := True;
         Self.Text.Clear;
      elsif Local_Name = +"states" then
         Self.Data.States := new P.State_Table
           (1 .. -"count", Self.Data.Max_Term + 1 .. Self.Data.Max_NT);
         Self.Collect_Text := True;
         Self.Text.Clear;
      elsif Local_Name = +"actions" then
         Self.Data.Actions := new P.Action_Table
           (Self.Data.States'Range (1), 0 .. Self.Data.Max_NT);
         Self.Collect_Text := True;
         Self.Text.Clear;
      elsif Local_Name = +"commit" then
         Self.Commands.Append ((Kind => Commands.Commit));
      elsif Local_Name = +"set-eos-text" then
         Self.Collect_Text := True;
         Self.Text.Clear;
      elsif Local_Name = +"set-token-text" then
         Self.Index := -"index";
         Self.Collect_Text := True;
         Self.Text.Clear;
      elsif Local_Name = +"dump-tree" then
         Self.Collect_XML := True;
         Self.Collect_Text := True;
         Self.Vector.Clear;
      elsif Local_Name = +"run" then
         Self.Commands.Append ((Kind => Commands.Run));
      end if;
   end Start_Element;

end Tests.Parser_Data.XML_Reader;
