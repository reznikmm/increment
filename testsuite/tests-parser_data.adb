--  Copyright (c) 2015-2017 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Incr.Nodes.Joints;

package body Tests.Parser_Data is

   -------------
   -- Actions --
   -------------

   overriding function Actions
     (Self : Provider) return P.Action_Table_Access is
   begin
      return P.Action_Table_Access (Self.Actions);
   end Actions;

   package body Constructors is
      function Create
        (Document  : Incr.Documents.Document_Access;
         NT        : Node_Kind_Array;
         Parts     : P.Parts_Count_Table;
         Names     : League.String_Vectors.Universal_String_Vector;
         Max_State : P.Parser_State;
         Max_Term  : Incr.Nodes.Token_Kind) return Provider
      is
         use type Incr.Nodes.Node_Kind;
      begin
         return Result : Provider
           (Document)
         do
            Result.Max_Term := Max_Term;
            Result.Max_NT := Max_Term;
            Result.Names := Names;
            Result.Actions := new P.Action_Table
              (1 .. Max_State, 1 .. Result.Max_NT);
            Result.States := new P.State_Table
              (1 .. Max_State, Max_Term + 1 .. Result.Max_NT);
            Result.NT := new Node_Kind_Array'(NT);
            Result.Parts := new P.Parts_Count_Table'(Parts);
         end return;
      end Create;
   end Constructors;

   -----------------
   -- Create_Node --
   -----------------

   overriding procedure Create_Node
     (Self     : aliased in out Provider;
      Prod     : Incr.Parsers.Incremental.
        Parser_Data_Providers.Production_Index;
      Children : Incr.Nodes.Node_Array;
      Node     : out Incr.Nodes.Node_Access;
      Kind     : out Incr.Nodes.Node_Kind)
   is
      Result : Incr.Nodes.Joints.Joint_Access;
   begin
      Kind := Self.NT (Prod);

      if Children'Length = 0 then
         Node := null;
         return;
      end if;

      Result := new Incr.Nodes.Joints.Joint (Self.Document, Children'Length);
      Incr.Nodes.Joints.Constructors.Initialize (Result.all, Kind, Children);

      Node := Incr.Nodes.Node_Access (Result);
   end Create_Node;

   ----------------
   -- Kind_Image --
   ----------------

   overriding function Kind_Image
     (Self : Provider;
      Kind : Incr.Nodes.Node_Kind) return Wide_Wide_String
   is
      use type Incr.Nodes.Node_Kind;
   begin
      if Kind = 0 then
         return "EOF";
      elsif Positive (Kind) <= Self.Names.Length then
         return Self.Names (Positive (Kind)).To_Wide_Wide_String;
      else
         return "unknown";
      end if;
   end Kind_Image;

   ------------
   -- States --
   ------------

   overriding function States (Self : Provider) return P.State_Table_Access is
   begin
      return P.State_Table_Access (Self.States);
   end States;

   -----------------
   -- Part_Counts --
   -----------------

   overriding function Part_Counts
     (Self : Provider) return P.Parts_Count_Table_Access is
   begin
      return P.Parts_Count_Table_Access (Self.Parts);
   end Part_Counts;

end Tests.Parser_Data;
