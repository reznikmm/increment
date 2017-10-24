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

with Ada.Wide_Wide_Text_IO;
with Ada.Characters.Wide_Wide_Latin_1;

with Ada_Outputs;
with League.Strings;

with Gela.Grammars.LR;
with Gela.Grammars.LR_Tables;

procedure Gen.Write_Parser_Data
  (Plain : Gela.Grammars.Grammar;
   Table : Gela.Grammars.LR_Tables.Table)
is

   function "+" (Text : Wide_Wide_String)
                 return League.Strings.Universal_String
                 renames League.Strings.To_Universal_String;

   function AD_Init return Ada_Outputs.Node_Access;
   function SD_Init return Ada_Outputs.Node_Access;
   function CD_Init return Ada_Outputs.Node_Access;
   function NT_Init return Ada_Outputs.Node_Access;

   F : aliased Ada_Outputs.Factory;
   LF : constant Wide_Wide_Character := Ada.Characters.Wide_Wide_Latin_1.LF;

   -------------
   -- AD_Init --
   -------------

   function AD_Init return Ada_Outputs.Node_Access is
      use Gela.Grammars.LR_Tables;
      List   : Ada_Outputs.Node_Access;
      List_2 : Ada_Outputs.Node_Access;
   begin
      for State in 1 .. Last_State (Table) loop
         for Term in 0 .. Plain.Last_Terminal loop
            declare
               Item : Ada_Outputs.Node_Access;
               S    : constant Gela.Grammars.LR.State_Count :=
                 Shift (Table, State, Term);
               R    : constant Reduce_Iterator := Reduce (Table, State, Term);
            begin
               if Term in 0 and Finish (Table, State) then
                  Item := F.New_Name (+"F");
               elsif S not in 0 then
                  Item := F.New_Parentheses
                    (F.New_List
                       ((F.New_Component_Association
                          (Value => F.New_Name (+"S")),
                        F.New_Component_Association
                          (Value => F.New_Literal (Natural (S))))));
               elsif not Is_Empty (R) then
                  Item := F.New_Parentheses
                    (F.New_List
                       ((F.New_Component_Association
                          (Value => F.New_Name (+"R")),
                        F.New_Component_Association
                          (Value => F.New_Literal
                               (Natural (Production (R)))))));
               else
                  Item := F.New_Name (+"E");
               end if;

               List_2 := F.New_List
                 (List_2, F.New_Component_Association (Value   => Item));
            end;
         end loop;

         List := F.New_List
           (List,
            F.New_Component_Association
              (Choices => F.New_Literal (Natural (State)),
               Value   => F.New_Parentheses (List_2)));
         List_2 := null;
      end loop;

      return F.New_Parentheses (List);
   end AD_Init;

   -------------
   -- CD_Init --
   -------------

   function CD_Init return Ada_Outputs.Node_Access is
      use type Gela.Grammars.Part_Count;
      List   : Ada_Outputs.Node_Access;
   begin
      for Prod of Plain.Production loop
         List := F.New_List
           (List,
            F.New_Component_Association
              (Choices => F.New_Literal (Natural (Prod.Index)),
               Value   => F.New_Literal
                 (Natural (Prod.Last - Prod.First + 1))));
      end loop;

      return F.New_Parentheses (List);
   end CD_Init;

   -------------
   -- NT_Init --
   -------------

   function NT_Init return Ada_Outputs.Node_Access is
      List   : Ada_Outputs.Node_Access;
   begin
      for NT of Plain.Non_Terminal loop
         List := F.New_List
           (List,
            F.New_Component_Association
              (Choices => F.New_List
                   (F.New_Literal (Natural (NT.First)),
                    F.New_Infix (+"..", F.New_Literal (Natural (NT.Last)))),
               Value => F.New_Literal
                           (Natural (NT.Index))));
      end loop;

      return F.New_Parentheses (List);
   end NT_Init;

   -------------
   -- SD_Init --
   -------------

   function SD_Init return Ada_Outputs.Node_Access is
      use Gela.Grammars.LR_Tables;
      List   : Ada_Outputs.Node_Access;
      List_2 : Ada_Outputs.Node_Access;
   begin
      for State in 1 .. Last_State (Table) loop
         for NT in 1 .. Plain.Last_Non_Terminal loop
            declare
               S : constant Gela.Grammars.LR.State_Count :=
                 Shift (Table, State, NT);
            begin
               List_2 := F.New_List
                 (List_2,
                  F.New_Component_Association
                    (Choices => F.New_Literal (Natural (NT)),
                     Value   => F.New_Literal (Natural (S))));
            end;
         end loop;

         List := F.New_List
           (List,
            F.New_Component_Association
              (Choices => F.New_Literal (Natural (State)),
               Value   => F.New_Parentheses (List_2)));
         List_2 := null;
      end loop;

      return F.New_Parentheses (List);
   end SD_Init;

   Clause : constant Ada_Outputs.Node_Access := F.New_With
     (F.New_Selected_Name (+"Incr.Nodes.Joints"));

   Name : constant Ada_Outputs.Node_Access :=
     F.New_Selected_Name (+"Incr.Ada_Parser_Data");

   Rename_List : constant Ada_Outputs.Node_Access :=
     F.New_List
       ((F.New_Variable
          (Name            => F.New_Name (+"S"),
           Type_Definition => F.New_Selected_Name (+"P.Action_Kinds"),
           Initialization  => F.New_Selected_Name (+"P.Shift"),
           Is_Constant     => True),
        F.New_Variable
          (Name            => F.New_Name (+"R"),
           Type_Definition => F.New_Selected_Name (+"P.Action_Kinds"),
           Initialization  => F.New_Selected_Name (+"P.Reduce"),
           Is_Constant     => True),
        F.New_Variable
          (Name            => F.New_Name (+"E"),
           Type_Definition => F.New_Selected_Name (+"P.Action"),
           Initialization  => F.New_Parentheses
             (F.New_Component_Association
                  (Choices => F.New_Name (+"Kind"),
                   Value   => F.New_Selected_Name (+"P.Error"))),
           Is_Constant     => True),
        F.New_Variable
          (Name            => F.New_Name (+"F"),
           Type_Definition => F.New_Selected_Name (+"P.Action"),
           Initialization  => F.New_Parentheses
             (F.New_Component_Association
                  (Choices => F.New_Name (+"Kind"),
                   Value   => F.New_Selected_Name (+"P.Finish"))),
           Is_Constant     => True)));

   Action_Data : constant Ada_Outputs.Node_Access :=
     F.New_Variable
       (Name            => F.New_Name (+"Action_Data"),
        Type_Definition => F.New_Selected_Name (+"P.Action_Table"),
        Initialization  => AD_Init,
        Is_Constant     => True,
        Is_Aliased      => True);

   State_Data : constant Ada_Outputs.Node_Access :=
     F.New_Variable
       (Name            => F.New_Name (+"State_Data"),
        Type_Definition => F.New_Selected_Name (+"P.State_Table"),
        Initialization  => SD_Init,
        Is_Constant     => True,
        Is_Aliased      => True);

   Count_Data : constant Ada_Outputs.Node_Access :=
     F.New_Variable
       (Name            => F.New_Name (+"Count_Data"),
        Type_Definition => F.New_Selected_Name (+"P.Parts_Count_Table"),
        Initialization  => CD_Init,
        Is_Constant     => True,
        Is_Aliased      => True);

   NT : constant Ada_Outputs.Node_Access :=
     F.New_Variable
       (Name            => F.New_Name (+"NT"),
        Type_Definition => F.New_Name (+"Node_Kind_Array"),
        Initialization  => NT_Init,
        Is_Constant     => True);

   Self : constant Ada_Outputs.Node_Access :=
     F.New_Parameter
       (Name            => F.New_Name (+"Self"),
        Type_Definition => F.New_Name (+"Provider"));

   Self_Unreferenced : constant Ada_Outputs.Node_Access :=
     F.New_Pragma
       (Name      => F.New_Name (+"Unreferenced"),
        Arguments => F.New_Name (+"Self"));

   Actions : constant Ada_Outputs.Node_Access :=
     F.New_Subprogram_Body
       (F.New_Subprogram_Specification
          (Is_Overriding => True,
           Name          => F.New_Name (+"Actions"),
           Parameters    => Self,
           Result        => F.New_Selected_Name (+"P.Action_Table_Access")),
        Declarations => Self_Unreferenced,
        Statements => F.New_Return (F.New_Name (+"Action_Data'Access")));

   Part_Counts : constant Ada_Outputs.Node_Access :=
     F.New_Subprogram_Body
       (F.New_Subprogram_Specification
          (Is_Overriding => True,
           Name          => F.New_Name (+"Part_Counts"),
           Parameters    => Self,
           Result        => F.New_Selected_Name
                               (+"P.Parts_Count_Table_Access")),
        Declarations => Self_Unreferenced,
        Statements => F.New_Return (F.New_Name (+"Count_Data'Access")));

   States : constant Ada_Outputs.Node_Access :=
     F.New_Subprogram_Body
       (F.New_Subprogram_Specification
          (Is_Overriding => True,
           Name          => F.New_Name (+"States"),
           Parameters    => Self,
           Result        => F.New_Selected_Name
                               (+"P.State_Table_Access")),
        Declarations => Self_Unreferenced,
        Statements => F.New_Return (F.New_Name (+"State_Data'Access")));

   Joint_Access : constant Ada_Outputs.Node_Access := F.New_Selected_Name
     (+"Incr.Nodes.Joints.Joint_Access");

   Statements : constant Ada_Outputs.Node_Access :=
     F.New_List
       ((
        F.New_Assignment
          (Left  => F.New_Name (+"Kind"),
           Right => F.New_Apply
             (Prefix    => F.New_Name (+"NT"),
              Arguments => F.New_Name (+"Prod"))),
        F.New_Assignment
          (Left  => F.New_Name (+"Result"),
           Right => F.New_Apply
             (Prefix    => F.New_Selected_Name
                  (+"new Incr.Nodes.Joints.Joint"),
              Arguments => F.New_List
                (F.New_Argument_Association
                   (F.New_Selected_Name (+"Self.Document")),
                 F.New_Argument_Association
                   (F.New_Name (+"Children'Length"))))),
        F.New_Statement
          (F.New_Apply
             (Prefix    => F.New_Selected_Name
                  (+"Incr.Nodes.Joints.Constructors.Initialize"),
              Arguments => F.New_List
                ((F.New_Argument_Association
                     (F.New_Selected_Name (+"Result.all")),
                  F.New_Argument_Association
                      (F.New_Selected_Name (+"Kind")),
                 F.New_Argument_Association
                     (F.New_Selected_Name (+"Children")))))),
        F.New_Assignment
          (Left  => F.New_Name (+"Node"),
           Right => F.New_Apply
             (Prefix    => F.New_Selected_Name (+"Incr.Nodes.Node_Access"),
              Arguments => F.New_Name (+"Result")))
        ));

   Create_Node : constant Ada_Outputs.Node_Access :=
     F.New_Subprogram_Body
       (F.New_Subprogram_Specification
          (Is_Overriding => True,
           Name          => F.New_Name (+"Create_Node"),
           Parameters    => F.New_List
             ((F.New_Parameter
              (Name            => F.New_Name (+"Self"),
               Type_Definition => F.New_Name (+"Node_Factory"),
               Is_In           => True,
               Is_Out          => True,
               Is_Aliased      => True),
              F.New_Parameter
                (Name            => F.New_Name (+"Prod"),
                 Type_Definition => F.New_Selected_Name
                   (+"P.Production_Index")),
              F.New_Parameter
                (Name            => F.New_Name (+"Children"),
                 Type_Definition => F.New_Selected_Name
                   (+"Incr.Nodes.Node_Array")),
              F.New_Parameter
                (Name            => F.New_Name (+"Node"),
                 Type_Definition => F.New_Selected_Name
                   (+"Incr.Nodes.Node_Access"),
                 Is_Out         => True),
              F.New_Parameter
                (Name            => F.New_Name (+"Kind"),
                 Type_Definition => F.New_Selected_Name
                   (+"Incr.Nodes.Node_Kind"),
                 Is_Out         => True)))),
        Declarations => F.New_Variable
          (Name            => F.New_Name (+"Result"),
           Type_Definition => Joint_Access),
        Statements => Statements);

   Tables : constant Ada_Outputs.Node_Access :=
     F.New_List
       ((F.New_Pragma (F.New_Name (+"Page")),
        Action_Data, State_Data, Count_Data, NT,
        Actions, Part_Counts, States, Create_Node));

   List : constant Ada_Outputs.Node_Access := F.New_List (Rename_List, Tables);

   Root : constant Ada_Outputs.Node_Access :=
     F.New_Package_Body (Name, List);

   Unit : constant Ada_Outputs.Node_Access :=
     F.New_Compilation_Unit (Root, Clause);
begin
   Ada.Wide_Wide_Text_IO.Put_Line
     (F.To_Text (Unit).Join (LF).To_Wide_Wide_String);
end Gen.Write_Parser_Data;
