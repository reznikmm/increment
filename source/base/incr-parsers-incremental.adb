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
--  $Revision: 449 $ $Date: 2015-12-31 14:45:07 +0200 (Чт, 31 дек 2015) $
------------------------------------------------------------------------------

with League.Strings;

with Incr.Nodes.Tokens;

package body Incr.Parsers.Incremental is

   ---------
   -- Run --
   ---------

   procedure Run
     (Self      : Incremental_Parser;
      Lexer     : Incr.Lexers.Incremental.Incremental_Lexer_Access;
      Provider  : Parser_Data_Providers.Parser_Data_Provider_Access;
      Factory   : Parser_Data_Providers.Node_Factory_Access;
      Document  : Documents.Document_Access;
      Reference : Version_Trees.Version)
   is
      pragma Unreferenced (Self);

      use Parser_Data_Providers;
      use type Nodes.Tokens.Token_Access;
      use type Nodes.Node_Kind;

      Stack_Length : constant := 256;

      type State_Array is array (1 .. Stack_Length) of Parser_State;

      type Parser_Stack is record
         Top   : Natural;
         State : State_Array;
         Node  : Nodes.Node_Array (1 .. Stack_Length);
      end record;

      procedure Clear (Self : out Parser_Stack);

      procedure Push
        (Self  : in out Parser_Stack;
         State : Parser_State;
         Node  : Nodes.Node_Access);

      procedure Pop
        (Self  : in out Parser_Stack;
         State : out Parser_State;
         Node  : out Nodes.Node_Access);

      procedure On_Reduce (Parts : Production_Index);

      procedure On_Shift
        (State     : in out Parser_State;
         New_State : Parser_State;
         Node      : access Nodes.Node'Class);

      procedure Do_Shift (Node : Nodes.Node_Access);

      function Left_Breakdown
        (Node : Nodes.Node_Access) return Nodes.Node_Access;

      procedure Right_Breakdown;

      Stack  : Parser_Stack;
      State  : Parser_State := 1;

      Parent      : constant Version_Trees.Version :=
        Document.History.Parent (Document.History.Changing);
      Next_Action : constant Action_Table_Access := Provider.Actions;
      Next_State  : constant State_Table_Access := Provider.States;
      Counts      : constant Parts_Count_Table_Access := Provider.Part_Counts;

      procedure Clear (Self : out Parser_Stack) is
      begin
         Self.Top := 0;
      end Clear;

      --------------
      -- Do_Shift --
      --------------

      procedure Do_Shift (Node : Nodes.Node_Access) is
      begin
         if Node.Is_Token then
            --  Next_Action should be shift
            On_Shift (State, Next_Action (State, Node.Kind).State, Node);
         else
            On_Shift (State, Next_State (State, Node.Kind), Node);
         end if;
      end Do_Shift;

      --------------------
      -- Left_Breakdown --
      --------------------

      function Left_Breakdown
        (Node : Nodes.Node_Access) return Nodes.Node_Access is
      begin
         if Node.Arity > 0 then
            return Node.Child (1, Reference);
         else
            return Node.Next_Subtree (Reference);
         end if;
      end Left_Breakdown;

      ---------------
      -- On_Reduce --
      ---------------

      procedure On_Reduce (Parts : Production_Index) is
         Count : constant Natural := Counts (Parts);
         subtype First_Nodes is Nodes.Node_Array (1 .. Count);

         Kind : Nodes.Node_Kind;
      begin
         Stack.Top := Stack.Top - Count + 1;

         Factory.Create_Node
           (Parts,
            First_Nodes (Stack.Node (Stack.Top .. Stack.Top + Count - 1)),
            Stack.Node (Stack.Top),
            Kind);

         if Count = 0 then
            Stack.State (Stack.Top) := State;
            State := Next_State (State, Kind);
         else
            State := Next_State (Stack.State (Stack.Top), Kind);
         end if;
      end On_Reduce;

      --------------
      -- On_Shift --
      --------------

      procedure On_Shift
        (State     : in out Parser_State;
         New_State : Parser_State;
         Node      : access Nodes.Node'Class) is
      begin
         Push (Stack, State, Nodes.Node_Access (Node));
         State := New_State;
      end On_Shift;

      ---------
      -- Pop --
      ---------

      procedure Pop
        (Self  : in out Parser_Stack;
         State : out Parser_State;
         Node  : out Nodes.Node_Access) is
      begin
         State := Self.State (Self.Top);
         Node := Self.Node (Self.Top);
         Self.Top := Self.Top - 1;
      end Pop;

      ----------
      -- Push --
      ----------

      procedure Push
        (Self  : in out Parser_Stack;
         State : Parser_State;
         Node  : Nodes.Node_Access) is
      begin
         Self.Top := Self.Top + 1;
         Self.State (Self.Top) := State;
         Self.Node (Self.Top) := Node;
      end Push;

      ---------------------
      -- Right_Breakdown --
      ---------------------

      procedure Right_Breakdown is
         Node  : Nodes.Node_Access;
         Limit : constant Natural := Stack.Top - 1;
      begin
         loop
            Pop (Stack, State, Node);

            exit when Node.Is_Token;

            for J in 1 .. Node.Arity loop
               Do_Shift (Node.Child (J, Reference));
            end loop;

            if Stack.Top = Limit then
               --  Empty subtree was on the top of the stack
               return;
            end if;
         end loop;

         Do_Shift (Node);
      end Right_Breakdown;

      Lexing : Boolean := False;
      EOF    : Boolean := False;
      Term   : Nodes.Tokens.Token_Access;

      LA     : access Nodes.Node'Class :=
        Document.Start_Of_Stream.Next_Subtree (Reference);
      Next   : Action;
   begin
      Document.Start_Of_Stream.Set_Text
        (League.Strings.Empty_Universal_String);
      Document.End_Of_Stream.Set_Text
        (League.Strings.Empty_Universal_String);

      Lexer.Prepare_Document (Document, Reference);
      Clear (Stack);
      Push (Stack, 1, Nodes.Node_Access (Document.Start_Of_Stream));

      loop
         if LA.Is_Token then
            if not Lexing
              and then LA.Get_Flag (Nodes.Need_Analysis)
              and then not EOF
            then
               Term := Lexer.First_New_Token (Nodes.Tokens.Token_Access (LA));
               LA := Term;
               Lexing := True;
            else
               Term := Nodes.Tokens.Token_Access (LA);
            end if;

            Next := Next_Action (State, Term.Kind);

            case Next.Kind is
               when Finish =>
                  if Term.Kind = 0 then  --  End_Of_Stream
                     declare
                        Node  : Nodes.Node_Access;
                     begin
                        Pop (Stack, State, Node);
                        Document.Ultra_Root.Set_Child (2, Node);

                        return;
                     end;
                  else
                     raise Constraint_Error;
                  end if;

               when Reduce =>
                  On_Reduce (Next.Prod);

               when Shift =>
                  On_Shift (State, Next.State, Term);

                  if not Lexing then
                     LA := Term.Next_Subtree (Reference);
                  elsif Lexer.Is_Synchronized then
                     LA := Lexer.Synchronized_Token;
                     Lexing := False;

                     if LA = null then
                        EOF := True;
                        LA := Document.End_Of_Stream;
                     end if;
                  else
                     LA := Lexer.Next_New_Token;
                  end if;

               when Error =>
                  raise Constraint_Error;

            end case;

         elsif LA.Nested_Changes (Reference, Parent) then
            LA := Left_Breakdown (LA.all'Access);
         else
            --  perform_all_reductions_possible ???
            declare
               New_State : constant Parser_State :=
                 Next_State (State, LA.Kind);
            begin
               if New_State /= 0 then
                  On_Shift (State, New_State, Node => LA);
                  Right_Breakdown;
                  LA := LA.Next_Subtree (Reference);
               else
                  LA := Left_Breakdown (LA.all'Access);
               end if;
            end;
         end if;
      end loop;
   end Run;

end Incr.Parsers.Incremental;
