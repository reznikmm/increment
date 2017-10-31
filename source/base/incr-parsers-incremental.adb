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
      procedure Recover (LA : out Nodes.Node_Access);
      procedure Recover_2 (LA : out Nodes.Node_Access);

      Stack  : Parser_Stack;
      State  : Parser_State := 1;
      Now    : constant Version_Trees.Version := Document.History.Changing;

      Previous    : constant Version_Trees.Version :=
        Document.History.Parent (Now);
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
         end if;

         State := Next_State (Stack.State (Stack.Top), Kind);
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

      -------------
      -- Recover --
      -------------

      procedure Recover (LA : out Nodes.Node_Access) is
      begin
         if Stack.Top > 1 then
            --  Remove any default reductions from the parse stack
            Right_Breakdown;
         end if;

         Recover_2 (LA);
      end Recover;

      ---------------
      -- Recover_2 --
      ---------------

      procedure Recover_2 (LA : out Nodes.Node_Access) is

         type Offset_Array is array (Positive range <>) of Natural;

         function Is_Valid_Isolation
           (Node   : Nodes.Node_Access;
            Offset : Natural;
            State  : Parser_State) return Boolean;

         function Node_Offset
           (Node : Nodes.Node_Access;
            Time : Version_Trees.Version) return Natural;
         --  Compute offset of Node in given Time

         function Get_Cut
           (Node   : Nodes.Node_Access;
            Offset : Offset_Array;
            Top    : Positive) return Natural;
         --  Compute stack entry corresponding to leading edge of node’s
         --  subtree in the previous version. Returns False if no entry is
         --  so aligned. Top limits stack depth search.

         procedure Isolate (Node : Nodes.Node_Access; Top : Positive);
         --  Resets configuration so parsing can continue.

         procedure Refine (Node : Nodes.Node_Access);
         --  Isolate the argument and recursively recover the subtree that it
         --  roots.

         procedure Discard_Changes_And_Mark_Errors (Node : Nodes.Node_Access);

         Jam_Offset : Natural;

         -------------------------------------
         -- Discard_Changes_And_Mark_Errors --
         -------------------------------------

         procedure Discard_Changes_And_Mark_Errors
           (Node : Nodes.Node_Access) is
         begin
            Node.Discard;

            if Node.Local_Changes (Reference, Now) then
               Node.Set_Local_Errors (True);
            end if;
         end Discard_Changes_And_Mark_Errors;

         -------------
         -- Get_Cut --
         -------------

         function Get_Cut
           (Node   : Nodes.Node_Access;
            Offset : Offset_Array;
            Top    : Positive) return Natural
         is
            Old_Offset : constant Natural := Node_Offset (Node, Previous);
         begin
            for J in 1 .. Top loop
               if Offset (J) > Old_Offset then
                  return 0;
               elsif Offset (J) = Old_Offset then
                  for K in J + 1 .. Top loop
                     if Offset (K) /= Offset (J) then
                        return K - 1;
                     end if;
                  end loop;

                  return J;
               end if;
            end loop;

            return 0;
         end Get_Cut;

         ------------------------
         -- Is_Valid_Isolation --
         ------------------------

         function Is_Valid_Isolation
           (Node   : Nodes.Node_Access;
            Offset : Natural;
            State  : Parser_State) return Boolean
         is
            Old_Offset : constant Natural := Node_Offset (Node, Previous);
         begin
            if Offset /= Old_Offset then
               --  The starting offset of the subtree must be the same in both
               --  the previous and current versions.
               --  I have no idea how this could fail???
               return False;
            elsif Offset > Jam_Offset then
               --  Cannot be to the right of the point where the error was
               --  detected by the parser.
               return False;
            elsif Offset + Node.Span (Nodes.Text_Length, Previous) <=
              Jam_Offset
            then
               --  The ending offset must meet or exceed the detection point.
               return False;
            end if;

            --  Now see if the parser is willing to accept this isolation, as
            --  determined by the shiftability of its root symbol in the
            --  given state.

            return Next_Action (State, Node.Kind).Kind = Shift;
         end Is_Valid_Isolation;

         -------------
         -- Isolate --
         -------------

         procedure Isolate (Node : Nodes.Node_Access; Top : Positive) is
         begin
            Refine (Node);
            State := Stack.State (Top);
            Stack.Top := Top - 1;
            Do_Shift (Node);
            LA := Node.Next_Subtree (Reference);
         end Isolate;

         -----------------
         -- Node_Offset --
         -----------------

         function Node_Offset
           (Node : Nodes.Node_Access;
            Time : Version_Trees.Version) return Natural
         is
            use type Nodes.Node_Access;
            Left   : Nodes.Node_Access := Node.Previous_Subtree (Time);
            Result : Natural := 0;
         begin
            while Left /= null loop
               Result := Result + Left.Span (Nodes.Text_Length, Time);
               Left := Left.Previous_Subtree (Time);
            end loop;

            return Result;
         end Node_Offset;

         ------------
         -- Refine --
         ------------

         procedure Refine (Node : Nodes.Node_Access) is
            procedure Pass_2 (Node : Nodes.Node_Access; Offset : Natural);

            ------------
            -- Pass_2 --
            ------------

            procedure Pass_2 (Node : Nodes.Node_Access; Offset : Natural) is
               Prev  : Natural := Offset;
               Next  : Natural;
               Child : Nodes.Node_Access;
            begin
               for J in 1 .. Node.Arity loop
                  Child := Node.Child (J, Now);
                  --  I doubt getting span in current version will work due to
                  --  its caching in the node.
                  Next := Prev + Child.Span (Nodes.Text_Length, Now);

--                  if Prev <= Jam_Offset and Jam_Offset < Next then
                  Discard_Changes_And_Mark_Errors (Child);
--                  end if;

                  Pass_2 (Child, Next);
                  Prev := Next;
               end loop;
            end Pass_2;

            Old_Offset : constant Natural := Node_Offset (Node, Previous);

         begin
            Node.Discard;
            Node.Set_Local_Errors (False);
            Pass_2 (Node, Old_Offset);
         end Refine;

         use type Nodes.Node_Access;
         Ancestor : Nodes.Node_Access;
         Node : Nodes.Node_Access;
         Cut   : Natural;  --  Stack index or zero
         Offset : Offset_Array (1 .. Stack.Top + 1) := (0, others => <>);
         --  Computed offset of leftmost character not to the left of the
         --  Index-th stack entry. (In current version).
      begin
         for J in 1 .. Stack.Top loop
            Offset (J + 1) := Offset (J) +
              Stack.Node (J).Span (Nodes.Text_Length, Now);
         end loop;

         Jam_Offset := Offset (Offset'Last);

         --  Consider each node on the stack, until we reach bos.
         for J in reverse 1 .. Stack.Top loop
            Node := Stack.Node (J);
            --  If the root of this subtree is new, keep looking down the
            --  parse stack.
            if Node.Exists (Previous) then
               if Is_Valid_Isolation (Node, Offset (J), Stack.State (J)) then
                  Isolate (Node, J);
                  --  Isolate (Node, SP, 1);
                  return;
               end if;

               --  Try searching node's ancestors.
               loop
                  Ancestor := Node.Parent (Previous);
                  exit when Ancestor = Document.Ultra_Root;
                  Cut := Get_Cut (Ancestor, Offset, J);
                  exit when Cut = 0;
                  --  if not chached

                  if Is_Valid_Isolation
                    (Ancestor, Offset (Cut), Stack.State (Cut))
                  then
                     Isolate (Ancestor, Cut);
                     return;
                  end if;

                  Node := Ancestor;
               end loop;
            end if;
         end loop;

         --  Isolate root non-terminal
         Node := Document.Ultra_Root.Child (2, Reference);

         if Stack.Top = 1 then
            Do_Shift (Node);
         end if;

         Isolate (Node, 2);
      end Recover_2;

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

      Verify : Boolean := False;
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
      Push (Stack, State, Nodes.Node_Access (Document.Start_Of_Stream));

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
                     Recover (Nodes.Node_Access (LA));
                  end if;

               when Reduce =>
                  On_Reduce (Next.Prod);

               when Shift =>
                  Verify := False;
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
                  if Verify then
                     Verify := False;
                     Right_Breakdown;
                  else
                     Recover (Nodes.Node_Access (LA));
                  end if;

            end case;

         elsif LA.Nested_Changes (Reference, Now) then
            LA := Left_Breakdown (LA.all'Access);
         else
            Next := Next_Action (State, LA.Kind);
            case Next.Kind is
               when Finish =>
                  raise Program_Error;
               when Reduce =>
                  On_Reduce (Next.Prod);
               when Shift =>
                  Verify := True;
                  On_Shift (State, Next.State, LA);
                  LA := LA.Next_Subtree (Reference);
               when Error =>
                  if LA.Arity > 0 then
                     LA := Left_Breakdown (LA.all'Access);
                  else
                     LA := LA.Next_Subtree (Reference);
                  end if;
            end case;
         end if;
      end loop;
   end Run;

end Incr.Parsers.Incremental;
