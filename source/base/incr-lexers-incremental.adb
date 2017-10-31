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

package body Incr.Lexers.Incremental is

   procedure Apply_Marking
     (Node      : Nodes.Node_Access;
      Previous  : Version_Trees.Version;
      Reference : Version_Trees.Version);

   procedure Mark_From
     (Token     : Nodes.Tokens.Token_Access;
      Reference : Version_Trees.Version);

   procedure Lexing_Phase (Self : in out Incremental_Lexer);
   pragma Unreferenced (Lexing_Phase);

   function Find_Next_Region
     (Self : access Nodes.Node'Class)
      return Nodes.Tokens.Token_Access;

   -------------------
   -- Apply_Marking --
   -------------------

   procedure Apply_Marking
     (Node      : Nodes.Node_Access;
      Previous  : Version_Trees.Version;
      Reference : Version_Trees.Version)
   is
   begin
      if Node.Is_Token and then Node.Local_Changes (Reference, Previous) then
         declare
            Token  : constant Nodes.Tokens.Token_Access :=
              Nodes.Tokens.Token_Access (Node);
         begin
            --  Handle textual changes.
            Mark_From (Token, Reference);
         end;
      else
         --  Handle structural changes.
         for J in 1 .. Node.Arity loop
            declare
               use type Nodes.Node_Access;

               Now    : constant Nodes.Node_Access := Node.Child (J, Previous);
               Before : constant Nodes.Node_Access :=
                 Node.Child (J, Reference);
               Token  : Nodes.Tokens.Token_Access;
            begin
               if Now /= Before and Before /= null then
                  Token := Before.First_Token (Reference);
                  Mark_From (Token, Reference);
                  Token := Before.Last_Token (Reference)
                    .Next_Token (Reference);
                  Mark_From (Token, Reference);
               end if;
            end;
         end loop;

         --  Recursively process any edits within this subtree.
         if Node.Nested_Changes (Reference, Previous) then
            for J in 1 .. Node.Arity loop
               declare
                  use type Nodes.Node_Access;

                  Now : constant Nodes.Node_Access := Node.Child (J, Previous);
               begin
                  if Now /= null then
                     Apply_Marking (Now, Previous, Reference);
                  end if;
               end;
            end loop;
         end if;
      end if;
   end Apply_Marking;

   ----------------------
   -- Find_Next_Region --
   ----------------------

   function Find_Next_Region
     (Self : access Nodes.Node'Class)
      return Nodes.Tokens.Token_Access
   is
      use type Nodes.Tokens.Token_Access;

      Now : constant Version_Trees.Version := Self.Document.History.Changing;
   begin
      if Self.Is_Token then
         if Self.Document.End_Of_Stream = Self or else
           Self.Get_Flag (Nodes.Need_Analysis)
         then
            return Nodes.Tokens.Token_Access (Self);
         end if;
      elsif Self.Nested_Changes (From => Now, To => Now) then
         return Find_Next_Region (Self.Child (1, Now));
      end if;

      return Find_Next_Region (Self.Next_Subtree (Now));
   end Find_Next_Region;

   ---------------------
   -- First_New_Token --
   ---------------------

   function First_New_Token
     (Self  : in out Incremental_Lexer;
      Token : Nodes.Tokens.Token_Access)
      return Nodes.Tokens.Token_Access
   is
      use type Nodes.Tokens.Token_Access;

      Ref : constant Version_Trees.Version := Self.Reference;
   begin
      --  Reset internal state of batch lexer by setting new source
      Self.Batch.Set_Source (Self'Unchecked_Access);

      if Token = Token.Document.Start_Of_Stream then
         Self.State := Lexers.Batch_Lexers.INITIAL;
      else
         Self.State := Token.Previous_Token (Ref).State (Ref);
      end if;

      Self.Batch.Set_Start_Condition (Self.State);
      --  Self.New_State defined in Next_New_Token

      Self.Prev_Token := (null, null);
      Self.Last_Reused := null;
      Self.Token := Token;
      Self.Count := 0;
      Self.Text := Self.Token.Text (Self.Previous);
      Self.Cursor.First (Self.Text);

      return Next_New_Token (Self);
   end First_New_Token;

   --------------
   -- Get_Next --
   --------------

   overriding function Get_Next
     (Self : not null access Incremental_Lexer)
      return Wide_Wide_Character
   is
      use type Nodes.Tokens.Token_Access;
      Token : Nodes.Tokens.Token_Access;
   begin
      while not Self.Cursor.Has_Element loop
         Self.Count := Self.Count + Self.Token.Text (Self.Previous).Length;
         Self.State := Self.Token.State (Self.Previous);
         Token := Self.Token.Next_Token (Self.Previous);

         if Token = null then
            return Batch_Lexers.End_Of_Input;
         end if;

         if Self.Token /= Self.Last_Reused then
            Self.Prev_Token (1) := Self.Prev_Token (2);
            Self.Prev_Token (2) := Self.Token;
         end if;

         Self.Token := Token;
         Self.Text := Self.Token.Text (Self.Previous);
         Self.Cursor.First (Self.Text);
      end loop;

      return Result : Wide_Wide_Character do
         Result := Self.Cursor.Element;
         Self.Cursor.Next;
      end return;
   end Get_Next;

   ---------------------
   -- Is_Synchronized --
   ---------------------

   function Is_Synchronized (Self : Incremental_Lexer) return Boolean is
      use type Batch_Lexers.State;
      use type Nodes.Tokens.Token_Access;

      Token : constant Nodes.Tokens.Token_Access := Self.Token;
   begin
      if Self.Count /= 0 or Self.State /= Self.New_State then
         return False;
      end if;

      if Token = null then
         return True;
      end if;

      if Token.Get_Flag (Nodes.Need_Analysis) then
         return False;
      end if;

      return True;
   end Is_Synchronized;

   ------------------
   -- Lexing_Phase --
   ------------------

   procedure Lexing_Phase (Self : in out Incremental_Lexer) is
      use type Nodes.Tokens.Token_Access;

      Token : Nodes.Tokens.Token_Access :=
        Find_Next_Region (Self.Document.Ultra_Root);

   begin
      while Token /= Self.Document.End_Of_Stream loop
         Token := First_New_Token (Self, Token);

         while not Is_Synchronized (Self) loop
            Token := Next_New_Token (Self);
         end loop;

         Token := Find_Next_Region (Self.Token);
      end loop;
   end Lexing_Phase;

   ---------------
   -- Mark_From --
   ---------------

   procedure Mark_From
     (Token     : Nodes.Tokens.Token_Access;
      Reference : Version_Trees.Version)
   is
      Next : Nodes.Tokens.Token_Access := Token;
   begin
      if Token.Exists (Reference) then
         for J in 0 .. Token.Lookback (Reference) loop
            Next.Set_Flag (Nodes.Need_Analysis);
            Next := Next.Previous_Token (Reference);
         end loop;
      else
         Token.Set_Flag (Nodes.Need_Analysis);
      end if;
   end Mark_From;

   --------------------
   -- Next_New_Token --
   --------------------

   function Next_New_Token
     (Self : in out Incremental_Lexer)
      return Nodes.Tokens.Token_Access
   is
      function Could_Be_Reused
        (Token : Nodes.Tokens.Token_Access;
         Rule  : Batch_Lexers.Rule_Index) return Boolean;

      ---------------------
      -- Could_Be_Reused --
      ---------------------

      function Could_Be_Reused
        (Token : Nodes.Tokens.Token_Access;
         Rule  : Batch_Lexers.Rule_Index) return Boolean
      is
         use type Nodes.Token_Kind;
         use type Nodes.Tokens.Token_Access;
      begin
         return Token /= null
           and then Token.Kind = Nodes.Token_Kind (Rule)
           and then Token /= Self.Last_Reused;
      end Could_Be_Reused;

      Value  : League.Strings.Universal_String;
      Rule   : Batch_Lexers.Rule_Index;
      Result : Nodes.Tokens.Token_Access;
   begin
      Self.Batch.Get_Token (Rule);
      Value := Self.Batch.Get_Text;
      Self.Count := Self.Count - Value.Length;
      Self.New_State := Self.Batch.Get_Start_Condition;

      if Could_Be_Reused (Self.Prev_Token (1), Rule) then
         Result := Self.Prev_Token (1);
         Result.Set_Text (Value);
         Result.Set_Local_Errors (False);
--           Result.Set_State (Self.New_State);
--           Result.Set_Lookahead (Self.Batch.Get_Token_Lookahead);
         Self.Prev_Token (1) := null;
      elsif Could_Be_Reused (Self.Prev_Token (2), Rule) then
         Result := Self.Prev_Token (2);
         Result.Set_Text (Value);
         Result.Set_Local_Errors (False);
--           Result.Set_State (Self.New_State);
--           Result.Set_Lookahead (Self.Batch.Get_Token_Lookahead);
         Self.Prev_Token := (null, null);
      elsif Could_Be_Reused (Self.Token, Rule) then
         Self.Last_Reused := Self.Token;
         Result := Self.Token;
         Result.Set_Text (Value);
         Result.Set_Local_Errors (False);
--           Result.Set_State (Self.New_State);
--           Result.Set_Lookahead (Self.Batch.Get_Token_Lookahead);
      else
         Result := new Nodes.Tokens.Token (Self.Document);

         Nodes.Tokens.Constructors.Initialize
           (Result.all,
            Nodes.Token_Kind (Rule),
            Value,
            Self.New_State,
            Self.Batch.Get_Token_Lookahead);
      end if;

      return Result;
   end Next_New_Token;

   ----------------------
   -- Prepare_Document --
   ----------------------

   not overriding procedure Prepare_Document
     (Self      : in out Incremental_Lexer;
      Document  : Documents.Document_Access;
      Reference : Version_Trees.Version)
   is
      Now : constant Version_Trees.Version := Document.History.Changing;
   begin
      Self.Document  := Document;
      Self.Reference := Reference;
      Self.Previous  := Document.History.Parent (Now);

      Apply_Marking
        (Self.Document.Ultra_Root,
         Previous  => Self.Previous,
         Reference => Self.Reference);
   end Prepare_Document;

   ---------------------
   -- Set_Batch_Lexer --
   ---------------------

   not overriding procedure Set_Batch_Lexer
     (Self  : in out Incremental_Lexer;
      Lexer : Batch_Lexers.Batch_Lexer_Access) is
   begin
      Self.Batch := Lexer;
   end Set_Batch_Lexer;

   ------------------------
   -- Synchronized_Token --
   ------------------------

   not overriding function Synchronized_Token
     (Self : Incremental_Lexer)
      return Nodes.Tokens.Token_Access is
   begin
      return Self.Token;
   end Synchronized_Token;

end Incr.Lexers.Incremental;
