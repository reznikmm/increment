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


package body Incr.Nodes.Tokens is

   ------------------
   -- Constructors --
   ------------------

   package body Constructors is

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize
        (Self      : out Token'Class;
         Kind      : Token_Kind;
         Value     : League.Strings.Universal_String;
         State     : Scanner_State;
         Lookahead : Natural)
      is
         Now  : constant Version_Trees.Version :=
           Self.Document.History.Changing;
         Diff : Integer;
      begin
         Self.Kind := Kind;

         Nodes.Constructors.Initialize (Self);
         Versioned_Strings.Initialize
           (Self.Text, League.Strings.Empty_Universal_String);
         Versioned_Naturals.Initialize (Self.Back, 0);
         Versioned_Naturals.Initialize (Self.Ahead, 0);
         Versioned_Naturals.Initialize (Self.States, 0);

         Versioned_Booleans.Set (Self.Exist, True, Now, Diff);  --  UNDELETE??
         Self.Update_Local_Changes (Diff);

         Versioned_Strings.Set (Self.Text, Value, Now, Diff);
         Self.Update_Local_Changes (Diff);

         Versioned_Naturals.Set (Self.Ahead, Lookahead, Now, Diff);
         Self.Update_Local_Changes (Diff);

         Versioned_Naturals.Set (Self.States, Natural (State), Now, Diff);
         Self.Update_Local_Changes (Diff);
      end Initialize;

      ------------------------
      -- Initialize_Ancient --
      ------------------------

      procedure Initialize_Ancient
        (Self    : aliased in out Token'Class;
         Parent  : Node_Access) is
      begin
         Nodes.Constructors.Initialize_Ancient (Self, Parent);
         Versioned_Strings.Initialize
           (Self.Text, League.Strings.Empty_Universal_String);
         Versioned_Naturals.Initialize (Self.Back, 0);
         Versioned_Naturals.Initialize (Self.Ahead, 0);
         Versioned_Naturals.Initialize (Self.States, 0);
      end Initialize_Ancient;

   end Constructors;

   -----------
   -- Arity --
   -----------

   overriding function Arity (Self : Token) return Natural is
      pragma Unreferenced (Self);
   begin
      return 0;
   end Arity;

   -----------
   -- Child --
   -----------

   overriding function Child
     (Self  : Token;
      Index : Positive;
      Time  : Version_Trees.Version) return Node_Access
   is
      pragma Unreferenced (Self, Index, Time);
   begin
      return null;
   end Child;

   --------------
   -- Is_Token --
   --------------

   overriding function Is_Token (Self : Token) return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Token;

   ----------
   -- Kind --
   ----------

   not overriding function Kind (Self : Token) return Token_Kind is
   begin
      return Self.Kind;
   end Kind;

   --------------
   -- Lookback --
   --------------

   not overriding function Lookback
     (Self  : Token;
      Time  : Version_Trees.Version) return Natural is
   begin
      return Versioned_Naturals.Get (Self.Back, Time);
   end Lookback;

   --------------------
   -- Nested_Changes --
   --------------------

   overriding function Nested_Changes
     (Self : Token;
      From : Version_Trees.Version;
      To   : Version_Trees.Version) return Boolean
   is
      pragma Unreferenced (Self, From, To);
   begin
      return False;
   end Nested_Changes;

   ----------------
   -- Next_Token --
   ----------------

   not overriding function Next_Token
     (Self : aliased Token;
      Time : Version_Trees.Version) return Token_Access
   is
      Next_Subtree : Node_Access := Self.Next_Subtree (Time);
      Result       : Token_Access;
   begin
      while Next_Subtree /= null loop
         Result := Next_Subtree.First_Token (Time);

         exit when Result /= null;

         Next_Subtree := Next_Subtree.Next_Subtree (Time);
      end loop;

      return Result;
   end Next_Token;

   --------------------
   -- Previous_Token --
   --------------------

   not overriding function Previous_Token
     (Self : aliased Token;
      Time : Version_Trees.Version) return Token_Access
   is
      Prev_Subtree : Node_Access := Self.Previous_Subtree (Time);
      Result       : Token_Access;
   begin
      while Prev_Subtree /= null loop
         Result := Prev_Subtree.Last_Token (Time);

         exit when Result /= null;

         Prev_Subtree := Prev_Subtree.Previous_Subtree (Time);
      end loop;

      return Result;
   end Previous_Token;

   --------------
   -- Set_Text --
   --------------

   not overriding procedure Set_Text
     (Self  : in out Token;
      Value : League.Strings.Universal_String)
   is
      Now  : constant Version_Trees.Version := Self.Document.History.Changing;
      Diff : Integer;
   begin
      Versioned_Strings.Set (Self.Text, Value, Now, Diff);
      Self.Update_Local_Changes (Diff);
   end Set_Text;

   -----------
   -- State --
   -----------

   not overriding function State
     (Self  : access Token;
      Time  : Version_Trees.Version) return Scanner_State is
   begin
      return Scanner_State
        (Versioned_Naturals.Get (Self.States, Time));
   end State;

   ----------
   -- Text --
   ----------

   not overriding function Text
     (Self  : Token;
      Time  : Version_Trees.Version) return League.Strings.Universal_String is
   begin
      return Versioned_Strings.Get (Self.Text, Time);
   end Text;

end Incr.Nodes.Tokens;
