--  Copyright (c) 2015-2017 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

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
         Kind      : Node_Kind;
         Value     : League.Strings.Universal_String;
         State     : Scanner_State;
         Lookahead : Natural)
      is
         Now  : constant Version_Trees.Version :=
           Self.Document.History.Changing;
         Diff : Integer := 0;
      begin
         Self.Kind := Kind;

         Nodes.Constructors.Initialize (Self);
         Versioned_Strings.Initialize
           (Self.Text, League.Strings.Empty_Universal_String);
         Versioned_Naturals.Initialize (Self.Back, 1);
         Versioned_Naturals.Initialize (Self.Ahead, 1);
         Versioned_Naturals.Initialize (Self.States, 0);

         Versioned_Booleans.Set (Self.Exist, True, Now, Diff);  --  UNDELETE??
         Versioned_Strings.Set (Self.Text, Value, Now, Diff);
         Versioned_Naturals.Set (Self.Ahead, Lookahead, Now, Diff);
         Versioned_Naturals.Set (Self.States, Natural (State), Now, Diff);
         Self.Update_Local_Changes (Diff);
      end Initialize;

      ------------------------
      -- Initialize_Ancient --
      ------------------------

      procedure Initialize_Ancient
        (Self    : aliased in out Token'Class;
         Parent  : Node_Access;
         Back    : Natural) is
      begin
         Self.Kind := 0;
         Nodes.Constructors.Initialize_Ancient (Self, Parent);
         Versioned_Strings.Initialize
           (Self.Text, League.Strings.Empty_Universal_String);
         Versioned_Naturals.Initialize (Self.Back, Back);
         Versioned_Naturals.Initialize (Self.Ahead, 1);
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

   -------------
   -- Discard --
   -------------

   overriding procedure Discard (Self  : in out Token) is
      Now  : constant Version_Trees.Version := Self.Document.History.Changing;
      Diff : Integer;
   begin
      Versioned_Booleans.Discard (Self.Exist, Now, Diff);
      Versioned_Strings.Discard (Self.Text, Now, Diff);
      Versioned_Naturals.Discard (Self.Back, Now, Diff);
      Versioned_Naturals.Discard (Self.Ahead, Now, Diff);
      Versioned_Naturals.Discard (Self.States, Now, Diff);
      Self.Update_Local_Changes (Diff);
   end Discard;

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

   overriding function Kind (Self : Token) return Node_Kind is
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
      Diff : Integer := 0;
   begin
      Versioned_Strings.Set (Self.Text, Value, Now, Diff);
      Self.Update_Local_Changes (Diff);
   end Set_Text;

   ----------
   -- Span --
   ----------

   overriding function Span
     (Self : aliased in out Token;
      Kind : Span_Kinds;
      Time : Version_Trees.Version) return Natural is
   begin
      case Kind is
         when Text_Length =>
            return Tokens.Text (Self, Time).Length;
         when Token_Count =>
            return 1;
         when Line_Count =>
            return Tokens.Text (Self, Time).Count (LF);
      end case;
   end Span;
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
