package body Incr.Nodes.Joints is

   ------------------
   -- Constructors --
   ------------------

   package body Constructors is

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize
        (Self     : out Joint'Class;
         Kind     : Node_Kind;
         Children : Node_Array)
      is
         Now  : constant Version_Trees.Version :=
           Self.Document.History.Changing;
         Diff : Integer;
      begin
         Self.Kind := Kind;

         Nodes.Constructors.Initialize (Self);
         Versioned_Booleans.Initialize (Self.NC, False);

         for J in Children'Range loop
            Versioned_Nodes.Initialize (Self.Kids (J), null);
            Versioned_Nodes.Set (Self.Kids (J), Children (J), Now, Diff);
            Self.Update_Local_Changes (Diff);
         end loop;
      end Initialize;

      ------------------------
      -- Initialize_Ancient --
      ------------------------

      procedure Initialize_Ancient
        (Self   : out Joint'Class;
         Parent : Node_Access) is
      begin
         Self.Kind := 1;  --  ???
         Nodes.Constructors.Initialize_Ancient (Self, Parent);
         Versioned_Booleans.Initialize (Self.NC, False);
      end Initialize_Ancient;

   end Constructors;

   -----------
   -- Arity --
   -----------

   overriding function Arity (Self : Joint) return Natural is
   begin
      return Self.Arity;
   end Arity;

   -----------
   -- Child --
   -----------

   overriding function Child
     (Self  : Joint;
      Index : Positive;
      Time  : Version_Trees.Version) return Node_Access is
   begin
      return Versioned_Nodes.Get (Self.Kids (Index), Time);
   end Child;

   --------------
   -- Is_Token --
   --------------

   overriding function Is_Token (Self : Joint) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Token;

   ----------
   -- Kind --
   ----------

   overriding function Kind (Self : Joint) return Node_Kind is
   begin
      return Self.Kind;
   end Kind;

   --------------------
   -- Nested_Changes --
   --------------------

   overriding function Nested_Changes
     (Self : Joint;
      From : Version_Trees.Version;
      To   : Version_Trees.Version) return Boolean
   is
      use type Version_Trees.Version;

      Time : Version_Trees.Version := To;
   begin
      if Self.Document.History.Is_Changing (To) then
         if Self.Nested_Changes > 0 then
            return True;
         elsif Time = From then
            return False;
         end if;

         Time := Self.Document.History.Parent (Time);
      end if;

      loop
         if Versioned_Booleans.Get (Self.NC, Time) then
            return True;
         end if;

         exit when Time = From;

         Time := Self.Document.History.Parent (Time);
      end loop;

      return False;
   end Nested_Changes;

   ---------------
   -- On_Commit --
   ---------------

   overriding procedure On_Commit (Self : in out Joint) is
      Ignore : Integer;
   begin
      Versioned_Booleans.Set
        (Self    => Self.NC,
         Value   => Self.Nested_Changes > 0,
         Time    => Self.Document.History.Changing,
         Changes => Ignore);

      Node_With_Parent (Self).On_Commit;
   end On_Commit;

   ---------------
   -- Set_Child --
   ---------------

   overriding procedure Set_Child
     (Self  : in out Joint;
      Index : Positive;
      Value : Node_Access)
   is
      Diff : Integer;
      Now  : constant Version_Trees.Version :=
        Self.Document.History.Changing;
   begin
      Versioned_Nodes.Set (Self.Kids (Index), Value, Now, Diff);
      Self.Update_Local_Changes (Diff);
   end Set_Child;

end Incr.Nodes.Joints;
