package Incr.Nodes.Joints is

   type Joint (Document : Documents.Document_Access;
               Arity    : Natural) is new Node with private;

   type Joint_Access is access all Joint'Class;

   package Constructors is
      procedure Initialize
        (Self     : out Joint'Class;
         Kind     : Node_Kind;
         Children : Node_Array);

      procedure Initialize_Ancient
        (Self   : out Joint'Class;
         Parent : Node_Access);

   end Constructors;

private

   type Versioned_Node_Array is array (Positive range <>) of
     Versioned_Nodes.Container;

   type Joint (Document : Documents.Document_Access;
               Arity    : Natural) is
     new Node_With_Parent (Document) with
   record
      Kind : Node_Kind;
      Kids : Versioned_Node_Array (1 .. Arity);
      NC   : Versioned_Booleans.Container;
   end record;

   overriding function Arity (Self : Joint) return Natural;

   overriding function Is_Token (Self : Joint) return Boolean;

   overriding function Kind (Self : Joint) return Node_Kind;

   overriding function Child
     (Self  : Joint;
      Index : Positive;
      Time  : Version_Trees.Version) return Node_Access;

   overriding procedure Set_Child
     (Self  : in out Joint;
      Index : Positive;
      Value : Node_Access);

   overriding function Nested_Changes
     (Self : Joint;
      From : Version_Trees.Version;
      To   : Version_Trees.Version) return Boolean;

   overriding procedure On_Commit (Self : in out Joint);

end Incr.Nodes.Joints;
