--  Copyright (c) 2015-2017 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with System.Storage_Elements;

function Incr.Nodes.Hash
  (Self : Node_Access) return Ada.Containers.Hash_Type is
begin
   if Self = null then
      return 0;
   end if;

   declare
      Integer : constant System.Storage_Elements.Integer_Address :=
        System.Storage_Elements.To_Integer (Self.all'Address);
   begin
      return Ada.Containers.Hash_Type'Mod (Integer);
   end;
end Incr.Nodes.Hash;

