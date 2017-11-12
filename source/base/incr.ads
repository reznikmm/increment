--  Copyright (c) 2015-2017 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

--  @summary
--  Incremental Analysis Component
--
--  @description
--  This package provides namespace for incremental analysis algorithms
--  and related data structures.
--
--  We try to (re-)implement ideas described by Tim A. Wagner his work
--  "Practical Algorithms for Incremental Software Development Environments"
--
--  For each analysed document we keep a persistent history of its parsing
--  tree changes as a sequence (actually tree) of versions. Each such version
--  represents consistent state. Each node of parsing tree provides a flag
--  to report if nested nodes were changed in given version of the document.
--  This allows us to quickly locate the changed subtrees.
--
package Incr is
   pragma Pure;
end Incr;
