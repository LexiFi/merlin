(***************************************************************************)
(*  Copyright (C) 2000-2022 LexiFi SAS. All rights reserved.               *)
(*                                                                         *)
(*  No part of this document may be reproduced or transmitted in any       *)
(*  form or for any purpose without the express permission of LexiFi SAS.  *)
(***************************************************************************)

val stype_of_type: Typedtree.core_type -> Mlfi_types.stype * (int * Location.t * Env.address) list
