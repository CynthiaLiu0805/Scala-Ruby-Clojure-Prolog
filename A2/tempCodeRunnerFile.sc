TTrue), ULAbs(ULAbs(ULVar(1))), "Erase types of true"),
  // (_ => eraseTypes(STSuc(STZero)),
  //   ULApp(
  //     // Encoding of suc
  //     ULAbs( // lambda n .
  //       ULAbs( // lambda s.
  //         ULAbs( // lambda z.
  //           ULApp(ULVar(1),ULApp(ULApp(ULVar(2),ULVar(1)),ULVar(0)))))), // s (n s z)
  //     // Encoding of zero
  //     ULAbs( // lambda s .
  //       ULAbs( // lambda z .
  //         ULVar(0)))), // z
  //   "Erase types of one"),
  // (_ => eraseTypes(STApp(STAbs(STNat,STVar(0)),STZero)),
  //   ULApp(ULAbs(ULVar(0)), ULAbs(ULAbs(ULVar(0)))),
  //   "Erase ty