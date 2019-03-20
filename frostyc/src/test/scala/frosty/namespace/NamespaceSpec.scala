// package frosty.namespace._
// 
// import org.scalatest._
// /*
// class NamespaceSpec extends FlatSpec with Matchers {
//   
//   "Namespace tests" should "be reactivated or deleted" in {}
// 
//   /* TODO: reactivate / delete?
//   "Namespace" should "work in the simple artificial example" in {
//     // Symbol `banana` in global namespace
//     // Three packages: `south`, `north`, `foo`,
//     // with `south` and `north` having some colliding symbols, 
//     // and `foo` having some further sub-packages.
//     val root = Namespace(
//       AbsolutePath(),
//       Map("banana" -> AbsolutePath("banana")),
//       Map(
//         "south" -> Namespace(
//            AbsolutePath("south"),
//            Map(
//             "ananas" -> AbsolutePath("south", "ananas"),
//             "apple" -> AbsolutePath("south", "apple")
//            ),
//            Map.empty,
//            Map.empty
//         ),
//         "north" -> Namespace(
//            AbsolutePath("north"),
//            Map(
//             "apple" -> AbsolutePath("north", "apple")
//            ),
//            Map.empty,
//            Map.empty
//         ),
//         "foo" -> Namespace(
//           AbsolutePath("foo"),
//           Map.empty,
//           Map(
//             "bar" -> Namespace(
//               AbsolutePath("foo", "bar"),
//               Map("yeaha" -> AbsolutePath("foo", "bar", "yeaha")),
//               Map.empty,
//               Map.empty
//             )
//           ),
//           Map.empty
//         )
//       ),
//       Map.empty
//     )
//     
//     val v0 = NamespaceView.top(root)
//     val res = List(
//       EnterSubnamespace("north"),
//       ImportGlobal(ImportOne("banana")),
//       ImportGlobal(ImportRelative("foo", ImportRelative("bar", ImportAll))),
//       ImportGlobal(ImportRelative("south", ImportOneRename("ananas", "pineapple"))),
//       ImportGlobal(ImportRelative("foo", ImportOne("bar")))
//     ).foldLeft(v0)((ns, op) => op(ns))
//   }
//   */
// }
