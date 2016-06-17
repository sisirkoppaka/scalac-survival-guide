package guide
import scala.tools.nsc.Mode

object _21_MagicType extends App {
  val g = newGlobal("-usejavacp -Xprint:typer -Ystop-after:typer")
  val code = """
    |package test
    |class Foo[T]
    |class MagicType
    |class C {
    |  def bar$foo: Foo[MagicType] = new Foo
    |  def bar = 42
    |}
  """.stripMargin
  import g._, g.analyzer.Typer
  object p extends g.analyzer.AnalyzerPlugin {
    lazy val Foo = rootMirror.getClassByName(TypeName("test.Foo"))
    val MagicName = TypeName("MagicType").encodedName
    def fooT(t: Type) = appliedType(Foo, t :: Nil)
    override def pluginsTyped(tpe: Type, typer: Typer, tree: Tree, mode: Mode, pt: Type): Type = {
      tree match {
        case Ident(MagicName) =>
          val enclosingClass = typer.context.owner.owner
          val relatedMethodName = typer.context.owner.name.stripSuffix("$foo")
          val sym = enclosingClass.info.decl(relatedMethodName)
          sym match {
            case NoSymbol => tpe
            case _ => sym.info.resultType
          }
        case _ => tpe
      }
    }
  }
  g.analyzer.addAnalyzerPlugin(p)

  println(compile(code, g).assertNoErrors().tree)
}
